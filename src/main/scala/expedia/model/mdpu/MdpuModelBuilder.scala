package expedia.model.mdpu

import com.typesafe.scalalogging.slf4j.LazyLogging

import breeze.linalg.InjectNumericOps
import expedia.CompoundHyperParams
import expedia.data.Click
import expedia.data.ExDataSource
import expedia.model.country.CountryModelBuilder
import expedia.model.countryuser.CountryUserModelBuilder
import expedia.model.dest.DestModel
import expedia.model.dest.DestModelBuilder
import expedia.model.marketdest.MarketDestModel
import expedia.model.marketdest.MarketDestModelBuilder
import expedia.model.marketdestuser.MarketDestUserPredictionModel
import expedia.model.marketdestuser.MarketDestUserPredictionModelBuilder
import expedia.model.marketmodel.MarketModelBuilder
import expedia.model.marketuser.MarketUserModelBuilder
import expedia.model.mdp.MdpModel
import expedia.model.mdp.MdpModelBuilder
import expedia.stats.CounterMap
import expedia.stats.MulticlassHistByKey
import expedia.util.TimeDecayService

case class MdpuModelBuilder(testClicks: Seq[Click], hyperParams: CompoundHyperParams, timeDecayService: TimeDecayService) extends LazyLogging {

  //key ((marketId,destId,isPackage,userId)
  private val clusterHistByMDPU = MulticlassHistByKey[Tuple4[Int, Int, Int, Int]](100)
  testClicks.foreach { click =>
    val key = (click.marketId, click.destId, click.isPackage, click.userId)
    clusterHistByMDPU.add(key, click.cluster, value = 0)
  }

  private val userCounterMap = CounterMap[Int]()

  def processCluster(click: Click) = {
   
    val key = (click.marketId, click.destId, click.isPackage, click.userId)
    if (clusterHistByMDPU.getMap.contains(key)) {
      
       val beta1 = hyperParams.getParamValueForMarketId("expedia.model.mdpu.beta1", click.marketId).toFloat
    val isBookingWeight = hyperParams.getParamValueForMarketId("expedia.model.mdpu.isBookingWeight", click.marketId).toFloat

    val w = timeDecayService.getDecayForMarketId(click.dateTime,click.marketId)

      
      if (click.isBooking == 1) clusterHistByMDPU.add(key, click.cluster, value = w * isBookingWeight)
      else clusterHistByMDPU.add(key, click.cluster, value = w * beta1)
    }

    userCounterMap.add(click.userId)
  }

  def create(marketDestUserModel: MarketDestUserPredictionModel, marketDestModel: MarketDestModel, mdpModel: MdpModel,
             destCounterMap: CounterMap[Int], destMarketCounterMap: CounterMap[Tuple2[Int, Int]],
             destModel: DestModel): MdpuModel = {

    logger.info("Add prior stats to clusterHistByMDPU...")
    clusterHistByMDPU.getMap.foreach {

      case ((marketId, destId, isPackage, userId), userClusterProbs) =>

        val beta2 = hyperParams.getParamValueForMarketId("expedia.model.mdpu.beta2", marketId).toFloat

        userClusterProbs :+= 150f * (beta2 * mdpModel.predict(marketId, destId, isPackage) + (1 - beta2) * marketDestUserModel.predict(marketId, destId, userId))

    }
    clusterHistByMDPU.normalise()
    logger.info("Add prior stats to clusterHistByMDPU...done")

    MdpuModel(clusterHistByMDPU, userCounterMap, destCounterMap, destMarketCounterMap, destModel)
  }

}

object MdpuModelBuilder {

  def buildFromTrainingSet(trainDatasource: ExDataSource, testClicks: Seq[Click], hyperParams: CompoundHyperParams): MdpuModel = {

    /**
     * Create counters
     */
    val destMarketCounterMap = CounterMap[Tuple2[Int, Int]]
    val destCounterMap = CounterMap[Int]()
    val marketCounterMap = CounterMap[Int]()
    def onClickCounters(click: Click) = {
      if (click.isBooking == 1) {
        destMarketCounterMap.add((click.destId, click.marketId))
        destCounterMap.add(click.destId)
        marketCounterMap.add(click.marketId)
      }
    }
    trainDatasource.foreach { click => onClickCounters(click) }

    val timeDecayService = TimeDecayService(testClicks, hyperParams)

    /**
     * Create models
     */
    val marketModelBuilder = MarketModelBuilder(testClicks, hyperParams, timeDecayService)
    val destModelBuilder = DestModelBuilder(testClicks, hyperParams, timeDecayService)
    val countryModelBuilder = CountryModelBuilder(testClicks, hyperParams, timeDecayService)

    val countryUserModelBuilder = CountryUserModelBuilder(testClicks, hyperParams)

    val marketDestModelBuilder = MarketDestModelBuilder(testClicks, destMarketCounterMap, destCounterMap, marketCounterMap, hyperParams, timeDecayService)

    val marketUserModelBuilder = MarketUserModelBuilder(testClicks, hyperParams, timeDecayService)
    val mdpModelBuilder = MdpModelBuilder(testClicks, destMarketCounterMap, destCounterMap, marketCounterMap, hyperParams, timeDecayService)

    val markerDestUserBuilder = MarketDestUserPredictionModelBuilder(testClicks, hyperParams, timeDecayService)

    val mdpuModelBuilder = MdpuModelBuilder(testClicks, hyperParams, timeDecayService)

    def onClick(click: Click) = {

      marketModelBuilder.processCluster(click)
      destModelBuilder.processCluster(click)
      countryModelBuilder.processCluster(click)
      countryUserModelBuilder.processCluster(click)
      marketDestModelBuilder.processCluster(click)
      marketUserModelBuilder.processCluster(click)
      markerDestUserBuilder.processCluster(click)
      mdpModelBuilder.processCluster(click)
      mdpuModelBuilder.processCluster(click)

    }
    trainDatasource.foreach { click => onClick(click) }

    val countryModel = countryModelBuilder.create()
    val marketModel = marketModelBuilder.create(countryModel)
    val countryUserModel = countryUserModelBuilder.create(countryModel)
    val destModel = destModelBuilder.create(countryModel, null)
    val marketUserModel = marketUserModelBuilder.create(countryUserModel, marketModel)

    val marketDestModel = marketDestModelBuilder.create(
      destModel, marketModel, countryModel, destMarketCounterMap, destCounterMap, marketCounterMap, null, null)

    val mdpModel = mdpModelBuilder.create(destModel, marketModel, countryModel, destMarketCounterMap, destCounterMap, marketCounterMap, marketDestModel)

    val marketDestUserModel = markerDestUserBuilder.create(countryModel, destMarketCounterMap, destCounterMap, marketCounterMap, marketModel,
      countryUserModel, marketDestModel, marketUserModel)

    val mdpuModel = mdpuModelBuilder.create(marketDestUserModel, marketDestModel, mdpModel, destCounterMap, destMarketCounterMap, destModel)
    mdpuModel

  }
}