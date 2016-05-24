package expedia.model.mdpu

import com.typesafe.scalalogging.slf4j.LazyLogging
import expedia.data.Click
import expedia.data.ExDataSource
import expedia.model.country.CountryModelBuilder
import expedia.model.countryuser.CountryUserModelBuilder
import expedia.model.dest.DestModelBuilder
import expedia.model.marketdest.MarketDestModelBuilder
import expedia.model.marketdestuser.MarketDestUserPredictionModel
import expedia.model.marketdestuser.MarketDestUserPredictionModelBuilder
import expedia.model.marketmodel.MarketModelBuilder
import expedia.model.marketuser.MarketUserModelBuilder
import expedia.model.regdest.RegDestModelBuilder
import expedia.stats.CounterMap
import expedia.stats.MulticlassHistByKey
import expedia.model.marketdest.MarketDestModel
import expedia.model.mdp.MdpModel
import expedia.model.mdp.MdpModelBuilder
import expedia.model.dest.DestModel

case class MdpuModelBuilder(testClicks: Seq[Click]) extends LazyLogging {

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
      if (click.isBooking == 1) clusterHistByMDPU.add(key, click.cluster)
      else clusterHistByMDPU.add(key, click.cluster, value = 0.6f)
    }
    
      userCounterMap.add(click.userId)
  }

  def create(marketDestUserModel: MarketDestUserPredictionModel, marketDestModel: MarketDestModel, mdpModel: MdpModel,
       destCounterMap: CounterMap[Int], destMarketCounterMap: CounterMap[Tuple2[Int, Int]],
      destModel: DestModel): MdpuModel = {

    logger.info("Add prior stats to clusterHistByMDPU...")
    val beta = 0.1f
    clusterHistByMDPU.getMap.foreach {

      case ((marketId, destId, isPackage, userId), userClusterProbs) =>
        userClusterProbs :+= 150f * (beta * mdpModel.predict(marketId, destId, isPackage) + (1 - beta) * marketDestUserModel.predict(marketId, destId, userId))

    }
    clusterHistByMDPU.normalise()
    logger.info("Add prior stats to clusterHistByMDPU...done")

    MdpuModel(clusterHistByMDPU,userCounterMap,destCounterMap,destMarketCounterMap,destModel)
  }

}

object MdpuModelBuilder {

  def buildFromTrainingSet(trainDatasource: ExDataSource, testClicks: Seq[Click]): MdpuModel = {

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

    
    
    /**
     * Create models
     */
    val marketModelBuilder = MarketModelBuilder(testClicks)
    val destModelBuilder = DestModelBuilder(testClicks)
    val countryModelBuilder = CountryModelBuilder(testClicks)
    val regDestModelBuilder = RegDestModelBuilder()

    val countryUserModelBuilder = CountryUserModelBuilder(testClicks)

    val marketDestModelBuilder = MarketDestModelBuilder(testClicks, destMarketCounterMap, destCounterMap, marketCounterMap)

    val marketUserModelBuilder = MarketUserModelBuilder(testClicks)
    val mdpModelBuilder = MdpModelBuilder(testClicks, destMarketCounterMap, destCounterMap, marketCounterMap)

    val markerDestUserBuilder = MarketDestUserPredictionModelBuilder(testClicks)

    val mdpuModelBuilder = MdpuModelBuilder(testClicks)

    def onClick(click: Click) = {

      marketModelBuilder.processCluster(click)
      destModelBuilder.processCluster(click)
      countryModelBuilder.processCluster(click)
      regDestModelBuilder.processCluster(click)
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
    val regDestModel = regDestModelBuilder.create()
    val countryUserModel = countryUserModelBuilder.create(countryModel)
    val destModel = destModelBuilder.create(countryModel)
    val marketUserModel = marketUserModelBuilder.create(countryUserModel, marketModel)

    val marketDestModel = marketDestModelBuilder.create(destModel, marketModel, countryModel, destMarketCounterMap, destCounterMap, marketCounterMap)

    val mdpModel = mdpModelBuilder.create(destModel, marketModel, countryModel, destMarketCounterMap, destCounterMap, marketCounterMap)

    val marketDestUserModel = markerDestUserBuilder.create(destModel, countryModel, destMarketCounterMap, destCounterMap, marketCounterMap, regDestModel, marketModel,
      countryUserModel, marketDestModel, marketUserModel)

    val mdpuModel = mdpuModelBuilder.create(marketDestUserModel, marketDestModel, mdpModel,destCounterMap,destMarketCounterMap,destModel)
    mdpuModel

  }
}