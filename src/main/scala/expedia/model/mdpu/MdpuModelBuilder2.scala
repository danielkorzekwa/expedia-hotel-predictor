package expedia.model.mdpu

import expedia.CompoundHyperParams
import expedia.CompoundHyperParamsMap
import expedia.data.Click
import expedia.data.ExDataSource
import expedia.model.ClusterModelBuilder
import expedia.model.ClusterModelBuilderFactory
import expedia.HyperParamsService
import expedia.util.TimeDecayService
import expedia.stats.MulticlassHistByKey
import com.typesafe.scalalogging.slf4j.LazyLogging
import expedia.stats.CounterMap
import expedia.model.mdp.MdpModel
import expedia.model.mdp.MdpModelBuilder2
import expedia.model.marketdestuser.MarketDestUserPredictionModel
import expedia.model.dest.DestModel
import expedia.model.marketdest.MarketDestModelBuilder2
import expedia.model.dest.DestModelBuilder2
import expedia.model.dest.DestModelBuilder2
import expedia.model.mdp.MdpModelBuilder2
import expedia.model.marketdestuser.MarketDestUserModelBuilder

case class MdpuModelBuilder2(marketDestUserModel: MarketDestUserPredictionModel, mdpModel: MdpModel, destModel: DestModel, timeDecayService: TimeDecayService, hyperParamsService: HyperParamsService) extends ClusterModelBuilder with LazyLogging {

  def create(trainDatasource: ExDataSource, testClicks: Seq[Click], hyperParams: CompoundHyperParams): MdpuModel = {

    //key ((marketId,destId,isPackage,userId)
    val clusterHistByMDPU = MulticlassHistByKey[Tuple4[Int, Int, Int, Int]](100)
    testClicks.foreach { click =>
      val key = (click.marketId, click.destId, click.isPackage, click.userId)
      clusterHistByMDPU.add(key, click.cluster, value = 0)
    }

    val userCounterMap = CounterMap[Int]()

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
     * Process training set
     */
    def onClick(click: Click) = {

      val key = (click.marketId, click.destId, click.isPackage, click.userId)
      if (clusterHistByMDPU.getMap.contains(key)) {

        val beta1 = hyperParamsService.getParamValueForMarketId("expedia.model.mdpu.beta1", click.marketId, hyperParams).toFloat
        val isBookingWeight = hyperParamsService.getParamValueForMarketId("expedia.model.mdpu.isBookingWeight", click.marketId, hyperParams).toFloat

        val decayFactor = hyperParamsService.getParamValueForMarketId("expedia.model.mdpu.decayFactor", click.marketId, hyperParams).toFloat
        val w = timeDecayService.getDecay(click.dateTime, decayFactor)

        if (click.isBooking == 1) clusterHistByMDPU.add(key, click.cluster, value = w * isBookingWeight)
        else clusterHistByMDPU.add(key, click.cluster, value = w * beta1)
      }

      userCounterMap.add(click.userId)
    }
    trainDatasource.foreach { click => onClick(click) }

    /**
     * Build model
     */
    logger.info("Add prior stats to clusterHistByMDPU...")
    clusterHistByMDPU.getMap.foreach {

      case ((marketId, destId, isPackage, userId), userClusterProbs) =>

        val beta2 = hyperParamsService.getParamValueForMarketId("expedia.model.mdpu.beta2", marketId, hyperParams).toFloat
          val beta3 = hyperParamsService.getParamValueForMarketId("expedia.model.mdpu.beta3", marketId, hyperParams).toFloat

        userClusterProbs :+= beta3 * (beta2 * mdpModel.predict(marketId, destId, isPackage) + (1 - beta2) * marketDestUserModel.predict(marketId, destId, userId))

    }
    clusterHistByMDPU.normalise()
    logger.info("Add prior stats to clusterHistByMDPU...done")

    MdpuModel(clusterHistByMDPU, userCounterMap, destCounterMap, destMarketCounterMap, destModel)
  }
}
object MdpuModelBuilder2 extends ClusterModelBuilderFactory {

  def build(trainDatasource: ExDataSource, testClicks: Seq[Click], modelHyperParamsMap: CompoundHyperParamsMap): MdpuModelBuilder2 = {

    val timeDecayService = TimeDecayService(testClicks)
    val hyperParamsService = HyperParamsService(testClicks)

    val marketDestUserModel = MarketDestUserModelBuilder.build(trainDatasource, testClicks, modelHyperParamsMap)
      .create(trainDatasource, testClicks, modelHyperParamsMap.getModel("marketdestuser"))

    val mdpModel = MdpModelBuilder2.build(trainDatasource, testClicks, modelHyperParamsMap)
      .create(trainDatasource, testClicks, modelHyperParamsMap.getModel("mdp"))

    val destModel = DestModelBuilder2.build(trainDatasource, testClicks, modelHyperParamsMap)
      .create(trainDatasource, testClicks, modelHyperParamsMap.getModel("dest"))

    MdpuModelBuilder2(marketDestUserModel, mdpModel, destModel, timeDecayService, hyperParamsService)
  }
}