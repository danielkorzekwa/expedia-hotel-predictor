package expedia.model.mdp

import expedia.CompoundHyperParams
import expedia.CompoundHyperParamsMap
import expedia.data.Click
import expedia.data.ExDataSource
import expedia.model.ClusterModelBuilder
import expedia.model.ClusterModelBuilderFactory
import expedia.HyperParamsService
import expedia.util.TimeDecayService
import expedia.stats.MulticlassHistByKey
import expedia.stats.CounterMap
import breeze.numerics._
import expedia.model.marketdest.MarketDestModel
import expedia.model.marketdest.MarketDestModelBuilder2

case class MdpModelBuilder2(marketDestModel: MarketDestModel, timeDecayService: TimeDecayService, hyperParamsService: HyperParamsService) extends ClusterModelBuilder {

  def create(trainDatasource: ExDataSource, testClicks: Seq[Click], hyperParams: CompoundHyperParams): MdpModel = {

    val segmentSizeMap: Map[(Int, Int), Int] = testClicks.groupBy { c => (c.marketId, c.destId) }.map { x => x._1 -> x._2.size }

    //key ((marketId,destId,isPackage)
    val clusterHistByMDP = MulticlassHistByKey[Tuple3[Int, Int, Int]](100)
    testClicks.foreach { click =>
      clusterHistByMDP.add((click.marketId, click.destId, click.isPackage), click.cluster, value = 0)
    }

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

      val key = (click.marketId, click.destId, click.isPackage)
      if (clusterHistByMDP.getMap.contains(key)) {

        val isBookingWeight = hyperParamsService.getParamValueForMarketId("expedia.model.mdp.isBookingWeight", click.marketId, hyperParams).toFloat
        val beta1 = hyperParamsService.getParamValueForMarketId("expedia.model.mdp.beta1", click.marketId, hyperParams).toFloat
        val beta2 = hyperParamsService.getParamValueForMarketId("expedia.model.mdp.beta2", click.marketId, hyperParams).toFloat
        val beta3 = hyperParamsService.getParamValueForMarketId("expedia.model.mdp.beta3", click.marketId, hyperParams).toFloat
        val beta4 = hyperParamsService.getParamValueForMarketId("expedia.model.mdp.beta4", click.marketId, hyperParams).toFloat
        val beta5 = hyperParamsService.getParamValueForMarketId("expedia.model.mdp.beta5", click.marketId, hyperParams).toFloat

        val decayFactor = hyperParamsService.getParamValueForMarketId("expedia.model.mdp.decayFactor", click.marketId, hyperParams).toFloat
        val w = timeDecayService.getDecay(click.dateTime, decayFactor)

        val marketCounts = marketCounterMap.getOrElse(click.marketId, 0)
        val destMarketCounts = destMarketCounterMap.getOrElse((click.destId, click.marketId), 0)
        val destCounts = destCounterMap.getOrElse(click.destId, 0)
        val clickWeight =
          if (destMarketCounts < beta1) beta2
          else if (destMarketCounts < beta3) beta4
          else beta5

        if (click.isBooking == 1) clusterHistByMDP.add(key, click.cluster, value = w * isBookingWeight)
        else clusterHistByMDP.add(key, click.cluster, value = w * clickWeight)
      }
    }
    trainDatasource.foreach { click => onClick(click) }

    /**
     * Build model
     */
    clusterHistByMDP.getMap.foreach {
      case ((marketId, destId, isPackage), clusterCounts) =>

        val beta6 = hyperParamsService.getParamValueForMarketId("expedia.model.mdp.beta6", marketId, hyperParams).toFloat
        val beta7 = hyperParamsService.getParamValueForMarketId("expedia.model.mdp.beta7", marketId, hyperParams).toFloat
        val beta8 = hyperParamsService.getParamValueForMarketId("expedia.model.mdp.beta8", marketId, hyperParams).toFloat
        val segmentSizeWeight = hyperParamsService.getParamValueForMarketId("expedia.model.mdp.segmentSizeWeight", marketId, hyperParams).toFloat

        //
        //        val destMarketCounts = destMarketCounterMap.getOrElse((destId, marketId), 0)
        //        val destCounts = destCounterMap.getOrElse(destId, 0)
        //        val marketCounts = marketCounterMap.getOrElse(marketId, 0)
        //
        //        if (destMarketCounts > 0 && destCounts > 0 && destCounts == destMarketCounts) clusterCounts :+= beta6 * marketModel.predict(marketId)
        //        else if (destMarketCounts > 0 && destCounts > 0 && marketCounts == destMarketCounts) clusterCounts :+= beta7 * destModel.predict(destId)
        //        else clusterCounts :+= beta8 * marketModel.predict(marketId)

        clusterCounts :+= (beta8 + +segmentSizeWeight * log(segmentSizeMap((marketId, destId)).toFloat)) * marketDestModel.predict(marketId, destId)

    }

    //     clusterHistByMDP.getMap.foreach {
    //      case ((marketId, destId, isPackage), clusterCounts) =>
    //
    //       clusterCounts :+= 128f*marketDestModel.predict(marketId, destId)
    //
    //    }

    clusterHistByMDP.normalise()

    MdpModel(clusterHistByMDP)
  }
}
object MdpModelBuilder2 extends ClusterModelBuilderFactory {

  def build(trainDatasource: ExDataSource, testClicks: Seq[Click], modelHyperParamsMap: CompoundHyperParamsMap): MdpModelBuilder2 = {

    val timeDecayService = TimeDecayService(testClicks)
    val hyperParamsService = HyperParamsService(testClicks)

    val marketDestModel = MarketDestModelBuilder2.build(trainDatasource, testClicks, modelHyperParamsMap)
      .create(trainDatasource, testClicks, modelHyperParamsMap.getModel("marketdest"))

    MdpModelBuilder2(marketDestModel,timeDecayService, hyperParamsService)
  }
}