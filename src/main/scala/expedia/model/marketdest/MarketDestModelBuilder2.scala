package expedia.model.marketdest

import expedia.CompoundHyperParams
import expedia.CompoundHyperParamsMap
import expedia.data.Click
import expedia.data.ExDataSource
import expedia.model.ClusterModelBuilder
import expedia.model.ClusterModelBuilderFactory
import expedia.HyperParamsService
import expedia.util.TimeDecayService
import expedia.stats.MulticlassHistByKey
import scala.collection._
import expedia.model.destcluster.DestClusterModelBuilder2
import expedia.model.destcluster.DestClusterModel
import expedia.model.marketdestcluster.MarketDestClusterModel
import expedia.model.marketdestcluster.MarketDestClusterModelBuilder2
import breeze.numerics._
import expedia.stats.CounterMap
import expedia.model.marketmodel.MarketModelBuilder2
import expedia.model.dest.DestModel
import expedia.model.marketmodel.MarketModel
import expedia.model.dest.DestModelBuilder2
import expedia.model.dest.DestModelBuilder2
import expedia.model.dest.DestModelBuilder2

case class MarketDestModelBuilder2(marketModel: MarketModel, destModel: DestModel, marketDestClusterModel: MarketDestClusterModel, timeDecayService: TimeDecayService, hyperParamsService: HyperParamsService) extends ClusterModelBuilder {

  def create(trainDatasource: ExDataSource, testClicks: Seq[Click], hyperParams: CompoundHyperParams): MarketDestModel = {

    val segmentSizeMap: Map[(Int, Int), Int] = testClicks.groupBy { c => (c.marketId, c.destId) }.map { x => x._1 -> x._2.size }

    //key ((marketId,destId)
    val clusterHistByMarketDest = MulticlassHistByKey[Tuple2[Int, Int]](100)
    testClicks.foreach { click =>
      clusterHistByMarketDest.add((click.marketId, click.destId), click.cluster, value = 0)
    }

    val countryByDest: mutable.Map[Int, Int] = mutable.Map()
    testClicks.foreach(click => countryByDest += click.destId -> click.countryId)

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
      if (clusterHistByMarketDest.getMap.contains((click.marketId, click.destId))) {

        val destMarketCountsThreshold1 = hyperParamsService.getParamValueForMarketId("expedia.model.marketdest.destMarketCountsThreshold1", click.marketId, hyperParams).toFloat
        val destMarketCountsThresholdClickWeight1 = hyperParamsService.getParamValueForMarketId("expedia.model.marketdest.destMarketCountsThresholdClickWeight1", click.marketId, hyperParams).toFloat
        val destMarketCountsThreshold2 = hyperParamsService.getParamValueForMarketId("expedia.model.marketdest.destMarketCountsThreshold2", click.marketId, hyperParams).toFloat
        val destMarketCountsThresholdClickWeight2 = hyperParamsService.getParamValueForMarketId("expedia.model.marketdest.destMarketCountsThresholdClickWeight2", click.marketId, hyperParams).toFloat
        val destMarketCountsDefaultWeight = hyperParamsService.getParamValueForMarketId("expedia.model.marketdest.destMarketCountsDefaultWeight", click.marketId, hyperParams).toFloat

        val marketCounts = marketCounterMap.getOrElse(click.marketId, 0)
        val destMarketCounts = destMarketCounterMap.getOrElse((click.destId, click.marketId), 0)
        val destCounts = destCounterMap.getOrElse(click.destId, 0)
        val clickWeight =
          if (destMarketCounts < destMarketCountsThreshold1) destMarketCountsThresholdClickWeight1
          else if (destMarketCounts < destMarketCountsThreshold2) destMarketCountsThresholdClickWeight2
          else destMarketCountsDefaultWeight

        val decayFactor = hyperParamsService.getParamValueForMarketId("expedia.model.marketdest.decayFactor", click.marketId, hyperParams).toFloat
        val w = timeDecayService.getDecay(click.dateTime, decayFactor)
        val isBookingWeight = hyperParamsService.getParamValueForMarketId("expedia.model.marketdest.isBookingWeight", click.marketId, hyperParams).toFloat

        if (click.isBooking == 1) clusterHistByMarketDest.add((click.marketId, click.destId), click.cluster, value = w * isBookingWeight)
        else clusterHistByMarketDest.add((click.marketId, click.destId), click.cluster, value = w * clickWeight)
      }
    }
    trainDatasource.foreach { click => onClick(click) }

    /**
     * Build model
     */

    clusterHistByMarketDest.getMap.foreach {
      case ((marketId, destId), clusterCounts) =>

        val beta1 = hyperParamsService.getParamValueForMarketId("expedia.model.marketdest.beta1", marketId, hyperParams).toFloat
        val beta2 = hyperParamsService.getParamValueForMarketId("expedia.model.marketdest.beta2", marketId, hyperParams).toFloat
        val beta3 = hyperParamsService.getParamValueForMarketId("expedia.model.marketdest.beta3", marketId, hyperParams).toFloat
        val beta4 = hyperParamsService.getParamValueForMarketId("expedia.model.marketdest.beta4", marketId, hyperParams).toFloat
        val segmentSizeWeight = hyperParamsService.getParamValueForMarketId("expedia.model.marketdest.segmentSizeWeight", marketId, hyperParams).toFloat

        val destMarketCounts = destMarketCounterMap.getOrElse((destId, marketId), 0)
        val destCounts = destCounterMap.getOrElse(destId, 0)
        val marketCounts = marketCounterMap.getOrElse(marketId, 0)

        if (destMarketCounts > 0 && destCounts > 0 && destCounts == destMarketCounts) {
          if (marketDestClusterModel.predictionExists(marketId, destId) && destCounterMap.getOrElse(destId, -1) < 2 && destCounterMap.getOrElse(destId, 0) != -1) {
            clusterCounts :+= (beta4 + segmentSizeWeight * log(segmentSizeMap((marketId, destId)).toFloat)) * marketDestClusterModel.predict(marketId, destId)
          } else clusterCounts :+= (beta1 + segmentSizeWeight * log(segmentSizeMap((marketId, destId)).toFloat)) * marketModel.predict(marketId)
        } else if (destMarketCounts > 0 && destCounts > 0 && marketCounts == destMarketCounts) clusterCounts :+= {
          (beta2 + segmentSizeWeight * log(segmentSizeMap((marketId, destId)).toFloat)) * destModel.predict(destId)
        }
        else {
          if (marketDestClusterModel.predictionExists(marketId, destId) && destCounterMap.getOrElse(destId, -1) < 2 && destCounterMap.getOrElse(destId, 0) != -1)
            clusterCounts :+= (beta4 + segmentSizeWeight * log(segmentSizeMap((marketId, destId)).toFloat)) * marketDestClusterModel.predict(marketId, destId)
          else clusterCounts :+= (beta3 + segmentSizeWeight * log(segmentSizeMap((marketId, destId)).toFloat)) * marketModel.predict(marketId)
        }

    }
    clusterHistByMarketDest.normalise()

    MarketDestModel(clusterHistByMarketDest)
  }
}
object MarketDestModelBuilder2 extends ClusterModelBuilderFactory {

  def build(trainDatasource: ExDataSource, testClicks: Seq[Click], modelHyperParamsMap: CompoundHyperParamsMap): MarketDestModelBuilder2 = {

    val timeDecayService = TimeDecayService(testClicks)
    val hyperParamsService = HyperParamsService(testClicks)

    val marketModel = MarketModelBuilder2.build(trainDatasource, testClicks, modelHyperParamsMap)
      .create(trainDatasource, testClicks, modelHyperParamsMap.getModel("market"))

    val destModel = DestModelBuilder2.build(trainDatasource, testClicks, modelHyperParamsMap)
      .create(trainDatasource, testClicks, modelHyperParamsMap.getModel("dest"))

    val marketDestClusterModel = MarketDestClusterModelBuilder2.build(trainDatasource, testClicks, modelHyperParamsMap).
      create(trainDatasource, testClicks, modelHyperParamsMap.getModel("marketdestcluster"))

    MarketDestModelBuilder2(marketModel, destModel, marketDestClusterModel, timeDecayService, hyperParamsService)
  }
}