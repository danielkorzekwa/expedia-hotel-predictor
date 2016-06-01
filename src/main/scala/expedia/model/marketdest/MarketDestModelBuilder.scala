package expedia.model.marketdest

import scala.collection.Seq
import scala.collection.mutable
import breeze.linalg.InjectNumericOps
import expedia.HyperParams
import expedia.data.Click
import expedia.data.ExDataSource
import expedia.model.country.CountryModel
import expedia.model.country.CountryModelBuilder
import expedia.model.dest.DestModel
import expedia.model.dest.DestModelBuilder
import expedia.model.marketmodel.MarketModel
import expedia.model.marketmodel.MarketModelBuilder
import expedia.stats.CounterMap
import expedia.stats.MulticlassHistByKey
import expedia.util.TimeDecayService
import expedia.model.destcluster.DestClusterModel
import expedia.model.destcluster.DestClusterModelBuilder
import expedia.model.marketdestcluster.MarketDestClusterModel
import expedia.model.marketdestcluster.MarketDestClusterModelBuilder
import breeze.numerics._
case class MarketDestModelBuilder(testClicks: Seq[Click],
                                  destMarketCounterMap: CounterMap[Tuple2[Int, Int]],
                                  destCounterMap: CounterMap[Int], marketCounterMap: CounterMap[Int],
                                  hyperParams: HyperParams, timeDecayService: TimeDecayService) {

  private val segmentSizeMap: Map[(Int, Int), Int] = testClicks.groupBy { c => (c.marketId, c.destId) }.map { x => x._1 -> x._2.size }

  //key ((marketId,destId)
  private val clusterHistByMarketDest = MulticlassHistByKey[Tuple2[Int, Int]](100)
  testClicks.foreach { click =>
    clusterHistByMarketDest.add((click.marketId, click.destId), click.cluster, value = 0)
  }

  private val countryByDest: mutable.Map[Int, Int] = mutable.Map()
  testClicks.foreach(click => countryByDest += click.destId -> click.countryId)

  private val destMarketCountsThreshold1 = hyperParams.getParamValue("expedia.model.marketdest.destMarketCountsThreshold1").toFloat
  private val destMarketCountsThresholdClickWeight1 = hyperParams.getParamValue("expedia.model.marketdest.destMarketCountsThresholdClickWeight1").toFloat
  private val destMarketCountsThreshold2 = hyperParams.getParamValue("expedia.model.marketdest.destMarketCountsThreshold2").toFloat
  private val destMarketCountsThresholdClickWeight2 = hyperParams.getParamValue("expedia.model.marketdest.destMarketCountsThresholdClickWeight2").toFloat
  private val destMarketCountsDefaultWeight = hyperParams.getParamValue("expedia.model.marketdest.destMarketCountsDefaultWeight").toFloat

  private val beta1 = hyperParams.getParamValue("expedia.model.marketdest.beta1").toFloat
  private val beta2 = hyperParams.getParamValue("expedia.model.marketdest.beta2").toFloat
  private val beta3 = hyperParams.getParamValue("expedia.model.marketdest.beta3").toFloat
  private val beta4 = hyperParams.getParamValue("expedia.model.marketdest.beta4").toFloat
  private val segmentSizeWeight = hyperParams.getParamValue("expedia.model.marketdest.segmentSizeWeight").toFloat

  private val isBookingWeight = hyperParams.getParamValue("expedia.model.marketdest.isBookingWeight").toFloat

  def processCluster(click: Click) = {

    val w = timeDecayService.getDecay(click)

    val marketCounts = marketCounterMap.getOrElse(click.marketId, 0)
    val destMarketCounts = destMarketCounterMap.getOrElse((click.destId, click.marketId), 0)
    val destCounts = destCounterMap.getOrElse(click.destId, 0)
    val clickWeight =
      if (destMarketCounts < destMarketCountsThreshold1) destMarketCountsThresholdClickWeight1
      else if (destMarketCounts < destMarketCountsThreshold2) destMarketCountsThresholdClickWeight2
      else destMarketCountsDefaultWeight

    if (clusterHistByMarketDest.getMap.contains((click.marketId, click.destId))) {
      if (click.isBooking == 1) clusterHistByMarketDest.add((click.marketId, click.destId), click.cluster, value = w * isBookingWeight)
      else clusterHistByMarketDest.add((click.marketId, click.destId), click.cluster, value = w * clickWeight)
    }

  }

  def create(destModel: DestModel, marketModel: MarketModel, countryModel: CountryModel, destMarketCounterMap: CounterMap[Tuple2[Int, Int]],
             destCounterMap: CounterMap[Int], marketCounterMap: CounterMap[Int],
             destClusterModel: DestClusterModel, marketDestClusterModel: MarketDestClusterModel): MarketDestModel = {

    clusterHistByMarketDest.getMap.foreach {
      case ((marketId, destId), clusterCounts) =>

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

object MarketDestModelBuilder {
  def buildFromTrainingSet(trainDatasource: ExDataSource, testClicks: Seq[Click], hyperParams: HyperParams): MarketDestModel = {

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

    val countryModelBuilder = CountryModelBuilder(testClicks, hyperParams, timeDecayService)
    val destModelBuilder = DestModelBuilder(testClicks, hyperParams, timeDecayService)
    val marketModelBuilder = MarketModelBuilder(testClicks, hyperParams, timeDecayService)
    val marketDestModelBuilder = MarketDestModelBuilder(testClicks, destMarketCounterMap, destCounterMap, marketCounterMap, hyperParams, timeDecayService)

    val destClusterModelBuilder = DestClusterModelBuilder(testClicks, hyperParams, timeDecayService)
    val marketDestClusterModelBuilder = MarketDestClusterModelBuilder(testClicks, hyperParams, timeDecayService)

    def onClick(click: Click) = {
      destModelBuilder.processCluster(click)
      marketModelBuilder.processCluster(click)
      countryModelBuilder.processCluster(click)
      marketDestModelBuilder.processCluster(click)
      destClusterModelBuilder.processCluster(click)
      marketDestClusterModelBuilder.processCluster(click)
    }
    trainDatasource.foreach { click => onClick(click) }

    val countryModel = countryModelBuilder.create()
    val destClusterModel = destClusterModelBuilder.create(countryModel, null)

    val destModel = destModelBuilder.create(countryModel, destClusterModel)

    val marketModel = marketModelBuilder.create(countryModel)
    val marketDestClusterModel = marketDestClusterModelBuilder.create(countryModel, marketModel)
    val marketDestModel = marketDestModelBuilder.create(
      destModel, marketModel, countryModel, destMarketCounterMap, destCounterMap, marketCounterMap, destClusterModel, marketDestClusterModel)

    marketDestModel
  }
}