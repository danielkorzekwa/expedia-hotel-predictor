package expedia.model.mdp

import breeze.linalg.InjectNumericOps
import breeze.numerics.log
import expedia.CompoundHyperParams
import expedia.HyperParamsService
import expedia.data.Click
import expedia.data.ExDataSource
import expedia.model.country.CountryModel
import expedia.model.dest.DestModel
import expedia.model.marketdest.MarketDestModel
import expedia.model.marketmodel.MarketModel
import expedia.stats.CounterMap
import expedia.stats.MulticlassHistByKey
import expedia.util.TimeDecayService

case class MdpModelBuilder(testClicks: Seq[Click],
                           destMarketCounterMap: CounterMap[Tuple2[Int, Int]],
                           destCounterMap: CounterMap[Int], marketCounterMap: CounterMap[Int], hyperParamsService:HyperParamsService,hyperParams: CompoundHyperParams, timeDecayService: TimeDecayService) {

  private val segmentSizeMap: Map[(Int, Int), Int] = testClicks.groupBy { c => (c.marketId, c.destId) }.map { x => x._1 -> x._2.size }

  //key ((marketId,destId,isPackage)
  private val clusterHistByMDP = MulticlassHistByKey[Tuple3[Int, Int, Int]](100)
  testClicks.foreach { click =>
    clusterHistByMDP.add((click.marketId, click.destId, click.isPackage), click.cluster, value = 0)
  }

  def processCluster(click: Click) = {

  

    val key = (click.marketId, click.destId, click.isPackage)
    if (clusterHistByMDP.getMap.contains(key)) {
      
        val isBookingWeight = hyperParamsService.getParamValueForMarketId("expedia.model.mdp.isBookingWeight", click.marketId,hyperParams).toFloat
    val beta1 = hyperParamsService.getParamValueForMarketId("expedia.model.mdp.beta1", click.marketId,hyperParams).toFloat
    val beta2 = hyperParamsService.getParamValueForMarketId("expedia.model.mdp.beta2", click.marketId,hyperParams).toFloat
    val beta3 = hyperParamsService.getParamValueForMarketId("expedia.model.mdp.beta3", click.marketId,hyperParams).toFloat
    val beta4 = hyperParamsService.getParamValueForMarketId("expedia.model.mdp.beta4", click.marketId,hyperParams).toFloat
    val beta5 = hyperParamsService.getParamValueForMarketId("expedia.model.mdp.beta5", click.marketId,hyperParams).toFloat

    val w = timeDecayService.getDecayForMarketId(click.dateTime,click.marketId)

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

  def create(destModel: DestModel, marketModel: MarketModel, countryModel: CountryModel, destMarketCounterMap: CounterMap[Tuple2[Int, Int]],
             destCounterMap: CounterMap[Int], marketCounterMap: CounterMap[Int],
             marketDestModel: MarketDestModel): MdpModel = {

    clusterHistByMDP.getMap.foreach {
      case ((marketId, destId, isPackage), clusterCounts) =>

        val beta6 = hyperParamsService.getParamValueForMarketId("expedia.model.mdp.beta6", marketId,hyperParams).toFloat
        val beta7 = hyperParamsService.getParamValueForMarketId("expedia.model.mdp.beta7", marketId,hyperParams).toFloat
        val beta8 = hyperParamsService.getParamValueForMarketId("expedia.model.mdp.beta8", marketId,hyperParams).toFloat
        val segmentSizeWeight = hyperParamsService.getParamValueForMarketId("expedia.model.marketdest.segmentSizeWeight", marketId,hyperParams).toFloat

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

object MdpModelBuilder {

//  def buildFromTrainingSet(trainDatasource: ExDataSource, testClicks: Seq[Click], hyperParams: CompoundHyperParams): MdpModel = {
//
//    /**
//     * Create counters
//     */
//    val destMarketCounterMap = CounterMap[Tuple2[Int, Int]]
//    val destCounterMap = CounterMap[Int]()
//    val marketCounterMap = CounterMap[Int]()
//    def onClickCounters(click: Click) = {
//      if (click.isBooking == 1) {
//        destMarketCounterMap.add((click.destId, click.marketId))
//        destCounterMap.add(click.destId)
//        marketCounterMap.add(click.marketId)
//      }
//    }
//    trainDatasource.foreach { click => onClickCounters(click) }
//
//    val timeDecayService = TimeDecayService(testClicks, hyperParams)
//
//    val countryModelBuilder = CountryModelBuilder(testClicks, hyperParams, timeDecayService)
//    val destModelBuilder = DestModelBuilder(testClicks, hyperParams, timeDecayService)
//    val marketModelBuilder = MarketModelBuilder(testClicks, hyperParams, timeDecayService)
//    val marketDestModelBuilder = MarketDestModelBuilder(testClicks, destMarketCounterMap, destCounterMap, marketCounterMap, hyperParams, timeDecayService)
//
//    val mdpModelBuilder = MdpModelBuilder(testClicks, destMarketCounterMap, destCounterMap, marketCounterMap, hyperParams, timeDecayService)
//
//    def onClick(click: Click) = {
//      destModelBuilder.processCluster(click)
//      marketModelBuilder.processCluster(click)
//      countryModelBuilder.processCluster(click)
//      marketDestModelBuilder.processCluster(click)
//      mdpModelBuilder.processCluster(click)
//    }
//    trainDatasource.foreach { click => onClick(click) }
//
//    val countryModel = countryModelBuilder.create()
//    val destModel = destModelBuilder.create(countryModel, null)
//
//    val marketModel = marketModelBuilder.create(countryModel)
//    val marketDestModel = marketDestModelBuilder.create(
//      destModel, marketModel, countryModel, destMarketCounterMap, destCounterMap, marketCounterMap, null, null)
//
//    val mdpModel = mdpModelBuilder.create(destModel, marketModel, countryModel, destMarketCounterMap, destCounterMap, marketCounterMap, marketDestModel)
//
//    mdpModel
//  }

}