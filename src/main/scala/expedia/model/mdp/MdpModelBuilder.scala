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