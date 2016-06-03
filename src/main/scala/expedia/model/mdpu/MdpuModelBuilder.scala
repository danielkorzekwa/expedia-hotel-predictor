package expedia.model.mdpu

import com.typesafe.scalalogging.slf4j.LazyLogging

import breeze.linalg.InjectNumericOps
import expedia.CompoundHyperParams
import expedia.HyperParamsService
import expedia.data.Click
import expedia.model.dest.DestModel
import expedia.model.marketdest.MarketDestModel
import expedia.model.marketdestuser.MarketDestUserPredictionModel
import expedia.model.mdp.MdpModel
import expedia.stats.CounterMap
import expedia.stats.MulticlassHistByKey
import expedia.util.TimeDecayService

case class MdpuModelBuilder(testClicks: Seq[Click], hyperParamsService:HyperParamsService,hyperParams: CompoundHyperParams, timeDecayService: TimeDecayService) extends LazyLogging {

  //key ((marketId,destId,isPackage,userId)
  private val clusterHistByMDPU = MulticlassHistByKey[Tuple4[Int, Int, Int, Int]](100)
  testClicks.foreach { click =>
    val key = (click.marketId, click.destId, click.isPackage, click.userId)
    clusterHistByMDPU.add(key, click.cluster, value = 0)
  }

  private val userCounterMap = CounterMap[Int]()

  
  

}

object MdpuModelBuilder {

//  def buildFromTrainingSet(trainDatasource: ExDataSource, testClicks: Seq[Click], hyperParams: CompoundHyperParams): MdpuModel = {
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
//    /**
//     * Create models
//     */
//    val marketModelBuilder = MarketModelBuilder(testClicks, hyperParams, timeDecayService)
//    val destModelBuilder = DestModelBuilder(testClicks, hyperParams, timeDecayService)
//    val countryModelBuilder = CountryModelBuilder(testClicks, hyperParams, timeDecayService)
//
//    val countryUserModelBuilder = CountryUserModelBuilder(testClicks, hyperParams)
//
//    val marketDestModelBuilder = MarketDestModelBuilder(testClicks, destMarketCounterMap, destCounterMap, marketCounterMap, hyperParams, timeDecayService)
//
//    val marketUserModelBuilder = MarketUserModelBuilder(testClicks, hyperParams, timeDecayService)
//    val mdpModelBuilder = MdpModelBuilder(testClicks, destMarketCounterMap, destCounterMap, marketCounterMap, hyperParams, timeDecayService)
//
//    val markerDestUserBuilder = MarketDestUserPredictionModelBuilder(testClicks, hyperParams, timeDecayService)
//
//    val mdpuModelBuilder = MdpuModelBuilder(testClicks, hyperParams, timeDecayService)
//
//    def onClick(click: Click) = {
//
//      marketModelBuilder.processCluster(click)
//      destModelBuilder.processCluster(click)
//      countryModelBuilder.processCluster(click)
//      countryUserModelBuilder.processCluster(click)
//      marketDestModelBuilder.processCluster(click)
//      marketUserModelBuilder.processCluster(click)
//      markerDestUserBuilder.processCluster(click)
//      mdpModelBuilder.processCluster(click)
//      mdpuModelBuilder.processCluster(click)
//
//    }
//    trainDatasource.foreach { click => onClick(click) }
//
//    val countryModel = countryModelBuilder.create()
//    val marketModel = marketModelBuilder.create(countryModel)
//    val countryUserModel = countryUserModelBuilder.create(countryModel)
//    val destModel = destModelBuilder.create(countryModel, null)
//    val marketUserModel = marketUserModelBuilder.create(countryUserModel, marketModel)
//
//    val marketDestModel = marketDestModelBuilder.create(
//      destModel, marketModel, countryModel, destMarketCounterMap, destCounterMap, marketCounterMap, null, null)
//
//    val mdpModel = mdpModelBuilder.create(destModel, marketModel, countryModel, destMarketCounterMap, destCounterMap, marketCounterMap, marketDestModel)
//
//    val marketDestUserModel = markerDestUserBuilder.create(countryModel, destMarketCounterMap, destCounterMap, marketCounterMap, marketModel,
//      countryUserModel, marketDestModel, marketUserModel)
//
//    val mdpuModel = mdpuModelBuilder.create(marketDestUserModel, marketDestModel, mdpModel, destCounterMap, destMarketCounterMap, destModel)
//    mdpuModel
//
//  }
}