package expedia.model.marketdestcluster

import java.io.File
import scala.collection.Seq
import scala.collection.mutable
import com.typesafe.scalalogging.slf4j.LazyLogging
import breeze.linalg.InjectNumericOps
import breeze.linalg.csvread
import expedia.CompoundHyperParams
import expedia.data.Click
import expedia.data.ExDataSource
import expedia.model.country.CountryModel
import expedia.model.country.CountryModelBuilder
import expedia.model.marketmodel.MarketModel
import expedia.model.marketmodel.MarketModelBuilder
import expedia.stats.CounterMap
import expedia.stats.MulticlassHistByKey
import expedia.util.TimeDecayService
import expedia.HyperParamsService

case class MarketDestClusterModelBuilder(testClicks: Seq[Click],  hyperParamsService: HyperParamsService,hyperParams:CompoundHyperParams, timeDecayService: TimeDecayService) extends LazyLogging {

  val destClusterByDestMat = csvread(new File("c:/perforce/daniel/ex/statistics/clusterByDest_30K.csv"), skipLines = 1)
  val destClusterByDestMap: Map[Int, Int] = (0 until destClusterByDestMat.rows).map { i =>
    val destId = destClusterByDestMat(i, 0).toInt
    val clusterId = destClusterByDestMat(i, 1).toInt
    destId -> clusterId
  }.toMap

  val destCounterMap = CounterMap[Int]()

  private val countryByDestCluster: mutable.Map[Int, Int] = mutable.Map()
  testClicks.foreach { click =>
    if (destClusterByDestMap.contains(click.destId)) countryByDestCluster += destClusterByDestMap(click.destId) -> click.countryId
  }

  private val destClusterHistByMarketDestCluster = MulticlassHistByKey[Tuple2[Int, Int]](100)
  testClicks.foreach { click =>
    if (destClusterByDestMap.contains(click.destId)) {
      val key = (click.marketId, destClusterByDestMap(click.destId))
      destClusterHistByMarketDestCluster.add(key, click.cluster, value = 0)
    }
  }

  def processCluster(click: Click) = {

    if (destClusterByDestMap.contains(click.destId)) countryByDestCluster += destClusterByDestMap(click.destId) -> click.countryId

    if (click.isBooking == 1) {
      destCounterMap.add(click.destId)
    }

    destClusterByDestMap.get(click.destId) match {
      case Some(destCluster) => {
        val key = (click.marketId, destClusterByDestMap(click.destId))
        if (destClusterHistByMarketDestCluster.getMap.contains(key)) {

          val beta1 = hyperParamsService.getParamValueForMarketId("expedia.model.marketdestcluster.beta1", click.marketId,hyperParams).toFloat
          val isBookingWeight = hyperParamsService.getParamValueForMarketId("expedia.model.marketdestcluster.isBookingWeight", click.marketId,hyperParams).toFloat
          val w = timeDecayService.getDecayForMarketId(click.dateTime, click.marketId)

          if (click.isBooking == 1) destClusterHistByMarketDestCluster.add(key, click.cluster, value = w * isBookingWeight)
          else destClusterHistByMarketDestCluster.add(key, click.cluster, value = w * beta1)
        }
      }
      case None => //do nothing
    }

  }

  def create(countryModel: CountryModel, marketModel: MarketModel): MarketDestClusterModel = {

    destClusterHistByMarketDestCluster.getMap.foreach {
      case ((marketId, destCluster), clusterCounts) =>

        val beta2 = hyperParamsService.getParamValueForMarketId("expedia.model.marketdestcluster.beta2", marketId,hyperParams).toFloat

        clusterCounts :+= beta2 * marketModel.predict(marketId)
    }
    destClusterHistByMarketDestCluster.normalise()

    MarketDestClusterModel(destClusterHistByMarketDestCluster, destClusterByDestMap, countryModel)
  }

}

object MarketDestClusterModelBuilder {
//  def buildFromTrainingSet(trainDatasource: ExDataSource, testClicks: Seq[Click], hyperParams: CompoundHyperParams): MarketDestClusterModel = {
//
//    val timeDecayService = TimeDecayService(testClicks, hyperParams)
//
//    val marketDestClusterModelBuilder = MarketDestClusterModelBuilder(testClicks, hyperParams, timeDecayService)
//    val countryModelBuilder = CountryModelBuilder(testClicks, hyperParams, timeDecayService)
//    val marketModelBuilder = MarketModelBuilder(testClicks, hyperParams, timeDecayService)
//    def onClick(click: Click) = {
//
//      marketDestClusterModelBuilder.processCluster(click)
//      countryModelBuilder.processCluster(click)
//      marketModelBuilder.processCluster(click)
//    }
//    trainDatasource.foreach { click => onClick(click) }
//
//    val countryModel = countryModelBuilder.create()
//    val marketModel = marketModelBuilder.create(countryModel)
//    val marketDestClusterModel = marketDestClusterModelBuilder.create(countryModel, marketModel)
//
//    marketDestClusterModel
//  }
}