package expedia.model.destcluster

import java.io.File

import scala.collection.Seq
import scala.collection.mutable

import com.typesafe.scalalogging.slf4j.LazyLogging

import breeze.linalg.InjectNumericOps
import breeze.linalg.csvread
import expedia.CompoundHyperParams
import expedia.HyperParamsService
import expedia.data.Click
import expedia.model.country.CountryModel
import expedia.model.marketmodel.MarketModel
import expedia.stats.CounterMap
import expedia.stats.MulticlassHistByKey
import expedia.util.TimeDecayService

case class DestClusterModelBuilder(testClicks: Seq[Click],  hyperParamsService: HyperParamsService,hyperParams:CompoundHyperParams, timeDecayService: TimeDecayService) extends LazyLogging {

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

  private val destClusterHistByDestCluster = MulticlassHistByKey[Int](100)

  def processCluster(click: Click) = {

    if (destClusterByDestMap.contains(click.destId)) countryByDestCluster += destClusterByDestMap(click.destId) -> click.countryId

    if (click.isBooking == 1) {
      destCounterMap.add(click.destId)
    }

    destClusterByDestMap.get(click.destId) match {
      case Some(destCluster) => {

        if (destClusterHistByDestCluster.getMap.contains(destCluster)) {
          val isBookingWeight = hyperParamsService.getParamValueForDestId("expedia.model.destcluster.isBookingWeight", click.destId,hyperParams).toFloat
          val beta1 = hyperParamsService.getParamValueForDestId("expedia.model.destcluster.beta1", click.destId,hyperParams).toFloat
          val w = timeDecayService.getDecayForDestId(click.dateTime, click.destId)

          if (click.isBooking == 1) destClusterHistByDestCluster.add(destCluster, click.cluster, value = w * isBookingWeight)
          else destClusterHistByDestCluster.add(destCluster, click.cluster, value = w * beta1)
        }
      }
      case None => //do nothing
    }

  }

  def create(countryModel: CountryModel, marketModel: MarketModel): DestClusterModel = {

    destClusterHistByDestCluster.getMap.foreach {
      case (destCluster, clusterCounts) =>

        val beta3 = hyperParamsService.getParamValueForCountryId("expedia.model.destcluster.beta3", countryByDestCluster(destCluster),hyperParams).toFloat

        clusterCounts :+= beta3 * countryModel.predict(countryByDestCluster(destCluster))
    }
    destClusterHistByDestCluster.normalise()

    DestClusterModel(destClusterHistByDestCluster, destClusterByDestMap, countryModel)
  }

}

object DestClusterModelBuilder {
//  def buildFromTrainingSet(trainDatasource: ExDataSource, testClicks: Seq[Click], hyperParams: CompoundHyperParams): DestClusterModel = {
//
//    val timeDecayService = TimeDecayService(testClicks, hyperParams)
//
//    val destClusterModelBuilder = DestClusterModelBuilder(testClicks, hyperParams, timeDecayService)
//    val countryModelBuilder = CountryModelBuilder(testClicks, hyperParams, timeDecayService)
//    val marketModelBuilder = MarketModelBuilder(testClicks, hyperParams, timeDecayService)
//    def onClick(click: Click) = {
//
//      destClusterModelBuilder.processCluster(click)
//      countryModelBuilder.processCluster(click)
//      marketModelBuilder.processCluster(click)
//    }
//    trainDatasource.foreach { click => onClick(click) }
//
//    val countryModel = countryModelBuilder.create()
//    val marketModel = marketModelBuilder.create(countryModel)
//    val destClusterModel = destClusterModelBuilder.create(countryModel, marketModel)
//
//    destClusterModel
//  }
}