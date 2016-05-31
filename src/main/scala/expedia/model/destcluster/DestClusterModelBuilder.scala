package expedia.model.destcluster

import scala.collection.Seq
import scala.collection.mutable
import com.typesafe.scalalogging.slf4j.LazyLogging
import breeze.linalg.InjectNumericOps
import expedia.HyperParams
import expedia.data.Click
import expedia.data.ExDataSource
import expedia.model.country.CountryModel
import expedia.model.country.CountryModelBuilder
import expedia.stats.MulticlassHistByKey
import expedia.util.TimeDecayService
import java.io.File
import breeze.linalg._
import expedia.stats.MulticlassHistByKey
import expedia.stats.CounterMap
import expedia.model.marketmodel.MarketModel
import expedia.model.marketmodel.MarketModelBuilder

case class DestClusterModelBuilder(testClicks: Seq[Click], hyperParams: HyperParams, timeDecayService: TimeDecayService) extends LazyLogging {

  private val beta1 = hyperParams.getParamValue("expedia.model.destcluster.beta1").toFloat
   private val beta3 = hyperParams.getParamValue("expedia.model.destcluster.beta3").toFloat

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

    val w = timeDecayService.getDecay(click)

    destClusterByDestMap.get(click.destId) match {
      case Some(destCluster) => {
        if (click.isBooking == 1) destClusterHistByDestCluster.add(destCluster, click.cluster, value = w)
        else destClusterHistByDestCluster.add(destCluster, click.cluster, value = w * beta1)
      }
      case None => //do nothing
    }

  }

  def create(countryModel: CountryModel,marketModel:MarketModel): DestClusterModel = {

    destClusterHistByDestCluster.getMap.foreach {
      case (destCluster, clusterCounts) =>
        clusterCounts :+= beta3 * countryModel.predict(countryByDestCluster(destCluster))
    }
    destClusterHistByDestCluster.normalise()

    DestClusterModel(destClusterHistByDestCluster, destClusterByDestMap,countryModel)
  }

}

object DestClusterModelBuilder {
  def buildFromTrainingSet(trainDatasource: ExDataSource, testClicks: Seq[Click], hyperParams: HyperParams): DestClusterModel = {

    val timeDecayService = TimeDecayService(testClicks, hyperParams)

    val destClusterModelBuilder = DestClusterModelBuilder(testClicks, hyperParams, timeDecayService)
    val countryModelBuilder = CountryModelBuilder(testClicks, hyperParams, timeDecayService)
val marketModelBuilder = MarketModelBuilder(testClicks, hyperParams, timeDecayService)
    def onClick(click: Click) = {

      destClusterModelBuilder.processCluster(click)
      countryModelBuilder.processCluster(click)
      marketModelBuilder.processCluster(click)
    }
    trainDatasource.foreach { click => onClick(click) }

    val countryModel = countryModelBuilder.create()
val marketModel = marketModelBuilder.create(countryModel)
    val destClusterModel = destClusterModelBuilder.create(countryModel,marketModel)

    destClusterModel
  }
}