package expedia.model.dest

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

case class DestModelBuilder(testClicks: Seq[Click], hyperParams: HyperParams, timeDecayService: TimeDecayService) extends LazyLogging {

  private val clusterHistByDest = MulticlassHistByKey[Int](100)
  testClicks.foreach(click => clusterHistByDest.add(click.destId, click.cluster, value = 0))

  private val countryByDest: mutable.Map[Int, Int] = mutable.Map()
  testClicks.foreach(click => countryByDest += click.destId -> click.countryId)

  private val beta1 = hyperParams.getParamValue("expedia.model.dest.beta1").toFloat
  private val beta2 = hyperParams.getParamValue("expedia.model.dest.beta2").toFloat

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

    val w = timeDecayService.getDecay(click.dateTime)

    if (clusterHistByDest.getMap.contains(click.destId)) {

      if (click.isBooking == 1) clusterHistByDest.add(click.destId, click.cluster, value = w)
      else clusterHistByDest.add(click.destId, click.cluster, value = w * beta1)

    }

    destClusterByDestMap.get(click.destId) match {
      case Some(destCluster) => {
        if (click.isBooking == 1) destClusterHistByDestCluster.add(destCluster, click.cluster, value = w)
        else destClusterHistByDestCluster.add(destCluster, click.cluster, value = w * beta1)
      }
      case None => //do nothing
    }

  }

  def create(countryModel: CountryModel): DestModel = {

    destClusterHistByDestCluster.getMap.foreach {
      case (destCluster, clusterCounts) =>
        clusterCounts :+= 1f * countryModel.predict(countryByDestCluster(destCluster))
    }
    destClusterHistByDestCluster.normalise()

    clusterHistByDest.getMap.foreach {
      case (destId, clusterCounts) =>
        destClusterByDestMap.get(destId) match {
          case Some(destCluster) if (destCounterMap.getOrElse(destId, -1) < 2 && destCounterMap.getOrElse(destId, 0) != -1) => {

            if (destClusterHistByDestCluster.getMap.contains(destCluster)) {
              clusterCounts :+= beta2 * destClusterHistByDestCluster.getMap(destCluster)
            } else clusterCounts :+= beta2 * countryModel.predict(countryByDest(destId))

          }
          case _ => {
            clusterCounts :+= 1f * countryModel.predict(countryByDest(destId))
          }
        }

    }
    clusterHistByDest.normalise()

    DestModel(clusterHistByDest)
  }

}

object DestModelBuilder {
  def buildFromTrainingSet(trainDatasource: ExDataSource, testClicks: Seq[Click], hyperParams: HyperParams): DestModel = {

    val timeDecayService = TimeDecayService(testClicks, hyperParams)

    val destModelBuilder = DestModelBuilder(testClicks, hyperParams, timeDecayService)
    val countryModelBuilder = CountryModelBuilder(testClicks, hyperParams, timeDecayService)

    def onClick(click: Click) = {

      destModelBuilder.processCluster(click)
      countryModelBuilder.processCluster(click)
    }
    trainDatasource.foreach { click => onClick(click) }

    val countryModel = countryModelBuilder.create()

    val destModel = destModelBuilder.create(countryModel)

    destModel
  }
}