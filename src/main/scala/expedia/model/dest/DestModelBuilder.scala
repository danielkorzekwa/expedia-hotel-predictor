package expedia.model.dest

import scala.collection.Seq
import scala.collection.mutable
import com.typesafe.scalalogging.slf4j.LazyLogging
import breeze.linalg.InjectNumericOps
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
import expedia.model.destcluster.DestClusterModel
import expedia.model.destcluster.DestClusterModelBuilder
import expedia.CompoundHyperParams
import expedia.HyperParamsService

case class DestModelBuilder(testClicks: Seq[Click],  hyperParamsService: HyperParamsService,hyperParams:CompoundHyperParams, timeDecayService: TimeDecayService) extends LazyLogging {

  private val clusterHistByDest = MulticlassHistByKey[Int](100)
  testClicks.foreach(click => clusterHistByDest.add(click.destId, click.cluster, value = 0))

  private val countryByDest: mutable.Map[Int, Int] = mutable.Map()
  testClicks.foreach(click => countryByDest += click.destId -> click.countryId)

  val destCounterMap = CounterMap[Int]()

  def processCluster(click: Click) = {

    if (click.isBooking == 1) {
      destCounterMap.add(click.destId)
    }

    if (clusterHistByDest.getMap.contains(click.destId)) {
      val isBookingWeight = hyperParamsService.getParamValueForDestId("expedia.model.dest.isBookingWeight", click.destId,hyperParams).toFloat
      val beta1 = hyperParamsService.getParamValueForDestId("expedia.model.dest.beta1", click.destId,hyperParams).toFloat

      val w = timeDecayService.getDecayForDestId(click.dateTime, click.destId)

      if (click.isBooking == 1) clusterHistByDest.add(click.destId, click.cluster, value = w * isBookingWeight)
      else clusterHistByDest.add(click.destId, click.cluster, value = w * beta1)

    }

  }

  def create(countryModel: CountryModel, destClusterModel: DestClusterModel): DestModel = {

    clusterHistByDest.getMap.foreach {
      case (destId, clusterCounts) =>
        val beta2 = hyperParamsService.getParamValueForDestId("expedia.model.dest.beta2", destId,hyperParams).toFloat

        if (destClusterModel.predictionExists(destId) && destCounterMap.getOrElse(destId, -1) < 2 && destCounterMap.getOrElse(destId, 0) != -1) {
          clusterCounts :+= beta2 * destClusterModel.predict(destId)
        } else {
          clusterCounts :+= beta2 * countryModel.predict(countryByDest(destId))
        }

    }
    clusterHistByDest.normalise()

    DestModel(clusterHistByDest)
  }

}

object DestModelBuilder {
//  def buildFromTrainingSet(trainDatasource: ExDataSource, testClicks: Seq[Click], hyperParams: CompoundHyperParams): DestModel = {
//
//    val timeDecayService = TimeDecayService(testClicks, hyperParams)
//
//    val destClusterModelBuilder = DestClusterModelBuilder(testClicks, hyperParams, timeDecayService)
//    val destModelBuilder = DestModelBuilder(testClicks, hyperParams, timeDecayService)
//    val countryModelBuilder = CountryModelBuilder(testClicks, hyperParams, timeDecayService)
//
//    def onClick(click: Click) = {
//      destClusterModelBuilder.processCluster(click)
//      destModelBuilder.processCluster(click)
//      countryModelBuilder.processCluster(click)
//    }
//    trainDatasource.foreach { click => onClick(click) }
//
//    val countryModel = countryModelBuilder.create()
//    val destClusterModel = destClusterModelBuilder.create(countryModel, null)
//    val destModel = destModelBuilder.create(countryModel, destClusterModel)
//
//    destModel
//  }
}