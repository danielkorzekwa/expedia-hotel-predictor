package expedia.model.dest

import scala.collection.Seq
import scala.collection.mutable

import breeze.linalg.InjectNumericOps
import expedia.CompoundHyperParams
import expedia.CompoundHyperParamsMap
import expedia.HyperParamsService
import expedia.data.Click
import expedia.data.ExDataSource
import expedia.model.ClusterModelBuilder
import expedia.model.ClusterModelBuilderFactory
import expedia.model.country.CountryModel
import expedia.model.country.CountryModelBuilder2
import expedia.model.destcluster.DestClusterModel
import expedia.model.destcluster.DestClusterModelBuilder2
import expedia.stats.CounterMap
import expedia.stats.MulticlassHistByKey
import expedia.util.TimeDecayService

case class DestModelBuilder2(countryModel: CountryModel, destClusterModel: DestClusterModel, timeDecayService: TimeDecayService, hyperParamsService: HyperParamsService) extends ClusterModelBuilder {

  def create(trainDatasource: ExDataSource, testClicks: Seq[Click], hyperParams: CompoundHyperParams): DestModel = {

    val clusterHistByDest = MulticlassHistByKey[Int](100)
    testClicks.foreach(click => clusterHistByDest.add(click.destId, click.cluster, value = 0))

    val countryByDest: mutable.Map[Int, Int] = mutable.Map()
    testClicks.foreach(click => countryByDest += click.destId -> click.countryId)

    val destCounterMap = CounterMap[Int]()

    /**
     * Process training set
     */
    def onClick(click: Click) = {
      if (click.isBooking == 1) {
        destCounterMap.add(click.destId)
      }

      if (clusterHistByDest.getMap.contains(click.destId)) {
        val isBookingWeight = hyperParamsService.getParamValueForDestId("expedia.model.dest.isBookingWeight", click.destId, hyperParams).toFloat
        val beta1 = hyperParamsService.getParamValueForDestId("expedia.model.dest.beta1", click.destId, hyperParams).toFloat

        val decayFactor = hyperParamsService.getParamValueForDestId("expedia.model.dest.decayFactor", click.destId, hyperParams).toFloat
        val w = timeDecayService.getDecay(click.dateTime, decayFactor)

        if (click.isBooking == 1) clusterHistByDest.add(click.destId, click.cluster, value = w * isBookingWeight)
        else clusterHistByDest.add(click.destId, click.cluster, value = w * beta1)

      }
    }
    trainDatasource.foreach { click => onClick(click) }

    /**
     * Build model
     */
    clusterHistByDest.getMap.foreach {
      case (destId, clusterCounts) =>
        val beta2 = hyperParamsService.getParamValueForDestId("expedia.model.dest.beta2", destId, hyperParams).toFloat

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
object DestModelBuilder2 extends ClusterModelBuilderFactory {

  def build(trainDatasource: ExDataSource, testClicks: Seq[Click], modelHyperParamsMap: CompoundHyperParamsMap): DestModelBuilder2 = {

    val countryModel = CountryModelBuilder2.build(trainDatasource, testClicks, modelHyperParamsMap).
      create(trainDatasource, testClicks, modelHyperParamsMap.getModel("country"))

    val destClusterModel = DestClusterModelBuilder2.build(trainDatasource, testClicks, modelHyperParamsMap).
      create(trainDatasource, testClicks, modelHyperParamsMap.getModel("destcluster"))

    val timeDecayService = TimeDecayService(testClicks)
    val hyperParamsService = HyperParamsService(testClicks)
    DestModelBuilder2(countryModel, destClusterModel, timeDecayService, hyperParamsService)
  }
}