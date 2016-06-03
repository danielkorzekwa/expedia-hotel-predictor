package expedia.model.country

import expedia.model.ClusterModelBuilder
import expedia.data.Click
import expedia.model.ClusterModelBuilderFactory
import expedia.CompoundHyperParams
import expedia.data.ExDataSource
import expedia.CompoundHyperParamsMap
import expedia.HyperParamsService
import expedia.util.TimeDecayService
import expedia.stats.MulticlassHistByKey
import scala.collection._
import sun.misc.PerformanceLogger.TimeData
import expedia.util.TimeDecayService
import expedia.HyperParamsService

case class CountryModelBuilder2(timeDecayService: TimeDecayService, hyperParamsService: HyperParamsService) extends ClusterModelBuilder {

  def create(trainDatasource: ExDataSource, testClicks: Seq[Click], hyperParams: CompoundHyperParams): CountryModel = {

    val clusterHistByContinent = MulticlassHistByKey[Int](100)
    testClicks.foreach(click => clusterHistByContinent.add(click.continentId, click.cluster, value = 0))

    val clusterHistByCountry = MulticlassHistByKey[Int](100)
    testClicks.foreach(click => clusterHistByCountry.add(click.countryId, click.cluster, value = 0))

    val continentByCountry: mutable.Map[Int, Int] = mutable.Map()
    testClicks.foreach(click => continentByCountry += click.countryId -> click.continentId)

    /**
     * Process training set
     */
    def onClick(click: Click) = {

      clusterHistByContinent.add(click.continentId, click.cluster)

      if (clusterHistByCountry.getMap.contains(click.countryId)) {
        val isBookingWeight = hyperParamsService.getParamValueForCountryId("expedia.model.country.isBookingWeight", click.countryId, hyperParams).toFloat
        val beta1 = hyperParamsService.getParamValueForCountryId("expedia.model.country.beta1", click.countryId, hyperParams).toFloat
        val decayFactor = hyperParamsService.getParamValueForCountryId("expedia.model.country.decayFactor", click.countryId, hyperParams).toFloat
        val w = timeDecayService.getDecay(click.dateTime, decayFactor)
        if (click.isBooking == 1) clusterHistByCountry.add(click.countryId, click.cluster, value = w * isBookingWeight)
        else clusterHistByCountry.add(click.countryId, click.cluster, value = w * beta1)
      }
      continentByCountry += click.countryId -> click.continentId

    }
    trainDatasource.foreach { click => onClick(click) }

    /**
     * Build model
     */
    clusterHistByContinent.normalise()

    clusterHistByCountry.getMap.foreach {
      case (countryId, clusterCounts) =>
        val beta2 = hyperParamsService.getParamValueForCountryId("expedia.model.country.beta2", countryId, hyperParams).toFloat
        clusterCounts :+= beta2 * clusterHistByContinent.getMap(continentByCountry(countryId))
    }
    clusterHistByCountry.normalise()

    CountryModel(clusterHistByCountry)
  }
}
object CountryModelBuilder2 extends ClusterModelBuilderFactory {

  def build(trainDatasource: ExDataSource, testClicks: Seq[Click], modelHyperParamsMap: CompoundHyperParamsMap): CountryModelBuilder2 = {

    val timeDecayService = TimeDecayService(testClicks)
    val hyperParamsService = HyperParamsService(testClicks)

    apply(timeDecayService, hyperParamsService)
  }

 
}