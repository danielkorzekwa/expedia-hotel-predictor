package expedia.model.country

import scala.collection.Seq
import scala.collection.mutable
import breeze.linalg.InjectNumericOps
import expedia.data.Click
import expedia.data.ExDataSource
import expedia.stats.MulticlassHistByKey
import expedia.util.TimeDecayService
import expedia.CompoundHyperParams
import expedia.HyperParamsService

case class CountryModelBuilder(testClicks: Seq[Click], hyperParamsService: HyperParamsService,hyperParams:CompoundHyperParams, timeDecayService: TimeDecayService) {

  private val clusterHistByContinent = MulticlassHistByKey[Int](100)
  testClicks.foreach(click => clusterHistByContinent.add(click.continentId, click.cluster, value = 0))

  private val clusterHistByCountry = MulticlassHistByKey[Int](100)
  testClicks.foreach(click => clusterHistByCountry.add(click.countryId, click.cluster, value = 0))

  private val continentByCountry: mutable.Map[Int, Int] = mutable.Map()
  testClicks.foreach(click => continentByCountry += click.countryId -> click.continentId)

  def processCluster(click: Click) = {

    clusterHistByContinent.add(click.continentId, click.cluster)

    if (clusterHistByCountry.getMap.contains(click.countryId)) {
      val isBookingWeight = hyperParamsService.getParamValueForCountryId("expedia.model.country.isBookingWeight", click.countryId,hyperParams).toFloat
      val beta1 = hyperParamsService.getParamValueForCountryId("expedia.model.country.beta1", click.countryId,hyperParams).toFloat
      val w = timeDecayService.getDecayForCountryId(click.dateTime,click.countryId)
      if (click.isBooking == 1) clusterHistByCountry.add(click.countryId, click.cluster, value = w * isBookingWeight)
      else clusterHistByCountry.add(click.countryId, click.cluster, value = w * beta1)
    }
    continentByCountry += click.countryId -> click.continentId
  }

  def create(): CountryModel = {

    clusterHistByContinent.normalise()

    clusterHistByCountry.getMap.foreach {
      case (countryId, clusterCounts) =>
        val beta2 = hyperParamsService.getParamValueForCountryId("expedia.model.country.beta2", countryId,hyperParams).toFloat
        clusterCounts :+= beta2 * clusterHistByContinent.getMap(continentByCountry(countryId))
    }
    clusterHistByCountry.normalise()

    CountryModel(clusterHistByCountry)
  }
}

object CountryModelBuilder {
//  def buildFromTrainingSet(trainDatasource: ExDataSource, testClicks: Seq[Click], modelHyperParamsMap: Map[String, CompoundHyperParams]): CountryModel = {
//
//    val hyperParamsService = HyperParamsService(testClicks)
//    
//    val timeDecayService = TimeDecayService(testClicks, hyperParamsService,hyperParams)
//
//    val countryModelBuilder = CountryModelBuilder(testClicks, hyperParams, timeDecayService)
//
//    def onClick(click: Click) = {
//      countryModelBuilder.processCluster(click)
//    }
//    trainDatasource.foreach { click => onClick(click) }
//
//    val countryModel = countryModelBuilder.create()
//
//    countryModel
//  }
}