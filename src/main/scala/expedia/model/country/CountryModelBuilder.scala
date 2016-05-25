package expedia.model.country

import scala.collection.Seq
import scala.collection.mutable

import breeze.linalg.InjectNumericOps
import expedia.HyperParams
import expedia.data.Click
import expedia.data.ExDataSource
import expedia.stats.MulticlassHistByKey
import expedia.util.TimeDecayService

case class CountryModelBuilder(testClicks: Seq[Click], hyperParams: HyperParams,timeDecayService:TimeDecayService){

  private val clusterHistByContinent = MulticlassHistByKey[Int](100)
  testClicks.foreach(click => clusterHistByContinent.add(click.continentId, click.cluster, value = 0))

  private val clusterHistByCountry = MulticlassHistByKey[Int](100)
  testClicks.foreach(click => clusterHistByCountry.add(click.countryId, click.cluster, value = 0))

  private val continentByCountry: mutable.Map[Int, Int] = mutable.Map()
  testClicks.foreach(click => continentByCountry += click.countryId -> click.continentId)

  private val beta1 = hyperParams.getParamValue("expedia.model.country.beta1").toFloat
  private val beta2 = hyperParams.getParamValue("expedia.model.country.beta2").toFloat

  def processCluster(click: Click) = {

       val w = timeDecayService.getDecay(click.dateTime)
    
    clusterHistByContinent.add(click.continentId, click.cluster)

    if (click.isBooking == 1) clusterHistByCountry.add(click.countryId, click.cluster,value=w)
    else clusterHistByCountry.add(click.countryId, click.cluster, value = w*beta1)

    continentByCountry += click.countryId -> click.continentId
  }

  def create(): CountryModel = {

    clusterHistByContinent.normalise()

    clusterHistByCountry.getMap.foreach {
      case (countryId, clusterCounts) =>
        clusterCounts :+= beta2 * clusterHistByContinent.getMap(continentByCountry(countryId))
    }
    clusterHistByCountry.normalise()

    CountryModel(clusterHistByCountry)
  }
}

object CountryModelBuilder {
  def buildFromTrainingSet(trainDatasource: ExDataSource, testClicks: Seq[Click],hyperParams:HyperParams): CountryModel = {

    val timeDecayService = TimeDecayService(testClicks,hyperParams)
    
    val countryModelBuilder = CountryModelBuilder(testClicks,hyperParams,timeDecayService)

    def onClick(click: Click) = {
      countryModelBuilder.processCluster(click)
    }
    trainDatasource.foreach { click => onClick(click) }

    val countryModel = countryModelBuilder.create()

    countryModel
  }
}