package expedia.model.country

import expedia.data.Click
import expedia.stats.MulticlassHistByKey
import scala.collection._
import expedia.data.ExDataSource

case class CountryModelBuilder(testClicks: Seq[Click]) {

  private val clusterHistByContinent = MulticlassHistByKey[Int](100)
  testClicks.foreach(click => clusterHistByContinent.add(click.continentId, click.cluster, value = 0))

  private val clusterHistByCountry = MulticlassHistByKey[Int](100)
  testClicks.foreach(click => clusterHistByCountry.add(click.countryId, click.cluster, value = 0))

  private val continentByCountry: mutable.Map[Int, Int] = mutable.Map()
  testClicks.foreach(click => continentByCountry += click.countryId -> click.continentId)

  def processCluster(click: Click) = {

    clusterHistByContinent.add(click.continentId, click.cluster)

    if (click.isBooking == 1) clusterHistByCountry.add(click.countryId, click.cluster)
    else clusterHistByCountry.add(click.countryId, click.cluster, value = 0.05f)

    continentByCountry += click.countryId -> click.continentId
  }

  def create(): CountryModel = {

    clusterHistByContinent.normalise()

    clusterHistByCountry.getMap.foreach {
      case (countryId, clusterCounts) =>
        if (countryId == 3) {
          println(clusterCounts)
          println(clusterHistByContinent.getMap(continentByCountry(countryId)).map(x => "%.4f".format(x)))
        }
        clusterCounts :+= 1000f * clusterHistByContinent.getMap(continentByCountry(countryId))
    }
    clusterHistByCountry.normalise()

    CountryModel(clusterHistByCountry)
  }
}

object CountryModelBuilder {
  def buildFromTrainingSet(trainDatasource: ExDataSource, testClicks: Seq[Click]): CountryModel = {

    val countryModelBuilder = CountryModelBuilder(testClicks)

    def onClick(click: Click) = {
      countryModelBuilder.processCluster(click)
    }
    trainDatasource.foreach { click => onClick(click) }

    val countryModel = countryModelBuilder.create()

    countryModel
  }
}