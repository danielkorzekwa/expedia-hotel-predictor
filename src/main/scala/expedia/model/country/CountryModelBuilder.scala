package expedia.model.country

import expedia.data.Click
import expedia.stats.MulticlassHistByKey
import scala.collection._

case class CountryModelBuilder(testClicks: Seq[Click]) {

  private val clusterHistByContinent = MulticlassHistByKey[Int](100)
  testClicks.foreach(click => clusterHistByContinent.add(click.continentId, click.cluster, value = 0))

  
  private val clusterHistByCountry = MulticlassHistByKey[Int](100)
  testClicks.foreach(click => clusterHistByCountry.add(click.countryId, click.cluster, value = 0))

  private val continentByCountry: mutable.Map[Int, Int] = mutable.Map()
  testClicks.foreach(click => continentByCountry += click.countryId -> click.continentId)

  def processCluster(click: Click) = {

    clusterHistByCountry.add(click.countryId, click.cluster)
    clusterHistByContinent.add(click.continentId, click.cluster)

    continentByCountry += click.countryId -> click.continentId
  }

  def create(): CountryModel = {

   clusterHistByContinent.normalise()

    clusterHistByCountry.getMap.foreach { case (countryId, clusterCounts) => clusterCounts :+= clusterHistByContinent.getMap(continentByCountry(countryId)) }
   clusterHistByCountry.normalise()

    CountryModel(clusterHistByCountry)
  }
}