package expedia

import scala.collection._
import expedia.data.Click

case class HyperParamsService(testClicks: Seq[Click]) {

  private val continentByMarket: mutable.Map[Int, Int] = mutable.Map()
  testClicks.foreach(click => continentByMarket += click.marketId -> click.continentId)

  private val countryByMarket: mutable.Map[Int, Int] = mutable.Map()
  testClicks.foreach(click => countryByMarket += click.marketId -> click.countryId)

  private val countryByDest: mutable.Map[Int, Int] = mutable.Map()
  testClicks.foreach(click => countryByDest += click.destId -> click.countryId)

  private val continentByCountry: mutable.Map[Int, Int] = mutable.Map()
  testClicks.foreach(click => continentByCountry += click.countryId -> click.continentId)

  private val continentByDestId: mutable.Map[Int, Int] = mutable.Map()
  testClicks.foreach(click => continentByDestId += click.destId -> click.continentId)

  def getParamValueForCountryId(param: String, countryId: Int, hyperParams: CompoundHyperParams): Double = {
    val continentId = continentByCountry(countryId)
    hyperParams.getParamValueForContAndCountry(param, continentId, countryId)

  }

  def getParamValueForMarketId(param: String, marketId: Int, hyperParams: CompoundHyperParams): Double = {

    val countryId = countryByMarket(marketId)
    val continentId = continentByCountry(countryId)

    hyperParams.getParamValueForContAndCountry(param, continentId, countryId)

  }

  def getParamValueForDestId(param: String, destId: Int, hyperParams: CompoundHyperParams): Double = {

    val countryId = countryByDest(destId)
    val continentId = continentByCountry(countryId)

    hyperParams.getParamValueForContAndCountry(param, continentId, countryId)

  }

}