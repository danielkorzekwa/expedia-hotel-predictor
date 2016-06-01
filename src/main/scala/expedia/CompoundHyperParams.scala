package expedia

import scala.collection.Seq
import scala.collection.mutable

import expedia.data.Click

case class CompoundHyperParams(testClicks: Seq[Click]) {

  private val defaultHyperParams = SimpleHyperParams.createParamsCMU3()
  private val cont3Params = SimpleHyperParams.createParamsCont3()
  private val cont4Params = SimpleHyperParams.createParamsCont4()
  private val cont6Params = SimpleHyperParams.createParamsCont6()

  private val country198 = SimpleHyperParams.createParamsCont2Country198()

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

  def getParamValueForCountryId(param: String, countryId: Int): Double = {
    if (countryId == 198) country198.getParamValue(param)
    else if (continentByCountry(countryId) == 3) cont3Params.getParamValue(param)
    else if (continentByCountry(countryId) == 4) cont4Params.getParamValue(param)
    else if (continentByCountry(countryId) == 6) cont6Params.getParamValue(param)
    else defaultHyperParams.getParamValue(param)
  }

  def getParamValueForMarketId(param: String, marketId: Int): Double = {
    if (countryByMarket(marketId) == 198) country198.getParamValue(param)
    else if (continentByMarket(marketId) == 3) cont3Params.getParamValue(param)
    else if (continentByMarket(marketId) == 4) cont4Params.getParamValue(param)
    else if (continentByMarket(marketId) == 6) cont6Params.getParamValue(param)
    else defaultHyperParams.getParamValue(param)
  }

  def getParamValueForDestId(param: String, destId: Int): Double = {
    if (countryByDest(destId) == 198) country198.getParamValue(param)
    else if (continentByDestId(destId) == 3) cont3Params.getParamValue(param)
    else if (continentByDestId(destId) == 4) cont4Params.getParamValue(param)
    else if (continentByDestId(destId) == 6) cont6Params.getParamValue(param)
    else defaultHyperParams.getParamValue(param)
  }

}