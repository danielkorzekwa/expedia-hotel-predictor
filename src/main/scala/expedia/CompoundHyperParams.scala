package expedia

import scala.collection.Seq
import scala.collection.mutable

import expedia.data.Click

case class CompoundHyperParams(testClicks: Seq[Click], prioritizedHyperParams: Seq[SimpleHyperParams]) {

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
    val continentId = continentByCountry(countryId)
    val hyperParams = prioritizedHyperParams.find { params => params.containsClick(continentId, countryId) }
    hyperParams.get.getParamValue(param)

  }

  def getParamValueForMarketId(param: String, marketId: Int): Double = {

    val countryId = countryByMarket(marketId)
    val continentId = continentByCountry(countryId)

    val hyperParams = prioritizedHyperParams.find { params => params.containsClick(continentId, countryId) }
    hyperParams.get.getParamValue(param)

  }

  def getParamValueForDestId(param: String, destId: Int): Double = {

    val countryId = countryByDest(destId)
    val continentId = continentByCountry(countryId)

    val hyperParams = prioritizedHyperParams.find { params => params.containsClick(continentId, countryId) }
    hyperParams.get.getParamValue(param)

  }

}

object CompoundHyperParams {

//  def getPrioritizedHyperParams(): Seq[SimpleHyperParams] = {
//    val prioritizedHyperParams = List(
//      SimpleHyperParams.createParamsCont2Country198(),
//      SimpleHyperParams.createParamsCont3(),
//      SimpleHyperParams.createParamsCont4(),
//      SimpleHyperParams.createParamsCont6(),
//      SimpleHyperParams.createParamsCMU3())
//
//    prioritizedHyperParams
//  }

}