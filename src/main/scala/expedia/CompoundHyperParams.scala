package expedia

import scala.collection.Seq
import scala.collection.mutable
import expedia.data.Click
import expedia.model.cmu.CmuModelParams

case class CompoundHyperParams(prioritizedHyperParams: Seq[SimpleHyperParams]) {

//  private val continentByMarket: mutable.Map[Int, Int] = mutable.Map()
//  testClicks.foreach(click => continentByMarket += click.marketId -> click.continentId)
//
//  private val countryByMarket: mutable.Map[Int, Int] = mutable.Map()
//  testClicks.foreach(click => countryByMarket += click.marketId -> click.countryId)
//
//  private val countryByDest: mutable.Map[Int, Int] = mutable.Map()
//  testClicks.foreach(click => countryByDest += click.destId -> click.countryId)
//
//  private val continentByCountry: mutable.Map[Int, Int] = mutable.Map()
//  testClicks.foreach(click => continentByCountry += click.countryId -> click.continentId)
//
//  private val continentByDestId: mutable.Map[Int, Int] = mutable.Map()
//  testClicks.foreach(click => continentByDestId += click.destId -> click.continentId)

  def getParamValueForContAndCountry(param: String, continentId:Int,countryId: Int): Double = {
    val hyperParams = prioritizedHyperParams.find { params => params.containsClick(continentId, countryId) }
   if(hyperParams.isEmpty) {
     println("error")
   }
    hyperParams.get.getParamValue(param)

  }

}

object CompoundHyperParams {

    def getPrioritizedHyperParams(): CompoundHyperParams = {
    val prioritizedHyperParams = List(
      SimpleHyperParams.createParamsCont2Country198(),
      SimpleHyperParams.createParamsCont3(),
      SimpleHyperParams.createParamsCont4(),
     SimpleHyperParams.createParamsCont6(),
      SimpleHyperParams.createParamsCMU3())
   CompoundHyperParams( prioritizedHyperParams)
  }
  
  def createHyperParamsByModel(): Map[String, CompoundHyperParams] = {
    Map(
      "cmu" -> CmuModelParams.getPrioritizedHyperParams(),
      "default" -> CompoundHyperParams.getPrioritizedHyperParams())
  }

}