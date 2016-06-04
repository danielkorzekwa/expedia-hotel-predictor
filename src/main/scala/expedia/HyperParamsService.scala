package expedia

import scala.collection._
import expedia.data.Click

case class HyperParamsService(testClicks: Seq[Click]) {

  private val marketByDest: mutable.Map[Int, Int] = mutable.Map()
  testClicks.foreach(click => marketByDest += click.destId -> click.marketId)

  def getParamValueForCountryId(param: String, countryId: Int, hyperParams: CompoundHyperParams): Double = {
    hyperParams.getParamValueForCountry(param, countryId)

  }

  def getParamValueForMarketId(param: String, marketId: Int, hyperParams: CompoundHyperParams): Double = {
    hyperParams.getParamValueForMarketId(param, marketId)
  }

  def getParamValueForDestId(param: String, destId: Int, hyperParams: CompoundHyperParams): Double = {
    val marketId = marketByDest(destId)
    hyperParams.getParamValueForMarketId(param, marketId)
  }

}