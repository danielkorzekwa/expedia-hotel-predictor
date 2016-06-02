package expedia.util

import java.util.Date
import breeze.numerics.exp
import expedia.CompoundHyperParams
import expedia.data.Click
import expedia.HyperParamsService

case class TimeDecayService(testClicks: Seq[Click], hyperParamsService: HyperParamsService,hyperParams:CompoundHyperParams) {

  private val testTime = testClicks.map(c => c.dateTime.getTime).min

  private val TWO_MONTH = (1000L * 3600 * 24 * 30 * 2)

  private val MONTH = (1000L * 3600 * 24 * 30)
  private val DAY = (1000L * 3600 * 24)
  private val WEEK = (1000L * 3600 * 24 * 7)

  //  def getDecay(dateTime: Date): Float = {
  //    val clickAge = (testTime - dateTime.getTime) / MONTH
  //    //val w = exp(-0.07 * clickAge).toFloat
  //       val w = 1 + -0.035*clickAge
  //    w.toFloat
  //  }

  def getDecayForCountryId(dateTime: Date, countryId: Int): Float = {
    val timeDecay = hyperParamsService.getParamValueForCountryId("expedia.timeDecay", countryId,hyperParams).toFloat
    getDecay(dateTime, timeDecay)
  }
  
   def getDecayForDestId(dateTime: Date, destId: Int): Float = {
    val timeDecay = hyperParamsService.getParamValueForDestId("expedia.timeDecay", destId,hyperParams).toFloat
    getDecay(dateTime, timeDecay)
  }
   
    def getDecayForMarketId(dateTime: Date, marketId: Int): Float = {
    val timeDecay = hyperParamsService.getParamValueForMarketId("expedia.timeDecay", marketId,hyperParams).toFloat
    getDecay(dateTime, timeDecay)
  }

  private def getDecay(dateTime: Date, timeDecay: Float): Float = {
    val clickAge = (testTime - dateTime.getTime) / MONTH
    val w = exp(timeDecay * clickAge).toFloat
    w
  }
}