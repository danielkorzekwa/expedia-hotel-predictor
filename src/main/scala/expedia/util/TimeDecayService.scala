package expedia.util

import java.util.Date
import breeze.numerics._
import expedia.data.Click
import expedia.HyperParams

case class TimeDecayService(testClicks: Seq[Click], hyperParams: HyperParams) {

  private val testTime = testClicks.map(c => c.dateTime.getTime).min
  
   private val TWO_MONTH = (1000L * 3600 * 24 * 30*2)
  
  private val MONTH = (1000L * 3600 * 24 * 30)
   private val DAY = (1000L * 3600 * 24 )
   private val WEEK = (1000L * 3600 * 24 * 7 )
  private val timeDecay = hyperParams.getParamValue("expedia.timeDecay").toFloat

//  def getDecay(dateTime: Date): Float = {
//    val clickAge = (testTime - dateTime.getTime) / MONTH
//    //val w = exp(-0.07 * clickAge).toFloat
//       val w = 1 + -0.035*clickAge
//    w.toFloat
//  }
  
   def getDecay(dateTime: Date): Float = {
    val clickAge = (testTime - dateTime.getTime) / MONTH
    val w = exp(timeDecay * clickAge).toFloat
    w
  }
}