package expedia.util

import java.util.Date
import breeze.numerics._
import expedia.data.Click
import expedia.HyperParams

case class TimeDecayService(testClicks: Seq[Click], hyperParams: HyperParams) {

  private val testTime = testClicks.map(c => c.dateTime.getTime).min
  private val MONTH = (1000L * 3600 * 24 * 30)
  private val timeDecay = hyperParams.getParamValue("timeDecay").toFloat

  def getDecay(dateTime: Date): Float = {
    val clickAge = (testTime - dateTime.getTime) / MONTH
    val w = exp(timeDecay * clickAge).toFloat
    w
  }
}