package expedia.util

import java.util.Date
import breeze.numerics.exp
import expedia.CompoundHyperParams
import expedia.data.Click
import expedia.HyperParamsService

case class TimeDecayService(testClicks: Seq[Click]) {

  private val testTime = testClicks.map(c => c.dateTime.getTime).min

  private val TWO_MONTH = (1000L * 3600 * 24 * 30 * 2)

  private val MONTH = (1000L * 3600 * 24 * 30)
  private val DAY = (1000L * 3600 * 24)
  private val WEEK = (1000L * 3600 * 24 * 7)

  def getDecay(dateTime: Date, timeDecay: Float): Float = {
    val clickAge = (testTime - dateTime.getTime) / MONTH
    val w = exp(timeDecay * clickAge).toFloat
    w
  }
}