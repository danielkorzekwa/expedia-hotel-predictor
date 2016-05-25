package expedia.util

import java.text.SimpleDateFormat
import java.util.TimeZone
import breeze.numerics._
import java.util.Date

object getTimeDecay {

  val df = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
  df.setTimeZone(TimeZone.getTimeZone("UTC"))
  val testTime = df.parse("2014-01-01 00:00:00").getTime
  val MONTH = (1000L * 3600 * 24 * 30)

  def apply(dateTime: Date): Float = {
    val clickAge = (testTime - dateTime.getTime) / MONTH
    val w = exp(-0.07 * clickAge).toFloat
    w
  }
}