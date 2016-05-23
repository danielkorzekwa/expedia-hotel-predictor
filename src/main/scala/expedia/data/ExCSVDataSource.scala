package expedia.data

import java.io.File
import scala.io.Source
import com.typesafe.scalalogging.slf4j.LazyLogging
import java.text.SimpleDateFormat
import java.util.TimeZone
import org.joda.time.LocalDate
import java.util.Date

case class ExCSVDataSource(dsName: String, expediaFile: String, filter: (Click) => Boolean = (click) => true) extends ExDataSource with LazyLogging {

  def getAllClicks(): Seq[Click] = {

    logger.info("Processing %s...".format(dsName))
    val df = new SimpleDateFormat("yyyy-MM-dd") // testing
    //    val df = new SimpleDateFormat("yyyy-MM-dd") //submission
    val df2 = new SimpleDateFormat("yyyy-MM-dd hh:mm:ss")
    df.setTimeZone(TimeZone.getTimeZone("UTC"))

    var i = 0
    val clicks = Source.fromFile(new File(expediaFile)).getLines().drop(1).map { l =>
      val click = createClick(l, df, df2)

      i += 1
      if (i % 1000000 == 0) logger.info("Processed expedia rows(%s): %d".format(dsName, i))

      click
    }

    clicks.filter(c => filter(c)).toList
  }

  def foreach(onClick: (Click) => Unit) = {

    logger.info("Processing %s...".format(dsName))
    val df = new SimpleDateFormat("yyyy-MM-dd")
    val df2 = new SimpleDateFormat("yyyy-MM-dd hh:mm:ss")
    df.setTimeZone(TimeZone.getTimeZone("UTC"))

    var i = 0
    Source.fromFile(new File(expediaFile)).getLines().drop(1).foreach { l =>
      val click = createClick(l, df, df2)

      if (filter(click)) onClick(click)
      i += 1
      if (i % 1000000 == 0) logger.info("Processed expedia rows(%s): %d".format(dsName, i))
    }

  }

  private def createClick(l: String, df: SimpleDateFormat, df2: SimpleDateFormat): Click = {
    val lArray = l.split(",")

    val userRegion = lArray(4).toInt
    val userLoc = lArray(5).toInt
    val dist = if (lArray(6).equals("NA") || lArray(6).isEmpty()) -1d else lArray(6).toDouble
    val userId = lArray(7).toInt

    val dateTime = if (lArray(0).size > 2) df2.parse(lArray(0)) else new Date(0)
    val checkinDate = if (lArray(11).size > 2) df.parse(lArray(11)) else new Date(0)
    val checkin = if (lArray(11).size > 2) df.parse(lArray(11)).getTime else 0
    val checkout = if (lArray(12).size > 2) df.parse(lArray(12)).getTime else 0
    val destId = lArray(16).toInt
    val isBooking = if (lArray(18).length > 0) lArray(18).toInt else -1
    val hotelContinent = lArray(20).toInt
    val countryId = lArray(21).toInt
    val market = lArray(22).toInt
    val cluster = if (lArray.size == 24) lArray(23).toInt else -1

    val stayDays = ((checkout - checkin) / (1000L * 60 * 60 * 24)).toInt
    val click = Click(userRegion, userLoc, dist, userId, destId, isBooking, hotelContinent, countryId, market, stayDays, dateTime, cluster)
    click
  }
}