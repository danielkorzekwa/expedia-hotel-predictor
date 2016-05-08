package expedia.data

import java.io.File

import scala.io.Source

import com.typesafe.scalalogging.slf4j.LazyLogging

case class ExDataSource(expediaFile: String) extends LazyLogging {

  def getAllClicks(): Seq[Click] = {
    var i = 0
    val clicks = Source.fromFile(new File(expediaFile)).getLines().drop(1).map { l =>
      val click = createClick(l)

      i += 1
      if (i % 10000 == 0) logger.info("Processed expedia rows: %d".format(i))

      click
    }

    clicks.toList
  }

  def foreach(onClick: (Click) => Unit) = {

    var i = 0
    Source.fromFile(new File(expediaFile)).getLines().drop(1).foreach { l =>
      val click = createClick(l)
      onClick(click)

      i += 1
      if (i % 10000 == 0) logger.info("Processed expedia rows: %d".format(i))
    }

  }

  private def createClick(l: String): Click = {
    val lArray = l.split(",")

    val userLoc = lArray(5).toInt
    val dist = if (lArray(6).equals("NA") || lArray(6).isEmpty()) -1d else lArray(6).toDouble
    val userId = lArray(7).toInt
    val destId = lArray(16).toInt
    val isBooking = if(lArray(18).length>0) lArray(18).toInt else -1
    val hotelContinent = lArray(20).toInt
    val countryId = lArray(21).toInt
    val market = lArray(22).toInt
    val cluster = if(lArray.size==24) lArray(23).toInt else -1

    val click = Click(userLoc, dist, userId, destId, isBooking, hotelContinent, countryId,market, cluster)
    click
  }
}