package expedia.data

import java.io.File

import scala.io.Source

import com.typesafe.scalalogging.slf4j.LazyLogging

case class ExDataSource(expediaFile: String) extends LazyLogging{

  def foreach(onClick: (Click) => Unit) = {

    var i = 0
    Source.fromFile(new File(expediaFile)).getLines().drop(1).foreach { l =>
      val lArray = l.split(",")

      val userLoc = lArray(5).toInt
      val dist = if (lArray(6).equals("NA") || lArray(6).isEmpty()) -1d else lArray(6).toDouble
      val userId = lArray(7).toInt
      val destId = lArray(16).toInt
      val isBooking = lArray(18).toInt
      val hotelContinent = lArray(20).toInt
      val market = lArray(22).toInt
      val cluster = lArray(23).toInt

      val click = Click(userLoc, dist, userId, destId, isBooking,hotelContinent,market,cluster)
      onClick(click)
      
      i += 1
      if (i % 10000 == 0) logger.info("Processed expedia rows: %d".format(i))
    }

  }
}