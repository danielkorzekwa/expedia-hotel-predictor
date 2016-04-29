package expedia.data

import scala.io.Source
import java.io.File

object loadTestData {

  def apply(expediaFile: String): Seq[ClusterX] = {

    Source.fromFile(new File(expediaFile)).getLines().drop(1).toSeq.par.map { l =>
      val lArray = l.split(",")

      val userLoc = lArray(5).toInt
      val dist = if (lArray(6).equals("NA") || lArray(6).isEmpty()) -1d else lArray(6).toDouble
      val userId = lArray(7).toInt
      val destId = lArray(16).toInt
      val isBooking = lArray(18).toInt
      val hotelContinent = lArray(20).toInt
      val market = lArray(22).toInt
      val cluster = lArray(23).toInt

      ClusterX(userLoc, dist, userId, destId, market)
    }.toList
  }
}