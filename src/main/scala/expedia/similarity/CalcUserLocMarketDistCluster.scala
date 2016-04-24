package expedia.similarity

import breeze.linalg.DenseMatrix
import breeze.linalg._
import scala.collection._
import scala.io.Source
import java.io.File
import java.text.SimpleDateFormat
import java.util.TimeZone

class CalcUserLocMarketDistCluster() {

  private val clusterMap: mutable.Map[Tuple3[Double, Double, Double], Double] = mutable.Map()

  /**
   * @param row [user_location_city,orig_destination_distance,hotel_market,hotel_cluster]
   */
  def add(row: DenseVector[Double]) = {

    val key = (row(0), row(1), row(2))

    if (row(1) != -1) {

      val cluster = clusterMap.get(key)
      cluster match {
        case None          => clusterMap.update(key, row(3))
        case Some(cluster) => //if (cluster != row(3)) println("error: %s %.2f %.2f".format(key, cluster, row(3)))

      }
    }
  }

  def toMap(): immutable.Map[Tuple3[Double, Double, Double], Double] = clusterMap.toMap

}

object CalcUserLocMarketDistCluster {
  /**
   * @param data [user_location_city,orig_destination_distance,hotel_market,hotel_cluster]
   */
  def apply(data: DenseMatrix[Double]): CalcUserLocMarketDistCluster = {

    val distClusterMap = new CalcUserLocMarketDistCluster()

    data(*, ::).foreach { row =>
      distClusterMap.add(row)
    }

    distClusterMap

  }

  def apply(rawDataFile: String): CalcUserLocMarketDistCluster = {
    val distClusterMap = new CalcUserLocMarketDistCluster()

    val df = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
    df.setTimeZone(TimeZone.getTimeZone("UTC"))

    var i = 0
    val testSetStart = df.parse("2014-01-01 00:00:00").getTime

    Source.fromFile(new File(rawDataFile)).getLines().drop(1).foreach { l =>
      val lArray = l.split(",")

      val datetime = df.parse(lArray(0).drop(1).dropRight(1))

      if (true ||  datetime.getTime < testSetStart) {
        val userLoc = lArray(5).toDouble
        val dist = if (lArray(6).equals("NA") || lArray(6).isEmpty()) -1d else lArray(6).toDouble
        val market = lArray(22).toDouble
        val cluster = lArray(23).toDouble

        val row = DenseVector(userLoc, dist, market, cluster)
        distClusterMap.add(row)

      }
      i += 1
      if (i % 100000 == 0) println("CalcUserLocMarketDistCluster from file:" + i)
    }

    distClusterMap

  }
}