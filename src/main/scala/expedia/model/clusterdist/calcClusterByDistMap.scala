package expedia.model.clusterdist

import scala.collection._
import breeze.linalg.DenseVector
import scala.io.Source
import java.io.File
import com.typesafe.scalalogging.slf4j.LazyLogging

object calcClusterByDistMap extends LazyLogging {

  /**
   * @returns Map[(user_location_city,orig_destination_distance,hotel_market),hotel_cluster]
   */
  def apply(expediaFile: String): Map[Tuple3[Double, Double, Double], Double] = {

    val clusterMap: mutable.Map[Tuple3[Double, Double, Double], Double] = mutable.Map()

    var all = 0

    Source.fromFile(new File(expediaFile)).getLines().drop(1).foreach { l =>
      val lArray = l.split(",")

      val userLoc = lArray(5).toDouble
      val dist = if (lArray(6).equals("NA") || lArray(6).isEmpty()) -1d else lArray(6).toDouble
      val market = lArray(22).toDouble
      val cluster = lArray(23).toDouble

      val row = DenseVector(userLoc, dist, market, cluster)
      addToMap(clusterMap, row)

      all += 1
      if (all % 10000 == 0) logger.info("Processed rows: %d".format(all))
    }

    clusterMap.toMap
  }

  private def addToMap(clusterMap: mutable.Map[Tuple3[Double, Double, Double], Double], row: DenseVector[Double]) = {

    val key = (row(0), row(1), row(2))

    if (row(1) != -1) {
      val cluster = clusterMap.get(key)
      cluster match {
        case None          => clusterMap.update(key, row(3))
        case Some(cluster) => //if (cluster != row(3)) println("error: %s %.2f %.2f".format(key, cluster, row(3)))

      }

    }

  }
}