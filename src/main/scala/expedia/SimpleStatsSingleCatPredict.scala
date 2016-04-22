

package expedia

import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix
import breeze.linalg._
import breeze.stats._
import scala.collection._
import java.util.concurrent.atomic.AtomicInteger

/**
 * @param trainData [category,hotel_cluster]
 */
case class SimpleStatsSingleCatPredict(trainData: DenseMatrix[Double], catIndex: Int) {

  val probByCategoryAndClusterMap: Map[Double, Map[Double, Double]] = computedProbByCategoryAndCluster()

  def predict(data: DenseMatrix[Double], hotelCluster: Double): DenseVector[Double] = {

    data(*, ::).map { c =>
     val catClusterProbs =  probByCategoryAndClusterMap.getOrElse(c(catIndex), Map())
     catClusterProbs.getOrElse(hotelCluster,Double.NaN)
    }

  }

  private def computedProbByCategoryAndCluster(): Map[Double, Map[Double, Double]] = {
    val bookedByCategoryAndClusterMap: mutable.Map[Double, mutable.Map[Double, AtomicInteger]] = mutable.Map()

    (0 until trainData.rows).foreach { i =>
      val row = trainData(i, ::)
      val cluster = row(trainData.cols - 1)
      val category = row(catIndex)

      bookedByCategoryAndClusterMap.getOrElseUpdate(category, mutable.Map()).getOrElseUpdate(cluster, new AtomicInteger(0)).incrementAndGet()
    }

    val probByCategoryAndClusterMap = bookedByCategoryAndClusterMap.mapValues { probByCluster =>
      val clusterBookingCount = probByCluster.values.map(v => v.get).sum
      probByCluster.map {
        case (cluster, count) =>
          (cluster, count.get.toDouble / clusterBookingCount)
      }.toMap
    }

    probByCategoryAndClusterMap
  }

}