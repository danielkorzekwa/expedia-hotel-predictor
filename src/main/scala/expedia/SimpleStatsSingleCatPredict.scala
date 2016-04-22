

package expedia

import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix
import breeze.linalg._
import breeze.stats._
import scala.collection._
import java.util.concurrent.atomic.AtomicInteger
import expedia.similarity.calcCatStatsMap
import expedia.similarity.calcCatStats
import expedia.similarity.calcCatProbs

/**
 *
 * @param priorCatStats [itemIt,count]
 * @param trainData [category,hotel_cluster]
 */
case class SimpleStatsSingleCatPredict(trainData: DenseMatrix[Double]) {

  val probByCategoryAndClusterMap: Map[Double, Map[Double, Double]] = computedProbByCategoryAndCluster()

  /**
   * @param data [category]
   * @param hotelCluster
   */
  def predict(data: DenseVector[Double], hotelCluster: Double): DenseVector[Double] = {

    val emptyMap = Map[Double,Double]()
    
    data.map { c =>
      val catClusterProbs = probByCategoryAndClusterMap.getOrElse(c, emptyMap)
      val prob = catClusterProbs.getOrElse(hotelCluster, Double.NaN)
    //  if(prob==0d) Double.NaN else prob
    prob
    }

  }

  private def computedProbByCategoryAndCluster(): Map[Double, Map[Double, Double]] = {

   val priorCatStats = calcCatStats(trainData(::,1))
   var priorCatProbs = calcCatProbs(priorCatStats)//.map{case (cat,prob) => (cat,0d)}
    
    val bookedByCategoryAndClusterMap = calcCatStatsMap(trainData,priorCatProbs)

    val probByCategoryAndClusterMap = bookedByCategoryAndClusterMap.map { case (cat,probByCluster) =>
      val clusterBookingCount = probByCluster.values.sum
     val newProbByClusterMap = probByCluster.map {
        case (cluster, count) =>
          (cluster, count.toDouble / clusterBookingCount)
      }.toMap
      
      (cat,newProbByClusterMap)
    }

    probByCategoryAndClusterMap
  }

}