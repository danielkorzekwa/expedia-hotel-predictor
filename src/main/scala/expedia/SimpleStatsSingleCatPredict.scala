

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
import dk.gp.math.sqDist

/**
 *
 * @param priorCatStats [itemIt,count]
 * @param trainData [category,hotel_cluster]
 */
case class SimpleStatsSingleCatPredict(trainData: DenseMatrix[Double]) {

  val clusterStatMap = calcCatStats(trainData(::, 1))
  var clusterProbMap: Map[Double, Double] = calcCatProbs(clusterStatMap)

  val clusterStatByDestMap = calcCatStatsMap(trainData, destId => clusterProbMap)
  val clusterProbByDestMap: Map[Double, Map[Double, Double]] = calcCatProbs(clusterStatByDestMap)

  /**
   * @param data [category]
   * @param hotelCluster
   */
  def predict(data: DenseVector[Double], hotelCluster: Double): DenseVector[Double] = {

    data.map { c =>
      val catClusterProbs = clusterProbByDestMap.getOrElse(c, clusterProbMap)(hotelCluster)
      catClusterProbs
    }

  }

}