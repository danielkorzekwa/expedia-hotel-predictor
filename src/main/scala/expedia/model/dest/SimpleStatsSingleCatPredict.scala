

package expedia.model.dest

import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix
import breeze.linalg._
import breeze.stats._
import scala.collection._
import expedia.stats.calcCatStatsMap
import expedia.stats.calcCatStats
import expedia.stats.calcCatProbs

/**
 *
 * @param priorCatStats [itemIt,count]
 * @param trainData [category,hotel_cluster]
 */
case class SimpleStatsSingleCatPredict(trainData: DenseMatrix[Double]) {

  val clusterStatMap = calcCatStats(trainData(::, 1))
  var clusterProbMap: DenseVector[Double] = calcCatProbs(clusterStatMap)

  val clusterStatByDestMap = calcCatStatsMap(trainData, destId => clusterProbMap)
  val clusterProbByDestMap: Map[Double, DenseVector[Double]] = calcCatProbs(clusterStatByDestMap)

  /**
   * @param data [category]
   * @param hotelCluster
   */
  def predict(data: DenseVector[Double], hotelCluster: Double): DenseVector[Double] = {

    data.map { c =>
      val catClusterProbs = clusterProbByDestMap.getOrElse(c, clusterProbMap)(hotelCluster.toInt)
      catClusterProbs
    }

  }

}