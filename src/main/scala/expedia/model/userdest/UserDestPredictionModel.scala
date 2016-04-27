package expedia.model.userdest

import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix
import breeze.linalg._
import expedia.stats.calcCatStatsMap
import expedia.stats.calcCatStats
import com.typesafe.scalalogging.slf4j.LazyLogging
import java.util.concurrent.atomic.AtomicInteger
import scala.collection._
import expedia.model.svm.loadClusterProbsByDestMap
import expedia.model.svm.SVMPredictionModel

case class UserDestPredictionModel(clusterProbsByUser: Map[Double, Map[Double, DenseVector[Double]]],
                                   clusterProbByDestMap: Map[Double, DenseVector[Double]],
                                   clusterProbByDestMapSVM: Map[Double, DenseVector[Double]],
                                   clusterProbMap: DenseVector[Double]) extends LazyLogging {

  /**
   * @param data [user_id,dest]
   * @param hotelCluster
   */
  def predict(row: DenseVector[Double], hotelCluster: Double): Double = {

    val userId = row(0)
    val destId = row(1)

    val userProb = clusterProbsByUser.getOrElse(userId, clusterProbByDestMap)
    val userCluster = userProb.getOrElse(destId, clusterProbByDestMap.getOrElse(destId, clusterProbByDestMapSVM.getOrElse(destId, clusterProbMap)))
    val prob = userCluster(hotelCluster.toInt)

    prob

  }

}