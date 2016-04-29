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

case class UserDestPredictionModel(clusterProbsByUser: Map[Int, Map[Int, DenseVector[Float]]],
                                   clusterProbByDestMap: Map[Int, DenseVector[Float]],
                                   clusterProbByDestMapSVM: Map[Int, DenseVector[Float]],
                                   clusterProbMap: DenseVector[Float],
                                   clusterStatByContinentMapNoPrior: Map[Int, DenseVector[Float]]) extends LazyLogging {

  /**
   * @param data [user_id,dest]
   * @param hotelCluster
   */
  def predict(userId: Int, destId: Int, continent: Int, hotelCluster: Double): Double = {

    val userProb = clusterProbsByUser.getOrElse(userId, clusterProbByDestMap)

    val userCluster = userProb.getOrElse(destId, clusterProbByDestMap.getOrElse(destId, clusterProbByDestMapSVM.getOrElse(destId, clusterStatByContinentMapNoPrior.getOrElse(continent, clusterProbMap))))

    val prob = userCluster(hotelCluster.toInt)

    prob

  }

}