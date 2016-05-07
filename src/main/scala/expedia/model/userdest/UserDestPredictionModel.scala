package expedia.model.userdest

import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix
import breeze.linalg._
import com.typesafe.scalalogging.slf4j.LazyLogging
import java.util.concurrent.atomic.AtomicInteger
import scala.collection._
import expedia.model.svm.loadClusterProbsByDestMap
import expedia.model.svm.SVMPredictionModel
import expedia.data.ExDataSource

case class UserDestPredictionModel(clusterProbsByUser: Map[Tuple2[Int, Int], DenseVector[Float]],
                                   clusterProbByDestMap: Map[Int, DenseVector[Float]],
                                   clusterProbByDestMapSVM: Map[Int, DenseVector[Float]],
                                   clusterProbMap: DenseVector[Float],
                                   clusterStatByContinentMapNoPrior: Map[Int, DenseVector[Float]]) extends LazyLogging {

  /**
   * @param data [user_id,dest]
   * @param hotelCluster
   */
  def predict(userId: Int, destId: Int, continent: Int): DenseVector[Float] = {

    def userProbDefault(destId: Int): DenseVector[Float] = {
      clusterProbByDestMap.getOrElse(destId, clusterProbByDestMap.getOrElse(destId, clusterProbByDestMapSVM.getOrElse(destId, clusterStatByContinentMapNoPrior.getOrElse(continent, clusterProbMap))))
    }

    val userProb = clusterProbsByUser.getOrElse((destId, userId), userProbDefault(destId))
    userProb

  }

}

object UserDestPredictionModel {
  def apply(expediaTrainFile: String, svmPredictionsData: DenseMatrix[Double], userIds: Set[Int]): UserDestPredictionModel = {

    val modelBuilder = UserDestPredictionModelBuilder(svmPredictionsData: DenseMatrix[Double], userIds: Set[Int])

    ExDataSource(expediaTrainFile).foreach { click => modelBuilder.processCluster(click) }

    modelBuilder.create()
  }
}