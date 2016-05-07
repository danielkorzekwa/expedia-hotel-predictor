package expedia.model.userdest

import scala.collection.Map
import scala.collection.Set
import com.typesafe.scalalogging.slf4j.LazyLogging
import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import expedia.data.Click
import expedia.data.ExDataSource
import expedia.model.dest.DestModelBuilder
import expedia.model.dest.DestModel

case class UserDestPredictionModel(
    destModel:DestModel,
    clusterProbsByUser: Map[Tuple2[Int, Int], DenseVector[Float]]) extends LazyLogging {

  /**
   * @param data [user_id,dest]
   * @param hotelCluster
   */
  def predict(userId: Int, destId: Int, continent: Int): DenseVector[Float] = {

   

    val userProb = clusterProbsByUser.getOrElse((destId, userId), destModel.predict(destId, continent))
    userProb

  }

}

object UserDestPredictionModel {
  def apply(expediaTrainFile: String, svmPredictionsData: DenseMatrix[Double], userIds: Set[Int]): UserDestPredictionModel = {

    val destModelBuilder = DestModelBuilder(svmPredictionsData)

    val modelBuilder = UserDestPredictionModelBuilder( userIds: Set[Int])

    def onClick(click: Click) = {
      destModelBuilder.create()
      modelBuilder.processCluster(click)

    }
    ExDataSource(expediaTrainFile).foreach { click => onClick(click) }

    val destModel = destModelBuilder.create()
    modelBuilder.create(destModel)
  }
}