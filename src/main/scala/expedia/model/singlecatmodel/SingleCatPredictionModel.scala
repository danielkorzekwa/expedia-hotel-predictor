package expedia.model.singlecatmodel

import breeze.linalg.DenseVector
import scala.collection._

case class SingleCatPredictionModel(
    globalCusterProb: DenseVector[Float],
    clusterProbByCatMap: Map[Int, DenseVector[Float]]) {

  /**
   * @param data [user_id,dest]
   * @param hotelCluster
   */
  def predict(categoryId: Int, hotelCluster: Int): Double = {
    clusterProbByCatMap.getOrElse(categoryId, globalCusterProb)(hotelCluster)
  }
}