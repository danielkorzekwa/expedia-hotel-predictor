package expedia.model.userdestsim

import scala.collection._
import breeze.linalg.DenseVector
import breeze.linalg._
import scala.collection.mutable.ListBuffer

case class UserDestSimPredictionModelBuilder() {

  private val clusterMap: mutable.Map[Tuple2[Double, Double], ListBuffer[Double]] = mutable.Map()

  def processCluster(userId: Double, destId: Double, cluster: Double) = {

    val key = (userId, destId)
    clusterMap.getOrElseUpdate(key, ListBuffer()) += cluster

  }

  def toClusterDistPredictionModel(): UserDestSimPredictionModel = {
    val map = clusterMap.map { case (key, clusters) => key -> clusters.toList }.toMap
    UserDestSimPredictionModel(map)
  }
}