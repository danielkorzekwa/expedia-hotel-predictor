package expedia.model.clusterdist

import scala.collection._
import breeze.linalg.DenseVector
import breeze.linalg._
import scala.collection.mutable.ListBuffer
case class ClusterDistPredictionModelBuilder() {

  private val clusterMap: mutable.Map[Tuple3[Double, Double, Double], ListBuffer[Double]] = mutable.Map()

  def processCluster(userLoc: Double, dist: Double, market: Double, cluster: Double) = {

    if (dist != -1) {
      val key = (userLoc, dist, market)
      clusterMap.getOrElseUpdate(key, ListBuffer()) += cluster
    }

  }

  def toClusterDistPredictionModel(): ClusterDistPredictionModel3 = {
    val map = clusterMap.map { case (key, clusters) => key -> clusters.toList }
    ClusterDistPredictionModel3(map)
  }
}