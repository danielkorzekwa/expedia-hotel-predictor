package expedia.model.clusterdist

import scala.collection._
import breeze.linalg.DenseVector

case class ClusterDistPredictionModelBuilder() {

  //Map[(userLoc,dist,market),Map[cluster,count]]
  private val clusterMap: mutable.Map[Tuple3[Double, Double, Double], mutable.Map[Double, Double]] = mutable.Map()

  def processCluster(userLoc: Double, dist: Double, market: Double, cluster: Double) = {

    if (dist != -1) {

      val key = (userLoc, dist, market)

      //Map[cluster,count]
      val countByClusterMap: mutable.Map[Double, Double] = clusterMap.getOrElseUpdate(key, mutable.Map())
      val currVal = countByClusterMap.getOrElseUpdate(cluster, 0d)
      countByClusterMap.update(cluster, currVal + 1d)

    }

  }

  def toClusterDistPredictionModel(): ClusterDistPredictionModel3 = {
    
    //Map[(userLoc,dist,market),[sorted clusters vector by cluster counts]]
    val sortedClusterMap: Map[Tuple3[Double, Double, Double], DenseVector[Double]] = clusterMap.toMap.map {
      case (key, countByClusterMap) =>

        val clusterVec = countByClusterMap.toList.sortWith((a, b) => a._2 > b._2).map(_._1).toArray
        (key, DenseVector(clusterVec))
    }

    ClusterDistPredictionModel3(sortedClusterMap)
  }
}