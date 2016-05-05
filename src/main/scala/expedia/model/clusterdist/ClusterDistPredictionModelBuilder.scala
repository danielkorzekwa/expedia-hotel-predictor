package expedia.model.clusterdist

import scala.collection._
import breeze.linalg.DenseVector
import breeze.linalg._
import scala.collection.mutable.ListBuffer
case class ClusterDistPredictionModelBuilder() {

  //Map[(userLoc,dist,market),Map[cluster,count]]
//  private val clusterMap: mutable.Map[Tuple3[Double, Double, Double], mutable.Map[Double, Double]] = mutable.Map()

  
  //Map[(userLoc,dist,market),ListBuffer[cluster]]
  private val clusterMap: mutable.Map[Tuple3[Double, Double, Double], ListBuffer[Double]] = mutable.Map()

  
  def processCluster(userLoc: Double, dist: Double, market: Double, cluster: Double) = {

    if (dist != -1) {

      val key = (userLoc, dist, market)

//      //Map[cluster,count]
//      val countByClusterMap: mutable.Map[Double, Double] = clusterMap.getOrElseUpdate(key, mutable.Map())
//      val currVal = countByClusterMap.getOrElseUpdate(cluster, 0d)
//      countByClusterMap.update(cluster, currVal + 1d)
      
      
      //Map[cluster,count]
      val clusters: ListBuffer[Double] = clusterMap.getOrElseUpdate(key, ListBuffer())
      clusters += cluster

    }

  }

  def toClusterDistPredictionModel(): ClusterDistPredictionModel3 = {

//    //Map[(userLoc,dist,market),[sorted clusters vector by cluster counts]]
//    val sortedClusterMap: Map[Tuple3[Double, Double, Double], DenseVector[Double]] = clusterMap.toMap.map {
//      case (key, countByClusterMap) =>
//        if (countByClusterMap.map(_._2).sum<10) {
//          val clusterVec = countByClusterMap.toList.sortWith((a, b) => a._2 > b._2).map(_._1).toArray
//          (key, DenseVector(clusterVec))
//        } else (key, DenseVector[Double]())
//    }

    val map = clusterMap.map{case (key,clusters) => if(clusters.size<10) key -> clusters.toList else key -> List()}
    
    ClusterDistPredictionModel3(map)
  }
}