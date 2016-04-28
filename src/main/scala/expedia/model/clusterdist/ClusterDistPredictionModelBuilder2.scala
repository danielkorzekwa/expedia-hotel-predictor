package expedia.model.clusterdist

import scala.collection._
import breeze.linalg.DenseVector
import scala.math._

case class ClusterDistPredictionModelBuilder2() {

  //Map[(userLoc,dist,market),Map[cluster,count]]
  private val clusterMap: mutable.Map[Tuple3[Double, Double, Double], mutable.HashSet[Double]] = mutable.Map()

  def processCluster(userLoc: Double, dist: Double, market: Double, cluster: Double) = {

    if (dist != -1) {

      val now = System.currentTimeMillis()
      clusterMap.foreach {
        case (key, similarClusters) =>

          val distDelta = abs(dist - key._2)
          if (userLoc == key._1 && distDelta < 0.0001d && distDelta > 0 && market == key._3) {
            similarClusters += cluster
            
              println(similarClusters.size + ":" + similarClusters)
          }
      }
    
      val key = (userLoc, dist, market)

      //Map[cluster,count]
      clusterMap.getOrElseUpdate(key, mutable.HashSet(cluster))

    }

  }

}