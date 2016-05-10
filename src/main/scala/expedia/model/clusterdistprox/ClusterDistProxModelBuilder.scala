package expedia.model.clusterdistprox

import expedia.data.Click
import expedia.stats.MulticlassHistByKey
import expedia.stats.calcVectorMapProbsMutable
import expedia.stats.calcVectorProbsMutable
import scala.collection._
import breeze.linalg.DenseVector
import breeze.numerics._

case class ClusterDistProxModelBuilder(testClicks: Seq[Click]) {

  //key - (userLoc,market),value - map[dist,clusterProbs]
  private val clusterHistByKey: mutable.Map[Tuple2[Int, Int], mutable.Map[Double, DenseVector[Float]]] = mutable.Map()

  testClicks.foreach { click =>

    val distClusterProbs = clusterHistByKey.getOrElseUpdate((click.userLoc, click.marketId), mutable.Map())
    distClusterProbs.getOrElseUpdate(click.dist, DenseVector.fill(100)(0f))
  }

  def processCluster(click: Click) = {

    if (click.dist != -1) {

      clusterHistByKey.get((click.userLoc, click.marketId)) match {
        case Some(distClusterProbs) => {

          distClusterProbs.foreach {
            case (dist, clusterProbs) =>
              if (abs(click.dist - dist) < 0.02) {

                val currVal = clusterProbs(click.cluster)
                clusterProbs(click.cluster) = currVal + 1
              }
          }
        }
        case None =>
      }
    }
  }

  def create(): ClusterDistProxModel = {
    clusterHistByKey.foreach {
      case (key, distProbs) =>
        distProbs.foreach { case (dist, clusterProbs) => calcVectorProbsMutable(clusterProbs) }
    }
    ClusterDistProxModel(clusterHistByKey)
  }
}