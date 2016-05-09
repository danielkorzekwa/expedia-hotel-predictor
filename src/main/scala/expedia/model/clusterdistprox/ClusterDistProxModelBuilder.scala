package expedia.model.clusterdistprox

import expedia.data.Click
import expedia.stats.MulticlassHistByKey
import expedia.stats.calcVectorMapProbsMutable
import expedia.stats.calcVectorProbsMutable
import scala.collection._
import breeze.linalg.DenseVector
import breeze.numerics._

case class ClusterDistProxModelBuilder(testClicks: Seq[Click]) {

  private val clusterHistByKey: mutable.Map[Tuple3[Int, Double, Int], DenseVector[Float]] = mutable.Map()

  testClicks.foreach { click =>
    val key = (click.userLoc, click.dist, click.marketId)
    clusterHistByKey += key -> DenseVector.fill(100)(0f)
  }

  def processCluster(click: Click) = {

    if (click.dist != -1) {

      clusterHistByKey.foreach {
        case (key, clusterProbs) =>
          if (click.userLoc == key._1 && click.marketId == key._3 && abs(click.dist - key._2) < 0.5) {

            val currVal = clusterProbs(click.cluster)
            clusterProbs(click.cluster) = currVal + 1
          }
      }
    }

  }

  def create(): ClusterDistProxModel = {
    clusterHistByKey.foreach { case (key, stats) => calcVectorProbsMutable(stats) }

    ClusterDistProxModel(clusterHistByKey)
  }
}