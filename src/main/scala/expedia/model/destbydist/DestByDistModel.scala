package expedia.model.destbydist

import scala.collection._
import expedia.data.Click
import breeze.linalg.DenseVector

case class DestByDistModel( clusterDestsByKey: mutable.Map[Tuple2[Int, Int], mutable.Map[Double, mutable.HashSet[Int]]]) {
  
  def predict(click: Click):  mutable.Set[Int] = {

    val key = (click.userLoc, click.dist, click.marketId)

    val clusterDestIds = clusterDestsByKey((click.userLoc, click.marketId))(click.dist)

    clusterDestIds
  }
}