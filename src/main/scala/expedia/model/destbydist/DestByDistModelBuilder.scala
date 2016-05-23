package expedia.model.destbydist

import expedia.data.Click
import scala.collection._
import breeze.numerics._

case class DestByDistModelBuilder(testClicks: Seq[Click]) {

  //key - (userLoc,market),value - map[dist,destIds]
  private val clusterDestsByKey: mutable.Map[Tuple2[Int, Int], mutable.Map[Double, mutable.HashSet[Int]]] = mutable.Map()

  testClicks.foreach { click =>

    val distDestIds = clusterDestsByKey.getOrElseUpdate((click.userLoc, click.marketId), mutable.Map())
    distDestIds.getOrElseUpdate(click.dist, mutable.HashSet())
  }

  def processCluster(click: Click) = {

    if (click.dist != -1) {

      clusterDestsByKey.get((click.userLoc, click.marketId)) match {
        case Some(distDestIds) => {

          distDestIds.foreach {
            case (dist, distDestIds) =>
              if (abs(click.dist - dist) == 0.0) {
                distDestIds += click.destId
              }
          }
        }
        case None => //do nothing
      }
    }
  }

  def create(): DestByDistModel = {

    DestByDistModel(clusterDestsByKey)
  }
}