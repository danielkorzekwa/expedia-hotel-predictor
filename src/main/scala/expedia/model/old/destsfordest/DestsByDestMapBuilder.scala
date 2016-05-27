package expedia.model.old.destsfordest

import scala.collection._
import expedia.data.Click

case class DestsForDestMapBuilder() {

  //key - (userLoc,dist,market),value destIds
  private val clusterDestsByKey: mutable.Map[Tuple3[Int, Int, Int], mutable.HashSet[Int]] = mutable.Map()

  def processCluster(click: Click) = {

    val key = (click.userLoc, (click.dist * 10000).toInt, click.marketId)

    clusterDestsByKey.getOrElseUpdate(key, mutable.HashSet()) += click.destId
  }

  /**
   * @returns [destId,other destIds that coexists with destId]
   */
  def create(): Map[Int, Set[Int]] = {

    val destsForDestMap: mutable.Map[Int, mutable.HashSet[Int]] = mutable.Map()

    clusterDestsByKey.values.foreach { destIds =>

      destIds.foreach { destId =>
        destsForDestMap.getOrElseUpdate(destId, mutable.HashSet()) ++= destIds
      }

    }

    destsForDestMap.map { case (destId, destIds) => destId -> destIds.toSet }.toMap

  }
}