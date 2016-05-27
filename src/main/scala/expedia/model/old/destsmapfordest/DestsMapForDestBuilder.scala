package expedia.model.old.destsmapfordest

import scala.collection._
import expedia.data.Click

case class DestsMapForDestBuilder() {

  //key - (userLoc,dist,market),value destIds
  private val clusterDestsByKey: mutable.Map[Tuple3[Int, Int, Int], mutable.HashSet[Int]] = mutable.Map()

  def processCluster(click: Click) = {

    val key = (click.userLoc, (click.dist * 10000).toInt, click.marketId)

    clusterDestsByKey.getOrElseUpdate(key, mutable.HashSet()) += click.destId
  }

  /**
   * @returns [destId, Map[destId,coexistence counts]]
   */
  def create(): Map[Int, Map[Int, Int]] = {

    val destsForDestMap: mutable.Map[Int, mutable.Map[Int, Int]] = mutable.Map()

    clusterDestsByKey.values.foreach { destIds =>

      destIds.foreach { destId1 =>

        val destsMap = destsForDestMap.getOrElseUpdate(destId1, mutable.HashMap())

        destIds.foreach { destId2 =>

          val currValue = destsMap.getOrElseUpdate(destId2, 0)
          destsMap(destId2) = currValue + 1
        }
      }
    }

    destsForDestMap.map { case (destId, destIdsMap) => destId -> destIdsMap.toMap }.toMap

  }
}