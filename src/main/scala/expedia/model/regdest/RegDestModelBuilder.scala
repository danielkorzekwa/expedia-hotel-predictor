package expedia.model.regdest

import expedia.data.Click
import expedia.stats.MulticlassHistByKey
import breeze.linalg._
import java.io.File
import expedia.model.dest.DestModel

case class RegDestModelBuilder() {

  //key ((userRegId,destId)
  private val clusterHistByRegDest = MulticlassHistByKey[Tuple2[Int, Int]](100)

  val countsByRegDest = csvread(new File("c:/perforce/daniel/ex/statistics/countsByRegDest.csv"), skipLines = 1)
  (0 until countsByRegDest.rows).foreach { r =>
    val userReg = countsByRegDest(r, 0).toInt
    val destId = countsByRegDest(r, 1).toInt
    val counts = countsByRegDest(r, 2).toInt
    if (destId == 8250 && counts>50000) clusterHistByRegDest.add((userReg, destId), classId = 0, value = 0)
  }

  def processCluster(click: Click) = {

    val key = (click.userRegion, click.destId)
    if (clusterHistByRegDest.getMap.contains(key)) {

      if (click.isBooking == 1) clusterHistByRegDest.add(key, click.cluster)
      else clusterHistByRegDest.add(key, click.cluster, value = 0.05f)

    }
  }

  def create(): RegDestModel = {

    clusterHistByRegDest.normalise()
    RegDestModel(clusterHistByRegDest)
  }
}