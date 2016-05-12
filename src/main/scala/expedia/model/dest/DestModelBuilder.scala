package expedia.model.dest

import scala.collection._
import scala.collection._
import com.typesafe.scalalogging.slf4j.LazyLogging
import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import expedia.data.Click
import expedia.model.country.CountryModel
import expedia.model.svm.loadClusterProbsByDestMap
import expedia.model.svm.loadClusterProbsByKeyMap
import expedia.stats.MulticlassHistByKey
import expedia.stats.calcVectorMapProbsMutable
import breeze.linalg._
import java.io.File

case class DestModelBuilder(testClicks: Seq[Click]) extends LazyLogging {

  private val clusterHistByDest = MulticlassHistByKey[Int](100)
  testClicks.foreach(click => clusterHistByDest.add(click.destId, click.cluster, value = 0))

  private val countryByDest: mutable.Map[Int, Int] = mutable.Map()
  testClicks.foreach(click => countryByDest += click.destId -> click.countryId)

  def processCluster(click: Click) = {

    if (clusterHistByDest.getMap.contains(click.destId)) {
      if (click.isBooking == 1) clusterHistByDest.add(click.destId, click.cluster)
      else clusterHistByDest.add(click.destId, click.cluster, value = 0.05f)
    }
    countryByDest += click.destId -> click.countryId
  }

  def create(countryModel: CountryModel): DestModel = {

    clusterHistByDest.getMap.foreach { case (destId, clusterCounts) => clusterCounts :+= countryModel.predict(countryByDest(destId)) }
    calcVectorMapProbsMutable(clusterHistByDest.getMap.toMap)

    DestModel(clusterHistByDest)
  }

}