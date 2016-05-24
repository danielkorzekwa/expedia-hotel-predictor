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
import breeze.linalg._
import java.io.File
import expedia.model.country.CountryModelBuilder
import expedia.data.ExDataSource
import expedia.HyperParams

case class DestModelBuilder(testClicks: Seq[Click], hyperParams: HyperParams) extends LazyLogging {

  private val clusterHistByDest = MulticlassHistByKey[Int](100)
  testClicks.foreach(click => clusterHistByDest.add(click.destId, click.cluster, value = 0))

  private val countryByDest: mutable.Map[Int, Int] = mutable.Map()
  testClicks.foreach(click => countryByDest += click.destId -> click.countryId)

  private val beta1 = hyperParams.getParamValue("expedia.model.dest.beta1").toFloat
  private val beta2 = hyperParams.getParamValue("expedia.model.dest.beta2").toFloat

  def processCluster(click: Click) = {

    if (clusterHistByDest.getMap.contains(click.destId)) {

      if (click.isBooking == 1) clusterHistByDest.add(click.destId, click.cluster)
      else clusterHistByDest.add(click.destId, click.cluster, value = beta1)

    }
  }

  def create(countryModel: CountryModel): DestModel = {

    clusterHistByDest.getMap.foreach { case (destId, clusterCounts) => clusterCounts :+= beta2 * countryModel.predict(countryByDest(destId)) }
    clusterHistByDest.normalise()

    DestModel(clusterHistByDest)
  }

}

object DestModelBuilder {
  def buildFromTrainingSet(trainDatasource: ExDataSource, testClicks: Seq[Click], hyperParams: HyperParams): DestModel = {

    val destModelBuilder = DestModelBuilder(testClicks, hyperParams)
    val countryModelBuilder = CountryModelBuilder(testClicks,hyperParams)

    def onClick(click: Click) = {

      destModelBuilder.processCluster(click)
      countryModelBuilder.processCluster(click)
    }
    trainDatasource.foreach { click => onClick(click) }

    val countryModel = countryModelBuilder.create()

    val destModel = destModelBuilder.create(countryModel)

    destModel
  }
}