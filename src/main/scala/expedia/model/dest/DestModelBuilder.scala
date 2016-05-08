package expedia.model.dest

import expedia.data.Click
import expedia.stats.MulticlassHistByKey
import expedia.stats.calcVectorProbsMutable
import expedia.stats.calcVectorMapProbsMutable
import expedia.model.marketdest.MarketDestPredictionModel
import expedia.stats.CatStats
import expedia.model.svm.loadClusterProbsByDestMap
import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix
import scala.collection._
import com.typesafe.scalalogging.slf4j.LazyLogging
import scala.collection._
import expedia.model.country.CountryModel
case class DestModelBuilder(svmPredictionsData: DenseMatrix[Double], testClicks: Seq[Click]) extends LazyLogging {

  private val clusterHistByDest = MulticlassHistByKey[Int](100)
  val clusterProbByDestMapSVM: Map[Int, DenseVector[Float]] = loadClusterProbsByDestMap(svmPredictionsData)

  private val countryByDest: mutable.Map[Int, Int] = mutable.Map()
  testClicks.foreach(click => countryByDest += click.destId -> click.countryId)

  def processCluster(click: Click) = {

    if (click.isBooking == 1) clusterHistByDest.add(click.destId, click.cluster)
    else clusterHistByDest.add(click.destId, click.cluster, value = 0.05f)

    countryByDest += click.destId -> click.countryId
  }

  def create(countryModel: CountryModel): DestModel = {

    clusterHistByDest.getMap.foreach { case (destId, clusterCounts) => clusterCounts :+= countryModel.predict(countryByDest(destId)) }
    calcVectorMapProbsMutable(clusterHistByDest.getMap.toMap)

    DestModel(clusterHistByDest, clusterProbByDestMapSVM)
  }

}