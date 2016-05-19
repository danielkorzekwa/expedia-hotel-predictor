package expedia.model.dest

import java.io.File
import scala.collection.Map
import scala.collection.Seq
import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import breeze.linalg.csvread
import expedia.data.Click
import expedia.data.ExDataSource
import expedia.model.country.CountryModelBuilder
import expedia.model.svm.loadClusterProbsByKeyMap
import expedia.stats.MulticlassHistByKey
import scala.collection._
import java.util.concurrent.atomic.AtomicInteger
import com.typesafe.scalalogging.slf4j.LazyLogging
import expedia.util.getTop5Clusters

case class DestModel(
    clusterHistByDest: MulticlassHistByKey[Int]) extends LazyLogging {

  val svmDestIds = List(8250, 8267, 8253, 8279, 12206, 8745, 8268, 8230, 8791, 8260, 8254)
  //  val svmDestIds = Set(8250, 8267,  8253,  8279, 12206,  8745,  8268,  8230,  8791,  8260,  8254 , 8291 , 7635, 8223 , 8746 , 8220,  8788,  8242 , 8278 , 8819 ,468 ,26022 , 8281 , 8213, 669 , 8288 , 8282 , 8287 ,11353 , 8739 ,12603 , 8747, 11439 , 8266 ,12233 , 8818 , 8255, 12175)

  //key - destId, val Map[stayDays,clusterProbs]]
  val clusterProbsByStaydays: Map[Int, Map[Int, DenseVector[Float]]] = svmDestIds.map { destId =>

    val svmPredictionsByStaydaysData = csvread(new File("c:/perforce/daniel/ex/svm/svm_predictions_" + destId + ".csv"), skipLines = 1)

    destId -> loadClusterProbsByKeyMap(svmPredictionsByStaydaysData)
  }.toMap

  def predict(destId: Int, continentId: Int, stayDays: Int): DenseVector[Float] = {

    val clusterProbs = clusterProbsByStaydays(destId).getOrElse(stayDays, clusterHistByDest.getMap(destId))

    clusterProbs

    // clusterHistByDest.getMap(destId)

  }

  def predict(destId: Int): DenseVector[Float] = {

    clusterHistByDest.getMap(destId)

  }

  def predictTop5(clicks: Seq[Click]): DenseMatrix[Double] = {
    val i = new AtomicInteger(0)
    val predictionRecords = clicks.par.map { click =>
      val predicted = predict(click.destId)

      val record = getTop5Clusters(predicted)
      if (i.incrementAndGet() % 100000 == 0) logger.info("Predicting clusters: %d".format(i.get))
      record
    }.toList

    val predictionMatrixMarketDest = DenseVector.horzcat(predictionRecords: _*).t
    predictionMatrixMarketDest
  }

}

object DestModel {

  def apply(expediaTrainFile: String, svmPredictionsData: DenseMatrix[Double], testClicks: Seq[Click]): DestModel = {

    val countryModelBuilder = CountryModelBuilder(testClicks)
    val destModelBuilder = DestModelBuilder(testClicks)

    def onClick(click: Click) = {
      countryModelBuilder.processCluster(click)
      destModelBuilder.processCluster(click)

    }
    ExDataSource(dsName = "trainDS", expediaTrainFile).foreach { click => onClick(click) }

    val countryModel = countryModelBuilder.create()
    val destModel = destModelBuilder.create(countryModel)
    destModel
  }

}