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
import dk.gp.gpr.GprModel
import breeze.numerics._
import dk.gp.cov.CovSEiso
import dk.gp.gpr.predict
import dk.gp.gpr.gpr
import expedia.rankgpr.RankGprModel
import expedia.rankgpr.RankGprPredict
import expedia.model.destmonth.DestMonthModel

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

  def predictTop5(clicks: Seq[Click], destMonthModelsMap: Map[Int, DestMonthModel]): DenseMatrix[Double] = {

    val i = new AtomicInteger(0)
    val predictionRecords = clicks.map { click =>

      val probsAndRanksVec = destMonthModelsMap.get(click.destId) match {
        case Some(destMonthModel) => {
          val rankedClasses = destMonthModel.predictRankedClasses(click.dateTime)(0 until 5)
          val probsVec = DenseVector.fill(5)(Double.NaN)
          DenseVector.vertcat(probsVec, rankedClasses)
        }
        case _ => {
          val predicted = predict(click.destId)
          val record = getTop5Clusters(predicted)
          record
        }
      }

      if (i.incrementAndGet() % 100000 == 0) logger.info("Predicting clusters: %d".format(i.get))
      probsAndRanksVec
    
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