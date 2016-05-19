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

    val gpModel = buildGPModel()
    var a = new AtomicInteger(0)
    val i = new AtomicInteger(0)
    val predictionRecords = clicks.map { click =>
   
      val predicted = predict(click.destId).copy
       if(click.destId==12217) {
      val predictedGP = dk.gp.gpr.predict(DenseMatrix(click.checkinMonth.toDouble), gpModel)(0, 0)

      if (predictedGP < 0.5) {
        val old19 = predicted(19)
        val old21 = predicted(21)
        predicted(21) = old19
        predicted(19) = old21
      }
    }
      val record = getTop5Clusters(predicted)

      if (i.incrementAndGet() % 100000 == 0) logger.info("Predicting clusters: %d".format(i.get))
      record
    }.toList

    val predictionMatrixMarketDest = DenseVector.horzcat(predictionRecords: _*).t
    predictionMatrixMarketDest
  }

  def buildGPModel(): GprModel = {
    val allClicks = ExDataSource(dsName = "test", "c:/perforce/daniel/ex/segments/dest_12217/train_2013_dest12217.csv").getAllClicks()
    val filteredClicks = allClicks.filter { c => c.isBooking == 1 && (c.cluster == 19 || c.cluster == 21) && c.checkinMonth > -1 }

    val dataX = DenseVector(filteredClicks.map(c => c.checkinMonth.toDouble).toArray).toDenseMatrix.t
    val dataY = DenseVector(filteredClicks.map(c => if (c.cluster == 19) 1.0 else 0).toArray)


    println("trainning set size = " + dataX.rows)
    val covFunc = CovSEiso()
    val covFuncParams = DenseVector[Double](log(1), log(1))
    val noiseLogStdDev = log(0.5d)
    val mean = 0
    val model = gpr(dataX, dataY, covFunc, covFuncParams, noiseLogStdDev, mean)
    model
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