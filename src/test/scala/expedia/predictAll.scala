package expedia

import breeze.linalg.DenseMatrix
import breeze.util.LazyLogger
import com.typesafe.scalalogging.slf4j.LazyLogging
import breeze.linalg.unique
import breeze.linalg.DenseVector
import dk.gp.util.averagePrecision
import java.util.concurrent.atomic.AtomicInteger
import breeze.linalg._
import expedia.model.ensemble.EnsemblePredictionModel
import expedia.model.svm.SVMPredictionModel
import scala.io.Source
import java.io.File
import expedia.model.popularhotels.PopularHotelsModelBuilder

object predictAll extends LazyLogging {

  /**
   * @return p1..p5,r1..r5
   */
  def apply(expediaTrainFile: String, expediaTestFile: String, svmPredictionsData: DenseMatrix[Double], test: DenseMatrix[Double]): DenseMatrix[Double] = {

    //  val testData = loadTestData(expediaTestFile)

    val userIds = test(::, 2).toArray.distinct.map(_.toInt).toSet

    logger.info("Computing stats...")

      val ensemblePredict = EnsemblePredictionModel(expediaTrainFile, svmPredictionsData, userIds)
    //val popularHotelsModel = PopularHotelsModel(expediaTrainFile)
    logger.info("Making predictions...")

    var c = new AtomicInteger(0)
    val predictionRecords = Source.fromFile(new File(expediaTestFile)).getLines().drop(1).toSeq.par.map { l =>
      val lArray = l.split(",")

      val userLoc = lArray(5).toInt
      val dist = if (lArray(6).equals("NA") || lArray(6).isEmpty()) -1d else lArray(6).toDouble
      val userId = lArray(7).toInt
      val destId = lArray(16).toInt
      val hotelContinent = lArray(20).toInt
      val market = lArray(22).toInt

         val predicted = ensemblePredict.predict(userLoc,dist,userId,destId,hotelContinent,market)
      //val predicted = popularHotelsModel.predict()

      val predictedProbTuples = predicted.toArray.toList.zipWithIndex.sortWith((a, b) => a._1 > b._1).take(5).toArray

      val predictionProbs = predictedProbTuples.map(_._1.toDouble)
      val predictionRanks = predictedProbTuples.map(_._2.toDouble)

      if (c.incrementAndGet() % 100000 == 0) logger.info("Processed test rows: %d".format(c.get))

      val record = DenseVector.vertcat(DenseVector(predictionProbs), DenseVector(predictionRanks))
      record
    }.toList

    val predictionMatrix = DenseVector.horzcat(predictionRecords: _*).t
    predictionMatrix
    //  DenseVector.horzcat(predictionRanksSeq: _*).t

  }
}