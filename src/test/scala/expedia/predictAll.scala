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
import expedia.model.marketdest.MarketDestPredictionModel
import expedia.data.ExDataSource

object predictAll extends LazyLogging {

  /**
   * @return p1..p5,r1..r5
   */
  def apply(expediaTrainFile: String, expediaTestFile: String, svmPredictionsData: DenseMatrix[Double], test: DenseMatrix[Double]): DenseMatrix[Double] = {

    //  val testData = loadTestData(expediaTestFile)

    val userIds = test(::, 2).toArray.distinct.map(_.toInt).toSet

     val testClicks = ExDataSource(expediaTestFile).getAllClicks()
    
    logger.info("Computing stats...")

    
      val ensemblePredict = EnsemblePredictionModel(expediaTrainFile, svmPredictionsData, userIds,testClicks)
   // val marketDestModel = MarketDestPredictionModel(expediaTrainFile,svmPredictionsData)
    logger.info("Making predictions...")

    var c = new AtomicInteger(0)
    
   
    
    val predictionRecords = testClicks.par.map{click =>
         val predicted = ensemblePredict.predict(click.userLoc,click.dist,click.userId,click.destId,click.continentId,click.marketId)
      //val predicted = marketDestModel.predict(userId, marketId,destId, hotelContinent)

      val predictedProbTuples = predicted.toArray.toList.zipWithIndex.sortWith((a, b) => a._1 > b._1).take(5).toArray

      val predictionProbs = predictedProbTuples.map(_._1.toDouble)
      val predictionRanks = predictedProbTuples.map(_._2.toDouble)
      
         if (c.incrementAndGet() % 100000 == 0) logger.info("Predicting clusters: %d".format(c.get))

      val record = DenseVector.vertcat(DenseVector(predictionProbs), DenseVector(predictionRanks))
      record
    }.toList
    

    val predictionMatrix = DenseVector.horzcat(predictionRecords: _*).t
    predictionMatrix
    //  DenseVector.horzcat(predictionRanksSeq: _*).t

  }
}