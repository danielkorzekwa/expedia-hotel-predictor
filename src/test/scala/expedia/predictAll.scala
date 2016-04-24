package expedia

import breeze.linalg.DenseMatrix
import breeze.util.LazyLogger
import com.typesafe.scalalogging.slf4j.LazyLogging
import breeze.linalg.unique
import breeze.linalg.DenseVector
import dk.gp.util.averagePrecision
import java.util.concurrent.atomic.AtomicInteger
import breeze.linalg._
import expedia.similarity.calcCatStats

object predictAll extends LazyLogging {

  /**
   * @return p1..p5,r1..r5,actual,mapk
   */
  def apply(train: DenseMatrix[Double], test: DenseMatrix[Double]): DenseMatrix[Double] = {

    val hotelClusters: DenseVector[Double] = unique(test(::, test.cols - 1)) //DenseVector(15, 46, 91, 1, 2) //

    logger.info("Computing stats...")

    val ensemblePredict = EnsemblePredict(train)

    logger.info("Making predictions...")
    var allCount = new AtomicInteger(0)
    var defModelCount = new AtomicInteger(0)

    val testData = test(::, List(0, 1, 2, 3, 4)).toDenseMatrix

    val predictionSeq = (0 until hotelClusters.size).map { i =>

      logger.info("predicting hotel cluster=" + i)
      val hotelCluster = hotelClusters(i)

      //  val svmFromCSV = svmFromCSVPredict.predict(testDataX, hotelCluster)
      //    val predicted = userLocMarketDistClusterMap.predict(testData, hotelCluster)
      val predicted = ensemblePredict.predict(testData, hotelCluster)
      predicted

    }.toList

    println("all count = %d, def model count = %d".format(allCount.get, defModelCount.get))

    logger.info("Computing output matrix")
    val predictionRanksSeq = (0 until test.rows).map { predRecId =>

      //seq[(prob,cluster)]
      val predictedProbTuples = (0 until hotelClusters.size).map { hotelCluster => (predictionSeq(hotelCluster)(predRecId), hotelClusters(hotelCluster)) }
        .toArray.sortWith((a, b) => a._1 > b._1).take(5)

      val predictionProbs = predictedProbTuples.map(_._1.toDouble)
      val predictionRanks = predictedProbTuples.map(_._2.toDouble)
      val actual = test(predRecId, test.cols - 1)
      val apk = averagePrecision(predictionRanks, Array(actual), 5)

      DenseVector.vertcat(DenseVector(predictionProbs), DenseVector(predictionRanks), DenseVector(actual, apk))
    }

    DenseVector.horzcat(predictionRanksSeq: _*).t
  }
}