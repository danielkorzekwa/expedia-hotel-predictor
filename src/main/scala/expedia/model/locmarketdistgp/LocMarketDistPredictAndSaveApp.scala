package expedia.model.locmarketdistgp

import expedia.data.ExCSVDataSource
import breeze.linalg.DenseVector
import java.io.File
import breeze.linalg._
import expedia.rankgpr.RankGprModel
import expedia.rankgpr.RankGprPredict
import dk.gp.util.csvwrite
import java.util.concurrent.atomic.AtomicInteger
import com.typesafe.scalalogging.slf4j.LazyLogging

object LocMarketDistPredictAndSaveApp extends LazyLogging {

  val covFunc = DistGPCovFunc()
  val covFuncParams = DenseVector[Double](-0.794353361706918, -11.251867145609713, -1.1522147378772258, 0.28935151100974615)
  val noiseLogStdDev = -3.0328025222890753

  def main(args: Array[String]): Unit = {
    val userLocMarketList = csvread(new File("c:/perforce/daniel/ex/segments/loc_market/more_than_100/userLocMarketList.csv"), skipLines = 1)

    val progress = new AtomicInteger(1)
    val distPredictionSeq = (0 until userLocMarketList.rows).map { r =>
      val userLoc = userLocMarketList(r, 0).toInt
      val marketId = userLocMarketList(r, 1).toInt
      val marketSize = userLocMarketList(r, 2).toInt

      logger.info("Predicting distGP for cluster %d/%d, loc=%d, market=%d, ".format(progress.getAndIncrement, userLocMarketList.rows, userLoc, marketId))

      val predOutputFile = "c:/perforce/daniel/ex/segments/loc_market/more_than_100/predictions/predicted_clusters_loc_%d_market_%d.csv".format(userLoc, marketId)

      if (!(userLoc == 36643 && marketId == 12) && marketSize < 5000 && !(new File(predOutputFile).exists())) {

        val trainDS = ExCSVDataSource(dsName = "trainDS", "c:/perforce/daniel/ex/segments/loc_market/more_than_100/train_2013_loc_%d_market_%d.csv".format(userLoc, marketId))
        val trainClicks = trainDS.getAllClicks()

        val testDS = ExCSVDataSource(dsName = "testDS", "c:/perforce/daniel/ex/segments/loc_market/more_than_100/train_2014_booked_only_loc_%d_market_%d.csv".format(userLoc, marketId))
        val testClicks = testDS.getAllClicks()

        if (testClicks.size > 0) {
          val trainDataX = DenseVector(trainClicks.map(c => c.dist).toArray).toDenseMatrix.t
          val trainDataY = DenseVector(trainClicks.map(c => c.cluster.toDouble).toArray)

          val testDataX = DenseVector(testClicks.map(c => c.dist).toArray).toDenseMatrix.t

          val model = RankGprModel(trainDataX, trainDataY, covFunc, covFuncParams, noiseLogStdDev)

          val gpPredict = RankGprPredict(model)

          val predictedClustersAll = gpPredict.predict(testDataX)
          val predictedClusters = predictedClustersAll(::, 0 until 5.min(predictedClustersAll.cols))

          val outputPrediction = DenseMatrix.horzcat(testDataX, predictedClusters)
          csvwrite(predOutputFile, outputPrediction, header = "dist,r1,r2,r3,r4,r5")

          outputPrediction
        }
      }
    }.toList

  }
}