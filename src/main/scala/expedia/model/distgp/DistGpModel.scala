package expedia.model.distgp

import expedia.data.Click
import expedia.rankgpr.RankGprModel
import expedia.data.ExCSVDataSource
import breeze.linalg.DenseVector
import expedia.model.ClusterModel
import expedia.rankgpr.RankGprPredict
import dk.gp.cov.CovSEiso
import expedia.stats.normaliseMutable
import breeze.linalg._
import expedia.rankgpr.rankGprTrain
import expedia.util.calcTopNClusters
import expedia.model.old.destmonth.DestMonthModel
import expedia.stats.MulticlassHist
import java.io.File
import breeze.numerics._
import com.typesafe.scalalogging.slf4j.LazyLogging
import expedia.rankgpr.RankGprModel
import expedia.rankgpr.RankGprPredict

case class DistGpModel(distGPModelMap: Map[(Int, Int, Int), RankGprPredict]) extends ClusterModel {

  def predict(click: Click): DenseVector[Float] = {
val key = (click.userLoc,click.marketId,click.destId)
    if (click.dist > -1 && distGPModelMap.contains(key)) {
      
      val rankGPPredict = distGPModelMap(key)
      val predictedRanks = rankGPPredict.predict(DenseVector(click.dist))

      val probVector = DenseVector.fill(100)(0f)
      predictedRanks.toArray.zipWithIndex.foreach { case (cluster, index) => probVector(cluster.toInt) = (100f - index) }
      normaliseMutable(probVector)
      probVector
    } else DenseVector.fill(100)(0f)
  }
}

object DistGpModel extends LazyLogging {

  /**
   * key - (loc,market,dest)
   */
  def build(): DistGpModel = {

    val userLocMarketList = csvread(new File("c:/perforce/daniel/ex/segments/loc_market_dest/userLocMarketList.csv"), skipLines = 1)

    val distGPModelMap = (0 until userLocMarketList.rows).par.map { r =>
      val userLoc = userLocMarketList(r, 0).toInt
      val marketId = userLocMarketList(r, 1).toInt
      val destId = userLocMarketList(r, 2).toInt

      val trainDS = ExCSVDataSource(dsName = "trainDS", "c:/perforce/daniel/ex/segments/loc_market_dest/train_2013_loc_%d_market_%d_dest_%d.csv".format(userLoc, marketId, destId))
      val trainClicks = trainDS.getAllClicks()

      val dataX = DenseVector(trainClicks.map(c => c.dist).toArray).toDenseMatrix.t
      val dataY = DenseVector(trainClicks.map(c => c.cluster.toDouble).toArray)

      val covFunc = CovSEiso()
      val covFuncParams = DenseVector[Double](log(1), log(1))
      val noiseLogStdDev = log(1d)
      val model = RankGprModel(dataX, dataY, covFunc, covFuncParams, noiseLogStdDev)
      val trainedModel = rankGprTrain(model)
      logger.info("Trained covFuncParams=" + trainedModel.covFuncParams)
      val rankGprPredict = try {
        RankGprPredict(trainedModel)
      } catch {
        case e: Exception => {
          logger.error("Creating rankGprPredict model for _loc_%d_market_%d_dest_%d failed".format(userLoc, marketId, destId))
          throw e
        }
      }

      (userLoc, marketId, destId) -> rankGprPredict
    }.toList.toMap

    DistGpModel(distGPModelMap)
  }
}