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
import scala.collection._
import java.util.concurrent.atomic.AtomicInteger

/**
 * @param clusterProbsByLocMarketDest  //key - (userLoc,market,destId), val Map[dist,clusterProbs]]
 */
case class DistGpModel(clusterProbsByLocMarketDest: Map[Tuple3[Int, Int, Int], Map[Double, DenseVector[Double]]]) extends ClusterModel {

  // logger.info("Number of GP models:" + distGPModelMap.values.filter(m => m.isDefined).size)
  val counter = new AtomicInteger(0)
  def predict(click: Click): DenseVector[Float] = {
    val key = (click.userLoc, click.marketId, click.destId)

    if (click.dist > -1 && clusterProbsByLocMarketDest.contains(key) && clusterProbsByLocMarketDest(key).contains(click.dist)) {
      println("DistGpModel:" + counter.incrementAndGet())

      val clusterProbsByDistMap = clusterProbsByLocMarketDest(key)
      val predictedClusterProbs = clusterProbsByDistMap(click.dist).map(_.toFloat)

      predictedClusterProbs
    } else DenseVector.fill(100)(0f)
  }
}

object DistGpModel extends LazyLogging {

  def build2(): DistGpModel = {
    val userLocMarketList = csvread(new File("c:/perforce/daniel/ex/segments/loc_market_dest/more_than_100/userLocMarketList.csv"), skipLines = 1)

    //key - (userLoc,market,destId), val Map[dist,rankedClusters]]
    val clusterProbsByLocMarketDest: Map[Tuple3[Int, Int, Int], Map[Double, DenseVector[Double]]] = (0 until userLocMarketList.rows).

      map { row =>
        val userLoc = userLocMarketList(row, 0).toInt
        val marketId = userLocMarketList(row, 1).toInt
        val destId = userLocMarketList(row, 2).toInt
        val modelFile = new File("c:/perforce/daniel/ex/segments/loc_market_dest/more_than_100/predictions/predicted_clusters_loc_%d_market_%d_dest_%d.csv.csv".format(userLoc, marketId, destId))

        if (modelFile.exists()) {
          val clusterProbsByDistData = csvread(
            modelFile, skipLines = 1)

          val distPredictionMap = createClusterProbsByDistMap(clusterProbsByDistData)

          (userLoc, marketId, destId) -> distPredictionMap
        } else (userLoc, marketId, destId) -> Map[Double, DenseVector[Double]]()

      }.toMap

    DistGpModel(clusterProbsByLocMarketDest)
  }

}