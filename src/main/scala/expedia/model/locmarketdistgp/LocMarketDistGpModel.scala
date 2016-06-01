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
 * @param rankedClustersByLocMarket  //key - (userLoc,market), val Map[dist,rankedClusters]]
 */
case class LocMarketDistGpModel(rankedClustersByLocMarket: Map[Tuple2[Int, Int], Map[Double, DenseVector[Int]]]) extends ClusterModel {

  val counter = new AtomicInteger(0)
  def predict(click: Click): DenseVector[Float] = {
    val key = (click.userLoc, click.marketId)

    if (click.dist > -1 && rankedClustersByLocMarket.contains(key) && rankedClustersByLocMarket(key).contains(click.dist)) {
      println("LocMarketDistGpModel:" + counter.incrementAndGet())

      val rankedClusterByDistMap = rankedClustersByLocMarket(key)
      val predictedRanks = rankedClusterByDistMap(click.dist)

      val probVector = DenseVector.fill(100)(0f)
      predictedRanks.toArray.zipWithIndex.foreach { case (cluster, index) => probVector(cluster.toInt) = (100f - index) }
      normaliseMutable(probVector)
      probVector
    } else DenseVector.fill(100)(0f)
  }
}

object LocMarketDistGpModel extends LazyLogging {

  def build2(): LocMarketDistGpModel = {
    val userLocMarketList = csvread(new File("c:/perforce/daniel/ex/segments/loc_market/more_than_100/userLocMarketList.csv"), skipLines = 1)

    //key - (userLoc,market), val Map[dist,rankedClusters]]
    val rankedClustersByLocMarket: Map[Tuple2[Int, Int], Map[Double, DenseVector[Int]]] = (0 until userLocMarketList.rows).

      map { row =>
        val userLoc = userLocMarketList(row, 0).toInt
        val marketId = userLocMarketList(row, 1).toInt
        val modelFile = new File("c:/perforce/daniel/ex/segments/loc_market/more_than_100/predictions/predicted_clusters_loc_%d_market_%d.csv".format(userLoc, marketId))

        if (modelFile.exists()) {
          val rankedClustersByDistData = csvread(
            modelFile, skipLines = 1)

          val distPredictionMap = createRankedClustersByDistMap(rankedClustersByDistData)

          (userLoc, marketId) -> distPredictionMap
        } else (userLoc, marketId) -> Map[Double, DenseVector[Int]]()

      }.toMap

    LocMarketDistGpModel(rankedClustersByLocMarket)
  }

}