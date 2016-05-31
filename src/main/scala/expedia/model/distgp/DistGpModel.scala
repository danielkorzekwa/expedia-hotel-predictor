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
 * @param rankedClustersByLocMarketDest  //key - (userLoc,market,destId), val Map[dist,rankedClusters]]
 */
case class DistGpModel(distGPModelMap: Map[(Int, Int, Int), Option[RankGprPredict]],rankedClustersByLocMarketDest:Map[Tuple3[Int, Int, Int], Map[Double, DenseVector[Int]]]) extends ClusterModel {

 // logger.info("Number of GP models:" + distGPModelMap.values.filter(m => m.isDefined).size)

  def predict(click: Click): DenseVector[Float] = {
    val key = (click.userLoc, click.marketId, click.destId)
//    if (click.dist > -1 && distGPModelMap.contains(key) && distGPModelMap(key).isDefined) {
//
//      val rankGPPredict = distGPModelMap(key).get
//      val predictedRanks = rankGPPredict.predict(DenseVector(click.dist))
//
//      val probVector = DenseVector.fill(100)(0f)
//      predictedRanks.toArray.zipWithIndex.foreach { case (cluster, index) => probVector(cluster.toInt) = (100f - index) }
//      normaliseMutable(probVector)
//      probVector
//    } else DenseVector.fill(100)(0f)
    
     val counter = new AtomicInteger(0)
     if (click.dist > -1 && rankedClustersByLocMarketDest.contains(key) && rankedClustersByLocMarketDest(key).contains(click.dist)) {
  println("DistGpModel:" + counter.incrementAndGet())
       
      val rankedClusterByDistMap = rankedClustersByLocMarketDest(key)
      val predictedRanks = rankedClusterByDistMap(click.dist)

      val probVector = DenseVector.fill(100)(0f)
      predictedRanks.toArray.zipWithIndex.foreach { case (cluster, index) => probVector(cluster.toInt) = (100f - index) }
      normaliseMutable(probVector)
      probVector
    } else DenseVector.fill(100)(0f)
  }
}

object DistGpModel extends LazyLogging {

  def build2(): DistGpModel = {
    val userLocMarketList = csvread(new File("c:/perforce/daniel/ex/segments/loc_market_dest/more_than_1000/userLocMarketList.csv"), skipLines = 1)

    //key - (userLoc,market,destId), val Map[dist,rankedClusters]]
    val rankedClustersByLocMarketDest: Map[Tuple3[Int, Int, Int], Map[Double, DenseVector[Int]]] = (0 until userLocMarketList.rows).

      map { row =>
        val userLoc = userLocMarketList(row, 0).toInt
        val marketId = userLocMarketList(row, 1).toInt
        val destId = userLocMarketList(row, 2).toInt
        val modelFile =  new File("c:/perforce/daniel/ex/segments/loc_market_dest/more_than_1000/predictions/predicted_clusters_loc_%d_market_%d_dest_%d.csv.csv".format(userLoc, marketId, destId))
        
        if(modelFile.exists()) {
        val rankedClustersByDistData = csvread(
        modelFile, skipLines = 1)

        val distPredictionMap = createRankedClustersByDistMap(rankedClustersByDistData)

        (userLoc, marketId, destId) -> distPredictionMap
        } else (userLoc, marketId, destId) -> Map[Double,DenseVector[Int]]()
        
      }.toMap

    DistGpModel(null,rankedClustersByLocMarketDest)
  }

 
}