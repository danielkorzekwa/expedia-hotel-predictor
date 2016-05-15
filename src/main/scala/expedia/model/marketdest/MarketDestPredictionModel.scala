package expedia.model.marketdest

import java.io.File
import java.util.concurrent.atomic.AtomicInteger

import scala.collection._

import com.typesafe.scalalogging.slf4j.LazyLogging

import breeze.linalg._
import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import expedia.data.Click
import expedia.model.clusterdistprox.ClusterDistProxModel
import expedia.model.dest.DestModel
import expedia.model.svm.loadClusterProbsByKeyMap2
import expedia.stats.CounterMap
import expedia.stats.CounterMap
import expedia.stats.CounterMap

case class MarketDestPredictionModel(
    destModel: DestModel,
    clusterHistByDestMarketUser: Map[Tuple3[Int, Int, Int], DenseVector[Float]],
    clusterProbsByDestMarket: Map[Tuple2[Int, Int], DenseVector[Float]],
    clusterDistProxModel: ClusterDistProxModel,
    userCounterMap: CounterMap[Int], destCounterMap: CounterMap[Int], destMarketCounterMap: CounterMap[Tuple2[Int, Int]]) extends LazyLogging {

  val userLocMarketList = csvread(new File("c:/perforce/daniel/ex/svm/svm_dist1000/userLocMarketList.csv"), skipLines = 1)

  //key - (userLoc,market), val Map[dist,clusterProbs]]
  val svmDistPredictionsByLocMarket: Map[Tuple2[Int, Int], Map[Double, DenseVector[Float]]] = (0 until userLocMarketList.rows).
    filter { row =>
      val userLoc = userLocMarketList(row, 0).toInt
      val marketId = userLocMarketList(row, 1).toInt
      new File("c:/perforce/daniel/ex/svm/svm_dist1000/svm_predictions_loc_%d_market_%d.csv".format(userLoc, marketId)).exists()
    }.
    map { row =>
      val userLoc = userLocMarketList(row, 0).toInt
      val marketId = userLocMarketList(row, 1).toInt

      val svmPredictionsByDistData = csvread(new File("c:/perforce/daniel/ex/svm/svm_dist1000/svm_predictions_loc_%d_market_%d.csv".format(userLoc, marketId)), skipLines = 1)

      val svmMap = try {
        loadClusterProbsByKeyMap2[Double](svmPredictionsByDistData)
      } catch {
        case e: Exception => {
          Map[Double, DenseVector[Float]]()
        }
      }

      (userLoc, marketId) -> svmMap
    }.filter(_._2.size > 0).toMap
  println(svmDistPredictionsByLocMarket.size)
  /**
   * @param data [user_id,dest]
   * @param hotelCluster
   */

  var svmDistProbCounter = new AtomicInteger(0)
  def predict(click: Click): DenseVector[Float] = {
    // val clusterProb = clusterHistByDestMarketUser((click.destId, click.marketId, click.userId))

    val destCounts = destCounterMap.getOrElse(click.destId, 0)
    val destMarketCounts = destMarketCounterMap.getOrElse((click.destId, click.marketId), 0)

    var clusterProb =
      if (!userCounterMap.contains(click.userId) && destModel.svmDestIds.contains(click.destId) && click.stayDays < 3
        && !(destMarketCounts < 300 || destCounts / destMarketCounts > 1.5)) {
        destModel.predict(click.destId, click.continentId, click.stayDays)
      } else {
        clusterHistByDestMarketUser((click.destId, click.marketId, click.userId))
      }
    clusterProb = clusterProb.copy
    val clusterDistProxProbs = clusterDistProxModel.predict(click)

    if (click.dist > -1) {
      val svmDistPrediction = svmDistPredictionsByLocMarket.get((click.userLoc, click.marketId))
      svmDistPrediction match {
        case Some(svmDistPrediction) => {
          val probVec = svmDistPrediction(click.dist)

          probVec.foreachPair { (index, prob) =>
            if (prob < 0.008 && prob > 0) clusterProb(index) = prob
//clusterProb(index) = prob
          }
        }
        case None => //do nothing
      }
    }

    clusterDistProxProbs.foreachPair { (index, prob) =>

      if (prob < 0.005) {

        //  clusterProb(index) = prob
      }
    }

    clusterProb
  }

  def predict(clicks: Seq[Click]): DenseMatrix[Float] = {
    val i = new AtomicInteger(0)
    val predictionRecords = clicks.par.map { click =>
      val predicted = predict(click)
      predicted
    }.toList

    val predictionMatrix = DenseVector.horzcat(predictionRecords: _*).t
    predictionMatrix
  }

  def predictTop5(clicks: Seq[Click], param: Double = 0.003): DenseMatrix[Double] = {
    val i = new AtomicInteger(0)
    val predictionRecordsMarketDest = clicks.par.map { click =>
      val predicted = predict(click)

      val predictedProbTuples = predicted.toArray.toList.zipWithIndex.sortWith((a, b) => a._1 > b._1).take(5).toArray

      val predictionProbs = predictedProbTuples.map(_._1.toDouble)
      val predictionRanks = predictedProbTuples.map(_._2.toDouble)

      if (i.incrementAndGet() % 100000 == 0) logger.info("Predicting clusters: %d".format(i.get))

      val record = DenseVector.vertcat(DenseVector(predictionProbs), DenseVector(predictionRanks))
      record
    }.toList

    val predictionMatrixMarketDest = DenseVector.horzcat(predictionRecordsMarketDest: _*).t
    predictionMatrixMarketDest
  }
}

