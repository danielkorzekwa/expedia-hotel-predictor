package expedia.model.marketdest

import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix
import breeze.linalg._
import com.typesafe.scalalogging.slf4j.LazyLogging
import java.util.concurrent.atomic.AtomicInteger
import scala.collection._
import expedia.model.svm.loadClusterProbsByDestMap
import expedia.model.svm.SVMPredictionModel
import expedia.data.ExDataSource
import expedia.data.Click
import expedia.stats.CounterMap
import expedia.model.dest.DestModelBuilder
import expedia.model.dest.DestModel
import expedia.stats.MulticlassHistByKey
import expedia.model.country.CountryModelBuilder
import expedia.model.country.CountryModelBuilder
import expedia.model.country.CountryModelBuilder
import expedia.model.clusterdist.ClusterDistPredictionModel
import expedia.model.clusterdistprox.ClusterDistProxModelBuilder
import expedia.model.clusterdistprox.ClusterDistProxModel
import expedia.stats.CounterMap
import expedia.stats.CounterMap
import expedia.model.svm.loadClusterProbsByKeyMap
import java.io.File
import expedia.model.svm.loadClusterProbsByKeyMap2

case class MarketDestPredictionModel(
    destModel: DestModel,
    clusterHistByDestMarketUser: Map[Tuple3[Int, Int, Int], DenseVector[Float]],
    clusterProbsByDestMarket: Map[Tuple2[Int, Int], DenseVector[Float]],
    clusterDistProxModel: ClusterDistProxModel,
    userCounterMap: CounterMap[Int], destCounterMap: CounterMap[Int], destMarketCounterMap: CounterMap[Tuple2[Int, Int]]) extends LazyLogging {

   val svmPredictionsByDistData1 = csvread(new File("c:/perforce/daniel/ex/svm/svm_predictions_dist.csv"), skipLines = 1)
val svmPredictionsByDistMap1 = loadClusterProbsByKeyMap2[Double](svmPredictionsByDistData1)
   
  val svmPredictionsByDistData2 = csvread(new File("c:/perforce/daniel/ex/svm/svm_predictions_dist2.csv"), skipLines = 1)
val svmPredictionsByDistMap2 = loadClusterProbsByKeyMap2[Double](svmPredictionsByDistData2)

  

  /**
   * @param data [user_id,dest]
   * @param hotelCluster
   */
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

    if (click.userLoc == 24103 && click.marketId == 365 && click.dist > -1) {
      val probVec = svmPredictionsByDistMap1(click.dist)
      println(click)
      println(probVec.toArray.map(x => "%.3f".format(x)).toList)
      println(clusterDistProxProbs.toArray.map(x => "%.3f".format(x)).toList)
      println("--------------------------")

      probVec.foreachPair { (index, prob) =>
   //    if (prob < 0.005) clusterProb(index) = prob
      }
    }
    
      if (click.userLoc == 24103 && click.marketId == 628 && click.dist > -1) {
      val probVec = svmPredictionsByDistMap2(click.dist)
      println(click)
      println(probVec.toArray.map(x => "%.3f".format(x)).toList)
      println(clusterDistProxProbs.toArray.map(x => "%.3f".format(x)).toList)
      println("--------------------------")

      probVec.foreachPair { (index, prob) =>

    //    if (prob < 0.005) clusterProb(index) = prob
      }
    }

    clusterDistProxProbs.foreachPair { (index, prob) =>

      if (click.userLoc == 24103 && click.marketId == 365  && prob < 0.008) {

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

