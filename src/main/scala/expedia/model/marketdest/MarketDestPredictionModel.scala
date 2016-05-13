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

case class MarketDestPredictionModel(
    destModel: DestModel,
    clusterHistByDestMarketUser: Map[Tuple3[Int, Int, Int], DenseVector[Float]],
    clusterProbsByDestMarket: Map[Tuple2[Int, Int], DenseVector[Float]],
    clusterDistProxModel: ClusterDistProxModel,
    userCounterMap:CounterMap[Int],destCounterMap:CounterMap[Int],destMarketCounterMap:CounterMap[Tuple2[Int, Int]]) extends LazyLogging {

  /**
   * @param data [user_id,dest]
   * @param hotelCluster
   */
  def predict(click: Click): DenseVector[Float] = {
   // val clusterProb = clusterHistByDestMarketUser((click.destId, click.marketId, click.userId))

      val destCounts = destCounterMap.getOrElse(click.destId, 0)
      val destMarketCounts = destMarketCounterMap.getOrElse((click.destId, click.marketId), 0)
      
        val clusterProb =
      if (!userCounterMap.contains(click.userId) && destModel.svmDestIds.contains(click.destId)
        && !(destMarketCounts < 300 || destCounts / destMarketCounts > 1.5)) {
        destModel.predict(click.destId, click.continentId, click.stayDays)
      } else clusterHistByDestMarketUser((click.destId, click.marketId, click.userId))
    
    
    val clusterDistProxProbs = clusterDistProxModel.predict(click)

    
    
     clusterDistProxProbs.foreachPair{(index,prob) => 
       if(prob<0.0010) {
         
//         if(click.userLoc==24103 && click.marketId==628 && click.dist==227.5322) {
//           println("...")
//         }
        clusterProb(index)=prob
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
  
  def predictTop5(clicks: Seq[Click],param:Double=0.003): DenseMatrix[Double] = {
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

