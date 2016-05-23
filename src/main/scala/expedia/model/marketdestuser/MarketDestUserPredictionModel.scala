package expedia.model.marketdestuser

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
import expedia.model.regdest.RegDestModel
import expedia.util.calcTopNClusters
import expedia.util.getTop5Clusters
import expedia.model.destbydist.DestByDistModel
import expedia.stats.MulticlassHist

case class MarketDestUserPredictionModel(
    destModel: DestModel,
    clusterHistByDestMarketUser: Map[Tuple3[Int, Int, Int], DenseVector[Float]],

    userCounterMap: CounterMap[Int], destCounterMap: CounterMap[Int], destMarketCounterMap: CounterMap[Tuple2[Int, Int]],
    regDestModel: RegDestModel, clusterDistProxModel: ClusterDistProxModel,
     destByDistModel:DestByDistModel) extends LazyLogging {

  val userLocMarketList = csvread(new File("c:/perforce/daniel/ex/svm/svm_dest_dist1000/userLocMarketList.csv"), skipLines = 1)

  //key - (userLoc,market,destId), val Map[dist,clusterProbs]]
  val svmDistPredictionsByLocMarketDist: Map[Tuple3[Int, Int, Int], Map[Double, DenseVector[Float]]] = (0 until userLocMarketList.rows).
    filter { row =>
      val userLoc = userLocMarketList(row, 0).toInt
      val marketId = userLocMarketList(row, 1).toInt
      val destId = userLocMarketList(row, 2).toInt
      new File("c:/perforce/daniel/ex/svm/svm_dest_dist1000/svm_predictions_loc_%d_market_%d_dest_%d.csv".format(userLoc, marketId, destId)).exists()
    }.
    map { row =>
      val userLoc = userLocMarketList(row, 0).toInt
      val marketId = userLocMarketList(row, 1).toInt
      val destId = userLocMarketList(row, 2).toInt
      val svmPredictionsByDistData = csvread(new File("c:/perforce/daniel/ex/svm/svm_dest_dist1000/svm_predictions_loc_%d_market_%d_dest_%d.csv".format(userLoc, marketId, destId)), skipLines = 1)

      val svmMap = try {
        loadClusterProbsByKeyMap2[Double](svmPredictionsByDistData)
      } catch {
        case e: Exception => {
          Map[Double, DenseVector[Float]]()
        }
      }

      (userLoc, marketId, destId) -> svmMap
    }.filter(_._2.size > 0).toMap

  /**
   * @param data [user_id,dest]
   * @param hotelCluster
   */

  var svmDistProbCounter = new AtomicInteger(0)

    private val clusterHist = MulticlassHist(100)
//    def predict(click: Click): DenseVector[Float] = {
//
//   val destId =  if (click.dist > -1 && click.destId == 8250) {
//    val destIds =  destByDistModel.predict(click)
//    if(destIds.contains(12208) && click.marketId==628){ 
//    
//        clusterHist.add(click.cluster)
//        println(clusterHist.getHistogram)
//     // println(destIds + ":" + click.marketId)
//    //  println( clusterHistByDestMarketUser.get((12208, click.marketId, click.userId)))
//     
//      12208
//      } else click.destId 
//    } 
//   else click.destId
//   
////   if(click.destId==12208) {
////     
////   }
//   
//    val destCounts = destCounterMap.getOrElse(destId, 0)
//    val destMarketCounts = destMarketCounterMap.getOrElse((destId, click.marketId), 0)
//
//    var clusterProb =
//      if (false && !userCounterMap.contains(click.userId) && destModel.svmDestIds.contains(destId) && click.stayDays < 3
//        && !(destMarketCounts < 300 || destCounts / destMarketCounts > 1.5)) {
//        destModel.predict(destId, click.continentId, click.stayDays)
//      } else {
//        try {
//         val prob =  clusterHistByDestMarketUser((destId, click.marketId, click.userId))
//      
//         prob
//        }
//        catch {
//          case e:Exception =>   clusterHistByDestMarketUser((click.destId, click.marketId, click.userId))
//        }
//      }
//    clusterProb = clusterProb.copy
//
////    if (click.dist > -1) {
////      val svmDistPrediction = svmDistPredictionsByLocMarketDist.get((click.userLoc, click.marketId, destId))
////      svmDistPrediction match {
////        case Some(svmDistPrediction) => {
////          //   logger.info(" svmDistProbCounter=" + svmDistProbCounter.getAndIncrement)
////          val probVec = svmDistPrediction.get(click.dist)
////
////          if (probVec.isDefined && (max(clusterProb) < 0.83)) probVec.get.foreachPair { (index, prob) => clusterProb(index) = prob }
////        }
////        case None => //do nothing
////      }
////    }
//
//    clusterProb
//  }

    
       def predict(click: Click): DenseVector[Float] = {

   val destId =  if (click.destId == 8250) {
    val destIds =  destByDistModel.predict(click)
    if(destIds.contains(12206)) 12206 else click.destId 
    } 
   else click.destId
   
   
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

    if (click.dist > -1) {
      val svmDistPrediction = svmDistPredictionsByLocMarketDist.get((click.userLoc, click.marketId, click.destId))
      svmDistPrediction match {
        case Some(svmDistPrediction) => {
          //   logger.info(" svmDistProbCounter=" + svmDistProbCounter.getAndIncrement)
          val probVec = svmDistPrediction.get(click.dist)

          if (probVec.isDefined && (max(clusterProb) < 0.83)) probVec.get.foreachPair { (index, prob) => clusterProb(index) = prob }
        }
        case None => //do nothing
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

  def predictTop5(clicks: Seq[Click]): DenseMatrix[Double] = {
    val i = new AtomicInteger(0)
    val predictionRecords = clicks.par.map { click =>
      val predicted = predict(click)
      val record = getTop5Clusters(predicted)
      if (i.incrementAndGet() % 100000 == 0) logger.info("Predicting clusters: %d".format(i.get))
      record
    }.toList

    val predictionMatrixMarketDest = DenseVector.horzcat(predictionRecords: _*).t
    predictionMatrixMarketDest
  }
}

