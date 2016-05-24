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
import expedia.model.ClusterModel

case class MarketDestUserPredictionModel(
    destModel: DestModel,
    clusterHistByDestMarketUser: Map[Tuple3[Int, Int, Int], DenseVector[Float]],

    userCounterMap: CounterMap[Int], destCounterMap: CounterMap[Int], destMarketCounterMap: CounterMap[Tuple2[Int, Int]],
    regDestModel: RegDestModel) extends ClusterModel with LazyLogging {

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

  //  def predict(click: Click): DenseVector[Float] = {
  //  
  //      var clusteProb =  clusterHistByDestMarketUser((click.destId, click.marketId, click.userId))
  //      
  //   if (click.dist > -1 && click.destId == 8250 && click.marketId==628) {
  //      val destIds =  destByDistModel.predict(click)
  //      if(destIds.contains(12208) && click.marketId==628){  
  //          clusterHist.add(click.cluster)
  //         
  //          
  //          try {
  //          clusteProb = clusterHistByDestMarketUser((12208, click.marketId, click.userId))
  //           println(clusterHist.getHistogram)
  //          }
  //          catch {
  //            case e:Exception => //
  //          }
  //      } 
  //   }
  //          
  //   clusteProb
  //  
  //    }

  def predict(marketId:Int,destId:Int,userId:Int): DenseVector[Float] = {
       clusterHistByDestMarketUser((destId, marketId, userId))
    }
  
  def predict(click: Click): DenseVector[Float] = {
//    val destId = if (click.destId == 8250) {
//      val destIds = destByDistModel.predict(click)
//      if (destIds.contains(12206)) 12206 else click.destId
//    } else click.destId

    val destCounts = destCounterMap.getOrElse(click.destId, 0)
    val destMarketCounts = destMarketCounterMap.getOrElse((click.destId, click.marketId), 0)

    var clusterProb =
      if (!userCounterMap.contains(click.userId) && destModel.svmDestIds.contains(click.destId) && click.stayDays < 3
        && !(destMarketCounts < 300 || destCounts / destMarketCounts > 1.5)) {
        destModel.predict(click.destId, click.continentId, click.stayDays)
      } else {
        clusterHistByDestMarketUser((click.destId, click.marketId, click.userId))
      }

//    if (click.dist > -1 && click.destId == 8250 && click.marketId == 628) {
//      val destIds = destByDistModel.predict(click)
//     
//      if (destIds.contains(12208) && click.marketId == 628) {
//        clusterHist.add(click.cluster)
//        clusterProb = clusterHistByDestMarketUser((12208, click.marketId, click.userId))
//      }
//      else if (destIds.contains(12206) && click.marketId == 628) {
//        clusterHist.add(click.cluster)
//        clusterProb = clusterHistByDestMarketUser((12206, click.marketId, click.userId))
//      }
//    }

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

}

