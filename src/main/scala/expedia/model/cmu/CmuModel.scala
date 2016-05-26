package expedia.model.cmu

import expedia.data.Click
import breeze.linalg.DenseVector
import expedia.model.ClusterModel
import expedia.stats.MulticlassHistByKey
import scala.collection._
import breeze.linalg.max
import expedia.model.svm.loadClusterProbsByKeyMap2
import java.io.File
import breeze.linalg._
import expedia.stats.CounterMap
import expedia.model.dest.DestModel

case class CmuModel( 
    clusterHistByMDPU: Map[Tuple4[Int,Int, Int, Int], DenseVector[Float]],
      userCounterMap: CounterMap[Int], destCounterMap: CounterMap[Int], destMarketCounterMap: CounterMap[Tuple2[Int, Int]],
      destModel: DestModel) extends ClusterModel{
  
    
  
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

  
  
   
    def predict(click:Click): DenseVector[Float] = {

 
   val destCounts = destCounterMap.getOrElse(click.destId, 0)
    val destMarketCounts = destMarketCounterMap.getOrElse((click.destId, click.marketId), 0)

    var clusterProb =
      if (!userCounterMap.contains(click.userId) && destModel.svmDestIds.contains(click.destId) && click.stayDays < 3
        && !(destMarketCounts < 300 || destCounts / destMarketCounts > 1.5)) {
        destModel.predict(click.destId, click.continentId, click.stayDays)
      } else {
        clusterHistByMDPU((click.marketId,click.destId, click.isPackage,click.userId))
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
  
}