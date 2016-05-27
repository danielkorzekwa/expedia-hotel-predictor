package expedia.model.marketdestuser

import java.util.concurrent.atomic.AtomicInteger

import scala.collection._

import com.typesafe.scalalogging.slf4j.LazyLogging

import breeze.linalg._
import breeze.linalg.DenseVector
import expedia.data.Click
import expedia.model.ClusterModel
import expedia.stats.MulticlassHist

case class MarketDestUserPredictionModel(clusterHistByDestMarketUser: Map[Tuple3[Int, Int, Int], DenseVector[Float]]) extends ClusterModel with LazyLogging {

 
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
  
  def predict(click: Click): DenseVector[Float] =  predict(click.marketId,click.destId,click.userId)

}

