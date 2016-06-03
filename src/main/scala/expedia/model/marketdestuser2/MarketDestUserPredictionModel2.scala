package expedia.model.marketdestuser2

import java.util.concurrent.atomic.AtomicInteger
import scala.collection._
import com.typesafe.scalalogging.slf4j.LazyLogging
import breeze.linalg._
import breeze.linalg.DenseVector
import expedia.data.Click
import expedia.model.ClusterModel
import expedia.stats.MulticlassHist
import expedia.model.marketdest.MarketDestModel

case class MarketDestUserPredictionModel2(clusterHistByDestMarketUser: Map[Tuple3[Int, Int, Int], DenseVector[Float]]) extends ClusterModel with LazyLogging {

  def predict(marketId:Int,destId:Int,userId:Int): DenseVector[Float] = {
       clusterHistByDestMarketUser((destId, marketId, userId))
    }
  
  def predict(click: Click): DenseVector[Float] =  predict(click.marketId,click.destId,click.userId)

}

