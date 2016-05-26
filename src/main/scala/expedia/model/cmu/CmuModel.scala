package expedia.model.cmu

import expedia.data.Click
import breeze.linalg.DenseVector
import expedia.model.ClusterModel
import expedia.stats.MulticlassHistByKey
import scala.collection._

case class CmuModel(clusterHistByMarketUser: MulticlassHistByKey[Tuple2[Int, Int]],
 
    clusterHistByDestMarketUser: Map[Tuple3[Int, Int, Int], DenseVector[Float]]) extends ClusterModel{
  
  
   def predict(marketId: Int, userId: Int): DenseVector[Float] = {

    clusterHistByMarketUser.getMap((marketId, userId))

  }
   
    def predict(click:Click): DenseVector[Float] = {

  //  clusterHistByMarketUser.getMap((click.marketId, click.userId))
 clusterHistByDestMarketUser((click.destId, click.marketId, click.userId))
  }
  
}