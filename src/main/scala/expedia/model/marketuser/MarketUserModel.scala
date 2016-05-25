package expedia.model.marketuser

import breeze.linalg.DenseVector
import expedia.model.ClusterModel
import expedia.stats.MulticlassHistByKey
import expedia.data.Click

case class MarketUserModel(clusterHistByMarketUser: MulticlassHistByKey[Tuple2[Int, Int]]) extends ClusterModel{
  
   def predict(marketId: Int, userId: Int): DenseVector[Float] = {

    clusterHistByMarketUser.getMap((marketId, userId))

  }
   
    def predict(click:Click): DenseVector[Float] = {

    clusterHistByMarketUser.getMap((click.marketId, click.userId))

  }
}