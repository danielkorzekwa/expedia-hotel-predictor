package expedia.model.marketuser

import expedia.stats.MulticlassHistByKey
import breeze.linalg.DenseVector

case class MarketUserModel(clusterHistByMarketUser: MulticlassHistByKey[Tuple2[Int, Int]]) {
  
   def predict(marketId: Int, userId: Int): DenseVector[Float] = {

    clusterHistByMarketUser.getMap((marketId, userId))

  }
}