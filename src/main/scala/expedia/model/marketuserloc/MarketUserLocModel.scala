package expedia.model.marketuserloc

import breeze.linalg.DenseVector
import expedia.data.Click
import expedia.model.ClusterModel
import expedia.stats.MulticlassHistByKey

case class MarketUserLocModel(clusterHistByMarketUserLoc: MulticlassHistByKey[Tuple2[Int, Int]]) extends ClusterModel{
  
   def predict(marketId: Int, userLoc: Int): DenseVector[Float] = {

    clusterHistByMarketUserLoc.getMap((marketId, userLoc))

  }
   
    def predict(click:Click): DenseVector[Float] = {

    clusterHistByMarketUserLoc.getMap((click.marketId, click.userLoc))

  }
}