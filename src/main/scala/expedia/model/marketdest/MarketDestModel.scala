package expedia.model.marketdest

import expedia.stats.MulticlassHistByKey
import breeze.linalg.DenseVector
import com.typesafe.scalalogging.slf4j.LazyLogging
import expedia.model.ClusterModel
import expedia.data.Click
import expedia.model.dest.DestModel

case class MarketDestModel(clusterHistByMarketDest: MulticlassHistByKey[Tuple2[Int, Int]],destModel:DestModel)  extends ClusterModel with LazyLogging{

  def predict(marketId: Int, destId: Int): DenseVector[Float] = {
    clusterHistByMarketDest.getMap((marketId, destId))
  }
  
   def predict(click:Click): DenseVector[Float] = {
     predict(click.marketId,click.destId)
   }
}