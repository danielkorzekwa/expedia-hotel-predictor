package expedia.model.marketdest

import expedia.stats.MulticlassHistByKey
import breeze.linalg.DenseVector

case class MarketDestModel(clusterHistByMarketDest: MulticlassHistByKey[Tuple2[Int, Int]]) {

  def predict(marketId: Int, destId: Int): DenseVector[Float] = {

    clusterHistByMarketDest.getMap((marketId, destId))

  }
}