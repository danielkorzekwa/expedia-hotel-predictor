package expedia.model.marketmodel

import expedia.stats.MulticlassHistByKey
import breeze.linalg.DenseVector

case class MarketModel(clusterHistByMarket: MulticlassHistByKey[Int]) {

  def predict(marketId: Int): DenseVector[Float] = {

    clusterHistByMarket.getMap(marketId)

  }
}