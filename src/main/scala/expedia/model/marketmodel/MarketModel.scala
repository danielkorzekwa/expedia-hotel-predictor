package expedia.model.marketmodel

import expedia.stats.MulticlassHistByKey
import breeze.linalg.DenseVector
import expedia.data.Click
import breeze.linalg.DenseMatrix
import expedia.util.getTop5Clusters
import java.util.concurrent.atomic.AtomicInteger
import com.typesafe.scalalogging.slf4j.LazyLogging

case class MarketModel(clusterHistByMarket: MulticlassHistByKey[Int]) extends LazyLogging{

  def predict(marketId: Int): DenseVector[Float] = {

    clusterHistByMarket.getMap(marketId)

  }
  
   def predictTop5(clicks: Seq[Click]): DenseMatrix[Double] = {
    val i = new AtomicInteger(0)
    val predictionRecords = clicks.par.map { click =>
      val predicted = predict(click.marketId)

      val record = getTop5Clusters(predicted)
      if (i.incrementAndGet() % 100000 == 0) logger.info("Predicting clusters: %d".format(i.get))
      record
    }.toList

    val predictionMatrixMarketDest = DenseVector.horzcat(predictionRecords: _*).t
    predictionMatrixMarketDest
  }

}