package expedia.model.regdest

import expedia.stats.MulticlassHistByKey
import breeze.linalg.DenseVector
import expedia.data.Click
import breeze.linalg.DenseMatrix
import expedia.util.getTop5Clusters
import java.util.concurrent.atomic.AtomicInteger
import com.typesafe.scalalogging.slf4j.LazyLogging

/**
 * @param clusterHistByRegDest key ((userRegId,destId)
 */
case class RegDestModel(clusterHistByRegDest: MulticlassHistByKey[Tuple2[Int, Int]]) extends LazyLogging {

  def predictionExists(userReg: Int, destId: Int): Boolean = {
    val key = (userReg, destId)
    clusterHistByRegDest.getMap.contains(key)
  }
  def predict(userReg: Int, destId: Int): DenseVector[Float] = {
    val key = (userReg, destId)
    clusterHistByRegDest.getMap(key)
  }

  def predictTop5(clicks: Seq[Click]): DenseMatrix[Double] = {
    val i = new AtomicInteger(0)
    val predictionRecords = clicks.par.map { click =>
      val predicted = predict(click.userRegion, click.destId)

      val record = getTop5Clusters(predicted)
      if (i.incrementAndGet() % 100000 == 0) logger.info("Predicting clusters: %d".format(i.get))
      record
    }.toList

    val predictionMatrixMarketDest = DenseVector.horzcat(predictionRecords: _*).t
    predictionMatrixMarketDest
  }
}