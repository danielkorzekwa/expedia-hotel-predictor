package expedia.model.clusterdist2

import expedia.data.Click
import breeze.linalg.DenseMatrix
import expedia.stats.MulticlassHistByKey
import breeze.linalg.DenseVector
import java.util.concurrent.atomic.AtomicInteger
import com.typesafe.scalalogging.slf4j.LazyLogging

case class ClusterDist2Model(clusterHistByKey: MulticlassHistByKey[Tuple3[Double, Double, Double]]) extends LazyLogging{

  def predict(userLoc: Double, dist: Double, marketId: Double): DenseVector[Float] = {
    val key = (userLoc.toDouble, dist, marketId)
    clusterHistByKey.getMap(key)
  }

   def predict(clicks: Seq[Click]): DenseMatrix[Float] = {
      val i = new AtomicInteger(0)
    val predictionRecords = clicks.par.map { click =>
      val predicted = predict(click.userLoc, click.dist, click.marketId)
      predicted
    }.toList

    val predictionMatrix = DenseVector.horzcat(predictionRecords: _*).t
    predictionMatrix
   }
  
  def predictTop5(clicks: Seq[Click]): DenseMatrix[Double] = {
    val i = new AtomicInteger(0)
    val predictionRecords = clicks.par.map { click =>
      val predicted = predict(click.userLoc, click.dist, click.marketId)

      val predictedProbTuples = predicted.toArray.toList.zipWithIndex.sortWith((a, b) => a._1 > b._1).take(5).toArray

      val predictionProbs = predictedProbTuples.map(_._1.toDouble)
      val predictionRanks = predictedProbTuples.map(_._2.toDouble)

      if (i.incrementAndGet() % 5000000 == 0) logger.info("Predicting clusters: %d".format(i.get))

      val record = DenseVector.vertcat(DenseVector(predictionProbs), DenseVector(predictionRanks))
      record
    }.toList

    val predictionMatrix = DenseVector.horzcat(predictionRecords: _*).t
    predictionMatrix
  }

}
