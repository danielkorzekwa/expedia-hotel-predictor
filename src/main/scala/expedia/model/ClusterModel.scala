package expedia.model

import expedia.data.Click
import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import java.util.concurrent.atomic.AtomicInteger
import com.typesafe.scalalogging.slf4j.LazyLogging
import expedia.util.getTop5Clusters

trait ClusterModel extends LazyLogging{
  
   def predict(click:Click): DenseVector[Float] 
  
   def predict(clicks: Seq[Click]): DenseMatrix[Float] = {
    val i = new AtomicInteger(0)
    val predictionRecords = clicks.par.map { click =>
      val predicted = predict(click)
      predicted
    }.toList

    val predictionMatrix = DenseVector.horzcat(predictionRecords: _*).t
    predictionMatrix
  }
   
     def predictTop5(clicks: Seq[Click]): DenseMatrix[Double] = {
    val i = new AtomicInteger(0)
    val predictionRecords = clicks.par.map { click =>
      val predicted = predict(click)

      val record = getTop5Clusters(predicted)
      if (i.incrementAndGet() % 100000 == 0) logger.info("Predicting clusters: %d".format(i.get))
      record
    }.toList

    val predictionMatrixMarketDest = DenseVector.horzcat(predictionRecords: _*).t
    predictionMatrixMarketDest
  }

}