package expedia.model.clusterdistprox

import breeze.linalg.DenseVector
import expedia.data.Click
import expedia.data.ExDataSource
import expedia.stats.MulticlassHistByKey
import scala.collection._
import expedia.util.calcTopNClusters
import breeze.linalg.DenseMatrix
import java.util.concurrent.atomic.AtomicInteger
import com.typesafe.scalalogging.slf4j.LazyLogging

//key - (userLoc,market),value - map[dist,clusterProbs]
case class ClusterDistProxModel(clusterHistByKey: mutable.Map[Tuple2[Int, Int], mutable.Map[Double, DenseVector[Float]]]) extends LazyLogging{

  def predict(click: Click): DenseVector[Float] = {

    val key = (click.userLoc, click.dist, click.marketId)

    val clusterProbs = clusterHistByKey((click.userLoc, click.marketId))(click.dist)

    //     if(click.dist==227.6473) {
    //      println("..." + calcTopNClusters(clusterProbs,100))
    //    }
    clusterProbs
  }

  def predictTop5(clicks: Seq[Click]): DenseMatrix[Double] = {
    val i = new AtomicInteger(0)
    val predictionRecordsClusterDistProx = clicks.par.map { click =>
      val predicted = predict(click)

      val predictedProbTuples = predicted.toArray.toList.zipWithIndex.sortWith((a, b) => a._1 > b._1).take(5).toArray

      val predictionProbs = predictedProbTuples.map(_._1.toDouble)
      val predictionRanks = predictedProbTuples.map(_._2.toDouble)

      if (i.incrementAndGet() % 100000 == 0) logger.info("Predicting clusters: %d".format(i.get))

      val record = DenseVector.vertcat(DenseVector(predictionProbs), DenseVector(predictionRanks))
      record
    }.toList

    val predictionMatrixClusterDistProx = DenseVector.horzcat(predictionRecordsClusterDistProx: _*).t
    predictionMatrixClusterDistProx

  }
}

object ClusterDistProxModel {

  def apply(trainDatasource: ExDataSource, testClicks: Seq[Click]): ClusterDistProxModel = {

    val modelBuilder = ClusterDistProxModelBuilder(testClicks)

    def onClick(click: Click) = {
      modelBuilder.processCluster(click)
    }
    trainDatasource.foreach { click => onClick(click) }

    val model = modelBuilder.create()
    model
  }
}