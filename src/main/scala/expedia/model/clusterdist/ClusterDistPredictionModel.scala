package expedia.model.clusterdist

import breeze.linalg.DenseVector
import com.typesafe.scalalogging.slf4j.LazyLogging
import scala.collection._
import breeze.linalg.DenseMatrix
import expedia.data.Click
import java.util.concurrent.atomic.AtomicInteger

/**
 * @param clusterByDistMap Map[(userLoc,dist,market),[all clusters for the key]]
 */
case class ClusterDistPredictionModel(topClusterByDistMap: Map[Tuple3[Double, Double, Double], DenseVector[Int]]) extends LazyLogging {

  logger.info("DistClusterMap size=%d".format(topClusterByDistMap.size))

  val distClutersSeq = topClusterByDistMap.map { case (key, clusters) => clusters }.toList
  val clusterCoExistMat = calcClusterCoExistMatrix(distClutersSeq)
  val similarClustersMatrix = calcSimilarClustersMap(clusterCoExistMat)

  
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
    val predictionRecordsClusterDist = clicks.par.map { click =>
      val predicted = predict(click.userLoc, click.dist, click.marketId)

      val predictedProbTuples = predicted.toArray.toList.zipWithIndex.sortWith((a, b) => a._1 > b._1).take(5).toArray

      val predictionProbs = predictedProbTuples.map(_._1.toDouble)
      val predictionRanks = predictedProbTuples.map(_._2.toDouble)

      if (i.incrementAndGet() % 5000000 == 0) logger.info("Predicting clusters: %d".format(i.get))

      val record = DenseVector.vertcat(DenseVector(predictionProbs), DenseVector(predictionRanks))
      record
    }.toList

    val predictionMatrixClusterDist = DenseVector.horzcat(predictionRecordsClusterDist: _*).t
predictionMatrixClusterDist
  }
  
  def predict(userLoc: Double, dist: Double, market: Double): DenseVector[Float] = {

    val clusterProbs = DenseVector.tabulate[Float](100) { hotelCluster =>

      val key = (userLoc, dist, market)
      val clusterVec = topClusterByDistMap.get(key)
      val prob2 = clusterVec match {
        case Some(clusterVec) => {
          val clusterIndex = clusterVec.toArray.toList.indexOf(hotelCluster)
          val prob = if (clusterIndex == -1d) {

            if (false && clusterVec.size > 0 && clusterVec.size <= 2) {
              val topCluster = clusterVec(0)

              if (hotelCluster == similarClustersMatrix(topCluster.toInt, 1)) {
                0.99f
              } else if (hotelCluster == similarClustersMatrix(topCluster.toInt, 2)) {
                0.99f - 0.0001
              } else if (hotelCluster == similarClustersMatrix(topCluster.toInt, 3)) {
                0.99f - 0.0002
              } else if (hotelCluster == similarClustersMatrix(topCluster.toInt, 4)) {
                0.99f - 0.0003
              } else 0f//Double.NaN
            } else 0f//Double.NaN
          } else 1f - 0.0001 * clusterIndex
          prob.toFloat
        }
        case None => {
        //  if(userLoc==24103 && market==628 && dist>227.2 && dist<227.8) {
        //    println(key)
        //  }
          0f//Double.NaN
        }
      }

      prob2

    }

    clusterProbs 
  }
}