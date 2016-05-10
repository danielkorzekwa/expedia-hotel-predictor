package expedia

import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import scala.collection._
import scala.collection.mutable.ListBuffer

object combineClusterPredictions {

  /**
   * @param Top 5 predictions for three models[clusterDist,marketDest,clusterDistProx]. ClusterDist:  [p1,p2,p3,p4,p5,c1,c2,c3,c4,c5]
   *
   * @return Top 5 predictions  [p1,p2,p3,p4,p5,c1,c2,c3,c4,c5]
   */
  def apply(clusterDistPred: DenseMatrix[Double], marketDestPred: DenseMatrix[Double], clusterDistProxPred: DenseMatrix[Double]): DenseMatrix[Double] = {

    val top5ClustersSeq = (0 until clusterDistPred.rows).map { r =>
      val clusterDistPredVec = clusterDistPred(r, ::).t
      val marketDestPredVec = marketDestPred(r, ::).t

      // seq of (prob,cluster)
      val top5ClustersBuffer = ListBuffer[Tuple2[Double, Int]]()

      //fill clusterDistPred
      (0 until (5 - top5ClustersBuffer.size)).foreach { i =>
        if (clusterDistPredVec(i) > 0) top5ClustersBuffer += clusterDistPredVec(i) -> clusterDistPredVec(5 + i).toInt
      }

      //fill marketDestPred
      (0 until (5 - top5ClustersBuffer.size)).foreach { i =>
        top5ClustersBuffer += marketDestPredVec(i) -> marketDestPredVec(5 + i).toInt
      }

      val predictionProbs = top5ClustersBuffer.map(_._1.toDouble).toArray
      val predictionRanks = top5ClustersBuffer.map(_._2.toDouble).toArray
      val record = DenseVector.vertcat(DenseVector(predictionProbs), DenseVector(predictionRanks))
      record
    }
    val top5ClustersMat = DenseVector.horzcat(top5ClustersSeq: _*).t
    top5ClustersMat
  }
}