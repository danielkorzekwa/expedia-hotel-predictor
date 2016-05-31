package expedia

import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import scala.collection._
import scala.collection.mutable.ListBuffer
import expedia.model.svm.libsvm.svr.SvrModel
import expedia.model.svm.libsvm.svr.svrPredict
import java.util.concurrent.atomic.AtomicInteger
import com.typesafe.scalalogging.slf4j.LazyLogging
import breeze.linalg._

object combineClusterPredictions extends LazyLogging {

  /**
   * @param Top 5 predictions for three models[clusterDist,marketDest,clusterDistProx]. ClusterDist:  [p1,p2,p3,p4,p5,c1,c2,c3,c4,c5]
   *
   * @return Top 5 predictions  [p1,p2,p3,p4,p5,c1,c2,c3,c4,c5]
   */
  def apply(clusterDistPred: DenseMatrix[Double], marketDestPred: DenseMatrix[Double], clusterDistProxPred: DenseMatrix[Double], 
      distSvmPred: DenseMatrix[Double],distGPPred: DenseMatrix[Double]): DenseMatrix[Double] = {

    val i = new AtomicInteger(0)
    val top5ClustersSeq = (0 until clusterDistPred.rows).map { r =>
      val clusterDistPredVec = clusterDistPred(r, ::).t
      val marketDestPredVec = {
        val marketDestPredVec = marketDestPred(r, ::).t
        val distSvmPredVec = distSvmPred(r, ::).t
        val distGPPredVec =  distGPPred(r, ::).t
       if ( max(distGPPredVec(0 until 5)) > 0.0 && (max(marketDestPredVec(0 until 5)) < 0.7885)) distGPPredVec
       else if ( max(distSvmPredVec(0 until 5)) > 0.0 && (max(marketDestPredVec(0 until 5)) < 0.7885)) {
          distSvmPredVec
        }
        else marketDestPredVec
      }
      val clusterDistProxPredVec = clusterDistProxPred(r, ::).t

      //(modelPriority,prob,cluster)
      val clusterDistVotes: Seq[Tuple3[Int, Double, Int]] = (0 until 5).map(i => (1, clusterDistPredVec(i), clusterDistPredVec(5 + i).toInt))
      val marketDestVotes: Seq[Tuple3[Int, Double, Int]] = (0 until 5).map(i => (2, marketDestPredVec(i), marketDestPredVec(5 + i).toInt))
      val clusterDistProxVotes: Seq[Tuple3[Int, Double, Int]] = (0 until 5).map(i => (3, clusterDistProxPredVec(i), clusterDistProxPredVec(5 + i).toInt))

      val prioritizedVotes = ListBuffer[Tuple3[Int, Double, Int]]()

      //fill clusterDistPred
      (0 until 5).foreach { i =>
        if (clusterDistVotes(i)._2 > 0) prioritizedVotes += clusterDistVotes(i)
      }

      //marketDestVotes
      (0 until 5).foreach { i =>
        val vote = marketDestVotes(i)

        val worseVote = prioritizedVotes.find { otherVote =>

          if (otherVote._2 == 1 && vote._2 > 0.9) { true }
          else if (otherVote._2 > 0.985 && otherVote._2 < 0.995 && vote._2 > 0.8) { true }
          else if (otherVote._2 > 0.975 && otherVote._2 < 0.985 && vote._2 > 0.8) { true }
          else if (otherVote._2 > 0.965 && otherVote._2 < 0.975 && vote._2 > 0.8) { true }
          else if (otherVote._2 > 0.955 && otherVote._2 < 0.965 && vote._2 > 0.8) { true }
          else if (otherVote._2 > 0.495 && otherVote._2 < 0.505 && vote._2 > 0.5) { true }
          else if (otherVote._2 > 0.485 && otherVote._2 < 0.495 && vote._2 > 0.5) { true }
          else if (otherVote._2 > 0.475 && otherVote._2 < 0.485 && vote._2 > 0.5) { true }
          else if (otherVote._2 > 0.465 && otherVote._2 < 0.475 && vote._2 > 0.5) { true }
          else false
        }

        if (worseVote.isDefined) {
          prioritizedVotes.insert(prioritizedVotes.indexOf(worseVote.get), vote)
        } else prioritizedVotes += vote

      }

      //fill clusterDistPredProx
      (0 until 5).foreach { i =>

        val vote = clusterDistProxVotes(i)
        if (vote._2 > 0.3) {
          val worseVote = prioritizedVotes.find(otherVote => otherVote._1 == 2 && vote._2 > otherVote._2)
          if (worseVote.isDefined) {
            prioritizedVotes.insert(prioritizedVotes.indexOf(worseVote.get), vote)
          }
        }

      }

      val uniquePrioritizedVotes = ListBuffer[Tuple2[Double, Int]]()
      prioritizedVotes.foreach { vote =>
        if (!uniquePrioritizedVotes.exists(uniqueVote => uniqueVote._2 == vote._3)) uniquePrioritizedVotes += vote._2 -> vote._3
      }

      val top5ClustersBuffer = uniquePrioritizedVotes.toList.take(5)

      val predictionProbs = top5ClustersBuffer.map(_._1.toDouble).toArray
      val predictionRanks = top5ClustersBuffer.map(_._2.toDouble).toArray

      val record = DenseVector.vertcat(DenseVector(predictionProbs), DenseVector(predictionRanks))

      if (i.incrementAndGet() % 500000 == 0) logger.info("Combine predictions: %d".format(i.get))

      record
    }
    val top5ClustersMat = DenseVector.horzcat(top5ClustersSeq: _*).t
    top5ClustersMat
  }
}