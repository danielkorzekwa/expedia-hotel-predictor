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

      //fill marketDestPred
      (0 until 5).foreach { i =>
        val vote = marketDestVotes(i)
        val worseVote = prioritizedVotes.find(otherVote => vote._2 > otherVote._2 || vote._2 > 0.80)
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
      record
    }
    val top5ClustersMat = DenseVector.horzcat(top5ClustersSeq: _*).t
    top5ClustersMat
  }
}