package expedia

import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import scala.collection._
import scala.collection.mutable.ListBuffer
import expedia.model.svm.libsvm.svr.SvrModel
import expedia.model.svm.libsvm.svr.svrPredict
import java.util.concurrent.atomic.AtomicInteger
import com.typesafe.scalalogging.slf4j.LazyLogging

object combineClusterPredictions extends LazyLogging {

  /**
   * @param Top 5 predictions for three models[clusterDist,marketDest,clusterDistProx]. ClusterDist:  [p1,p2,p3,p4,p5,c1,c2,c3,c4,c5]
   *
   * @return Top 5 predictions  [p1,p2,p3,p4,p5,c1,c2,c3,c4,c5]
   */
  def apply(clusterDistPred: DenseMatrix[Double], marketDestPred: DenseMatrix[Double], clusterDistProxPred: DenseMatrix[Double],
            marketDestPredNoUser: DenseMatrix[Double]): DenseMatrix[Double] = {

    val i = new AtomicInteger(0)
    val top5ClustersSeq = (0 until clusterDistPred.rows).map { r =>
      val clusterDistPredVec = clusterDistPred(r, ::).t
      val marketDestPredVec = marketDestPred(r, ::).t
      val clusterDistProxPredVec = clusterDistProxPred(r, ::).t

      //(modelPriority,prob,cluster)
      val clusterDistVotes: Seq[Tuple3[Int, Double, Int]] = (0 until 5).map(i => (1, clusterDistPredVec(i), clusterDistPredVec(5 + i).toInt))
      val marketDestVotes: Seq[Tuple3[Int, Double, Int]] = (0 until 5).map(i => (2, marketDestPredVec(i), marketDestPredVec(5 + i).toInt))
      val clusterDistProxVotes: Seq[Tuple3[Int, Double, Int]] = (0 until 5).map(i => (3, clusterDistProxPredVec(i), clusterDistProxPredVec(5 + i).toInt))

      //  val clusterDistModel = SvrModel.loadFromFile("target/apk_clusterDistModel.svr")
      //  val marketDestModel = SvrModel.loadFromFile("target/apk_marketDestModel.svr")

      val prioritizedVotes = ListBuffer[Tuple3[Int, Double, Int]]()

      //fill clusterDistPred
      (0 until 5).foreach { i =>
        if (clusterDistVotes(i)._2 > 0) prioritizedVotes += clusterDistVotes(i)
      }

      //fill marketDestPred
      //  if(!prioritizedVotes.isEmpty && ((marketDestVotes(0)._2 > 0.87 && prioritizedVotes(0)._2 < 0.99) ||  ((marketDestVotes(0)._2 - prioritizedVotes(0)._2) > 0)) ) prioritizedVotes.insertAll(0,marketDestVotes)
      //   else  prioritizedVotes ++= marketDestVotes

      (0 until 5).foreach { i =>
        val vote = marketDestVotes(i)

        //        val worseVote = prioritizedVotes.find { otherVote =>
        //          val voteApk = svrPredict(DenseMatrix(vote._2), marketDestModel)(0)
        //          val otherApk = svrPredict(DenseMatrix(otherVote._2), clusterDistModel)(0)
        //          voteApk > otherApk
        //        }

      
           val worseVote = prioritizedVotes.find{otherVote =>
            if (otherVote._2==1 && vote._2>0.9) {println("true");true}
              else if (otherVote._2<0.995 && vote._2>0.7) {println("true");true}
             else if (otherVote._2<0.51 && vote._2>0.5) {println("true");true}
             else false
          }
     
        
      //best  
     //   val worseVote = prioritizedVotes.find(otherVote => (vote._2 > 0.5 && otherVote._2 < 0.5) || (vote._2 > 0.80 && otherVote._2 < 0.99) || otherVote._2 == 0.00)
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