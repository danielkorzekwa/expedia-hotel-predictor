package expedia.model.distsvm

import breeze.linalg._
import expedia.model.svm.loadClusterProbsByKeyMap2
import java.io.File
import scala.collection._
import expedia.data.Click
import expedia.model.ClusterModel

case class DistSvmModel() extends ClusterModel {

  val userLocMarketList = csvread(new File("c:/perforce/daniel/ex/svm/svm_dest_dist1000/userLocMarketList.csv"), skipLines = 1)

  //key - (userLoc,market,destId), val Map[dist,clusterProbs]]
  val svmDistPredictionsByLocMarketDist: Map[Tuple3[Int, Int, Int], Map[Double, DenseVector[Float]]] = (0 until userLocMarketList.rows).
    filter { row =>
      val userLoc = userLocMarketList(row, 0).toInt
      val marketId = userLocMarketList(row, 1).toInt
      val destId = userLocMarketList(row, 2).toInt
      val count = userLocMarketList(row, 3).toInt
      new File("c:/perforce/daniel/ex/svm/svm_dest_dist100/svm_predictions_loc_%d_market_%d_dest_%d.csv".format(userLoc, marketId, destId)).exists()
    }.
    map { row =>
      val userLoc = userLocMarketList(row, 0).toInt
      val marketId = userLocMarketList(row, 1).toInt
      val destId = userLocMarketList(row, 2).toInt
      val svmPredictionsByDistData = csvread(new File("c:/perforce/daniel/ex/svm/svm_dest_dist1000/svm_predictions_loc_%d_market_%d_dest_%d.csv".format(userLoc, marketId, destId)), skipLines = 1)

      val svmMap = try {
        loadClusterProbsByKeyMap2[Double](svmPredictionsByDistData)
      } catch {
        case e: Exception => {
          Map[Double, DenseVector[Float]]()
        }
      }

      (userLoc, marketId, destId) -> svmMap
    }.filter(_._2.size > 0).toMap

  def predict(click: Click): DenseVector[Float] = {

    if (click.dist > -1) {
      val svmDistPrediction = svmDistPredictionsByLocMarketDist.get((click.userLoc, click.marketId, click.destId))
      svmDistPrediction match {
        case Some(svmDistPrediction) => {
          val probVec = svmDistPrediction.getOrElse(click.dist, DenseVector.fill(100)(0f))
          probVec

        }
        case None => {
          DenseVector.fill(100)(0f)
        }
      }
    } else {
      DenseVector.fill(100)(0f)
    }
  }

}