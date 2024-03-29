package expedia.model.dest

import java.io.File

import scala.collection._
import scala.collection.Map

import com.typesafe.scalalogging.slf4j.LazyLogging

import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import breeze.linalg.csvread
import expedia.data.Click
import expedia.model.ClusterModel
import expedia.model.svm.loadClusterProbsByKeyMap
import expedia.stats.MulticlassHistByKey

object DestModel {

  val svmDestIds = List(8250, 8267, 8253, 8279, 12206, 8745, 8268, 8230, 8791, 8260, 8254)
  //  val svmDestIds = Set(8250, 8267,  8253,  8279, 12206,  8745,  8268,  8230,  8791,  8260,  8254 , 8291 , 7635, 8223 , 8746 , 8220,  8788,  8242 , 8278 , 8819 ,468 ,26022 , 8281 , 8213, 669 , 8288 , 8282 , 8287 ,11353 , 8739 ,12603 , 8747, 11439 , 8266 ,12233 , 8818 , 8255, 12175)

  //key - destId, val Map[stayDays,clusterProbs]]
  val clusterProbsByStaydays: Map[Int, Map[Int, DenseVector[Float]]] = svmDestIds.map { destId =>

    val svmPredictionsByStaydaysData = csvread(new File("c:/perforce/daniel/ex/svm/svm_predictions_" + destId + ".csv"), skipLines = 1)

    destId -> loadClusterProbsByKeyMap(svmPredictionsByStaydaysData)
  }.toMap
}

case class DestModel(
    clusterHistByDest: MulticlassHistByKey[Int]) extends ClusterModel with LazyLogging {

  def predict(destId: Int, continentId: Int, stayDays: Int): DenseVector[Float] = {

    val clusterProbs = DestModel.clusterProbsByStaydays(destId).getOrElse(stayDays, clusterHistByDest.getMap(destId))

    clusterProbs

    // clusterHistByDest.getMap(destId)

  }

  def predict(destId: Int): DenseVector[Float] = {

    clusterHistByDest.getMap(destId)

  }

  def predict(click: Click): DenseVector[Float] = {

    predict(click.destId)

  }

}
