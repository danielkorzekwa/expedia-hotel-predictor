package expedia.model.dest

import breeze.linalg.DenseVector
import expedia.stats.MulticlassHistByKey
import expedia.stats.CatStats
import scala.collection._

case class DestModel(
    clusterHistByDest:MulticlassHistByKey[Int],
    clusterProbByDestMapSVM: Map[Int, DenseVector[Float]],
    clusterHistByContinent:MulticlassHistByKey[Int],
    clusterHist:CatStats) {
  
  def predict(destId: Int,continentId:Int): DenseVector[Float] = {
  
    clusterHistByDest.getMap.getOrElse(destId, clusterProbByDestMapSVM.getOrElse(destId, clusterHistByContinent.getMap.getOrElse(continentId, clusterHist.getItemVec())))
  }
}