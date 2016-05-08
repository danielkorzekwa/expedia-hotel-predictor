package expedia.model.dest

import breeze.linalg.DenseVector
import expedia.stats.MulticlassHistByKey
import expedia.stats.CatStats
import scala.collection._

case class DestModel(
    clusterHistByDest:MulticlassHistByKey[Int],
    clusterProbByDestMapSVM: Map[Int, DenseVector[Float]]) {
  
  def predict(destId: Int,continentId:Int): DenseVector[Float] = {
  
    clusterHistByDest.getMap(destId)
  }
}