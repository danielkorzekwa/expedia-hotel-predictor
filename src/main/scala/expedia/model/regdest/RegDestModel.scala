package expedia.model.regdest

import expedia.stats.MulticlassHistByKey
import breeze.linalg.DenseVector

/**
 * @param clusterHistByRegDest key ((userRegId,destId)
 */
case class RegDestModel(clusterHistByRegDest: MulticlassHistByKey[Tuple2[Int, Int]]) {

  def predictionExists(userReg: Int, destId: Int): Boolean = {
    val key = (userReg, destId)
    clusterHistByRegDest.getMap.contains(key)
  }
  def predict(userReg: Int, destId: Int): DenseVector[Float] = {
    val key = (userReg, destId)
    clusterHistByRegDest.getMap(key)
  }
}