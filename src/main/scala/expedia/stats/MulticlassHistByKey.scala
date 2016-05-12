package expedia.stats

import scala.collection._
import breeze.linalg.DenseVector
import breeze.linalg._

case class MulticlassHistByKey[K](classNum: Int) {

  private val histByKeyMap: mutable.Map[K, DenseVector[Float]] = mutable.Map()

  /**
   * @param x Vec[categoryId,itemId]
   *
   */
  def add(key: K, classId: Int, value: Float = 1f): Unit = {

    val currVal = histByKeyMap.getOrElseUpdate(key, DenseVector.fill(classNum)(0f))(classId)
    histByKeyMap(key)(classId) = currVal + value

  }

  def getMap: Map[K, DenseVector[Float]] = histByKeyMap

  def normalise() = {
    histByKeyMap.foreach {
      case (key, stats) =>
        val Z = sum(stats)
        if(Z>0) stats :/= Z
    }
  }
}