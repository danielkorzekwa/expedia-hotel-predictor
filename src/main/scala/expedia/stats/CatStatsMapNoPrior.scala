package expedia.stats

import breeze.linalg.DenseVector
import scala.collection._
import com.google.common.util.concurrent.AtomicDouble

case class CatStatsMapNoPrior() {

  private val catStatsMap: mutable.Map[Int, DenseVector[Float]] = mutable.Map()

  /**
   * @param x Vec[categoryId,itemId]
   *
   */
  def add(category:Int,itemId:Int): Unit = {

    val currVal = catStatsMap.getOrElseUpdate(category, createPriorCatStatsAtomic(category))(itemId)
    catStatsMap.getOrElseUpdate(category, createPriorCatStatsAtomic(category))(itemId.toInt) = currVal + 1

  }

  def getMap(): Map[Int, DenseVector[Float]] = catStatsMap

  private def createPriorCatStatsAtomic(categoryId: Int): DenseVector[Float] =  DenseVector.fill(100)(0f)
}