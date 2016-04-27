package expedia.stats

import breeze.linalg.DenseVector
import scala.collection._
import com.google.common.util.concurrent.AtomicDouble

case class CatStatsMapNoPrior() {

  private val catStatsMap: mutable.Map[Double, DenseVector[Float]] = mutable.Map()

  /**
   * @param x Vec[categoryId,itemId]
   *
   */
  def add(category:Double,itemId:Double): Unit = {

    val currVal = catStatsMap.getOrElseUpdate(category, createPriorCatStatsAtomic(category))(itemId.toInt)
    catStatsMap.getOrElseUpdate(category, createPriorCatStatsAtomic(category))(itemId.toInt) = currVal + 1

  }

  def toMap(): immutable.Map[Double, DenseVector[Float]] = catStatsMap.toMap

  private def createPriorCatStatsAtomic(categoryId: Double): DenseVector[Float] =  DenseVector.fill(100)(0f)
}