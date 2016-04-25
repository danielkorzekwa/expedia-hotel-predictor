package expedia.stats

import breeze.linalg.DenseVector
import scala.collection._
import com.google.common.util.concurrent.AtomicDouble

case class CatStatsMap3(priorCatStats: Double =>DenseVector[Double]) {

  private val catStatsMap: mutable.Map[Double, DenseVector[Double]] = mutable.Map()

  /**
   * @param x Vec[categoryId,itemId]
   *
   */
  def add(x: DenseVector[Double]): Unit = {
    val category = x(0)
    val itemId = x(1)

    val currVal = catStatsMap.getOrElseUpdate(category, createPriorCatStatsAtomic(category))(itemId.toInt)
    catStatsMap.getOrElseUpdate(category, createPriorCatStatsAtomic(category))(itemId.toInt) = currVal + 1

  }

  def toMap(): immutable.Map[Double, DenseVector[Double]] = catStatsMap.toMap

  // private def createPriorCatStatsAtomic(categoryId: Double) = priorCatStats(categoryId).map { case (item, count) => (item, new AtomicDouble(count)) }
  private def createPriorCatStatsAtomic(categoryId: Double): DenseVector[Double] =  priorCatStats(categoryId.toInt).copy
}