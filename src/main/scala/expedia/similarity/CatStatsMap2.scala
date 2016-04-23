package expedia.similarity

import breeze.linalg.DenseVector
import scala.collection._
import com.google.common.util.concurrent.AtomicDouble

case class CatStatsMap2(priorCatStats: Double => Map[Double, Double]) {

  private val catStatsMap: mutable.Map[Double, Map[Double, AtomicDouble]] = mutable.Map()

  /**
   * @param x Vec[categoryId,itemId]
   *
   */
  def add(x: DenseVector[Double]): Unit = {
    val category = x(0)
    val itemId = x(1)

 catStatsMap.getOrElseUpdate(category, createPriorCatStatsAtomic(category))(itemId).addAndGet(1)    
    
  }

  def toMap(): immutable.Map[Double, immutable.Map[Double, Double]] = {
    catStatsMap.map {
      case (cat, catStats) =>
        val newCatStatsMap = catStats.map { case (item, count) => (item, count.get) }.toMap
        (cat, newCatStatsMap)
    }.toMap
  }

  private def createPriorCatStatsAtomic(categoryId: Double) = priorCatStats(categoryId).map { case (item, count) => (item, new AtomicDouble(count)) }
}