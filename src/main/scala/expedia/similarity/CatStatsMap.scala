package expedia.similarity

import breeze.linalg.DenseVector
import scala.collection._
import com.google.common.util.concurrent.AtomicDouble

case class CatStatsMap(priorCatStats: Double => Map[Double, Double]) {

 // private val catStatsMap: mutable.Map[Double, Map[Double, AtomicDouble]] = mutable.Map()
  private val catStatsMap: mutable.Map[Double, mutable.Map[Double, Double]] = mutable.Map()

  /**
   * @param x Vec[categoryId,itemId]
   *
   */
  def add(x: DenseVector[Double]): Unit = {
    val category = x(0)
    val itemId = x(1)

 //catStatsMap.getOrElseUpdate(category, createPriorCatStatsAtomic(category))(itemId).addAndGet(1)    
    
  val currVal =   catStatsMap.getOrElseUpdate(category, createPriorCatStatsAtomic(category))(itemId)
   catStatsMap.getOrElseUpdate(category, createPriorCatStatsAtomic(category)).update(itemId,currVal+1)
  }

  def toMap(): immutable.Map[Double, immutable.Map[Double, Double]] = {
    catStatsMap.map {
      case (cat, catStats) =>
        val newCatStatsMap = catStats.map { case (item, count) => (item, count) }.toMap
        (cat, newCatStatsMap)
    }.toMap
  }

 // private def createPriorCatStatsAtomic(categoryId: Double) = priorCatStats(categoryId).map { case (item, count) => (item, new AtomicDouble(count)) }
  private def createPriorCatStatsAtomic(categoryId: Double):mutable.Map[Double,Double] = collection.mutable.Map( priorCatStats(categoryId).toSeq: _*) 
}