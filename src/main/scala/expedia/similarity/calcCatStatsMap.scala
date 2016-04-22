package expedia.similarity

import breeze.linalg.DenseMatrix
import scala.collection._
import java.util.concurrent.atomic.AtomicInteger
import com.google.common.util.concurrent.AtomicDouble

object calcCatStatsMap {

  /**
   *
   * @param data Matrix[categoryId,itemId]
   *
   * @param priorCatStats [itemIt,count]
   * @return Map[categoryId,Map[itemId,count]]
   */
  def apply(data: DenseMatrix[Double],priorCatStats:Map[Double,Double]): immutable.Map[Double, immutable.Map[Double, Double]] = {

    val catStatsMap: mutable.Map[Double, Map[Double, AtomicDouble]] = mutable.Map()

     def  createPriorCatStatsAtomic() = priorCatStats.map{case (item,count) => (item,new AtomicDouble(count))}
    
    (0 until data.rows).foreach { i =>
      val row = data(i, ::)
      val category = row(0)
      val itemId = row(1)

      catStatsMap.getOrElseUpdate(category, createPriorCatStatsAtomic)(itemId).addAndGet(1)
    }

    catStatsMap.mapValues(catStats => catStats.mapValues(count => count.get).toMap).toMap
  }
}