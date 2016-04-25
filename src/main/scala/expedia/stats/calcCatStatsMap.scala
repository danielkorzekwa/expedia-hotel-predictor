package expedia.stats

import breeze.linalg.DenseMatrix
import scala.collection._
import java.util.concurrent.atomic.AtomicInteger
import com.google.common.util.concurrent.AtomicDouble
import breeze.linalg.DenseVector

object calcCatStatsMap {

  /**
   *
   * @param data Matrix[categoryId,itemId]
   *
   * @param priorCatStats categoryId => [categoryId,count]
   * @return Map[categoryId,Vector(item counts)]
   */
  def apply(data: DenseMatrix[Double], priorCatStats: Double => DenseVector[Double]): immutable.Map[Double, DenseVector[Double]] = {

    val catStatsMap: mutable.Map[Double, DenseVector[Double]] = mutable.Map()

    def createPriorCatStatsAtomic(categoryId: Double) = priorCatStats(categoryId).copy

    (0 until data.rows).foreach { i =>
      val row = data(i, ::)
      val category = row(0)
      val itemId = row(1).toInt

      val currVal = catStatsMap.getOrElseUpdate(category, createPriorCatStatsAtomic(category))(itemId)
      catStatsMap.getOrElseUpdate(category, createPriorCatStatsAtomic(category))(itemId) = currVal + 1
    }

    catStatsMap.toMap
  }
}