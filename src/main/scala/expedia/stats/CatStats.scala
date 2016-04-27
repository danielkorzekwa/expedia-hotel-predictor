package expedia.stats

import breeze.linalg.DenseVector
import scala.collection._
import java.util.concurrent.atomic.AtomicInteger
case class CatStats() {

  private val itemVec = DenseVector.fill(100)(0d)

  def add(item: Double) = {

    val currVal = itemVec(item.toInt)
    itemVec(item.toInt) = currVal + 1
  }

  def getItemVec(): DenseVector[Double] = itemVec.copy

}