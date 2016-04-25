package expedia.stats

import breeze.linalg.DenseVector
import scala.collection._
import java.util.concurrent.atomic.AtomicInteger
object calcCatStats {

  def apply(data: DenseVector[Double]): DenseVector[Double] = {

    val itemMap: mutable.Map[Double, AtomicInteger] = mutable.Map()

    val itemVec = DenseVector.fill(100)(0d)

    data.foreach { item =>

      val currVal = itemVec(item.toInt)
      itemVec(item.toInt) = currVal + 1
    }
    itemVec
  }
}