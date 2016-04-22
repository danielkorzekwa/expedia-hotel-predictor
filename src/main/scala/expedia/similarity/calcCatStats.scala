package expedia.similarity

import breeze.linalg.DenseVector
import scala.collection._
import java.util.concurrent.atomic.AtomicInteger
object calcCatStats {

  def apply(data: DenseVector[Double]): immutable.Map[Double, Double] = {

    val itemMap: mutable.Map[Double, AtomicInteger] = mutable.Map()

    data.foreach { item =>
      itemMap.getOrElseUpdate(item, new AtomicInteger(0)).incrementAndGet()
    }

    itemMap.map { case (cat, count) => (cat, count.get.toDouble) }.toMap

  }
}