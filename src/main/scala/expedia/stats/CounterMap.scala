package expedia.stats

import scala.collection._

case class CounterMap[K]() extends mutable.HashMap[K, Int] {

  def add(key: K, value: Int = 1) = {
    val currVal = getOrElseUpdate(key, 0)
    update(key, currVal + 1)
  }

}
