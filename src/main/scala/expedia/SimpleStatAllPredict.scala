package expedia

import java.util.concurrent.atomic.AtomicInteger

import scala.collection._
import breeze.linalg._
import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector

case class SimpleStatAllPredict(trainData: DenseMatrix[Double]) {

  val probByClusterMap: Map[Double, Double] = computedProbByCluster()

  def predict(data: DenseMatrix[Double], hotelCluster: Double): DenseVector[Double] = {

    data(*, ::).map { c => probByClusterMap.getOrElse(hotelCluster,Double.NaN) }

  }

  private def computedProbByCluster(): Map[Double, Double] = {
    val bookedByClusterMap: mutable.Map[Double, AtomicInteger] = mutable.Map()

    (0 until trainData.rows).foreach { i =>
      val row = trainData(i, ::)
      val cluster = row(trainData.cols - 1)
      bookedByClusterMap.getOrElseUpdate(cluster, new AtomicInteger(0)).incrementAndGet()
    }

    val probByClusterMap = bookedByClusterMap.map { case (cluster, count) => (cluster, count.get.toDouble / trainData.rows) }.toMap
    probByClusterMap
  }
}