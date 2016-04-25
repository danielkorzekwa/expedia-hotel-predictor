package expedia.model.simple

import scala.collection._
import breeze.linalg._
import breeze.linalg.DenseVector
import expedia.stats.calcCatStats
import expedia.stats.calcCatProbs

case class SimpleStatAllPredict(trainData: DenseVector[Double]) {

  val probByClusterMap: DenseVector[Double] = {
    val bookedByClusterMap = calcCatStats(trainData)
    val probByClusterMap2 = calcCatProbs(bookedByClusterMap)
    probByClusterMap2
  }

  def predict(data: DenseVector[Double], hotelCluster: Double): DenseVector[Double] = {

    data.map { c => probByClusterMap(hotelCluster.toInt) }

  }

}