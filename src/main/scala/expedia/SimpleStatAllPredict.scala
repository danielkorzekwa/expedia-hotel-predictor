package expedia

import java.util.concurrent.atomic.AtomicInteger
import scala.collection._
import breeze.linalg._
import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import expedia.similarity.calcCatStats
import expedia.similarity.calcCatProbs

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