package expedia.stats

import breeze.linalg.DenseVector
import breeze.linalg._

case class MulticlassHist(classNum: Int) {

  private val histogram = DenseVector.fill(classNum)(0f)

  def add(classId: Int, value: Float = 1f): Unit = {

    val currVal = histogram(classId)
    histogram(classId) = currVal + value
  }

  def getHistogram: DenseVector[Float] = histogram

  def normalise() = {
    val Z = sum(histogram)
    if (Z > 0) histogram :/= Z
  }

}