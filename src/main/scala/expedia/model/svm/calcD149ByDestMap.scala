package expedia.model.svm

import breeze.linalg.DenseVector
import scala.collection._
import breeze.linalg.DenseMatrix

object calcD149ByDestMap {

  /**
   *  @param destMatrix [destId,d1,d2,...d149]
   *
   *  @return Map[dest,Vec(d1..d149)]
   */
  def apply(destMatrix: DenseMatrix[Double]): Map[Double, DenseVector[Double]] = {
    val d149Map: mutable.Map[Double, DenseVector[Double]] = mutable.Map()

    (0 until destMatrix.rows).foreach { i =>
      val row = destMatrix(i, ::).t
      val srchDestId = row(0)
      val d1To149Vec = row(1 to row.size - 1)

      d149Map.update(srchDestId, d1To149Vec)
    }

    d149Map.toMap
  }
}