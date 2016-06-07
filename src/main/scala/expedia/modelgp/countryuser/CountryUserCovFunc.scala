package expedia.modelgp.countryuser

import dk.gp.cov.CovFunc
import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import dk.gp.cov.CovSEiso

case class CountryUserCovFunc() extends CovFunc {

  val covSEIso = CovSEiso()

  def cov(x1: DenseMatrix[Double], x2: DenseMatrix[Double], covFuncParams: DenseVector[Double]): DenseMatrix[Double] = {

    val countrySqDistMat = DenseMatrix.tabulate(x1.rows, x2.rows) { (i, j) =>
      if (x1(i, 1) == x2(j, 1)) 0d else 1.0
    }

    val covMat = covSEIso.cov(countrySqDistMat, covFuncParams(0 to 1))
    covMat
  }

  def covD(x1: DenseMatrix[Double], x2: DenseMatrix[Double], covFuncParams: DenseVector[Double]): Array[DenseMatrix[Double]] = {
    val countrySqDistMat = DenseMatrix.tabulate(x1.rows, x2.rows) { (i, j) =>
      if (x1(i, 1) == x2(j, 1)) 0d else 1.0
    }

    val covMatD = covSEIso.covD(countrySqDistMat, covFuncParams(0 to 1))
    covMatD
  }

}