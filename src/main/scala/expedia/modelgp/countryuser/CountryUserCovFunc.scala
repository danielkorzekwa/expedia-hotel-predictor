package expedia.modelgp.countryuser

import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import dk.gp.cov.CovFunc
import dk.gp.cov.CovSEiso

case class CountryUserCovFunc() extends CovFunc {

  val covSEIso = CovSEiso()

  def cov(x1: DenseMatrix[Double], x2: DenseMatrix[Double], covFuncParams: DenseVector[Double]): DenseMatrix[Double] = {

      val userSqDistMat = DenseMatrix.tabulate(x1.rows, x2.rows) { (i, j) =>
      if (x1(i, 0) == x2(j, 0)) 0d else 1.0
    }
   
    val countrySqDistMat = DenseMatrix.tabulate(x1.rows, x2.rows) { (i, j) =>
      if (x1(i, 1) == x2(j, 1)) 0d else 1.0
    }

    val covUser = covSEIso.cov(userSqDistMat, covFuncParams(0 to 1))
    val covCountry = covSEIso.cov(countrySqDistMat, covFuncParams(2 to 3))
   
   covUser + covCountry
  }

  def covD(x1: DenseMatrix[Double], x2: DenseMatrix[Double], covFuncParams: DenseVector[Double]): Array[DenseMatrix[Double]] = {
      val userSqDistMat = DenseMatrix.tabulate(x1.rows, x2.rows) { (i, j) =>
      if (x1(i, 0) == x2(j, 0)) 0d else 1.0
    }
    
    val countrySqDistMat = DenseMatrix.tabulate(x1.rows, x2.rows) { (i, j) =>
      if (x1(i, 1) == x2(j, 1)) 0d else 1.0
    }

     val covUserD = covSEIso.covD(userSqDistMat, covFuncParams(0 to 1))
    val covCountryD = covSEIso.covD(countrySqDistMat, covFuncParams(2 to 3))
    covUserD ++ covCountryD
  }

}