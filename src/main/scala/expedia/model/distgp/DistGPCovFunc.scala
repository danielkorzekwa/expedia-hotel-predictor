package expedia.model.distgp

import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import dk.gp.cov.CovSEiso
import dk.gp.cov.CovFunc

case class DistGPCovFunc() extends CovFunc {

    private val covFunc = CovSEiso()
    def cov(x1: DenseMatrix[Double], x2: DenseMatrix[Double], covFuncParams: DenseVector[Double]): DenseMatrix[Double] = {
      val shortCov = covFunc.cov(x1, x2, covFuncParams(0 to 1))
       val longCov = covFunc.cov(x1, x2, covFuncParams(2 to 3))
      shortCov + longCov
    }

    def covD(x1: DenseMatrix[Double], x2: DenseMatrix[Double], covFuncParams: DenseVector[Double]): Array[DenseMatrix[Double]] = {
        val shortCovD = covFunc.covD(x1, x2, covFuncParams(0 to 1))
        val longCovD = covFunc.covD(x1, x2, covFuncParams(2 to 3))
      shortCovD ++ longCovD
    }
  }