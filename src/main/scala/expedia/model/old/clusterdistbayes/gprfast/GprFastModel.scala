package expedia.model.old.clusterdistbayes.gprfast

import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import dk.gp.cov.CovFunc
import breeze.linalg.cholesky
import dk.gp.math.invchol
import breeze.numerics._

case class GprFastModel(x: DenseMatrix[Double], xTest: DenseMatrix[Double], covFunc: CovFunc, covFuncParams: DenseVector[Double], noiseLogStdDev: Double) {

  val kXX = covFunc.cov(x, x, covFuncParams) + exp(2 * noiseLogStdDev) * DenseMatrix.eye[Double](x.rows) + DenseMatrix.eye[Double](x.rows) * 1e-7
  val kXXInv = invchol(cholesky(kXX).t)

  val kXZ = covFunc.cov(x, xTest, covFuncParams)
}