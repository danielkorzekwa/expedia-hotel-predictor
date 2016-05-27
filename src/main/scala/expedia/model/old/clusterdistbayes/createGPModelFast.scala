package expedia.model.old.clusterdistbayes

import dk.gp.gpr.GprModel
import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import breeze.numerics._
import expedia.model.old.clusterdistbayes.gprfast.GprFastModel
import expedia.model.old.clusterdistbayes.GpTestCovFunc

object createGPModelFast {

  def apply(x: DenseMatrix[Double],  jacardSimMatrix: DenseMatrix[Double]): GprFastModel = {

    val covFunc = GpTestCovFunc(jacardSimMatrix)
    val covFuncParams = DenseVector[Double]()
    val noiseLogStdDev = log(1d)

    val predictionDataX = DenseVector.rangeD(0.0, 100, 1).toDenseMatrix.t
    val gprModel = GprFastModel(x, predictionDataX, covFunc, covFuncParams, noiseLogStdDev)
    gprModel
  }
}