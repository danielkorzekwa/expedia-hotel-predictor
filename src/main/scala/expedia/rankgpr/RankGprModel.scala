package expedia.rankgpr

import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix
import dk.gp.cov.CovFunc
import breeze.linalg._

case class RankGprModel(x: DenseMatrix[Double], y: DenseVector[Double], covFunc: CovFunc, covFuncParams: DenseVector[Double], noiseLogStdDev: Double) {

  val classes =unique(y).toArray.toList.sorted

}