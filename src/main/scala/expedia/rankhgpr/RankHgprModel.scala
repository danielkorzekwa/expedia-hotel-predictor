package expedia.rankhgpr

import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix
import dk.gp.cov.CovFunc
import breeze.linalg._

/**
 * @param x [taskId,other variables]
 */
case class RankHgprModel(x: DenseMatrix[Double], y: DenseVector[Double],calcU:(DenseMatrix[Double]) => DenseMatrix[Double], covFunc: CovFunc, covFuncParams: DenseVector[Double], noiseLogStdDev: Double) {

  val classes = unique(y).toArray.toList.sorted

}