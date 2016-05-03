package expedia.model.clusterdistbayes

import dk.gp.cov.CovFunc
import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector

case class GpTestCovFunc() extends CovFunc {

  private val simMatrix = DenseMatrix.eye[Double](100)

  def cov(x1: DenseMatrix[Double], x2: DenseMatrix[Double], covFuncParams: DenseVector[Double]): DenseMatrix[Double] = {

    val x1ClusterVec = x1(::, 0)
    val x2ClusterVec = x2(::, 0)

    val covMat = DenseMatrix.tabulate(x1.rows, x2.rows) {
      case (i, j) =>
        val x1Val = x1ClusterVec(i).toInt
        val x2Val = x2ClusterVec(j).toInt

        simMatrix(x1Val, x2Val)
    }

    covMat
  }

  def covD(x1: DenseMatrix[Double], x2: DenseMatrix[Double], covFuncParams: DenseVector[Double]): Array[DenseMatrix[Double]] = {
    ???
  }

}