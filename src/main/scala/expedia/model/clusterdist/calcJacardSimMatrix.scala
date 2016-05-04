package expedia.model.clusterdist

import breeze.linalg.DenseMatrix

object calcJacardSimMatrix {

  /**
   * @param m Matrix of cluster co-existences
   * @return Jacard similarity matrix
   */
  def apply(m: DenseMatrix[Double]): DenseMatrix[Double] = {

    val jacardSimMatrix = DenseMatrix.fill(m.rows, m.cols)(0d)

    m.foreachKey {
      case (i, j) =>
      jacardSimMatrix(i,j) =  m(i, j) / (m(i, i) + m(j, j) - m(i, j))
    }
    jacardSimMatrix
  }
}