package expedia.rankgpr.util

import breeze.linalg._
import breeze.linalg.DenseVector

/**
 *
 * https://www.csie.ntu.edu.tw/~cjlin/papers/svmprob/svmprob.pdf
 *
 * Section 2.3
 */
object calcMultiClassProbsPKPD {

  /**
   * @param pairWiseProbs off diagonal matrix
   *
   * @return Vector of multi class probabilities
   */
  def apply(pairWiseProbsMat: DenseMatrix[Double]): DenseVector[Double] = {

    val r_ij_sum = sum(pairWiseProbsMat.map(x => if(x>0) 1d / x else 0), Axis._1)
    val classNum = r_ij_sum.size

    val probs = DenseVector.tabulate(classNum)(i => 1d / (r_ij_sum(i) - (classNum - 2)))

    val Z = sum(probs)

    probs / Z
  }
}