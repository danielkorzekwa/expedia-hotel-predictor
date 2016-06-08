package expedia.rankgpr.util

import breeze.linalg._
import breeze.linalg.DenseVector
import breeze.numerics._

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

    val r_ij_sum = sum(pairWiseProbsMat.map(x => if (x > 0) 1d / x else 0), Axis._1)
    val classNum = r_ij_sum.size

    val probs = DenseVector.tabulate(classNum) { i =>

      val prob = 1d / (r_ij_sum(i) - (classNum - 2))
      if (prob < 0) {
        println("prob=" + prob)
      }
      prob
    }

    val Z = sum(probs)

    val normProbs = probs / Z
    normProbs
  }
}