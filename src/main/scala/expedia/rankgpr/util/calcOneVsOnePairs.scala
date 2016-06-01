package expedia.rankgpr.util

import breeze.linalg.DenseVector

object calcOneVsOnePairs {

  def apply(classes: Seq[Double]): Seq[Seq[Double]] = classes.distinct.combinations(2).toList
}