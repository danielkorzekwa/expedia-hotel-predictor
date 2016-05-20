package expedia.rankgpr.util

import breeze.linalg.DenseVector

object calcOneVsOnePairs {

  def apply(classes: DenseVector[Double]): Seq[Seq[Double]] = classes.toArray.toList.distinct.combinations(2).toList
}