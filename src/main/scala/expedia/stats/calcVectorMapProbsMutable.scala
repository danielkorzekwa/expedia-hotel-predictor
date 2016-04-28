package expedia.stats

import breeze.linalg.DenseVector

object calcVectorMapProbsMutable {

  /**
   * Compute item probabilities by category
   * @param data Map[categoryId,Vector[items probs]]
   *
   */
  def apply(data: Map[Int, DenseVector[Float]]) = {

    val catProbsMap = data.foreach {
      case (cat, catStatsVec) =>

        calcVectorProbsMutable(catStatsVec)
    }

  }

}