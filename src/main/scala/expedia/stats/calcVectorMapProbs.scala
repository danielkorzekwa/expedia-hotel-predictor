package expedia.stats

import breeze.linalg.DenseVector

object calcVectorMapProbs {
  
   /**
   * Compute item probabilities by category
   * @param data Map[categoryId,Vector[items probs]]
   *
   * @return Map[categoryId,Vector[items probs]]
   */
   def apply(data: Map[Double, DenseVector[Double]]): Map[Double, DenseVector[Double]] = {

      val catProbsMap = data.map {
        case (cat, catStatsVec) =>

          val catProbs = calcVectorProbs(catStatsVec)
          (cat, catProbs)
      }

      catProbsMap
    }
   
  
}