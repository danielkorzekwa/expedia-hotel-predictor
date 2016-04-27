package expedia.stats

import breeze.linalg.sum
import breeze.linalg.DenseVector

object calcVectorProbs {
  
   /**
   * Compute item probabilities
   * @param data Vector[items probs]
   *
   * @return Vector[items probs]
   */
   def apply(data: DenseVector[Float]): DenseVector[Float] = {

      val Z = sum(data)
      val probsVec = data / Z
      probsVec
    }
}