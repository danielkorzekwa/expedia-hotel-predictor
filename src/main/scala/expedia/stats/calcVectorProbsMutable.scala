package expedia.stats

import breeze.linalg.sum
import breeze.linalg.DenseVector

object calcVectorProbsMutable {
  
   /**
   * Compute item probabilities
   * @param data Vector[items probs]
   *
   */
   def apply(data: DenseVector[Float]) = {

      val Z = sum(data)
     data :/= Z
     
    }
}