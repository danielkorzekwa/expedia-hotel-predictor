package expedia.stats

import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import breeze.linalg.sum
import breeze.generic.UFunc

object calcCatProbs extends UFunc {

  /**
   * Compute item probabilities by category
   * @param data Map[categoryId,Map[itemId,count]]
   *
   * @return Map[categoryId,Map[itemId,prob]]
   */
  implicit object implMapMap extends Impl[Map[Double, Map[Double, Double]], Map[Double, Map[Double, Double]]] {
    def apply(data: Map[Double, Map[Double, Double]]): Map[Double, Map[Double, Double]] = {

      val catProbsMap = data.map {
        case (cat, catStatsMap) =>

          val catProbs = implMap(catStatsMap)
          (cat, catProbs)
      }

      catProbsMap
    }
  }
  
  implicit object implVecMap extends Impl[Map[Double, DenseVector[Double]], Map[Double,DenseVector[Double]]] {
    def apply(data: Map[Double, DenseVector[Double]]): Map[Double,DenseVector[Double]] = {

      val catProbsMap = data.map {
        case (cat, catStatsVec) =>

          val catProbs = implVec(catStatsVec)
          (cat, catProbs)
      }

      catProbsMap
    }
  }

  /**
   * Compute item probabilities
   * @param data Map[itemId,count]]
   *
   * @return Map[itemId,prob]
   */
  implicit object implMap extends Impl[Map[Double, Double], Map[Double, Double]] {
    def apply(data: Map[Double, Double]): Map[Double, Double] = {

      val Z = data.values.sum
      val probsMap = data.map { case (itemId, count) => (itemId, count / Z) }.toMap
      probsMap
    }
  }

  implicit object implVec extends Impl[DenseVector[Double], DenseVector[Double]] {
    def apply(data: DenseVector[Double]): DenseVector[Double] = {

      val Z = sum(data)
      val probsVec = data / Z
      probsVec
    }
  }

}