package expedia.similarity

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

}