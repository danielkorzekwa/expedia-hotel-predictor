package expedia.similarity

import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector

object calcCatProbs {

  /**
   * Compute item probabilities by category
   * @param data Map[categoryId,Map[itemId,count]]
   *
   * @return Matrix[cat,item1,item2,itemn]
   */
  def apply(data: Map[Double, Map[Double, Int]]): DenseMatrix[Double] = {

    val items = data.values.flatMap(catProbsMap => catProbsMap.keys.toList).toList.distinct.sorted

    val catProbSeq = data.map {
      case (cat, catProbsMap) =>

        val Z = catProbsMap.values.sum
        val catProbs = items.map(itemId => catProbsMap.getOrElse(itemId, 0).toDouble / Z).toArray
        DenseVector(cat +: catProbs)
    }.toList

    val catProbsMat = DenseVector.horzcat(catProbSeq: _*).t
    catProbsMat
  }

  /**
   * Compute item probabilities
   * @param data Map[itemId,count]]
   *
   * @return Map[itemId,prob]
   */
  def apply(data: Map[Double, Double]): Map[Double, Double] = {

    val Z = data.values.sum
    val probsMap = data.map { case (itemId, count) => (itemId, count / Z) }.toMap
    probsMap
  }

}