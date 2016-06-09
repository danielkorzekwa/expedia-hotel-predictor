package expedia.rankhgpr

import com.typesafe.scalalogging.slf4j.LazyLogging
import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import expedia.rankgpr.util.calcOneVsOnePairs
import expedia.rankhgpr.util.calcOneToOneProbs
import expedia.rankgpr.util.calcMultiClassProbsPKPD

object rankHgprPredict extends LazyLogging {

  /**
   * @param tMat Matrix of N test points [NxD] in a format [taskId,other variables], D - dimensionality of prediction vector.
   *
   * @return Matrix of row vectors [vector of probabilities for sorted classes]
   */
  def apply(xTest: DenseMatrix[Double], model: RankHgprModel): DenseMatrix[Double] = {

    val oneToOnePairs = calcOneVsOnePairs(model.classes)

    // Seq of [pairwise probabilities] for n test points
    val oneToOneProbsSeq: Seq[DenseVector[Double]] = calcOneToOneProbs(oneToOnePairs, xTest, model)

    val predictedProbsSeq = (0 until xTest.rows).map { i =>
      val onetoOnesProbs = oneToOneProbsSeq(i)
      val predictedProbs = predict(oneToOnePairs, onetoOnesProbs, model)
      predictedProbs
    }

    val predictedProbsMat = DenseVector.horzcat(predictedProbsSeq: _*).t
    predictedProbsMat
  }

  /**
   *
   * @return Vector of probabilities for sorted classes
   */
  def predict(oneToOnePairs: Seq[Seq[Double]], oneToOneProbs: DenseVector[Double], model: RankHgprModel): DenseVector[Double] = {

    val classNum = model.classes.size
    val probsMat = DenseMatrix.fill(classNum, classNum)(0d)
    val classIndexByClass: Map[Double, Int] = model.classes.map(c => c -> model.classes.indexOf(c)).toMap

    (0 until oneToOnePairs.size).foreach { i =>

      val Seq(c1, c2) = oneToOnePairs(i)

      val c1Index = classIndexByClass(c1)
      val c2Index = classIndexByClass(c2)
      val probC1 = oneToOneProbs(i)

      probsMat(c1Index, c2Index) = probC1
      probsMat(c2Index, c1Index) = 1 - probC1
    }

    val probsVector = calcMultiClassProbsPKPD(probsMat)
    probsVector
  }

}