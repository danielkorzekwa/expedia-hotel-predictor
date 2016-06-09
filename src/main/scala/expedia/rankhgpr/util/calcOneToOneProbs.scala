package expedia.rankhgpr.util

import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import breeze.stats._
import dk.gp.hgpr.HgprModel
import dk.gp.hgpr.hgprPredict
import expedia.rankhgpr.RankHgprModel
import breeze.numerics._

object calcOneToOneProbs {

  /**
   * @returns Seq of [pairwise probabilities] for n test points
   */
  def apply(oneToOnePairs: Seq[Seq[Double]], xTest: DenseMatrix[Double], model: RankHgprModel): Seq[DenseVector[Double]] = {
    val testPointsProbs: Seq[DenseVector[Double]] = oneToOnePairs.map {
      case List(c1, c2) =>

        val classIdx = model.y.findAll { y => y == c1 || y == c2 }
        val classX = model.x(classIdx, ::).toDenseMatrix
        val classY = model.y(classIdx).map(y => if (y == c1) 1.0 else 0).toDenseVector
        val gpMean = mean(classY)

        val u = model.calcU(classX)
        val hgprModel = HgprModel(classX, classY - gpMean, u, model.covFunc, model.covFuncParams, model.noiseLogStdDev)
        val probC1 = hgprPredict(xTest, hgprModel).map(x => x.m+gpMean)

        val squeezedProbC1 = 1d / (exp(-5.0 * (probC1 - 0.5)) + 1d) //@TODO -5 coefficient should be tuned (fitted with data)
        squeezedProbC1
    }

    toOneToOneProbs(xTest, testPointsProbs)
  }

  private def toOneToOneProbs(xTest: DenseMatrix[Double], testPointsProbs: Seq[DenseVector[Double]]): Seq[DenseVector[Double]] = {
    val onetoOneProbsSeq: Seq[DenseVector[Double]] = (0 until xTest.rows).map { i =>
      DenseVector(testPointsProbs.map(testPointProbs => testPointProbs(i)).toArray)
    }
    onetoOneProbsSeq
  }
}