package expedia.rankgpr

import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix
import scala.collection._
import breeze.linalg._
import expedia.rankgpr.util.calcOneVsOnePairs
import dk.gp.gpr.GprModel
import breeze.numerics._
import dk.gp.cov.CovSEiso
import dk.gp.gpr.gpr
import dk.gp.gpr.predict
import breeze.stats._

case class RankGprPredict(model: RankGprModel) {

  private val classes = unique(model.y)
  private val oneToOnePairs = calcOneVsOnePairs(classes)

  val gpModelsByoneToOnePair: Map[List[Double], GprModel] = oneToOnePairs.par.map {
    case List(c1, c2) =>

      val classIdx = model.y.findAll { y => y == c1 || y == c2 }
      val classX = model.x(classIdx, ::).toDenseMatrix
      val classY = model.y(classIdx).map(y => if (y == c1) 1.0 else 0).toDenseVector
      val gpMean = mean(classY)

      val gprModel = GprModel(classX, classY, model.covFunc, model.covFuncParams, model.noiseLogStdDev, gpMean)

      List(c1, c2) -> gprModel
  }.toList.toMap

  /**
   * @param t test point [D], D - dimensionality of prediction vector
   *
   * @return Vector of ranked classes
   */
  def predict(t: DenseVector[Double]): DenseVector[Double] = {

    val votesMap: mutable.Map[Double, Int] = mutable.Map()
    classes.foreach { c => votesMap += c -> 0 }

    oneToOnePairs.foreach {
      case List(c1, c2) =>

        val gprModel = gpModelsByoneToOnePair(List(c1, c2))
        val probC1 = dk.gp.gpr.predict(t.toDenseMatrix.t, gprModel)(0, 0)
        val votedClass = if (probC1 > 0.5) c1 else c2
        val currClassVotes = votesMap(votedClass)
        votesMap += votedClass -> (currClassVotes + 1)

    }

    val rankedClasses = votesMap.toList.sortWith { case (c1Votes, c2Votes) => c1Votes._2 > c2Votes._2 }.map(_._1).toArray
    DenseVector(rankedClasses)
  }

  /**
   * @param tMat Matrix of N test points [NxD], D - dimensionality of prediction vector
   *
   * @return matrix of row vectors [classes sorted by probability of class]
   */
  def predict(tMat: DenseMatrix[Double]): DenseMatrix[Double] = {
    val predictedRankedClasses = tMat(*, ::).map { t => predict(t) }
    predictedRankedClasses
  }
}