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
import expedia.rankgpr.util.calcMultiClassProbsPKPD

case class RankGprPredict(model: RankGprModel) {

  
  private val oneToOnePairs = calcOneVsOnePairs(model.classes)

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
   * @return Vector of probabilities for sorted classes
   */
  def predict(t: DenseVector[Double]): DenseVector[Double] = {

    val classNum = model.classes.size
    val probsMat = DenseMatrix.fill(classNum,classNum)(0d)
    val classIndexByClass:Map[Double,Int] = model.classes.map(c => c -> model.classes.indexOf(c)).toMap

    oneToOnePairs.foreach {
      case List(c1, c2) =>
   
        val gprModel = gpModelsByoneToOnePair(List(c1, c2))
        val probC1 = dk.gp.gpr.predict(t.toDenseMatrix, gprModel)(0, 0)
       val squeezedProbC1 = 1d/(1d + exp(-5.0*(probC1-0.5))) //@TODO -5 coefficient should be tuned (fitted with data)
      // println(probC1 + ":" + squeezedProbC1)
        val c1Index = classIndexByClass(c1)
        val c2Index = classIndexByClass(c2)
        probsMat(c1Index,c2Index) = squeezedProbC1
        probsMat(c2Index,c1Index) = 1 - squeezedProbC1
    }
      val probsVector = calcMultiClassProbsPKPD(probsMat)

   probsVector
    
  }

  /**
   * @param tMat Matrix of N test points [NxD], D - dimensionality of prediction vector
   *
   * @return Matrix of row vectors [vector of probabilities for sorted classes]
   */
  def predict(tMat: DenseMatrix[Double]): DenseMatrix[Double] = {
    val predictedRankedClasses = tMat(*, ::).map { t => predict(t) }
    predictedRankedClasses
  }
}