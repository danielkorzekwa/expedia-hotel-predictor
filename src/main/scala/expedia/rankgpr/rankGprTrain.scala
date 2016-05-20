package expedia.rankgpr

import dk.gp.gpr.GprModel
import breeze.linalg.DenseVector
import dk.gp.cov.CovSEiso
import breeze.numerics._
import expedia.rankgpr.util.calcOneVsOnePairs
import breeze.linalg._
import breeze.stats._
import dk.gp.mtgp.learnMtGgHyperParams

object rankGprTrain {

  def apply(model: RankGprModel): RankGprModel = {

    val classes = unique(model.y)
    val oneToOnePairs = calcOneVsOnePairs(classes)

    val gpModelsByoneToOnePair: Map[List[Double], GprModel] = oneToOnePairs.par.map {
      case List(c1, c2) =>

        val classIdx = model.y.findAll { y => y == c1 || y == c2 }
        val classX = model.x(classIdx, ::).toDenseMatrix
        val classY = model.y(classIdx).map(y => if (y == c1) 1.0 else 0).toDenseVector

        val covFunc = CovSEiso()
        val covFuncParams = DenseVector[Double](log(1), log(1))
        val noiseLogStdDev = log(1d)
        val gpMean = mean(classY)
        val gprModel = GprModel(classX, classY, covFunc, covFuncParams, noiseLogStdDev, gpMean)

        List(c1, c2) -> gprModel
    }.toList.toMap

    val classXSeq = gpModelsByoneToOnePair.values.zipWithIndex.map {
      case (model, index) =>

        val x = model.x
        val taskIdVec = DenseVector.fill[Double](x.rows)(index)

        DenseMatrix.horzcat(taskIdVec.toDenseMatrix.t, x)
    }.toList
    val classXMat = DenseMatrix.vertcat(classXSeq: _*)

    val classYSeq = gpModelsByoneToOnePair.map { case (key, model) => model.y }.toList
    val classYVec = DenseVector.vertcat(classYSeq: _*)

    val covFunc = CovSEiso()
    val covFuncParams = DenseVector[Double](log(1), log(1))
    val noiseLogStdDev = log(1d)
    val gpMean = 0d

    val (newCovFuncParams, newLikNoiseLogStdDev) = learnMtGgHyperParams(classXMat, classYVec, covFunc, covFuncParams, noiseLogStdDev)

    val newModel = model.copy(covFuncParams = newCovFuncParams, noiseLogStdDev = newLikNoiseLogStdDev)
    newModel
  }

}