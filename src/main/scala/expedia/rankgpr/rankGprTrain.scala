package expedia.rankgpr

import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import breeze.linalg.unique
import breeze.numerics.log
import breeze.stats.mean
import breeze.stats.mean.reduce_Double
import dk.gp.cov.CovSEiso
import dk.gp.gpr.GprModel
import dk.gp.mtgp.learnMtGgHyperParams
import expedia.rankgpr.util.calcOneVsOnePairs

object rankGprTrain {

  def apply(model: RankGprModel): RankGprModel = {

    val classes = unique(model.y)
    val oneToOnePairs = calcOneVsOnePairs(classes)

    val gpModelsByoneToOnePair: Map[List[Double], GprModel] = oneToOnePairs.par.map {
      case List(c1, c2) =>

        val classIdx = model.y.findAll { y => y == c1 || y == c2 }
        val classX = model.x(classIdx, ::).toDenseMatrix
        val classY = model.y(classIdx).map(y => if (y == c1) 1.0 else 0).toDenseVector

        val gpMean = 0d
        val gprModel = GprModel(classX, classY, model.covFunc, model.covFuncParams, model.noiseLogStdDev, gpMean)

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

    val (newCovFuncParams, newLikNoiseLogStdDev) = learnMtGgHyperParams(classXMat, classYVec, model.covFunc, model.covFuncParams, model.noiseLogStdDev)

    val newModel = model.copy(covFuncParams = newCovFuncParams, noiseLogStdDev = newLikNoiseLogStdDev)
    newModel
  }

}