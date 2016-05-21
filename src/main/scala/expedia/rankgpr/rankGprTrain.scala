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

    val classXSeq = oneToOnePairs.zipWithIndex.map {
      case (List(c1, c2), index) =>

        val classIdx = model.y.findAll { y => y == c1 || y == c2 }
        val classX = model.x(classIdx, ::).toDenseMatrix
        val taskIdVec = DenseVector.fill[Double](classX.rows)(index)
        DenseMatrix.horzcat(taskIdVec.toDenseMatrix.t, classX)
    }
    val classXMat = DenseMatrix.vertcat(classXSeq: _*)

    val classYSeq = oneToOnePairs.zipWithIndex.map {
      case (List(c1, c2), index) =>

        val classIdx = model.y.findAll { y => y == c1 || y == c2 }
        val classY = model.y(classIdx).map(y => if (y == c1) 1.0 else 0).toDenseVector
        val classYMean = mean(classY)
        classY - classYMean
    }
    val classYVec = DenseVector.vertcat(classYSeq: _*)

    val (newCovFuncParams, newLikNoiseLogStdDev) = learnMtGgHyperParams(classXMat, classYVec, model.covFunc, model.covFuncParams, model.noiseLogStdDev)

    val newModel = model.copy(covFuncParams = newCovFuncParams, noiseLogStdDev = newLikNoiseLogStdDev)
    newModel
  }

}