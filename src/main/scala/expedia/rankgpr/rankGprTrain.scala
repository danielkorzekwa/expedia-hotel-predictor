package expedia.rankgpr

import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import breeze.linalg.unique
import breeze.numerics.log
import breeze.stats.mean
import breeze.stats.mean.reduce_Double
import dk.gp.cov.CovSEiso
import dk.gp.gpr.GprModel
import expedia.rankgpr.util.calcOneVsOnePairs
import dk.gp.mtgpr.MtGprModel
import dk.gp.mtgpr.mtgprTrain

object rankGprTrain {

  def apply(model: RankGprModel, tolerance: Double = 1.0E-6): RankGprModel = {

    val oneToOnePairs = calcOneVsOnePairs(model.classes)

    val trainingData = oneToOnePairs.zipWithIndex.map {
      case (List(c1, c2), index) =>

        val classIdx = model.y.findAll { y => y == c1 || y == c2 }
        val classX = model.x(classIdx, ::).toDenseMatrix
        val classY = model.y(classIdx).map(y => if (y == c1) 1.0 else 0).toDenseVector
        val classYMean = mean(classY)

        val taskData = DenseMatrix.horzcat(classX, (classY - classYMean).toDenseMatrix.t)
        taskData
    }
    val mtGprModel = MtGprModel(trainingData, model.covFunc, model.covFuncParams, model.noiseLogStdDev)

    val trainedModel = mtgprTrain(mtGprModel, tolerance)

    val newModel = model.copy(covFuncParams = trainedModel.covFuncParams, noiseLogStdDev = trainedModel.likNoiseLogStdDev)
    newModel
  }

}