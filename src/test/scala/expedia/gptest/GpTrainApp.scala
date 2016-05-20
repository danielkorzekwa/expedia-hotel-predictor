package expedia.gptest

import breeze.linalg._
import java.io.File
import dk.gp.gpr.GprModel
import breeze.linalg._
import breeze.numerics._
import dk.gp.gpr.predict
import dk.gp.cov.CovSEiso
import dk.gp.gpr.gpr
import expedia.data.ExDataSource
import breeze.stats.mean
import dk.gp.gpc.GpcModel
import dk.gp.gpc.gpcTrain
import dk.gp.gpc.gpcPredict
import expedia.rankgpr.rankGprTrain
import expedia.rankgpr.RankGprModel

object GpTrainApp {

  def main(args: Array[String]): Unit = {

    val clusterSet = Set(65,66,44,52,96)
    val allClicks = ExDataSource(dsName = "test", "c:/perforce/daniel/ex/segments/destmonthdata/train_2013_dest8824_booked_only.csv").getAllClicks()

    val filteredClicks = allClicks.filter { c => c.isBooking == 1 && (clusterSet.contains(c.cluster)) && c.checkinMonth > -1 }

    val dataX = DenseVector(filteredClicks.map(c => c.checkinMonth.toDouble).toArray).toDenseMatrix.t
    val dataY = DenseVector(filteredClicks.map(c => c.cluster.toDouble).toArray)

    val covFunc = CovSEiso()
    val covFuncParams = DenseVector[Double](log(1), log(1))
    val noiseLogStdDev = log(1d)
    val model = RankGprModel(dataX, dataY, covFunc, covFuncParams, noiseLogStdDev)

    val trainedModel = rankGprTrain(model)
    println("learned covFuncParams=" + trainedModel.covFuncParams)
    println("learned noiseLogStdDev=" + trainedModel.noiseLogStdDev)
  }

}