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
import expedia.data.ExDataSource
import expedia.data.ExCSVDataSource

object GpTrainApp {

   val DAY = (1000L * 3600 * 24).toDouble
  
  def main(args: Array[String]): Unit = {

    val clusterSet = Set(19, 21, 23)
    val allClicks = ExCSVDataSource(dsName = "test",  "c:/perforce/daniel/ex/segments/dest_12217/train_2013_dest12217.csv").getAllClicks()

    val filteredClicks = allClicks.filter { c => c.isBooking == 1 && (clusterSet.contains(c.cluster)) && c.dateTime.getTime > 0 }

    val dataX = DenseVector(filteredClicks.map(c => c.dateTime.getTime/DAY).toArray).toDenseMatrix.t
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