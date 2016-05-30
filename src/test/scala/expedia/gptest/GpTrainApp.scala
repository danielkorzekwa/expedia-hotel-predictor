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

    val clusterSet = Set(56,41)
    val allClicks = ExCSVDataSource(dsName = "test",  "c:/perforce/daniel/ex/segments/loc_market_dest/train_2013_loc_2096_market_675_dest_8267.csv").getAllClicks()

    val filteredClicks = allClicks//.filter { c => (clusterSet.contains(c.cluster))}

    val dataX = DenseVector(filteredClicks.map(c => c.dist).toArray).toDenseMatrix.t
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