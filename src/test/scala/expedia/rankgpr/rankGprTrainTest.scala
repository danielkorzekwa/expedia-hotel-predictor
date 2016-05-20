package expedia.rankgpr

import org.junit._
import Assert._
import expedia.data.ExDataSource
import breeze.linalg.DenseVector
import breeze.linalg._
import breeze.numerics._
import dk.gp.cov.CovSEiso
import breeze.stats._

class rankGprTrainTest {

  val DAY = (1000L * 3600 * 24).toDouble

  @Test def test = {

    val clusterSet = Set(19, 21, 23)

    val allClicks = ExDataSource(dsName = "test", "c:/perforce/daniel/ex/segments/dest_12217/train_2013_dest12217.csv").getAllClicks()
    val filteredClicks = allClicks.filter { c => c.isBooking == 1 && (clusterSet.contains(c.cluster)) && c.checkinDate.getTime > 0 }

    val dataX = DenseVector(filteredClicks.map(c => c.checkinDate.getTime / DAY).toArray).toDenseMatrix.t
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