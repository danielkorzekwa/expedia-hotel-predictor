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

object GpTestApp {

  def main(args: Array[String]): Unit = {

    
    
    val cluster1 = 52
    val cluster2 = 44
    val allClicks = ExDataSource(dsName = "test", "c:/perforce/daniel/ex/segments/destmonthdata/train_2013_dest8824_booked_only.csv").getAllClicks()
    
    val filteredClicks = allClicks.filter { c => c.isBooking == 1 && (c.cluster == cluster1 || c.cluster == cluster2) && c.checkinMonth > -1 }

    val dataX = DenseVector(filteredClicks.map(c => c.checkinMonth.toDouble).toArray).toDenseMatrix.t
    val dataY = DenseVector(filteredClicks.map(c => if (c.cluster == cluster1) 1.0 else 0).toArray)

    println("trainning set size = " + dataX.rows)
    val covFunc = CovSEiso()
    val covFuncParams = DenseVector[Double](-0.6201170019402429, 2.2992009698305256)
    val noiseLogStdDev = -0.869945388217244

    val gpMean = 0d//mean(dataY)
    val model = GprModel(dataX, dataY, covFunc, covFuncParams, noiseLogStdDev, gpMean)

    val xTest = DenseVector.rangeD(1d, 13, 1).toDenseMatrix.t
    val predicted = predict(xTest, model)

    println(DenseMatrix.horzcat(xTest, predicted))
  }
  

}