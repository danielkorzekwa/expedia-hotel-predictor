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

object GpTestApp {

  def main(args: Array[String]): Unit = {

    val cluster1 = 19
    val cluster2 = 23
    
    val allClicks = ExDataSource(dsName = "test", "c:/perforce/daniel/ex/segments/dest_12217/train_2013_dest12217.csv").getAllClicks()
    val filteredClicks = allClicks.filter { c => c.isBooking == 1 && (c.cluster == cluster1 || c.cluster == cluster2) && c.checkinMonth > -1 }

    val dataX = DenseVector(filteredClicks.map(c => c.checkinMonth.toDouble).toArray).toDenseMatrix.t
    val dataY = DenseVector(filteredClicks.map(c => if (c.cluster == cluster1) 1.0 else 0).toArray)


    println("trainning set size = " + dataX.rows)
    val covFunc = CovSEiso()
    val covFuncParams = DenseVector[Double](log(1), log(1))
    val noiseLogStdDev = log(1d)
    val mean = 0
    val gprModel = GprModel(dataX, dataY, covFunc, covFuncParams, noiseLogStdDev, mean)

    val model = gpr(dataX, dataY, covFunc, covFuncParams, noiseLogStdDev, mean)
println(model.covFuncParams)
    val xTest = DenseVector.rangeD(1d, 13, 1).toDenseMatrix.t
    val predicted = predict(xTest, model)

    println(DenseMatrix.horzcat(xTest, predicted))
  }
}