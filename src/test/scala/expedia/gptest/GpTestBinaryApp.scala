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
import org.joda.time.LocalTime
import java.text.SimpleDateFormat
import org.joda.time.LocalDate
import expedia.data.ExCSVDataSource

object GpTestBinaryApp {

  def main(args: Array[String]): Unit = {

    val cluster1 = 56
    val cluster2 = 41
    val allClicks = ExCSVDataSource(dsName = "test", "c:/perforce/daniel/ex/segments/loc_market_dest/train_2013_loc_2096_market_675_dest_8267.csv").getAllClicks()

    val filteredClicks = allClicks.filter { c => c.isBooking == 1 && (c.cluster == cluster1 || c.cluster == cluster2) }

    val dataX = DenseVector(filteredClicks.map(c => c.dist).toArray).toDenseMatrix.t
    val dataY = DenseVector(filteredClicks.map(c => if (c.cluster == cluster1) 1.0 else 0).toArray)

    println("trainning set size = " + dataX.rows)
    //    val covFunc = CovSEiso()
    //    val covFuncParams = DenseVector[Double](-0.6503421631104198, 0.36622005354726767)
    //    val noiseLogStdDev = -0.9264392185311553

    val covFunc = TestCovFunc()
    val covFuncParams = DenseVector[Double](-0.794353361706918, -11.251867145609713, -1.1522147378772258, 0.28935151100974615)
    val noiseLogStdDev = -3.0328025222890753

    val gpMean = mean(dataY)
    val model = GprModel(dataX, dataY, covFunc, covFuncParams, noiseLogStdDev, gpMean)

    val testX = DenseVector.rangeD(4015.0, 4025.0, 0.1).toDenseMatrix.t
    val predicted = predict(testX, model)

    println(DenseMatrix.horzcat(testX, predicted).toString(300, 100))
  }

}