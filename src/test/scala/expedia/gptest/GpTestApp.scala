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

object GpTestApp {

   val DAY = (1000L * 3600 * 24).toDouble
    val df = new SimpleDateFormat("yyyy-MM-dd")
   
  def main(args: Array[String]): Unit = {

    
    
    val cluster1 = 19
    val cluster2 = 23
    val allClicks = ExCSVDataSource(dsName = "test", "c:/perforce/daniel/ex/segments/dest_12217/train_2013_dest12217.csv").getAllClicks()
    
    val filteredClicks = allClicks.filter { c => c.isBooking == 1 && (c.cluster == cluster1 || c.cluster == cluster2) && c.dateTime.getTime > 0 }

    val dataX = DenseVector(filteredClicks.map(c => c.dateTime.getTime/DAY).toArray).toDenseMatrix.t
    val dataY = DenseVector(filteredClicks.map(c => if (c.cluster == cluster1) 1.0 else 0).toArray)

    println("trainning set size = " + dataX.rows)
    val covFunc = CovSEiso()
    val covFuncParams = DenseVector[Double](-1.5661650594032204, 0.2359353000844692)
    val noiseLogStdDev = -0.8175984176360551

    val gpMean = mean(dataY)
    val model = GprModel(dataX, dataY, covFunc, covFuncParams, noiseLogStdDev, gpMean)
    
    val p1 = new LocalDate(df.parse("2013-01-01"))
    val xTestArray = (0 to 24).map(i => p1.plusMonths(i).toDate().getTime/DAY).toArray
    val xTest = DenseVector(xTestArray).toDenseMatrix.t

    val predicted = predict(xTest, model)

    println(DenseMatrix.horzcat(xTest, predicted))
  }
  

}