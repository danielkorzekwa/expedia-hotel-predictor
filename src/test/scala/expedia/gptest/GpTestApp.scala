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

object GpTestApp {

   val DAY = (1000L * 3600 * 24).toDouble
    val df = new SimpleDateFormat("yyyy-MM-dd")
   
  def main(args: Array[String]): Unit = {

    
    
    val cluster1 = 18
    val cluster2 = 95
    val allClicks = ExDataSource(dsName = "test", "c:/perforce/daniel/ex/segments/destmonthdata/train_2013_dest12218_booked_only.csv").getAllClicks()
    
    val filteredClicks = allClicks.filter { c => c.isBooking == 1 && (c.cluster == cluster1 || c.cluster == cluster2) && c.checkinDate.getTime > 0 }

    val dataX = DenseVector(filteredClicks.map(c => c.checkinDate.getTime/DAY).toArray).toDenseMatrix.t
    val dataY = DenseVector(filteredClicks.map(c => if (c.cluster == cluster1) 1.0 else 0).toArray)

    println("trainning set size = " + dataX.rows)
    val covFunc = CovSEiso()
    val covFuncParams = DenseVector[Double](-0.6770088740246011, 0.000722419202977661)
    val noiseLogStdDev = log(1)

    val gpMean = mean(dataY)
    val model = GprModel(dataX, dataY, covFunc, covFuncParams, noiseLogStdDev, gpMean)
    
    val p1 = new LocalDate(df.parse("2013-01-01"))
    val xTestArray = (0 to 24).map(i => p1.plusMonths(i).toDate().getTime/DAY).toArray
    val xTest = DenseVector(xTestArray).toDenseMatrix.t

    val predicted = predict(xTest, model)

    println(DenseMatrix.horzcat(xTest, predicted))
  }
  

}