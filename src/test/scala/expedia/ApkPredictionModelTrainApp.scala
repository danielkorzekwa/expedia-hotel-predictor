package expedia

import java.io.File

import breeze.linalg.DenseVector
import breeze.linalg.csvread
import dk.gp.util.averagePrecision
import expedia.data.ExDataSource

object ApkPredictionModelTrainApp {

  def main(args: Array[String]): Unit = {

    val expediaTestFile = "c:/perforce/daniel/ex/data_booked/train_booked_2014_all_cols.csv"
    val testClicks = ExDataSource(dsName = "testDS", expediaTestFile).getAllClicks().take(1000)
    val actual = DenseVector(testClicks.map(c => c.cluster.toDouble).toArray)

    val top5predictions = csvread(new File("target/clusterDistPred_test.csv"), skipLines = 1)(0 to 999, ::)

    val apkVector = averagePrecision(top5predictions(::, 5 to 9), actual, k = 5)

    val p1ProbVec = top5predictions(::, 0)

    
    println(p1ProbVec.size)
   // println(p1ProbVec.toArray.map(p => BigDecimal(p).setScale(3, BigDecimal.RoundingMode.HALF_UP).toDouble).toList.distinct.size)

  }
}