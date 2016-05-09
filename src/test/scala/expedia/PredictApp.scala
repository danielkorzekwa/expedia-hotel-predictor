package expedia

import breeze.linalg._
import java.io.File
import dk.gp.util.meanAveragePrecision
import com.typesafe.scalalogging.slf4j.LazyLogging
import breeze.stats._
import dk.gp.util.csvwrite
import scala.collection.immutable.HashSet
import dk.gp.util.filterRows
import expedia.model.svm.SVMPredictionModel
import expedia.model.svm.libsvm.LibSvmModel
import dk.gp.util.averagePrecision
import expedia.data.ExDataSource
object PredictApp extends LazyLogging {

  def main(args: Array[String]): Unit = {

    val now = System.currentTimeMillis()

    logger.info("Loading data...")

    val expediaTrainFile = "c:/perforce/daniel/ex/data_all/train_all_2013.csv"
    // val expediaTrainFile = "c:/perforce/daniel/ex/data_500K/train_500K_2013.csv"

    val expediaTestFile = "c:/perforce/daniel/ex/data_booked/train_booked_2014_all_cols.csv"
    val testClicks = ExDataSource(expediaTestFile).getAllClicks()//.filter(c => c.destId==8250)

    val predictionData = predictAll(expediaTrainFile, testClicks)
    val actual = DenseVector(testClicks.map(c => c.cluster).toArray) //filteredDataB(::, filteredDataB.cols - 1)
    val apk = (0 until actual.size).map { i => averagePrecision(predictionData(i, 5 to 9).t.toArray, Array(actual(i)), 5)}
    val apkVector = DenseVector(apk.toArray)

    val idx = predictionData(::, 0).findAll(p => true)
    val filteredPredictonData = predictionData(idx, ::).toDenseMatrix
    println(filteredPredictonData.toString(20, 320))

    val mapk = mean(apkVector)

    println("mapk=%.5f, size=%d".format(mapk, filteredPredictonData.rows))
    //println("train/test size= %d / %d".format(dataA.rows, dataB.rows))
    println("analysis time=" + (System.currentTimeMillis() - now))

    //  csvwrite("target/predictions.csv", DenseMatrix.horzcat(filteredPredictonData, actual.toDenseMatrix.t, apkVector.toDenseMatrix.t), header = "p1,p2,p3,p4,p5,r1,r2,r3,r4,r5,hotel_cluster,mapk")
  }
}