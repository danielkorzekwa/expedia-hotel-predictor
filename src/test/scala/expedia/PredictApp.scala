package expedia

import breeze.linalg._
import java.io.File
import dk.gp.util.meanAveragePrecision
import com.typesafe.scalalogging.slf4j.LazyLogging
import breeze.stats._
import dk.gp.util.csvwrite
import scala.collection.immutable.HashSet
import dk.gp.util.filterRows
object PredictApp extends LazyLogging {

  def main(args: Array[String]): Unit = {

    val now = System.currentTimeMillis()

    logger.info("Loading data...")

    val dataA = csvread(new File("c:/perforce/daniel/ex/train_booked_sample_a.csv"), skipLines = 1) //(0 to 10000, ::)
    val dataB = csvread(new File("c:/perforce/daniel/ex/train_booked_sample_b.csv"), skipLines = 1) //(0 to 10001, ::)

    val expediaTrainFile = "c:/perforce/daniel/ex/data_500K/train_500K_2013.csv"

    val filteredDataB = dataB //filterRows(dataB,0, userId => userId == 195876)
    val predictionData = predictAll(dataA, expediaTrainFile, filteredDataB)
    val idx = predictionData(::, 0).findAll(p => true)
    val filteredPredictonData = predictionData(idx, ::).toDenseMatrix

    println(filteredPredictonData.toString(20, 320))

    val mapk = mean(filteredPredictonData(::, 11))

    println("mapk=%.5f, size=%d".format(mapk, filteredPredictonData.rows))
    println("train/test size= %d / %d".format(dataA.rows, dataB.rows))
    println("analysis time=" + (System.currentTimeMillis() - now))

    csvwrite("target/predictions.csv", filteredPredictonData, header = "p1,p2,p3,p4,p5,r1,r2,r3,r4,r5,hotel_cluster,mapk")
  }
}