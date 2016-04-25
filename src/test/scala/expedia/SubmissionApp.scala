package expedia

import breeze.linalg._
import java.io.File
import com.typesafe.scalalogging.slf4j.LazyLogging
import org.apache.commons.io.FileUtils
import scala.collection.JavaConversions._

object SubmissionApp extends LazyLogging {

  def main(args: Array[String]): Unit = {

    logger.info("Loading train data")
    val dataA = csvread(new File("c:/perforce/daniel/ex/train_booked_all_ab.csv"), skipLines = 1)
    val expediaTrainFile = "c:/perforce/daniel/ex/orig_data/train.csv"
 val svmPredictionsData =  csvread(new File("c:/perforce/daniel/ex/data_booked/svm_predictions_sample_b.csv"), skipLines = 1)
 
    logger.info("Loading test data...")
    val test = csvread(new File("c:/perforce/daniel/ex/test_all.csv"), skipLines = 1) //(0 to 1000, ::)

    logger.info("Loading test data...done")
    val predictionData = predictAll(dataA, expediaTrainFile, svmPredictionsData,test)

    logger.info("Saving predictions...")
    FileUtils.writeLines(new File("target/submission.csv"), List("id,hotel_cluster"), false)
    (0 until test.rows).foreach { i =>
      val predictionRow = predictionData(i, ::).t(5 to 9).toArray.map(x => x.toInt).mkString(" ")
      val predictions = predictionData(i, ::)
      val line = i + "," + predictionRow

      FileUtils.writeLines(new File("target/submission.csv"), List(line), true)
    }

  }

}