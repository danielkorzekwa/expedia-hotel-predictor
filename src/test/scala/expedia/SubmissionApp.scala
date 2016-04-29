package expedia

import breeze.linalg._
import java.io.File
import com.typesafe.scalalogging.slf4j.LazyLogging
import org.apache.commons.io.FileUtils
import scala.collection.JavaConversions._
import expedia.model.svm.SVMPredictionModel
import expedia.model.svm.libsvm.LibSvmModel

object SubmissionApp extends LazyLogging {

  def main(args: Array[String]): Unit = {

    logger.info("Loading train data")
    val expediaTrainFile = "c:/perforce/daniel/ex/orig_data/train.csv"
    val svmPredictionsData = csvread(new File("c:/perforce/daniel/ex/data_booked/svm_predictions_dest_20K.csv"), skipLines = 1)

    logger.info("Loading test data...")
     val expediaTestFile = "c:/perforce/daniel/ex/data_test/test_all.csv"
    val test = csvread(new File("c:/perforce/daniel/ex/data_test/test_all.csv"), skipLines = 1) //(0 to 1000, ::)

    logger.info("Loading test data...done")
    val predictionData = predictAll(expediaTrainFile, expediaTestFile,svmPredictionsData, test)

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