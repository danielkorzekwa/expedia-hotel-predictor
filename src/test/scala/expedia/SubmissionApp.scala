package expedia

import scala.collection.JavaConversions._
import com.typesafe.scalalogging.slf4j.LazyLogging
import expedia.data.ExDataSource
import org.apache.commons.io.FileUtils
import java.io.File
import expedia.data.ExCSVDataSource

object SubmissionApp extends LazyLogging {

  def main(args: Array[String]): Unit = {

    val expediaTrainFile = "c:/perforce/daniel/ex/orig_data/train.csv"
    val trainDataSource = ExCSVDataSource(dsName = "trainDS", expediaTrainFile)

    logger.info("Loading test data...")
    val expediaTestFile = "c:/perforce/daniel/ex/data_test/test_all_all_cols.csv"
    val testClicks = ExCSVDataSource(dsName = "testDS", expediaTestFile).getAllClicks()

    val (clusterDistPred, marketDestPred, clusterDistProxPred) = predictClusters(trainDataSource, testClicks)

    logger.info("combineClusterPredictions...")
    // [c1,c2,c3,c4,c5,p1,p2,p3,p4,p5]
    val top5predictions = combineClusterPredictions(clusterDistPred, marketDestPred, clusterDistProxPred)

    		 FileUtils.writeLines(new File("target/submission.csv"), List("id,hotel_cluster"), false)
    (0 until top5predictions.rows).foreach { i =>
      val predictionRow = top5predictions(i, ::).t(5 to 9).toArray.map(x => x.toInt).mkString(" ")
      val line = i + "," + predictionRow
      FileUtils.writeLines(new File("target/submission.csv"), List(line), true)
    }

  }

}