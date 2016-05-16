package expedia

import java.io.File
import com.typesafe.scalalogging.slf4j.LazyLogging
import breeze.linalg._
import expedia.data.Click
import expedia.data.ExDataSource
import dk.gp.util.averagePrecision
import breeze.stats._
import dk.gp.util.csvwrite
import expedia.model.clusterdistprox.ClusterDistProxModel
import expedia.data.ExDataSource
import expedia.model.marketdest.MarketDestPredictionModel
import expedia.model.clusterdist.ClusterDistPredictionModel
import expedia.model.marketdest.MarketDestPredictionModelBuilder
import expedia.model.clusterdist.ClusterDistPredictionModelBuilder
import expedia.data.ExDataSource
import expedia.model.clusterdist2.ClusterDist2ModelBuilder
import dk.bayes.math.accuracy.loglik
import scala.collection.mutable.ListBuffer

object AccuracySingleModelApp extends LazyLogging {

  def main(args: Array[String]): Unit = {

    val now = System.currentTimeMillis()

    def filter(click: Click) =  true

    //val expediaTrainFile = "c:/perforce/daniel/ex/data_500K/train_500K_2013.csv"
    val trainDS = ExDataSource(dsName = "trainDS", "c:/perforce/daniel/ex/data_all/train_all_2013.csv", filter)

    val expediaTestFile = "c:/perforce/daniel/ex/data_booked/train_booked_2014_all_cols.csv"
 
     val clusterDistPred = csvread(new File("target/clusterDistPred_test.csv"), skipLines = 1)
    val testClicks = ExDataSource(dsName = "testDS", expediaTestFile, filter).getAllClicks().
    zipWithIndex.filter{ case (c,index) => clusterDistPred(index,0)==0  }.
    map(_._1)
    //.filter(c => c.userLoc == 24103 && c.marketId == 628)
    //

    val model = MarketDestPredictionModelBuilder.buildFromTrainingSet(trainDS, testClicks)
    val top5predictions = model.predictTop5(testClicks)

  //  val predictedMat = model.predict(testClicks)

    println(top5predictions.toString(20, 320))

    logger.info("Compute mapk..")

    val actual = DenseVector(testClicks.map(c => c.cluster.toDouble).toArray)

    val apkVector = averagePrecision(top5predictions(::, 5 to 9), actual, k = 5)
    val mapk = mean(apkVector)

    val loglikValue = Double.NaN//loglik(predictedMat.map(x => x.toDouble), actual)

    println("mapk=%.8f, loglik=%6f, test size=%d".format(mapk, loglikValue, top5predictions.rows))

    csvwrite("target/predictions.csv", DenseMatrix.horzcat(top5predictions, actual.toDenseMatrix.t, apkVector.toDenseMatrix.t), header = "p1,p2,p3,p4,p5,r1,r2,r3,r4,r5,hotel_cluster,mapk")

  //  val calibrationData = computeCalibrationData(top5predictions, actual)
  //  logger.info("Calibration rows:" + calibrationData.rows)
  //  csvwrite("target/calibration.csv", calibrationData, header = "p,actual")

    // csvwrite("target/marketDestPred_no_user_test.csv", top5predictions, header = "p1,p2,p3,p4,p5,r1,r2,r3,r4,r5")

    println("analysis time=" + (System.currentTimeMillis() - now))
  }

  /**
   * @return [prob, actual(0/1)]
   */
  private def computeCalibrationData(top5predictions: DenseMatrix[Double], actual: DenseVector[Double]): DenseMatrix[Double] = {

    //List of [prob,actual]
    val calibrationDataBuffer = ListBuffer[DenseVector[Double]]()

    (0 until top5predictions.rows).foreach { r =>

      val predictionRow = top5predictions(r, ::)
      val actualVal = actual(r)

      (0 until 5).foreach { pIndex =>

        val p = predictionRow(pIndex)
        val r = predictionRow(pIndex + 5)

        if (p > 0) {
          val actualFlag = if (r == actualVal) {
            1
          } else 0

          calibrationDataBuffer += DenseVector(p, actualFlag)
        }
      }
    }

    val calibrationData = DenseVector.horzcat(calibrationDataBuffer.toList: _*).t
    calibrationData
  }

}