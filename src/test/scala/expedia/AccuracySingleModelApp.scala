package expedia

import scala.collection.mutable.ListBuffer
import com.typesafe.scalalogging.slf4j.LazyLogging
import breeze.linalg._
import breeze.stats._
import dk.gp.util.averagePrecision
import dk.gp.util.csvwrite
import dk.gp.util.loadObject
import expedia.data.Click
import expedia.data.ExKryoDataSource
import expedia.data.ExKryoDataSource
import expedia.data.ExKryoDataSource
import expedia.data.ExKryoDataSource
import expedia.model.marketdestuser2.MarketDestUserModelBuilder2
import dk.bayes.math.accuracy.loglik
import expedia.model.marketdestuser2.MarketDestUserModelBuilder2
import expedia.model.cmu.CmuModelBuilder2
import scala.collection._
import dk.gp.util.saveObject
import expedia.data.ExCSVDataSource
import expedia.model.country.CountryModelBuilder2
import expedia.model.countryuser.CountryUserModelBuilder2
import expedia.modelgp.country.CountryGPModel
import expedia.modelgp.countryuser.CountryUserGPModel

object AccuracySingleModelApp extends LazyLogging {

  def main(args: Array[String]): Unit = {

    val now = System.currentTimeMillis()

    //    val expediaTrainFileKryo = "c:/perforce/daniel/ex/segments/all/train_2013.kryo"
    //    val trainDS = ExKryoDataSource(dsName = "trainDS", expediaTrainFileKryo)
    //    val expediaTestFileKryo = "c:/perforce/daniel/ex/segments/all/train_2014_booked_only.kryo"
    //    val testClicks = ExKryoDataSource(dsName = "testDS", expediaTestFileKryo).getAllClicks()
    //
//        val expediaTrainFileKryo = "c:/perforce/daniel/ex/segments/continent_2/train_2013_continent2.kryo"
//        val trainDS = ExKryoDataSource(dsName = "trainDS", expediaTrainFileKryo)
//        val expediaTestFileKryo = "c:/perforce/daniel/ex/segments/continent_2/train_2014_continent2_booked_only.kryo"
//        val testClicks = ExKryoDataSource(dsName = "testDS", expediaTestFileKryo).getAllClicks()
    //    
    val expediaTrainGP13 = "c:/perforce/daniel/ex/segments/cont_3_14smallest_countries/train_2013_booked_only.csv"
    val trainDS = ExCSVDataSource(dsName = "trainDS", expediaTrainGP13)
    val expediaTestGP14 = "c:/perforce/daniel/ex/segments/cont_3_14smallest_countries/train_2014_booked_only.csv"
    val testClicks = ExCSVDataSource(dsName = "testDS", expediaTestGP14).getAllClicks()

   //   val hyperParamsMap = loadObject[CompoundHyperParamsMap]("target/hyperParamsMapByMarket_trained.kryo")

    val hyperParamsMap = CompoundHyperParamsMap(mutable.Map())
    val modelBuilder = CountryModelBuilder2.build(trainDS, testClicks, hyperParamsMap)
    val modelParams = hyperParamsMap.getModel("country")
 //   val model = modelBuilder.create(trainDS, testClicks, modelParams)
    val model = CountryUserGPModel(trainDS)
    val k = 5
    val top5predictions = model.predictTop5(testClicks)
    val predictedMat = model.predict(testClicks) + 1e-10f
    logger.info("Compute mapk..")

    val actual = DenseVector(testClicks.map(c => c.cluster.toDouble).toArray)
    println(DenseMatrix.horzcat(actual.toDenseMatrix.t, top5predictions).toString(20, 320))

    val apkVector = averagePrecision(top5predictions(::, top5predictions.cols / 2 until top5predictions.cols), actual, k)
    val mapk = mean(apkVector)

    val loglikValue = loglik(predictedMat.map(x => x.toDouble), actual)

    println("mapk=%.8f, loglik=%.1f, test size=%d".format(mapk, loglikValue, top5predictions.rows))

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