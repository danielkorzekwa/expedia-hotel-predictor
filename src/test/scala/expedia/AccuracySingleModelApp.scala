package expedia

import scala.collection.mutable.ListBuffer
import com.typesafe.scalalogging.slf4j.LazyLogging
import breeze.linalg._
import breeze.stats._
import dk.gp.util.averagePrecision
import dk.gp.util.csvwrite
import expedia.data.Click
import expedia.data.ExKryoDataSource
import expedia.model.countryuser.CountryUserModelBuilder
import dk.bayes.math.accuracy.loglik
import expedia.model.cmu.CmuModelBuilder
import expedia.model.countryuser.CountryUserModel
import expedia.model.dest.DestModelBuilder
import expedia.model.marketdest.MarketDestModelBuilder
import expedia.model.destcluster.DestClusterModelBuilder
import expedia.model.marketdestcluster.MarketDestClusterModelBuilder
import expedia.model.marketdestcluster.MarketDestClusterModel
import expedia.model.marketdestuser.MarketDestUserPredictionModel
import expedia.model.marketdestuser.MarketDestUserPredictionModelBuilder
import expedia.model.marketuser.MarketUserModelBuilder
import expedia.model.distsvm.DistSvmModel
import expedia.model.distgp.DistGpModel
import expedia.model.clusterdist.ClusterDistPredictionModel
import expedia.model.clusterdist.ClusterDistPredictionModelBuilder
import expedia.data.ExCSVDataSource
import expedia.data.ExKryoDataSource
import expedia.model.country.CountryModelBuilder
import expedia.model.marketmodel.MarketModelBuilder
import expedia.CompoundHyperParams
import dk.gp.util.saveObject
import dk.gp.util.loadObject

object AccuracySingleModelApp extends LazyLogging {

  def main(args: Array[String]): Unit = {

    val now = System.currentTimeMillis()

   
    val marketIds = Set(628, 675, 365, 1230, 637, 701)
    // val marketIds = Set(675)

    def filterTrain(click: Click) = {
    true//  click.continentId==3
    }

    val expediaTrainFileKryo = "c:/perforce/daniel/ex/segments/continent_2/train_2013_continent2.kryo"
    val trainDS = ExKryoDataSource(dsName = "trainDS", expediaTrainFileKryo, filterTrain)
    val expediaTestFileKryo = "c:/perforce/daniel/ex/segments/continent_2/train_2014_continent2_booked_only.kryo"
    val testClicks = ExKryoDataSource(dsName = "testDS", expediaTestFileKryo).getAllClicks()// .filter(click =>   click.marketId==1392)

    val hyperParamsListFromDisk = loadObject[List[SimpleHyperParams]]( "c:/perforce/daniel/ex/hyperparams/hyperParams_best_020616_test14.kryo")
     val hyperParams = CompoundHyperParams(testClicks,hyperParamsListFromDisk)

    
//    val expediaTrainFileKryo = "c:/perforce/daniel/ex/segments/by6months/train_until_140701.kryo"
//    val trainDS = ExKryoDataSource(dsName = "trainDS", expediaTrainFileKryo, filterTrain)
//    val expediaTestFileKryo = "c:/perforce/daniel/ex/segments/by6months/train_140701_150101_booked_only.kryo"
//    val testClicks = ExKryoDataSource(dsName = "testDS", expediaTestFileKryo).getAllClicks() .filter(click =>  click.continentId==3)

    val model = CmuModelBuilder.buildFromTrainingSet(trainDS, testClicks, hyperParams)
    // val model = DistGpModel.build()

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