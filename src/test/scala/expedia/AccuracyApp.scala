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
import expedia.model.clusterdist.ClusterDistPredictionModel
import expedia.model.clusterdist.ClusterDistPredictionModelBuilder
import expedia.data.ExDataSource
import expedia.model.clusterdist2.ClusterDist2ModelBuilder
import expedia.data.ExCSVDataSource
import expedia.data.ExDataSource
import dk.gp.util.saveObject
import dk.gp.util.loadObject
import expedia.data.ExKryoDataSource

object AccuracyApp extends LazyLogging {

  def main(args: Array[String]): Unit = {

    val now = System.currentTimeMillis()

    def filterTrain(click: Click) = {
      true //click.dist > -1
    }

    val expediaTrainFile = "c:/perforce/daniel/ex/segments/all/train_2013.csv"
    val trainDS = ExCSVDataSource(dsName = "trainDS", expediaTrainFile, filterTrain)
    val expediaTestFile = "c:/perforce/daniel/ex/segments/all/train_2014_booked_only.csv"
    val testClicks = ExCSVDataSource(dsName = "testDS", expediaTestFile).getAllClicks() //.filter(click =>   click.marketId==1392)

//            val expediaTrainFile = "c:/perforce/daniel/ex/segments/loc_market_dest/more_than_1000/train_2013.csv"
//            val trainDS = ExCSVDataSource(dsName = "trainDS", expediaTrainFile, filterTrain)
//            val expediaTestFile = "c:/perforce/daniel/ex/segments/loc_market_dest/more_than_1000/train_2014_booked_only.csv"
//            val testClicks = ExCSVDataSource(dsName = "testDS", expediaTestFile).getAllClicks() //.filter(click =>   click.marketId==1392)

    //    val expediaTrainFileKryo = "c:/perforce/daniel/ex/segments/continent_3/train_2013_continent3.kryo"
    //    val trainDS = ExKryoDataSource(dsName = "trainDS", expediaTrainFileKryo, filterTrain)
    //    val expediaTestFileKryo = "c:/perforce/daniel/ex/segments/continent_3/train_2014_continent3_booked_only.kryo"
    //    val testClicks = ExKryoDataSource(dsName = "testDS", expediaTestFileKryo).getAllClicks() //.filter(click =>   click.marketId==1392)

              val hyperParamsListFromDisk = loadObject[List[SimpleHyperParams]]( "c:/perforce/daniel/ex/hyperparams/hyperParams_best_020616_test14.kryo")
     val hyperParams = CompoundHyperParams(testClicks,hyperParamsListFromDisk)
    predictClustersAndSaveToFile(trainDS, testClicks, hyperParams)

    // [c1,c2,c3,c4,c5,p1,p2,p3,p4,p5]
    val top5predictions = loadPredictions()

    logger.info("Compute mapk..")

    val actual = DenseVector(testClicks.map(c => c.cluster.toDouble).toArray)
    val xData = DenseVector.horzcat(testClicks.map(c => DenseVector(c.userLoc.toDouble, c.marketId.toDouble, c.destId, c.dist)): _*).t
    val apkVector = averagePrecision(top5predictions(::, 5 to 9), actual, k = 5)
    println(DenseMatrix.horzcat(xData, actual.toDenseMatrix.t, top5predictions).toString(20, 320))

    val mapk = mean(apkVector)
    println("mapk=%.8f, test size=%d".format(mapk, top5predictions.rows))

    csvwrite("target/predictions.csv", DenseMatrix.horzcat(top5predictions, actual.toDenseMatrix.t, apkVector.toDenseMatrix.t), header = "p1,p2,p3,p4,p5,r1,r2,r3,r4,r5,hotel_cluster,mapk")

    println("analysis time=" + (System.currentTimeMillis() - now))
  }

  private def loadPredictions(): DenseMatrix[Double] = {
    logger.info("Load clusterPredictions...")
    val clusterDistPred = csvread(new File("target/clusterDistPred_test.csv"), skipLines = 1)
    val marketDestPred = csvread(new File("target/marketDestPred_test.csv"), skipLines = 1)
    val clusterDistProxPred = csvread(new File("target/clusterDistProxPred_test.csv"), skipLines = 1)
    val distSvmPred = csvread(new File("target/distSvmPred_test.csv"), skipLines = 1)
    val distGPPred = csvread(new File("target/distGPPred_test.csv"), skipLines = 1)

    logger.info("combineClusterPredictions...")
    // [c1,c2,c3,c4,c5,p1,p2,p3,p4,p5]
    val top5predictions = combineClusterPredictions(clusterDistPred, marketDestPred, clusterDistProxPred, distSvmPred, distGPPred)
    top5predictions
  }

  private def predictClustersAndSaveToFile(trainDS: ExDataSource, testClicks: Seq[Click], hyperParams: CompoundHyperParams) = {
    logger.info("predictClusters and save to files...")

    val (clusterDistPred, marketDestPred, clusterDistProxPred, distSvmPred, distGPPred) = predictClustersCMU(trainDS, testClicks, hyperParams)

    csvwrite("target/clusterDistPred_test.csv", clusterDistPred, header = "p1,p2,p3,p4,p5,r1,r2,r3,r4,r5")
    csvwrite("target/marketDestPred_test.csv", marketDestPred, header = "p1,p2,p3,p4,p5,r1,r2,r3,r4,r5")
    csvwrite("target/clusterDistProxPred_test.csv", clusterDistProxPred, header = "p1,p2,p3,p4,p5,r1,r2,r3,r4,r5")
    csvwrite("target/distSvmPred_test.csv", distSvmPred, header = "p1,p2,p3,p4,p5,r1,r2,r3,r4,r5")
    csvwrite("target/distGPPred_test.csv", distGPPred, header = "p1,p2,p3,p4,p5,r1,r2,r3,r4,r5")

  }
}