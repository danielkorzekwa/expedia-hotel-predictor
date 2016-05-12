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

object AccuracySingleModelApp extends LazyLogging {

  def main(args: Array[String]): Unit = {

    val now = System.currentTimeMillis()

    //val expediaTrainFile = "c:/perforce/daniel/ex/data_500K/train_500K_2013.csv"
    val trainDS = ExDataSource(dsName = "trainDS", "c:/perforce/daniel/ex/data_all/train_all_2013.csv")

    val expediaTestFile = "c:/perforce/daniel/ex/data_booked/train_booked_2014_all_cols.csv"
    val testClicks = ExDataSource(dsName = "testDS", expediaTestFile).getAllClicks().filter(c => c.dist != -1)

    val model = ClusterDist2ModelBuilder.buildFromTrainingSet(trainDS, testClicks)
    val top5predictions = model.predictTop5(testClicks)
    val predictedMat = model.predict(testClicks)

    println(top5predictions.toString(20, 320))

    logger.info("Compute mapk..")

    val actual = DenseVector(testClicks.map(c => c.cluster.toDouble).toArray)

    val apkVector = averagePrecision(top5predictions(::, 5 to 9), actual, k = 5)
    val mapk = mean(apkVector)
    
    val loglikValue = loglik(predictedMat.map(x => x.toDouble), actual)
    
    println("mapk=%.6f, loglik=%6f, test size=%d".format(mapk, loglikValue,top5predictions.rows))

    csvwrite("target/predictions.csv", DenseMatrix.horzcat(top5predictions, actual.toDenseMatrix.t, apkVector.toDenseMatrix.t), header = "p1,p2,p3,p4,p5,r1,r2,r3,r4,r5,hotel_cluster,mapk")

    println("analysis time=" + (System.currentTimeMillis() - now))
  }

  private def loadPredictions(): DenseMatrix[Double] = {
    logger.info("Load clusterPredictions...")
    val clusterDistPred = csvread(new File("target/clusterDistPred_test.csv"), skipLines = 1)
    val marketDestPred = csvread(new File("target/marketDestPred_test.csv"), skipLines = 1)
    val clusterDistProxPred = csvread(new File("target/clusterDistProxPred_test.csv"), skipLines = 1)

    logger.info("combineClusterPredictions...")
    // [c1,c2,c3,c4,c5,p1,p2,p3,p4,p5]
    val top5predictions = combineClusterPredictions(clusterDistPred, marketDestPred, clusterDistProxPred)
    top5predictions
  }

  private def predictClustersAndSaveToFile(trainDS: ExDataSource, testClicks: Seq[Click]) = {
    logger.info("predictClusters and save to files...")

    val (clusterDistPred, marketDestPred, clusterDistProxPred) = predictClusters(trainDS, testClicks)

    csvwrite("target/clusterDistPred_test.csv", clusterDistPred, header = "p1,p2,p3,p4,p5,r1,r2,r3,r4,r5")
    csvwrite("target/marketDestPred_test.csv", marketDestPred, header = "p1,p2,p3,p4,p5,r1,r2,r3,r4,r5")
    csvwrite("target/clusterDistProxPred_test.csv", clusterDistProxPred, header = "p1,p2,p3,p4,p5,r1,r2,r3,r4,r5")
  }
}