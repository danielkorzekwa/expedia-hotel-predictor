package expedia

import java.io.File
import breeze.linalg.DenseVector
import breeze.linalg.csvread
import dk.gp.util.averagePrecision
import expedia.data.ExDataSource
import expedia.model.svm.libsvm.svr.svrTrain
import expedia.model.svm.libsvm.svr.svrPredict
import breeze.linalg.DenseMatrix
import expedia.model.svm.libsvm.svr.SvrModel
import expedia.data.ExCSVDataSource

object ApkPredictionModelTrainApp {

  def main(args: Array[String]): Unit = {

    val n = 1000

    val expediaTestFile = "c:/perforce/daniel/ex/data_booked/train_booked_2014_all_cols.csv"
    val testClicks = ExCSVDataSource(dsName = "testDS", expediaTestFile).getAllClicks().take(n)
    val actual = DenseVector(testClicks.map(c => c.cluster.toDouble).toArray)

    val clusterDistTop5Pred = csvread(new File("target/clusterDistPred_test.csv"), skipLines = 1)(0 until n, ::)
    val clusterDistModel = trainApkModel(actual, clusterDistTop5Pred)
    clusterDistModel.save("target/apk_clusterDistModel.svr")

    val marketDestTop5Pred = csvread(new File("target/marketDestPred_test.csv"), skipLines = 1)(0 until n, ::)
    val marketDestModel = trainApkModel(actual, marketDestTop5Pred)
    marketDestModel.save("target/apk_marketDestModel.svr")

  }

  private def trainApkModel(actualApkVec: DenseVector[Double], top5predictions: DenseMatrix[Double]): SvrModel = {
    val apkVector = averagePrecision(top5predictions(::, 5 to 9), actualApkVec, k = 5)
    val p1ProbVec = top5predictions(::, 0)
    val model = svrTrain(p1ProbVec.toDenseMatrix.t, apkVector)

    model
  }
}