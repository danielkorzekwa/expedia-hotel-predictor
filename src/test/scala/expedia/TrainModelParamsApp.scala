package expedia

import breeze.optimize.ApproximateGradientFunction
import breeze.linalg.DenseVector
import breeze.optimize.LBFGS
import breeze.linalg._
import breeze.util.LazyLogger
import com.typesafe.scalalogging.slf4j.LazyLogging
import expedia.data.ExDataSource
import expedia.model.marketdest.MarketDestPredictionModel
import dk.gp.util.averagePrecision
import breeze.stats._

object TrainModelParamsApp extends LazyLogging {

  def main(args: Array[String]): Unit = {

    val expediaTestFile = "c:/perforce/daniel/ex/data_booked/train_booked_2014_all_cols.csv"
    val testClicks = ExDataSource(dsName = "testDS", expediaTestFile).getAllClicks()

    val trainDS = ExDataSource(dsName = "trainDS", "c:/perforce/daniel/ex/data_500K/train_500K_2013.csv")

    def g(x: DenseVector[Double]) = {

      val top5predictions = MarketDestPredictionModel(trainDS, testClicks, x(0)).predictTop5(testClicks)

      val actual = DenseVector(testClicks.map(c => c.cluster.toDouble).toArray)
      val mapk = mean(averagePrecision(top5predictions(::, 5 to 9), actual, k = 5))
     -mapk
    }
    val diffFunc = new ApproximateGradientFunction(g)

    val initialParams = DenseVector(300d)
    val optimizer = new LBFGS[DenseVector[Double]](maxIter = 100, m = 6, tolerance = 1.0E-6)
    val optIterations = optimizer.iterations(diffFunc, initialParams).map { state => println("iter=%d, loglik=%.4f, params=%s".format(state.iter, state.value, state.x)); state }.toList
    val newParams = optIterations.last.x

    logger.info("New params:" + newParams)
  }
}