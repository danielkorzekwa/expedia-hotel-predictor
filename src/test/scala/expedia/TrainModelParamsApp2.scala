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

object TrainModelParamsApp2 extends LazyLogging {

  def main(args: Array[String]): Unit = {

    val expediaTestFile = "c:/perforce/daniel/ex/data_booked/train_booked_2014_all_cols.csv"
    val testClicks = ExDataSource(dsName = "testDS", expediaTestFile).getAllClicks()

    val trainDS = ExDataSource(dsName = "trainDS", "c:/perforce/daniel/ex/data_all/train_all_2013.csv")

    val paramValues = 10d to 500 by 50
    logger.info("paramValues=" + paramValues)

    var bestMapk = 0d
    var bestParam = 0d
    paramValues.foreach { param =>

      val top5predictions = MarketDestPredictionModel(trainDS, testClicks, param).predictTop5(testClicks)

      val actual = DenseVector(testClicks.map(c => c.cluster.toDouble).toArray)
      val mapk = mean(averagePrecision(top5predictions(::, 5 to 9), actual, k = 5))

      if (mapk > bestMapk) {
        logger.info("Best!!!, oldMapk/newMapk=%.6f/%.6f, oldParam/newParam=%.2f/%.2f".format(bestMapk, mapk, bestParam, param))
        bestMapk = mapk
        bestParam = param
      } else logger.info("No best!!!, oldMapk/newMapk=%.6f/%.6f, oldParam/newParam=%.2f/%.2f".format(bestMapk, mapk, bestParam, param))
    }

  }
}