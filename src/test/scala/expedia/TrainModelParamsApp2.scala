package expedia

import com.typesafe.scalalogging.slf4j.LazyLogging

import breeze.linalg.DenseVector
import breeze.stats.mean
import breeze.stats.mean.reduce_Double
import dk.gp.util.averagePrecision
import expedia.data.Click
import expedia.data.ExDataSource
import expedia.data.ExKryoDataSource
import expedia.model.marketdest.MarketDestModelBuilder

object TrainModelParamsApp2 extends LazyLogging {

  def main(args: Array[String]): Unit = {
    logger.info("Learning hyper params...")
    val now = System.currentTimeMillis()

    val destIds = Set(23249)
    def filterTrain(click: Click) = {
      destIds.contains(click.destId)
    }

    val expediaTrainFileKryo = "c:/perforce/daniel/ex/segments/continent_2/train_2013_continent2.kryo"
    val trainDS = ExKryoDataSource(dsName = "trainDS", expediaTrainFileKryo, filterTrain)

    val expediaTestFileKryo = "c:/perforce/daniel/ex/segments/continent_2/train_2014_continent2_booked_only.kryo"
    val testClicks = ExKryoDataSource(dsName = "testDS", expediaTestFileKryo).getAllClicks().filter(click =>   destIds.contains(click.destId))

    learn(trainDS, testClicks)

    logger.info("Learning hyper params...done:" + (System.currentTimeMillis() - now)/1000 + " sec.")
  }

  private def learn(trainDS: ExDataSource, testClicks: Seq[Click]) = {

    val initialHyperParams = HyperParams.createBest()

    val initialMapk = computeMapk(initialHyperParams, trainDS, testClicks)
    var bestMapk = initialMapk
    var bestHyperParams = initialHyperParams

    val params = initialHyperParams.getParams()
    params.foreach { param =>

      val paramValues = initialHyperParams.getParamValues(param)
      paramValues.foreach { paramValue =>
        val currHyperParams = initialHyperParams.copy(param, paramValue)

        val currMapk = computeMapk(currHyperParams, trainDS, testClicks)

        if (currMapk > bestMapk) {
          logger.info("Best!!!, curr=%.4f ,best=%.4f, initial=%.4f".format(currMapk, bestMapk, initialMapk))
          bestMapk = currMapk
          bestHyperParams = currHyperParams
        } else logger.info("curr=%.4f ,best=%.4f, initial=%.4f".format(currMapk, bestMapk, initialMapk))
        logger.info("Current hyperParams:" + currHyperParams)
        logger.info("Best hyperParams:" + bestHyperParams)

      }
    }

  }

  private def computeMapk(hyperParams: HyperParams, trainDS: ExDataSource, testClicks: Seq[Click]): Double = {
    println("s:" + testClicks.size)
    val top5predictions = MarketDestModelBuilder.buildFromTrainingSet(trainDS, testClicks, hyperParams).predictTop5(testClicks)
    println("s1")
    val actual = DenseVector(testClicks.map(c => c.cluster.toDouble).toArray)
    val mapk = mean(averagePrecision(top5predictions(::, 5 to 9), actual, k = 5))
    mapk

  }
}