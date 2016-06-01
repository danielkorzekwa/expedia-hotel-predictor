package expedia

import com.typesafe.scalalogging.slf4j.LazyLogging
import breeze.linalg.DenseVector
import breeze.stats.mean
import dk.gp.util.averagePrecision
import expedia.data.Click
import expedia.data.ExDataSource
import expedia.data.ExKryoDataSource
import expedia.model.marketdest.MarketDestModelBuilder
import expedia.model.mdpu.MdpuModelBuilder
import scala.util.Random
import expedia.model.cmu.CmuModelBuilder

object TrainModelParamsApp3 extends LazyLogging {

  def main(args: Array[String]): Unit = {
    logger.info("Learning hyper params...")

    val now = System.currentTimeMillis()

    val destIds = Set(12208)
    def filterTrain(click: Click) = {
      true //    destIds.contains(click.destId)
    }

    val expediaTrainFileKryo = "c:/perforce/daniel/ex/segments/continent_3/train_2013_continent3.kryo"
    val trainDS = ExKryoDataSource(dsName = "trainDS", expediaTrainFileKryo, filterTrain)

    val expediaTestFileKryo = "c:/perforce/daniel/ex/segments/continent_3/train_2014_continent3_booked_only.kryo"
    val testClicks = ExKryoDataSource(dsName = "testDS", expediaTestFileKryo).getAllClicks() //.filter(click => destIds.contains(click.destId))

    learn(trainDS, testClicks)

    logger.info("Learning hyper params...done:" + (System.currentTimeMillis() - now) / 1000 + " sec.")
  }

  private def learn(trainDS: ExDataSource, testClicks: Seq[Click]) = {

    val initialHyperParams = HyperParams.createParamsCont3()

    val initialMapk = computeMapk(initialHyperParams, trainDS, testClicks)
    var bestMapk = -1d //initialMapk
    var bestHyperParams = initialHyperParams

    val params = initialHyperParams.getParams()//.filter(p => p.startsWith("expedia.model.marketuser.beta3") || p.startsWith("expedia.model.cmu."))
    logger.info("Number of hyper params:" + params.size)

    for (i <- 1 to 100) {
      Random.shuffle(params).zipWithIndex.foreach {
        case (param, paramIndex) =>

          val bestParamValue = bestHyperParams.getParamValue(param)

          (-3 to 3).filter(x => x != 0).foreach { i =>

            val currParamValue = bestParamValue + i * bestParamValue * 0.05
            logger.info("Learning param=%s %d/%d, bestValue/currValue=%.4f/%.4f".format(param, paramIndex, params.size, bestParamValue, currParamValue))
            val currHyperParams = bestHyperParams.copy(param, currParamValue)

            val currMapk = computeMapk(currHyperParams, trainDS, testClicks)

            if (currMapk > bestMapk) {
              logger.info("Best!!!, curr=%.8f ,best=%.8f, initial=%.8f".format(currMapk, bestMapk, initialMapk))
              bestMapk = currMapk
              bestHyperParams = currHyperParams
            } else logger.info("curr=%.8f ,best=%.8f, initial=%.8f".format(currMapk, bestMapk, initialMapk))
            logger.info("Current hyperParams:" + currHyperParams)
            logger.info("Best hyperParams:" + bestHyperParams)

          }

      }
    }
  }

  private def computeMapk(hyperParams: HyperParams, trainDS: ExDataSource, testClicks: Seq[Click]): Double = {
    val top5predictions = CmuModelBuilder.buildFromTrainingSet(trainDS, testClicks, hyperParams).predictTop5(testClicks)
    val actual = DenseVector(testClicks.map(c => c.cluster.toDouble).toArray)
    val mapk = mean(averagePrecision(top5predictions(::, 5 to 9), actual, k = 5))
    mapk

  }
}