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
import dk.gp.util.loadObject
import dk.gp.util.saveObject

object TrainModelParamsApp4 extends LazyLogging {

  def main(args: Array[String]): Unit = {
    logger.info("Learning hyper params...")

    val now = System.currentTimeMillis()

    val expediaTrainFileKryo = "c:/perforce/daniel/ex/segments/all/train_2013.kryo"
    val trainDS = ExKryoDataSource(dsName = "trainDS", expediaTrainFileKryo)

    val expediaTestFileKryo = "c:/perforce/daniel/ex/segments/all/train_2014_booked_only.kryo"
    val testClicks = ExKryoDataSource(dsName = "testDS", expediaTestFileKryo).getAllClicks()

    val hyperParamsListFromDisk = loadObject[List[SimpleHyperParams]]("c:/perforce/daniel/ex/hyperparams/hyperParams_best_020616_test14.kryo")
    val hyperParams = CompoundHyperParams(testClicks, hyperParamsListFromDisk)

    learn(trainDS, testClicks, hyperParams)

    logger.info("Learning hyper params...done:" + (System.currentTimeMillis() - now) / 1000 + " sec.")
  }

  private def learn(trainDS: ExDataSource, testClicks: Seq[Click], initialHyperParams: CompoundHyperParams) = {

    var bestHyperParams = initialHyperParams

    (0 to 100).foldLeft(initialHyperParams) { (bestHyperParams, i) =>
      logger.info("Training iteration=%d".format(i))
      val bestHyperParamsList = bestHyperParams.prioritizedHyperParams.map { params =>
        if (params.continentIdMatcher.getOrElse(0).equals(3)) trainHyperParams(params, trainDS, testClicks)
        else params
      }
      saveObject(bestHyperParamsList, "target/hyperParams_trained.kryo")
      bestHyperParams.copy(prioritizedHyperParams = bestHyperParamsList)
    }

  }

  private def trainHyperParams(initialHyperParams: SimpleHyperParams, trainDS: ExDataSource, testClicks: Seq[Click]): SimpleHyperParams = {

    val initialMapk = computeMapk(initialHyperParams, trainDS, testClicks)
    var bestMapk = initialMapk
    var bestHyperParams = initialHyperParams

    val params = initialHyperParams.getParams() //.filter(p => p.startsWith("expedia.model.marketuser.beta3") || p.startsWith("expedia.model.cmu."))

    Random.shuffle(params).zipWithIndex.foreach {
      case (param, paramIndex) =>

        val bestParamValue = bestHyperParams.getParamValue(param)
        (-3 to 3).filter(x => x != 0).foreach { i =>
          val currParamValue = bestParamValue + i * bestParamValue * 0.05
          //  logger.info("Learning param=%s %d/%d, bestValue/currValue=%.4f/%.4f".format(param, paramIndex, params.size, bestParamValue, currParamValue))
          val currHyperParams = bestHyperParams.copy(param, currParamValue)

          val currMapk = computeMapk(currHyperParams, trainDS, testClicks)

          if (currMapk > bestMapk) {
            logger.info("Best!!!, param=%s, curr=%.8f ,best=%.8f, initial=%.8f".format(param, currMapk, bestMapk, initialMapk))
            bestMapk = currMapk
            bestHyperParams = currHyperParams
          } //else logger.info("curr=%.8f ,best=%.8f, initial=%.8f".format(currMapk, bestMapk, initialMapk))
          //   logger.info("Current hyperParams:" + currHyperParams)
          //   logger.info("Best hyperParams:" + bestHyperParams)

        }
    }

    bestHyperParams
  }

  private def computeMapk(hyperParams: SimpleHyperParams, trainDS: ExDataSource, testClicks: Seq[Click]): Double = {
    logger.info("ComputeMPK")
    val segmentTestClicks = testClicks.filter { click => hyperParams.containsClick(click.continentId, click.countryId) }
    val segmentCompoundHyperParams = CompoundHyperParams(segmentTestClicks, List(hyperParams))

    val top5predictions = CmuModelBuilder.buildFromTrainingSet(trainDS, segmentTestClicks, segmentCompoundHyperParams).predictTop5(segmentTestClicks)
    val actual = DenseVector(segmentTestClicks.map(c => c.cluster.toDouble).toArray)
    val mapk = mean(averagePrecision(top5predictions(::, 5 to 9), actual, k = 5))
    mapk
  }

}