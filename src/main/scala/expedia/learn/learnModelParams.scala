package expedia.learn

import expedia.data.Click
import expedia.data.ExDataSource
import expedia.CompoundHyperParams
import expedia.SimpleHyperParams
import dk.gp.util.averagePrecision
import expedia.model.cmu.CmuModelBuilder2
import breeze.linalg.DenseVector
import scala.util.Random
import com.typesafe.scalalogging.slf4j.LazyLogging
import breeze.stats._
import dk.gp.util.saveObject

object learnModelParams extends LazyLogging {

  def apply(trainDS: ExDataSource, testClicks: Seq[Click], initialHyperParamsMap: Map[String, CompoundHyperParams]) = {

    (0 until 100).foldLeft(initialHyperParamsMap) { (bestHyperParams, i) =>
      logger.info("Learning iter=%d".format(i))
      val modelHyperParams: CompoundHyperParams = bestHyperParams("cmu")

      val bestModelHyperParamsList = modelHyperParams.prioritizedHyperParams.map { params =>
        val segmentTestClicks = testClicks.filter { click => params.containsClick(click.continentId, click.countryId) }
        if (params.continentIdMatcher.getOrElse(0).equals(3)) trainHyperParams(params, initialHyperParamsMap, trainDS, segmentTestClicks)
        else params
      }

      val newModelHyperParams = modelHyperParams.copy(prioritizedHyperParams=bestModelHyperParamsList)
      val newHyperParamsMap = bestHyperParams + ("cmu" -> newModelHyperParams)

      saveObject(newHyperParamsMap, "target/hyperParamsMap_trained.kryo")
      newHyperParamsMap
    }

  }

  private def trainHyperParams(initialHyperParams: SimpleHyperParams, modelHyperParamsMap: Map[String, CompoundHyperParams], trainDS: ExDataSource, testClicks: Seq[Click]): SimpleHyperParams = {
    val modelBuilder = CmuModelBuilder2(trainDS, testClicks, modelHyperParamsMap)

    val initialMapk = computeMapk(initialHyperParams, trainDS, testClicks, modelBuilder)
    var bestMapk = initialMapk
    var bestHyperParams = initialHyperParams

    val params = initialHyperParams.getParams() //.filter(p => p.startsWith("expedia.model.marketuser.beta3") || p.startsWith("expedia.model.cmu."))

    Random.shuffle(params).zipWithIndex.foreach {
      case (param, paramIndex) =>

        val bestParamValue = bestHyperParams.getParamValue(param)
        (-3 to 3).filter(x => x != 0).foreach { i =>
          val currParamValue = bestParamValue + i * bestParamValue * 0.05
          //  logger.info("Learning param=%s %d/%d, bestValue/currValue=%.4f/%.4f".format(param, paramIndex, params.size, bestParamValue, currParamValue))
          val currHyperParams = bestHyperParams.withParamValue(param, currParamValue)

          val currMapk = computeMapk(currHyperParams, trainDS, testClicks, modelBuilder)

          if (currMapk > bestMapk) {
            logger.info("Best!!!, param=%s %d/%d, curr=%.8f ,best=%.8f, initial=%.8f".format(param,paramIndex+1, params.size, currMapk, bestMapk, initialMapk))

            bestMapk = currMapk
            bestHyperParams = currHyperParams
            println(bestHyperParams)
          } else logger.info(" param=%s %d/%d, curr=%.8f ,best=%.8f, initial=%.8f".format(param, paramIndex, params.size,currMapk, bestMapk, initialMapk))

        }
    }

    bestHyperParams
  }

  private def computeMapk(hyperParams: SimpleHyperParams, trainDS: ExDataSource, testClicks: Seq[Click], cmuModelBuilder: CmuModelBuilder2): Double = {
    logger.info("ComputeMPK")

    val segmentCompoundHyperParams = CompoundHyperParams(List(hyperParams))

    val top5predictions = cmuModelBuilder.create(trainDS, testClicks, segmentCompoundHyperParams).predictTop5(testClicks)
    val actual = DenseVector(testClicks.map(c => c.cluster.toDouble).toArray)
    val mapk = mean(averagePrecision(top5predictions(::, 5 to 9), actual, k = 5))
    mapk
  }
}