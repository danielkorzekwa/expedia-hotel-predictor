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

object learnModelParams extends LazyLogging {

  def apply(trainDS: ExDataSource, testClicks: Seq[Click], initialHyperParamsMap: Map[String, CompoundHyperParams]) = {

    val modelHyperParams: CompoundHyperParams = initialHyperParamsMap("cmu")

    val bestModelHyperParamsList = modelHyperParams.prioritizedHyperParams.map { params =>

      val segmentTestClicks = testClicks.filter { click => params.containsClick(click.continentId, click.countryId) }

      if (params.continentIdMatcher.getOrElse(0).equals(3)) trainHyperParams(params, initialHyperParamsMap,trainDS, segmentTestClicks)
      else params
    }
    //  saveObject(bestHyperParamsList, "target/hyperParams_trained.kryo")
    //  bestHyperParams.copy(prioritizedHyperParams = bestHyperParamsList)

  }

  private def trainHyperParams(initialHyperParams: SimpleHyperParams, modelHyperParamsMap: Map[String, CompoundHyperParams],trainDS: ExDataSource, testClicks: Seq[Click]): SimpleHyperParams = {
    println("Train hyper params...")
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
          val currHyperParams = bestHyperParams.copy(param, currParamValue)

          val currMapk = computeMapk(currHyperParams, trainDS, testClicks, modelBuilder)

          if (currMapk > bestMapk) {
            logger.info("Best!!!, param=%s, curr=%.8f ,best=%.8f, initial=%.8f".format(param, currMapk, bestMapk, initialMapk))
            bestMapk = currMapk
            bestHyperParams = currHyperParams
          }
          else  logger.info(" param=%s, curr=%.8f ,best=%.8f, initial=%.8f".format(param, currMapk, bestMapk, initialMapk))

        }
    }

    bestHyperParams
  }

  private def computeMapk(hyperParams: SimpleHyperParams, trainDS: ExDataSource, testClicks: Seq[Click], cmuModelBuilder: CmuModelBuilder2): Double = {
    logger.info("ComputeMPK")

    val segmentCompoundHyperParams = CompoundHyperParams(testClicks, List(hyperParams))

    val top5predictions = cmuModelBuilder.create(trainDS, testClicks, segmentCompoundHyperParams).predictTop5(testClicks)
    val actual = DenseVector(testClicks.map(c => c.cluster.toDouble).toArray)
    val mapk = mean(averagePrecision(top5predictions(::, 5 to 9), actual, k = 5))
    mapk
  }
}