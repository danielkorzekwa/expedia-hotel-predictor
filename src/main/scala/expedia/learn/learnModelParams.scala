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
import expedia.model.ClusterModelBuilder
import expedia.model.ClusterModelBuilder
import expedia.model.ClusterModelBuilderFactory
import expedia.model.ClusterModelBuilder
import expedia.model.marketmodel.MarketModelBuilder2
import expedia.CompoundHyperParamsMap

object learnModelParams extends LazyLogging {

  private val modelBuilderFactoryMap: Map[String, ClusterModelBuilderFactory] = Map(
    "cmu" -> CmuModelBuilder2,
    "market" -> MarketModelBuilder2)

  def apply(trainDS: ExDataSource, testClicks: Seq[Click], initialHyperParamsMap: CompoundHyperParamsMap,
            modelsToLearn: Seq[String]): CompoundHyperParamsMap = {

    val newHyperParamsMap = modelsToLearn.foldLeft(initialHyperParamsMap) { (bestHyperParamsMap, model) =>
      logger.info("Learning model=%s".format(model))

      val modelBuilderFactory = modelBuilderFactoryMap(model)
      val modelHyperParams: CompoundHyperParams = bestHyperParamsMap.getModel(model)

      val newModelHyperParams = learnModelHyperParams(modelHyperParams, bestHyperParamsMap, trainDS, testClicks, modelBuilderFactory)

      val newHyperParamsMap = bestHyperParamsMap.addModel(model,newModelHyperParams)
      newHyperParamsMap
    }

    newHyperParamsMap

  }

  private def learnModelHyperParams(modelHyperParams: CompoundHyperParams, hyperParamsMap: CompoundHyperParamsMap,
                                    trainDS: ExDataSource, testClicks: Seq[Click], modelBuilderFactory: ClusterModelBuilderFactory): CompoundHyperParams = {

    val newModelHyperParamsList = modelHyperParams.prioritizedHyperParams.map { params =>

      if (params.continentIdMatcher.getOrElse(0).equals(3)) {
        val segmentTestClicks = testClicks.filter { click => params.containsClick(click.continentId, click.countryId) }
        val modelBuilder = modelBuilderFactory.build(trainDS, testClicks, hyperParamsMap)
        trainSimpleModelParams(modelBuilder, params, trainDS, segmentTestClicks)
      } else params
    }

    val newModelHyperParams = modelHyperParams.copy(prioritizedHyperParams = newModelHyperParamsList)
    newModelHyperParams
  }

 
}