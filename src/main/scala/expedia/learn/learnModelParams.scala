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
import expedia.model.country.CountryModelBuilder2
import expedia.model.countryuser.CountryUserModelBuilder2
import expedia.model.marketuser.MarketUserModelBuilder2
import expedia.model.dest.DestModelBuilder2
import expedia.model.destcluster.DestClusterModel
import expedia.model.destcluster.DestClusterModelBuilder2
import expedia.model.marketdestcluster.MarketDestClusterModelBuilder2
import expedia.model.marketdest.MarketDestModelBuilder2
import expedia.model.mdp.MdpModelBuilder2
import expedia.model.marketdestuser.MarketDestUserModelBuilder
import expedia.model.mdpu.MdpuModel
import expedia.model.mdpu.MdpuModelBuilder2
import expedia.model.dest.DestModelBuilder2
import expedia.model.mdpu.MdpuModelBuilder2

object learnModelParams extends LazyLogging {

  private val modelBuilderFactoryMap: Map[String, ClusterModelBuilderFactory] = Map(
    "country" -> CountryModelBuilder2,
    "countryuser" -> CountryUserModelBuilder2,
    "marketuser" -> MarketUserModelBuilder2,
    "market" -> MarketModelBuilder2,
    "dest" -> DestModelBuilder2,
    "destcluster" -> DestClusterModelBuilder2,
    "marketdestcluster" -> MarketDestClusterModelBuilder2,
    "marketdest" -> MarketDestModelBuilder2,
    "mdp" -> MdpModelBuilder2,
    "marketdestuser" -> MarketDestUserModelBuilder,
    "mdpu" -> MdpuModelBuilder2,
    "cmu" -> CmuModelBuilder2)

  def apply(trainDS: ExDataSource, testClicks: Seq[Click], initialHyperParamsMap: CompoundHyperParamsMap,
            modelsToLearn: Seq[String], hyperParamsMapFile: String): CompoundHyperParamsMap = {

    val newHyperParamsMap = modelsToLearn.foldLeft(initialHyperParamsMap) { (bestHyperParamsMap, model) =>
      logger.info("Learning model=%s".format(model))

      val modelBuilderFactory = modelBuilderFactoryMap(model)
      val modelHyperParams: CompoundHyperParams = bestHyperParamsMap.getModel(model)

      val newModelHyperParams = learnModelHyperParams(modelHyperParams, bestHyperParamsMap, trainDS, testClicks, modelBuilderFactory)

      val newHyperParamsMap = bestHyperParamsMap.addModel(model, newModelHyperParams)
    //  saveObject(newHyperParamsMap, hyperParamsMapFile)
      newHyperParamsMap
    }

    newHyperParamsMap

  }

  private def learnModelHyperParams(modelHyperParams: CompoundHyperParams, hyperParamsMap: CompoundHyperParamsMap,
                                    trainDS: ExDataSource, testClicks: Seq[Click], modelBuilderFactory: ClusterModelBuilderFactory): CompoundHyperParams = {

    val newModelHyperParamsList = modelHyperParams.prioritizedHyperParams.map { params =>

      if (params.continentIdMatcher.getOrElse(0).equals(3)) {

        //      if (params.countryIdMatcher.getOrElse(0).equals(50)) {
        val segmentTestClicks = testClicks.filter { click => params.containsClick(click.continentId, click.countryId) }

        val modelBuilder = modelBuilderFactory.build(trainDS, segmentTestClicks, hyperParamsMap)
        trainSimpleModelParams(modelBuilder, params, trainDS, segmentTestClicks)
      } else params
    }

    val newModelHyperParams = modelHyperParams.copy(prioritizedHyperParams = newModelHyperParamsList)
    newModelHyperParams
  }

}