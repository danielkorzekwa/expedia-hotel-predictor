package expedia.learn

import com.typesafe.scalalogging.slf4j.LazyLogging
import dk.gp.util.saveObject
import expedia.CompoundHyperParams
import expedia.CompoundHyperParamsMap
import expedia.SimpleHyperParams
import expedia.data.Click
import expedia.data.ExDataSource
import expedia.model.ClusterModelBuilder
import expedia.model.ClusterModelBuilder
import expedia.model.ClusterModelBuilder
import expedia.model.ClusterModelBuilderFactory
import expedia.model.cmu.CmuModelBuilder2
import expedia.model.country.CountryModelBuilder2
import expedia.model.countryuser.CountryUserModelBuilder2
import expedia.model.dest.DestModelBuilder2
import expedia.model.dest.DestModelBuilder2
import expedia.model.destcluster.DestClusterModelBuilder2
import expedia.model.marketdest.MarketDestModelBuilder2
import expedia.model.marketdestcluster.MarketDestClusterModelBuilder2
import expedia.model.marketdestuser.MarketDestUserModelBuilder
import expedia.model.marketdestuser2.MarketDestUserModelBuilder2
import expedia.model.marketmodel.MarketModelBuilder2
import expedia.model.marketuser.MarketUserModelBuilder2
import expedia.model.mdp.MdpModelBuilder2
import expedia.model.mdpu.MdpuModelBuilder2
import expedia.model.mdpu.MdpuModelBuilder2
import java.util.concurrent.atomic.AtomicInteger

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
    "marketdestuser2" -> MarketDestUserModelBuilder2,
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
      saveObject(newHyperParamsMap, hyperParamsMapFile)
      newHyperParamsMap
    }

    newHyperParamsMap

  }

  private def learnModelHyperParams(modelHyperParams: CompoundHyperParams, hyperParamsMap: CompoundHyperParamsMap,
                                    trainDS: ExDataSource, testClicks: Seq[Click], modelBuilderFactory: ClusterModelBuilderFactory): CompoundHyperParams = {

    logger.info("Markets to learn:" + modelHyperParams.hyperParamsByMarket.size)
    val i = new AtomicInteger(0)
    val newHyperParamsByMarket = modelHyperParams.hyperParamsByMarket.map {
      case (marketId, params) =>

        logger.info("Learning market %d/%d".format(i.getAndIncrement, modelHyperParams.hyperParamsByMarket.size))
        val segmentTestClicks = testClicks.filter { click => click.marketId == marketId }

        if(segmentTestClicks.size>0) {
        val modelBuilder = modelBuilderFactory.build(trainDS, segmentTestClicks, hyperParamsMap)
        val newParams = trainSimpleModelParamsForMarket(modelBuilder, params, trainDS, segmentTestClicks, modelHyperParams, marketId)
        marketId -> newParams
        }
        else marketId -> params
    }

    logger.info("Countries to learn to learn:" + modelHyperParams.hyperParamsByCountry.size)
    val i2 = new AtomicInteger(0)

    val newHyperParamsByCountry = modelHyperParams.hyperParamsByCountry.map {
      case (countryId, params) =>
        logger.info("Learning country %d/%d".format(i2.getAndIncrement, modelHyperParams.hyperParamsByCountry.size))
        val segmentTestClicks = testClicks.filter { click => click.countryId == countryId }
        if (segmentTestClicks.size > 0) {
          val modelBuilder = modelBuilderFactory.build(trainDS, segmentTestClicks, hyperParamsMap)
          val newParams = trainSimpleModelParamsForCountry(modelBuilder, params, trainDS, segmentTestClicks, modelHyperParams, countryId)
          countryId -> newParams
        } else countryId -> params
    }

    val newModelHyperParams = modelHyperParams.copy(hyperParamsByMarket = newHyperParamsByMarket, hyperParamsByCountry = newHyperParamsByCountry)
    newModelHyperParams
  }

}