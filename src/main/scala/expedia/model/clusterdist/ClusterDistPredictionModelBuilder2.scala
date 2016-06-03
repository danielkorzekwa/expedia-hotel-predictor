package expedia.model.clusterdist

import expedia.HyperParamsService
import expedia.data.ExDataSource
import expedia.model.ClusterModelBuilderFactory
import expedia.data.Click
import expedia.model.ClusterModelBuilder
import expedia.CompoundHyperParams
import expedia.CompoundHyperParamsMap
import expedia.stats.MulticlassHistByKey
import expedia.model.marketmodel.MarketModel
import expedia.util.calcTopNClusters
import breeze.linalg.DenseVector
import expedia.model.marketmodel.MarketModelBuilder2
import scala.collection._

case class ClusterDistPredictionModelBuilder2(marketModel: MarketModel, hyperParamsService: HyperParamsService) extends ClusterModelBuilder {

  def create(trainDatasource: ExDataSource, testClicks: Seq[Click], hyperParams: CompoundHyperParams): ClusterDistPredictionModel = {

    val clusterHistByKey = MulticlassHistByKey[Tuple3[Int, Int, Int]](100)
    testClicks.foreach { click =>
      val key = (click.userLoc, (click.dist * 10000).toInt, click.marketId)
      clusterHistByKey.add(key, click.cluster, value = 0)
    }

    val clusterHistByKey2 = MulticlassHistByKey[Tuple4[Int, Int, Int, Int]](100)
    testClicks.foreach { click =>
      val key = (click.userLoc, (click.dist * 10000).toInt, click.marketId, click.destId)
      clusterHistByKey2.add(key, click.cluster, value = 0)
    }

    /**
     * Process training set
     */
    def onClick(click: Click) = {

      if (click.dist != -1) {
        val key = (click.userLoc, (click.dist * 10000).toInt, click.marketId)
        if (clusterHistByKey.getMap.contains(key)) clusterHistByKey.add(key, click.cluster)

        val key2 = (click.userLoc, (click.dist * 10000).toInt, click.marketId, click.destId)
        if (clusterHistByKey2.getMap.contains(key2)) clusterHistByKey2.add(key2, click.cluster)
      }

    }
    trainDatasource.foreach { click => onClick(click) }

    /**
     * Build model
     */

    clusterHistByKey.getMap.foreach {
      case ((_, _, marketId), clusterCounts) =>
        val beta1 = hyperParamsService.getParamValueForMarketId("expedia.model.clusterdist.beta1", marketId, hyperParams).toFloat

        val prior = marketModel.predict(marketId.toInt).copy
        (0 until clusterCounts.size).foreach { i =>
          if (clusterCounts(i) == 0) prior(i) = 0f

        }

        clusterCounts :+= beta1 * prior
    }

    clusterHistByKey.normalise()
    clusterHistByKey2.getMap.foreach {
      case (key, clusterCounts) =>
        val beta2 = hyperParamsService.getParamValueForMarketId("expedia.model.clusterdist.beta2", key._3, hyperParams).toFloat
        clusterCounts :+= beta2 * clusterHistByKey.getMap((key._1, key._2, key._3))
    }

    val topClustersByKey: Map[Tuple4[Int, Int, Int, Int], DenseVector[Int]] =
      clusterHistByKey2.getMap.map { case (key, clusterProbs) => key -> calcTopNClusters(clusterProbs, 100, minProb = Some(0)) }

    ClusterDistPredictionModel(topClustersByKey)
  }
}

object ClusterDistPredictionModelBuilder2 extends ClusterModelBuilderFactory {

  def build(trainDatasource: ExDataSource, testClicks: Seq[Click], modelHyperParamsMap: CompoundHyperParamsMap): ClusterDistPredictionModelBuilder2 = {
    val hyperParamsService = HyperParamsService(testClicks)

    val marketModel = MarketModelBuilder2.build(trainDatasource, testClicks, modelHyperParamsMap)
      .create(trainDatasource, testClicks, modelHyperParamsMap.getModel("market"))

    ClusterDistPredictionModelBuilder2(marketModel, hyperParamsService)
  }
}