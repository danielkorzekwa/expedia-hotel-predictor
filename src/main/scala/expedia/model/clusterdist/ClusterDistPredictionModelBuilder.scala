package expedia.model.clusterdist

import scala.collection.Map
import scala.collection.Seq

import breeze.linalg.DenseVector
import breeze.linalg.InjectNumericOps
import expedia.HyperParams
import expedia.data.Click
import expedia.data.ExDataSource
import expedia.model.country.CountryModelBuilder
import expedia.model.marketmodel.MarketModel
import expedia.model.marketmodel.MarketModelBuilder
import expedia.stats.MulticlassHistByKey
import expedia.util.TimeDecayService
import expedia.util.calcTopNClusters
case class ClusterDistPredictionModelBuilder(testClicks: Seq[Click], hyperParams: HyperParams) {

  private val clusterHistByKey = MulticlassHistByKey[Tuple3[Int, Int, Int]](100)
  testClicks.foreach { click =>
    val key = (click.userLoc, (click.dist * 10000).toInt, click.marketId)
    clusterHistByKey.add(key, click.cluster, value = 0)
  }

  private val clusterHistByKey2 = MulticlassHistByKey[Tuple4[Int, Int, Int, Int]](100)
  testClicks.foreach { click =>
    val key = (click.userLoc, (click.dist * 10000).toInt, click.marketId, click.destId)
    clusterHistByKey2.add(key, click.cluster, value = 0)
  }

  private val beta1 = hyperParams.getParamValue("expedia.model.clusterdist.beta1").toFloat
  private val beta2 = hyperParams.getParamValue("expedia.model.clusterdist.beta2").toFloat

  def processCluster(click: Click) = {

    
    
    if (click.dist != -1) {
      val key = (click.userLoc, (click.dist * 10000).toInt, click.marketId)
      if (clusterHistByKey.getMap.contains(key)) clusterHistByKey.add(key, click.cluster)

      val key2 = (click.userLoc, (click.dist * 10000).toInt, click.marketId, click.destId)
      if (clusterHistByKey2.getMap.contains(key2)) clusterHistByKey2.add(key2, click.cluster)
    }

  }

  def create(marketModel: MarketModel): ClusterDistPredictionModel = {
    clusterHistByKey.getMap.foreach {
      case ((_, _, marketId), clusterCounts) =>
        val prior = marketModel.predict(marketId.toInt).copy
        (0 until clusterCounts.size).foreach { i =>
          if (clusterCounts(i) == 0) prior(i) = 0f

        }

        clusterCounts :+= beta1 * prior
    }

    clusterHistByKey.normalise()
    clusterHistByKey2.getMap.foreach { case (key, clusterCounts) => clusterCounts :+= beta2 * clusterHistByKey.getMap((key._1, key._2, key._3)) }

    val topClustersByKey: Map[Tuple4[Int, Int, Int, Int], DenseVector[Int]] =
      clusterHistByKey2.getMap.map { case (key, clusterProbs) => key -> calcTopNClusters(clusterProbs, 100, minProb = Some(0)) }

    ClusterDistPredictionModel(topClustersByKey)
  }
}

object ClusterDistPredictionModelBuilder {
  def buildFromTrainingSet(trainDS: ExDataSource, testClicks: Seq[Click], hyperParams: HyperParams): ClusterDistPredictionModel = {
  
    val timeDecayService = TimeDecayService(testClicks,hyperParams)
    
    val countryModelBuilder = CountryModelBuilder(testClicks, hyperParams,timeDecayService)
    val marketModelBuilder = MarketModelBuilder(testClicks, hyperParams,timeDecayService)
    val clusterDistModelBuilder = ClusterDistPredictionModelBuilder(testClicks, hyperParams)

    def onClick(click: Click) = {
      countryModelBuilder.processCluster(click)
      marketModelBuilder.processCluster(click)
      clusterDistModelBuilder.processCluster(click)
    }

    trainDS.foreach { click => onClick(click) }
    val countryModel = countryModelBuilder.create()
    val marketModel = marketModelBuilder.create(countryModel)
    val clusterDistModel = clusterDistModelBuilder.create(marketModel)

    clusterDistModel
  }
}