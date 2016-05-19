package expedia.model.clusterdist

import scala.collection._
import breeze.linalg.DenseVector
import breeze.linalg._
import scala.collection.mutable.ListBuffer
import expedia.data.Click
import expedia.stats.MulticlassHistByKey
import expedia.stats.MulticlassHistByKey
import expedia.util.calcTopNClusters
import expedia.data.ExDataSource
import expedia.model.marketmodel.MarketModel
import expedia.model.marketmodel.MarketModelBuilder
import expedia.model.country.CountryModelBuilder
case class ClusterDistPredictionModelBuilder(testClicks: Seq[Click]) {

  private val clusterMap: mutable.Map[Tuple3[Int, Int, Int], ListBuffer[Double]] = mutable.Map()

  private val clusterHistByKey = MulticlassHistByKey[Tuple3[Int, Int, Int]](100)
  testClicks.foreach { click =>
    val key = (click.userLoc, (click.dist * 10000).toInt, click.marketId)
    clusterHistByKey.add(key, click.cluster, value = 0)
  }
  
  def processCluster(click: Click) = {

    if (click.dist != -1) {
      val key = (click.userLoc, (click.dist * 10000).toInt, click.marketId)
      clusterMap.getOrElseUpdate(key, ListBuffer()) += click.cluster

      // clusterHistByKey.add(key, click.cluster)
   val key2 = (click.userLoc, (click.dist * 10000).toInt, click.marketId)

      if (clusterHistByKey.getMap.contains(key2)) clusterHistByKey.add(key2, click.cluster)
    }

  }

  def create(marketModel:MarketModel): ClusterDistPredictionModel = {
    clusterHistByKey.getMap.foreach {
      case ((_, _, marketId), clusterCounts) =>
        val prior = marketModel.predict(marketId.toInt).copy
        (0 until clusterCounts.size).foreach { i =>
          if (clusterCounts(i) == 0) prior(i) = 0f

        }

        clusterCounts :+= 1f * prior
    }

    val topClustersByKey: Map[Tuple3[Int, Int, Int], DenseVector[Int]] =
      clusterHistByKey.getMap.map { case (key, clusterProbs) => key -> calcTopNClusters(clusterProbs, 100, minProb = Some(0)) }
  
    
    val topClustersByKey2 = clusterMap.map {
      case (key, clusters) =>

        val sortedClusters = clusters.groupBy { c => c }.map { case (key, keyClusters) => key -> keyClusters.size }.toList.sortWith((a, b) => a._2 > b._2).map(_._1.toInt)
        (key, DenseVector(sortedClusters.toArray))
    }

    ClusterDistPredictionModel(topClustersByKey)
  }
}

object ClusterDistPredictionModelBuilder {
  def buildFromTrainingSet(trainDS: ExDataSource, testClicks: Seq[Click]): ClusterDistPredictionModel = {
    val countryModelBuilder = CountryModelBuilder(testClicks)
    val marketModelBuilder = MarketModelBuilder(testClicks)
    val clusterDistModelBuilder = ClusterDistPredictionModelBuilder(testClicks)

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