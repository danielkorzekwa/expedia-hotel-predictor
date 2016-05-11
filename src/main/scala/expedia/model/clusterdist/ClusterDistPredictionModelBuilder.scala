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
case class ClusterDistPredictionModelBuilder() {

  private val clusterMap: mutable.Map[Tuple3[Double, Double, Double], ListBuffer[Double]] = mutable.Map()

  // private val clusterHistByKey = MulticlassHistByKey[Tuple3[Double, Double, Double]](100)
  def processCluster(click: Click) = {

    if (click.dist != -1) {
      val key = (click.userLoc.toDouble, click.dist, click.marketId.toDouble)
      clusterMap.getOrElseUpdate(key, ListBuffer()) += click.cluster

      // clusterHistByKey.add(key, click.cluster)

    }

  }

  def create(): ClusterDistPredictionModel = {
    val map = clusterMap.map { case (key, clusters) => key -> clusters.toList }

    //   val topClustersByKey:Map[Tuple3[Double, Double, Double], DenseVector[Int]] =  clusterHistByKey.getMap().map{case (key,clusterProbs) => key ->  calcTopNClusters(clusterProbs,100,minProb=0)}

    val topClustersByKey = clusterMap.map {
      case (key, clusters) =>

        val sortedClusters = clusters.groupBy { c => c }.map { case (key, keyClusters) => key -> keyClusters.size }.toList.sortWith((a, b) => a._2 > b._2).map(_._1.toInt)
        (key, DenseVector(sortedClusters.toArray))
    }

    ClusterDistPredictionModel(topClustersByKey)
  }
}

object ClusterDistPredictionModelBuilder {
  def buildFromTrainingSet(trainDS: ExDataSource, testClicks: Seq[Click]): ClusterDistPredictionModel = {
    val clusterDistModelBuilder = ClusterDistPredictionModelBuilder()

    def onClick(click: Click) = {
      clusterDistModelBuilder.processCluster(click)
    }

    trainDS.foreach { click => onClick(click) }

    val clusterDistModel = clusterDistModelBuilder.create()

    clusterDistModel
  }
}