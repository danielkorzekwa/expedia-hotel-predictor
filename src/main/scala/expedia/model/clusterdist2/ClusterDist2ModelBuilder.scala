package expedia.model.clusterdist2

import expedia.data.Click
import expedia.data.ExDataSource
import expedia.stats.MulticlassHistByKey
import expedia.stats.calcVectorProbsMutable
import breeze.linalg.DenseVector
import scala.collection._
import scala.collection.mutable.ListBuffer
import expedia.model.clusterdist.calcClusterCoExistMatrix
import breeze.linalg._
import expedia.util.calcTopNClusters

case class ClusterDist2ModelBuilder(testClicks: Seq[Click]) {

  private val clusterHistByKey = MulticlassHistByKey[Tuple3[Double, Double, Double]](100)
 
  testClicks.foreach { click =>
    val key = (click.userLoc.toDouble, click.dist, click.marketId.toDouble)
    clusterHistByKey.add(key, click.cluster, value = 0)
  }

  def processCluster(click: Click) = {
    if (click.dist != -1) {
      val key = (click.userLoc.toDouble, click.dist, click.marketId.toDouble)
      clusterHistByKey.add(key, click.cluster)

    }
  }

  def create(): ClusterDist2Model = {

    val topClustersByKey: Map[Tuple3[Double, Double, Double], DenseVector[Int]] = clusterHistByKey.getMap.map { case (key, clusterProbs) => key -> calcTopNClusters(clusterProbs, 100, minProb = Some(0)) }

    val distClutersSeq = topClustersByKey.map {
      case (key, clusters) =>
        clusters
    }.toList
    
     val clusterCoExistMat = calcClusterCoExistMatrix(distClutersSeq)
     
    clusterHistByKey.getMap.foreach {
      case (key, clusterCounts) =>

        val clusterVec = topClustersByKey.get(key)

        clusterVec match {
          case Some(clusterVec) if(clusterVec.size>0)=> {
            val topCluster = clusterVec(0)
            val prior = clusterCoExistMat(topCluster, ::).t.copy
            val Z = sum(prior)
            prior :/= Z

            clusterCounts :+= prior.map(x => x.toFloat)
          }
          case _ => clusterCounts :+= DenseVector.fill(100)(0f)
        }

    }

    clusterHistByKey.normalise()
    ClusterDist2Model(clusterHistByKey)
  }
}

object ClusterDist2ModelBuilder {
  def buildFromTrainingSet(trainDS: ExDataSource, testClicks: Seq[Click]): ClusterDist2Model = {
    val clusterDistModelBuilder = ClusterDist2ModelBuilder(testClicks)

    def onClick(click: Click) = clusterDistModelBuilder.processCluster(click)
    trainDS.foreach { click => onClick(click) }

    val clusterDistModel = clusterDistModelBuilder.create()
    clusterDistModel
  }
}