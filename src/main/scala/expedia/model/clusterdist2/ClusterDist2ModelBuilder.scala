package expedia.model.clusterdist2

import expedia.data.Click
import expedia.data.ExDataSource
import expedia.stats.MulticlassHistByKey
import expedia.stats.calcVectorProbsMutable
import breeze.linalg.DenseVector

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
       clusterHistByKey.getMap.foreach { case (key, clusterCounts) => clusterCounts :+= DenseVector.fill(100)(1f/1000) }
  
    
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