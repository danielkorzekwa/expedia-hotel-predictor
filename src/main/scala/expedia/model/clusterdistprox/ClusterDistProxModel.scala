package expedia.model.clusterdistprox

import breeze.linalg.DenseVector
import expedia.data.Click
import expedia.data.ExDataSource
import expedia.stats.MulticlassHistByKey
import scala.collection._
import expedia.util.calcTopNClusters

//key - (userLoc,market),value - map[dist,clusterProbs]
case class ClusterDistProxModel(clusterHistByKey: mutable.Map[Tuple2[Int, Int], mutable.Map[Double, DenseVector[Float]]]) {

  private def nanClustProbs = DenseVector.fill(100)(Float.NaN)
  def predict(click:Click): DenseVector[Float] = {

    val key = (click.userLoc, click.dist, click.marketId)
   
    
    val clusterProbs = clusterHistByKey((click.userLoc, click.marketId))(click.dist)
    
//     if(click.dist==227.6473) {
//      println("..." + calcTopNClusters(clusterProbs,100))
//    }
   clusterProbs
  }
}

object ClusterDistProxModel {

  def apply(trainDatasource: ExDataSource,testClicks:Seq[Click]): ClusterDistProxModel = {

    val modelBuilder = ClusterDistProxModelBuilder(testClicks)

    def onClick(click: Click) = {
      modelBuilder.processCluster(click)
    }
    trainDatasource.foreach { click => onClick(click) }

    val model = modelBuilder.create()
    model
  }
}