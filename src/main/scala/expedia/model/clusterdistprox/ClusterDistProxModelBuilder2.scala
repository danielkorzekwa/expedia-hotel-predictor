package expedia.model.clusterdistprox

import expedia.data.Click
import expedia.model.clusterdist.ClusterDistPredictionModelBuilder2
import expedia.model.ClusterModelBuilderFactory
import expedia.data.ExDataSource
import expedia.CompoundHyperParamsMap
import expedia.model.ClusterModelBuilder
import expedia.CompoundHyperParams
import scala.collection._
import breeze.linalg.sum
import breeze.linalg.DenseVector
import breeze.numerics._

case class ClusterDistProxModelBuilder2() extends ClusterModelBuilder {

  def create(trainDatasource: ExDataSource, testClicks: Seq[Click], hyperParams: CompoundHyperParams): ClusterDistProxModel = {
  
    
  //key - (userLoc,market),value - map[dist,clusterProbs]
  val clusterHistByKey: mutable.Map[Tuple2[Int, Int], mutable.Map[Double, DenseVector[Float]]] = mutable.Map()

  testClicks.foreach { click =>

    val distClusterProbs = clusterHistByKey.getOrElseUpdate((click.userLoc, click.marketId), mutable.Map())
    distClusterProbs.getOrElseUpdate(click.dist, DenseVector.fill(100)(1f))
  }
    
    
    /**
     * Process training set
     */
    def onClick(click: Click) = {

    if (click.dist != -1) {

      clusterHistByKey.get((click.userLoc, click.marketId)) match {
        case Some(distClusterProbs) => {

          distClusterProbs.foreach {
            case (dist, clusterProbs) =>
              if (abs(click.dist - dist) < 0.02) {

                val currVal = clusterProbs(click.cluster)
                clusterProbs(click.cluster) = currVal + 1
              }
          }
        }
        case None =>
      }
    }
    }
    trainDatasource.foreach { click => onClick(click) }

    
     /**
     * Build model
     */
    
      clusterHistByKey.foreach {
      case (key, distProbs) =>
        distProbs.foreach {
          case (dist, clusterProbs) =>

            val Z = sum(clusterProbs)
            clusterProbs :/= Z
        }
    }
    ClusterDistProxModel(clusterHistByKey)
  }
}

object ClusterDistProxModelBuilder2 extends ClusterModelBuilderFactory {

  def build(trainDatasource: ExDataSource, testClicks: Seq[Click], modelHyperParamsMap: CompoundHyperParamsMap): ClusterDistProxModelBuilder2 = {

   

    ClusterDistProxModelBuilder2( )
  }
}