package expedia.model.cmu

import expedia.data.Click
import breeze.linalg.DenseVector
import expedia.model.ClusterModel
import expedia.stats.MulticlassHistByKey
import scala.collection._
import breeze.linalg.max
import expedia.model.svm.loadClusterProbsByKeyMap2
import java.io.File
import breeze.linalg._
import expedia.stats.CounterMap
import expedia.model.dest.DestModel
import expedia.CompoundHyperParams
import expedia.HyperParamsService

case class CmuModel( 
    clusterHistByMDPU: Map[Tuple4[Int,Int, Int, Int], DenseVector[Float]],
      userCounterMap: CounterMap[Int], destCounterMap: CounterMap[Int], destMarketCounterMap: CounterMap[Tuple2[Int, Int]],
      destModel: DestModel) extends ClusterModel{
  
    

  
    def predict(click:Click): DenseVector[Float] = {
// return  clusterHistByMDPU((click.marketId,click.destId, click.isPackage,click.userId))
   val destCounts = destCounterMap.getOrElse(click.destId, 0)
    val destMarketCounts = destMarketCounterMap.getOrElse((click.destId, click.marketId), 0)

    var clusterProb =
      if (!userCounterMap.contains(click.userId) && DestModel.svmDestIds.contains(click.destId) && click.stayDays < 3
        && !(destMarketCounts < 300 || destCounts / destMarketCounts > 1.5)) {
        destModel.predict(click.destId, click.continentId, click.stayDays)
      } else {
        clusterHistByMDPU((click.marketId,click.destId, click.isPackage,click.userId))
      }

    clusterProb = clusterProb.copy

    clusterProb
  }
  
}