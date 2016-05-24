package expedia.model.mdp

import com.typesafe.scalalogging.slf4j.LazyLogging
import expedia.stats.MulticlassHistByKey
import expedia.model.ClusterModel
import expedia.data.Click
import breeze.linalg.DenseVector

case class MdpModel(clusterHistByMDP: MulticlassHistByKey[Tuple3[Int, Int, Int]]) extends ClusterModel with LazyLogging {

   def predict(marketId: Int, destId: Int,isPackage:Int): DenseVector[Float] = {
    clusterHistByMDP.getMap((marketId, destId,isPackage))
  }
  
  def predict(click: Click): DenseVector[Float] = {
   predict(click.marketId, click.destId, click.isPackage)
  }

}