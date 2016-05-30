package expedia.model.marketdestcluster

import expedia.data.Click
import com.typesafe.scalalogging.slf4j.LazyLogging
import breeze.linalg.DenseVector
import expedia.model.ClusterModel
import expedia.stats.MulticlassHistByKey
import expedia.model.country.CountryModel

case class MarketDestClusterModel(
    destClusterHistByMarketDestCluster: MulticlassHistByKey[Tuple2[Int,Int]], 
    destClusterByDestMap: Map[Int, Int],countryModel:CountryModel) extends ClusterModel with LazyLogging {

  def predictionExists(marketId:Int,destId: Int): Boolean = {
    
    destClusterByDestMap.contains(destId) && destClusterHistByMarketDestCluster.getMap.contains((marketId,destClusterByDestMap(destId)))
  }

  def predict(marketId:Int,destId: Int): DenseVector[Float] = {
    
    val key = (marketId,destClusterByDestMap(destId))
    destClusterHistByMarketDestCluster.getMap(key)
  }

  def predict(click: Click): DenseVector[Float] = {
      destClusterByDestMap.get(click.destId) match {
      case Some(destCluster)  if(destClusterHistByMarketDestCluster.getMap.contains((click.marketId,destClusterByDestMap(click.destId)))) =>
        destClusterHistByMarketDestCluster.getMap((click.marketId,destClusterByDestMap(click.destId)))
      
      case _ => countryModel.predict(click.countryId)
    }
  }
}