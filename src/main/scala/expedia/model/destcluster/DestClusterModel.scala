package expedia.model.destcluster

import expedia.data.Click
import com.typesafe.scalalogging.slf4j.LazyLogging
import breeze.linalg.DenseVector
import expedia.model.ClusterModel
import expedia.stats.MulticlassHistByKey
import expedia.model.country.CountryModel

case class DestClusterModel(clusterHistByDestCluster: MulticlassHistByKey[Int], destClusterByDestMap: Map[Int, Int],countryModel:CountryModel) extends ClusterModel with LazyLogging {

  def predictionExists(destId: Int): Boolean = destClusterByDestMap.contains(destId) && clusterHistByDestCluster.getMap.contains(destClusterByDestMap(destId))

  def predict(destId: Int): DenseVector[Float] = {
    
    val destCluster = destClusterByDestMap(destId)
    clusterHistByDestCluster.getMap(destCluster)
  }

  def predict(click: Click): DenseVector[Float] = {
      destClusterByDestMap.get(click.destId) match {
      case Some(destCluster) if(clusterHistByDestCluster.getMap.contains(destCluster))=>  clusterHistByDestCluster.getMap(destCluster)
      case _ => countryModel.predict(click.countryId)
    }
  }
}