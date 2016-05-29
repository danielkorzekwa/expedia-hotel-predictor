package expedia.model.destcluster

import expedia.data.Click
import com.typesafe.scalalogging.slf4j.LazyLogging
import breeze.linalg.DenseVector
import expedia.model.ClusterModel
import expedia.stats.MulticlassHistByKey

case class DestClusterModel(clusterHistByDestCluster: MulticlassHistByKey[Int], destClusterByDestMap: Map[Int, Int]) extends ClusterModel with LazyLogging {

  def predictionExists(destId: Int): Boolean = destClusterByDestMap.contains(destId) && clusterHistByDestCluster.getMap.contains(destClusterByDestMap(destId))

  def predict(destId: Int): DenseVector[Float] = {
    clusterHistByDestCluster.getMap(destClusterByDestMap(destId))
  }

  def predict(click: Click): DenseVector[Float] = {
    predict(click.destId)
  }
}