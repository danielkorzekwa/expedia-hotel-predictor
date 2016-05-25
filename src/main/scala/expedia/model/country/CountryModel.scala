package expedia.model.country

import breeze.linalg.DenseVector
import expedia.stats.MulticlassHistByKey
import expedia.data.Click
import breeze.linalg.DenseMatrix
import expedia.util.getTop5Clusters
import java.util.concurrent.atomic.AtomicInteger
import com.typesafe.scalalogging.slf4j.LazyLogging
import expedia.model.ClusterModel

case class CountryModel(clusterHistByCountry: MulticlassHistByKey[Int])  extends ClusterModel {

  /**
   *
   * @param hotelCluster probs vector
   */
  def predict(countryId: Int): DenseVector[Float] = {

    val clusterProbs = clusterHistByCountry.getMap(countryId)
    clusterProbs
  }
  
   def predict(click:Click): DenseVector[Float] = {

   predict(click.countryId)
  }

}