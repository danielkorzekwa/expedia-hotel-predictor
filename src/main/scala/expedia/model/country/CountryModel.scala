package expedia.model.country

import breeze.linalg.DenseVector
import expedia.stats.MulticlassHistByKey

case class CountryModel(clusterHistByCountry: MulticlassHistByKey[Int]) {

  /**
   *
   * @param hotelCluster probs vector
   */
  def predict(countryId: Int): DenseVector[Float] = {

    val clusterProbs = clusterHistByCountry.getMap(countryId)
    clusterProbs
  }
}