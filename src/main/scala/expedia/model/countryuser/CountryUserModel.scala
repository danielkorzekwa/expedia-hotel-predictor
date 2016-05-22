package expedia.model.countryuser

import expedia.stats.MulticlassHistByKey
import breeze.linalg.DenseVector

case class CountryUserModel(clusterHistByCountryUser: MulticlassHistByKey[Tuple2[Int, Int]]) {

  def predict(countryId: Int, userId: Int): DenseVector[Float] = {

    clusterHistByCountryUser.getMap((countryId, userId))

  }
}