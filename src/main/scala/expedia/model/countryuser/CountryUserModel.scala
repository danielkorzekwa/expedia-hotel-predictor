package expedia.model.countryuser

import expedia.stats.MulticlassHistByKey
import breeze.linalg.DenseVector

case class CountryUserModel(clusterHistByCountryUser: MulticlassHistByKey[Tuple2[Int, Int]]) {

  def predictionExists(countryId: Int, userId: Int):Boolean = clusterHistByCountryUser.getMap.contains((countryId, userId))
  
  def predict(countryId: Int, userId: Int): DenseVector[Float] = {

    clusterHistByCountryUser.getMap((countryId, userId))

  }
}