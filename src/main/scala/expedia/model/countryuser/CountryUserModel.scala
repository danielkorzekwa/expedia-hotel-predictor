package expedia.model.countryuser

import expedia.stats.MulticlassHistByKey
import breeze.linalg.DenseVector
import expedia.model.ClusterModel
import expedia.data.Click

case class CountryUserModel(clusterHistByCountryUser: MulticlassHistByKey[Tuple2[Int, Int]]) extends ClusterModel {

  def predictionExists(countryId: Int, userId: Int):Boolean = clusterHistByCountryUser.getMap.contains((countryId, userId))
  
  def predict(countryId: Int, userId: Int): DenseVector[Float] = {

    clusterHistByCountryUser.getMap((countryId, userId))

  }
  
   def predict(click:Click): DenseVector[Float] = {

    clusterHistByCountryUser.getMap((click.countryId, click.userId))

  }
}