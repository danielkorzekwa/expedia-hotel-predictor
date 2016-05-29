package expedia.model.old.countryuser2

import expedia.stats.MulticlassHistByKey
import breeze.linalg.DenseVector
import expedia.model.ClusterModel
import expedia.data.Click
import scala.collection._
/**
 * clusterHistByCountryUser Map[countryId, hist[userId]]
 */
case class CountryUserModel2(clusterHistByCountryUser: Map[Int, MulticlassHistByKey[Int]], clusterHistByCountry: MulticlassHistByKey[Int]) extends ClusterModel {

  def predict(countryId: Int, userId: Int): DenseVector[Float] = {

    clusterHistByCountryUser.get(countryId) match {
      case Some(userHist) => {
        userHist.getMap.getOrElse(userId,clusterHistByCountry.getMap(countryId))
      }
      case None           => clusterHistByCountry.getMap(countryId)
    }
    
     //clusterHistByCountry.getMap(countryId)

  }

  def predict(click: Click): DenseVector[Float] = {

    predict(click.countryId, click.userId)

  }
}