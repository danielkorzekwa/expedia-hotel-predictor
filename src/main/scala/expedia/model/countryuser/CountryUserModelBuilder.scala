package expedia.model.countryuser

import expedia.data.Click
import expedia.stats.MulticlassHistByKey
import expedia.model.marketmodel.MarketModel
import expedia.model.country.CountryModel

case class CountryUserModelBuilder(testClicks: Seq[Click]) {

  //key ((countryId,userId)
  private val clusterHistByCountryUser = MulticlassHistByKey[Tuple2[Int, Int]](100)
  testClicks.foreach(click => clusterHistByCountryUser.add((click.countryId, click.userId), click.cluster, value = 0))

  def processCluster(click: Click) = {

    val countryUserKey = (click.countryId, click.userId)
    if (clusterHistByCountryUser.getMap.contains(countryUserKey)) {
      if (click.isBooking == 1) clusterHistByCountryUser.add(countryUserKey, click.cluster)
      else clusterHistByCountryUser.add(countryUserKey, click.cluster, value = 0.6f)
    }
  }

  def create(countryModel: CountryModel): CountryUserModel = {

    clusterHistByCountryUser.getMap.foreach { case ((countryId, userId), clusterCounts) => clusterCounts :+= 1f * countryModel.predict(countryId) }
    clusterHistByCountryUser.normalise()

    CountryUserModel(clusterHistByCountryUser)
  }
}