package expedia.model.countryuser

import expedia.data.Click
import expedia.stats.MulticlassHistByKey
import expedia.model.marketmodel.MarketModel
import expedia.model.country.CountryModel
import expedia.HyperParams

case class CountryUserModelBuilder(testClicks: Seq[Click], hyperParams: HyperParams) {

  //key ((countryId,userId)
  private val clusterHistByCountryUser = MulticlassHistByKey[Tuple2[Int, Int]](100)
  testClicks.foreach(click => clusterHistByCountryUser.add((click.countryId, click.userId), click.cluster, value = 0))

  private val beta1 = hyperParams.getParamValue("expedia.model.countryuser.beta1").toFloat
  private val beta2 = hyperParams.getParamValue("expedia.model.countryuser.beta2").toFloat

  def processCluster(click: Click) = {

    val countryUserKey = (click.countryId, click.userId)
    if (clusterHistByCountryUser.getMap.contains(countryUserKey)) {
      if (click.isBooking == 1) clusterHistByCountryUser.add(countryUserKey, click.cluster)
      else clusterHistByCountryUser.add(countryUserKey, click.cluster, value = beta1)
    }
  }

  def create(countryModel: CountryModel): CountryUserModel = {

    clusterHistByCountryUser.getMap.foreach { case ((countryId, userId), clusterCounts) => clusterCounts :+= beta2 * countryModel.predict(countryId) }
    clusterHistByCountryUser.normalise()

    CountryUserModel(clusterHistByCountryUser)
  }
}