package expedia.model.marketuser

import expedia.data.Click
import expedia.stats.MulticlassHistByKey
import scala.collection._
import expedia.model.country.CountryModel
import expedia.model.countryuser.CountryUserModel
import expedia.model.marketmodel.MarketModel
import expedia.HyperParams

case class MarketUserModelBuilder(testClicks: Seq[Click], hyperParams: HyperParams) {

  private val clusterHistByMarketUser = MulticlassHistByKey[Tuple2[Int, Int]](100)
  testClicks.foreach(click => clusterHistByMarketUser.add((click.marketId, click.userId), click.cluster, value = 0))

  private val countryByMarket: mutable.Map[Int, Int] = mutable.Map()
  testClicks.foreach(click => countryByMarket += click.marketId -> click.countryId)

  private val beta1 = hyperParams.getParamValue("expedia.model.marketuser.beta1").toFloat
  private val beta2 = hyperParams.getParamValue("expedia.model.marketuser.beta2").toFloat
   private val beta3 = hyperParams.getParamValue("expedia.model.marketuser.beta3").toFloat

  def processCluster(click: Click) = {

    val marketUserKey = (click.marketId, click.userId)
    if (clusterHistByMarketUser.getMap.contains(marketUserKey)) {
      if (click.isBooking == 1) clusterHistByMarketUser.add(marketUserKey, click.cluster)
      else clusterHistByMarketUser.add(marketUserKey, click.cluster, value = beta1)
    }

  }

  def create(countryUserModel: CountryUserModel, marketModel: MarketModel): MarketUserModel = {

   
    clusterHistByMarketUser.getMap.foreach {
      case ((marketId, userId), clusterCounts) =>
        if (countryUserModel.predictionExists(countryByMarket(marketId), userId)) {
          clusterCounts :+= beta3* (beta2 * marketModel.predict(marketId) + (1 - beta2) * countryUserModel.predict(countryByMarket(marketId), userId))
        } else clusterCounts :+= marketModel.predict(marketId)
    }
    clusterHistByMarketUser.normalise()

    MarketUserModel(clusterHistByMarketUser)
  }

}