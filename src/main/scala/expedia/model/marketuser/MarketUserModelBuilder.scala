package expedia.model.marketuser

import expedia.data.Click
import expedia.stats.MulticlassHistByKey
import scala.collection._
import expedia.model.country.CountryModel
import expedia.model.countryuser.CountryUserModel
import expedia.model.marketmodel.MarketModel

case class MarketUserModelBuilder(testClicks: Seq[Click]) {

  private val clusterHistByMarketUser = MulticlassHistByKey[Tuple2[Int, Int]](100)
  testClicks.foreach(click => clusterHistByMarketUser.add((click.marketId, click.userId), click.cluster, value = 0))

  private val countryByMarket: mutable.Map[Int, Int] = mutable.Map()
  testClicks.foreach(click => countryByMarket += click.marketId -> click.countryId)

  def processCluster(click: Click) = {

    val marketUserKey = (click.marketId, click.userId)
    if (clusterHistByMarketUser.getMap.contains(marketUserKey)) {
      if (click.isBooking == 1) clusterHistByMarketUser.add(marketUserKey, click.cluster)
      else clusterHistByMarketUser.add(marketUserKey, click.cluster, value = 0.6f)
    }

  }

  def create(countryUserModel: CountryUserModel, marketModel: MarketModel): MarketUserModel = {

    val beta = 0.70f
    clusterHistByMarketUser.getMap.foreach {
      case ((marketId, userId), clusterCounts) =>
        if (countryUserModel.predictionExists(countryByMarket(marketId), userId)) {
          clusterCounts :+= 1f * (beta * marketModel.predict(marketId) + (1 - beta) * countryUserModel.predict(countryByMarket(marketId), userId))
        } else clusterCounts :+= marketModel.predict(marketId)
    }
    clusterHistByMarketUser.normalise()

    MarketUserModel(clusterHistByMarketUser)
  }

}