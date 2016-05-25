package expedia.model.marketuser

import expedia.data.Click
import expedia.stats.MulticlassHistByKey
import scala.collection._
import expedia.model.country.CountryModel
import expedia.model.countryuser.CountryUserModel
import expedia.model.marketmodel.MarketModel
import expedia.HyperParams
import expedia.data.ExDataSource
import expedia.model.marketdest.MarketDestModelBuilder
import expedia.model.country.CountryModelBuilder
import expedia.model.marketmodel.MarketModelBuilder
import expedia.model.countryuser.CountryUserModelBuilder
import expedia.util.getTimeDecay

case class MarketUserModelBuilder(testClicks: Seq[Click], hyperParams: HyperParams) {

  private val clusterHistByMarketUser = MulticlassHistByKey[Tuple2[Int, Int]](100)
  testClicks.foreach(click => clusterHistByMarketUser.add((click.marketId, click.userId), click.cluster, value = 0))

  private val countryByMarket: mutable.Map[Int, Int] = mutable.Map()
  testClicks.foreach(click => countryByMarket += click.marketId -> click.countryId)

  private val beta1 = hyperParams.getParamValue("expedia.model.marketuser.beta1").toFloat
  private val beta2 = hyperParams.getParamValue("expedia.model.marketuser.beta2").toFloat
  private val beta3 = hyperParams.getParamValue("expedia.model.marketuser.beta3").toFloat

  def processCluster(click: Click) = {

      val w = getTimeDecay(click.dateTime)
    
    val marketUserKey = (click.marketId, click.userId)
    if (clusterHistByMarketUser.getMap.contains(marketUserKey)) {
      if (click.isBooking == 1) clusterHistByMarketUser.add(marketUserKey, click.cluster,value=w)
      else clusterHistByMarketUser.add(marketUserKey, click.cluster, value = w*beta1)
    }

  }

  def create(countryUserModel: CountryUserModel, marketModel: MarketModel): MarketUserModel = {

    clusterHistByMarketUser.getMap.foreach {
      case ((marketId, userId), clusterCounts) =>
        if (countryUserModel.predictionExists(countryByMarket(marketId), userId)) {
          clusterCounts :+= beta3 * (beta2 * marketModel.predict(marketId) + (1 - beta2) * countryUserModel.predict(countryByMarket(marketId), userId))
        } else clusterCounts :+= marketModel.predict(marketId)
    }
    clusterHistByMarketUser.normalise()

    MarketUserModel(clusterHistByMarketUser)
  }

}

object MarketUserModelBuilder {

  def buildFromTrainingSet(trainDatasource: ExDataSource, testClicks: Seq[Click], hyperParams: HyperParams): MarketUserModel = {

    val countryModelBuilder = CountryModelBuilder(testClicks, hyperParams)
    val marketModelBuilder = MarketModelBuilder(testClicks, hyperParams)
    val marketUserModelBuilder = MarketUserModelBuilder(testClicks, hyperParams)
    val countryUserModelBuilder = CountryUserModelBuilder(testClicks, hyperParams)

    def onClick(click: Click) = {

      countryModelBuilder.processCluster(click)
      countryUserModelBuilder.processCluster(click)
      marketModelBuilder.processCluster(click)
      marketUserModelBuilder.processCluster(click)

    }
    trainDatasource.foreach { click => onClick(click) }

    val countryModel = countryModelBuilder.create()
    val marketModel = marketModelBuilder.create(countryModel)
    val countryUserModel = countryUserModelBuilder.create(countryModel)
    val marketUserModel = marketUserModelBuilder.create(countryUserModel, marketModel)

    marketUserModel

  }
}