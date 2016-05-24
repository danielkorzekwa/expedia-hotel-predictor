package expedia.model.marketmodel

import expedia.data.Click
import expedia.stats.MulticlassHistByKey
import expedia.model.country.CountryModel
import scala.collection._
import expedia.model.country.CountryModelBuilder
import expedia.data.ExDataSource
import expedia.model.dest.DestModel
import expedia.model.dest.DestModelBuilder
import expedia.HyperParams

case class MarketModelBuilder(testClicks: Seq[Click], hyperParams: HyperParams) {

  private val clusterHistByMarket = MulticlassHistByKey[Int](100)
  testClicks.foreach { click => clusterHistByMarket.add(click.marketId, click.cluster, value = 0)
  }

  private val countryByMarket: mutable.Map[Int, Int] = mutable.Map()
  testClicks.foreach(click => countryByMarket += click.marketId -> click.countryId)

  private val beta1 = hyperParams.getParamValue("expedia.model.marketmodel.beta1").toFloat

  def processCluster(click: Click) = {

    if (clusterHistByMarket.getMap.contains(click.marketId)) {
      if (click.isBooking == 1) clusterHistByMarket.add(click.marketId, click.cluster)
      else clusterHistByMarket.add(click.marketId, click.cluster, value = beta1)
    }
  }

  def create(countryModel: CountryModel): MarketModel = {

    clusterHistByMarket.getMap.foreach { case (marketId, clusterCounts) => clusterCounts :+= countryModel.predict(countryByMarket(marketId)) }
    clusterHistByMarket.normalise()

    MarketModel(clusterHistByMarket)
  }
}

object MarketModelBuilder {
  def buildFromTrainingSet(trainDatasource: ExDataSource, testClicks: Seq[Click], hyperParams: HyperParams): MarketModel = {

    val marketModelBuilder = MarketModelBuilder(testClicks, hyperParams)
    val countryModelBuilder = CountryModelBuilder(testClicks,hyperParams)

    def onClick(click: Click) = {

      marketModelBuilder.processCluster(click)
      countryModelBuilder.processCluster(click)
    }
    trainDatasource.foreach { click => onClick(click) }

    val countryModel = countryModelBuilder.create()

    val model = marketModelBuilder.create(countryModel)

    model
  }
}