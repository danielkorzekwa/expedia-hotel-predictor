package expedia.model.marketmodel

import scala.collection.Seq
import scala.collection.mutable

import expedia.HyperParams
import expedia.data.Click
import expedia.data.ExDataSource
import expedia.model.country.CountryModel
import expedia.model.country.CountryModelBuilder
import expedia.stats.MulticlassHistByKey
import expedia.util.TimeDecayService
import breeze.numerics._

case class MarketModelBuilder(testClicks: Seq[Click], hyperParams: HyperParams, timeDecayService: TimeDecayService) {

  private val segmentSizeMap: Map[Int, Int] = testClicks.groupBy { c => c.marketId }.map { x => x._1 -> x._2.size }

  private val clusterHistByMarket = MulticlassHistByKey[Int](100)
  testClicks.foreach { click => clusterHistByMarket.add(click.marketId, click.cluster, value = 0)
  }

  private val countryByMarket: mutable.Map[Int, Int] = mutable.Map()
  testClicks.foreach(click => countryByMarket += click.marketId -> click.countryId)

  private val beta1 = hyperParams.getParamValue("expedia.model.marketmodel.beta1").toFloat
  private val beta2 = hyperParams.getParamValue("expedia.model.marketmodel.beta2").toFloat

  private val isBookingWeight = hyperParams.getParamValue("expedia.model.marketmodel.isBookingWeight").toFloat
  private val segmentSizeWeight = hyperParams.getParamValue("expedia.model.marketmodel.segmentSizeWeight").toFloat

  def processCluster(click: Click) = {

    val w = timeDecayService.getDecay(click)

    if (clusterHistByMarket.getMap.contains(click.marketId)) {
      if (click.isBooking == 1) clusterHistByMarket.add(click.marketId, click.cluster, value = w * isBookingWeight)
      else clusterHistByMarket.add(click.marketId, click.cluster, value = w * beta1)
    }
  }

  def create(countryModel: CountryModel): MarketModel = {

    clusterHistByMarket.getMap.foreach {
      case (marketId, clusterCounts) =>
        clusterCounts :+= (beta2 + segmentSizeWeight * log(segmentSizeMap(marketId).toFloat)) * countryModel.predict(countryByMarket(marketId))
    }
    clusterHistByMarket.normalise()

    MarketModel(clusterHistByMarket)
  }
}

object MarketModelBuilder {
  def buildFromTrainingSet(trainDatasource: ExDataSource, testClicks: Seq[Click], hyperParams: HyperParams): MarketModel = {

    val timeDecayService = TimeDecayService(testClicks, hyperParams)

    val marketModelBuilder = MarketModelBuilder(testClicks, hyperParams, timeDecayService)
    val countryModelBuilder = CountryModelBuilder(testClicks, hyperParams, timeDecayService)

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