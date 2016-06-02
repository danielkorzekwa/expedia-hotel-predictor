package expedia.model.marketmodel

import scala.collection.Seq
import scala.collection.mutable
import expedia.data.Click
import expedia.data.ExDataSource
import expedia.model.country.CountryModel
import expedia.model.country.CountryModelBuilder
import expedia.stats.MulticlassHistByKey
import expedia.util.TimeDecayService
import breeze.numerics._
import expedia.CompoundHyperParams
import expedia.HyperParamsService

case class MarketModelBuilder(testClicks: Seq[Click],  hyperParamsService: HyperParamsService,hyperParams:CompoundHyperParams, timeDecayService: TimeDecayService) {

  private val segmentSizeMap: Map[Int, Int] = testClicks.groupBy { c => c.marketId }.map { x => x._1 -> x._2.size }

  private val clusterHistByMarket = MulticlassHistByKey[Int](100)
  testClicks.foreach { click => clusterHistByMarket.add(click.marketId, click.cluster, value = 0)
  }

  private val countryByMarket: mutable.Map[Int, Int] = mutable.Map()
  testClicks.foreach(click => countryByMarket += click.marketId -> click.countryId)

  def processCluster(click: Click) = {

    if (clusterHistByMarket.getMap.contains(click.marketId)) {
      val w = timeDecayService.getDecayForMarketId(click.dateTime,click.marketId)
      val beta1 = hyperParamsService.getParamValueForMarketId("expedia.model.marketmodel.beta1", click.marketId,hyperParams).toFloat
      val isBookingWeight = hyperParamsService.getParamValueForMarketId("expedia.model.marketmodel.isBookingWeight", click.marketId,hyperParams).toFloat

      if (click.isBooking == 1) clusterHistByMarket.add(click.marketId, click.cluster, value = w * isBookingWeight)
      else clusterHistByMarket.add(click.marketId, click.cluster, value = w * beta1)
    }
  }

  def create(countryModel: CountryModel): MarketModel = {

    clusterHistByMarket.getMap.foreach {
      case (marketId, clusterCounts) =>
        val beta2 = hyperParamsService.getParamValueForMarketId("expedia.model.marketmodel.beta2", marketId,hyperParams).toFloat

        val segmentSizeWeight = hyperParamsService.getParamValueForMarketId("expedia.model.marketmodel.segmentSizeWeight", marketId,hyperParams).toFloat

        clusterCounts :+= (beta2 + segmentSizeWeight * log(segmentSizeMap(marketId).toFloat)) * countryModel.predict(countryByMarket(marketId))
    }
    clusterHistByMarket.normalise()

    MarketModel(clusterHistByMarket)
  }
}

object MarketModelBuilder {
//  def buildFromTrainingSet(trainDatasource: ExDataSource, testClicks: Seq[Click], hyperParams: CompoundHyperParams): MarketModel = {
//
//    val timeDecayService = TimeDecayService(testClicks, hyperParams)
//
//    val marketModelBuilder = MarketModelBuilder(testClicks, hyperParams, timeDecayService)
//    val countryModelBuilder = CountryModelBuilder(testClicks, hyperParams, timeDecayService)
//
//    def onClick(click: Click) = {
//
//      marketModelBuilder.processCluster(click)
//      countryModelBuilder.processCluster(click)
//    }
//    trainDatasource.foreach { click => onClick(click) }
//
//    val countryModel = countryModelBuilder.create()
//
//    val model = marketModelBuilder.create(countryModel)
//
//    model
//  }
}