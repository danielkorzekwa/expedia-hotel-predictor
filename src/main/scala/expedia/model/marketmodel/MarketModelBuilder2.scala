package expedia.model.marketmodel

import expedia.CompoundHyperParams
import expedia.HyperParamsService
import expedia.data.Click
import expedia.data.ExDataSource
import expedia.model.ClusterModelBuilderFactory
import expedia.model.country.CountryModel
import expedia.model.country.CountryModelBuilder
import expedia.util.TimeDecayService
import expedia.model.ClusterModelBuilder
import scala.collection._
import expedia.stats.MulticlassHistByKey
import expedia.util.TimeDecayService
import breeze.numerics._
import expedia.CompoundHyperParamsMap

case class MarketModelBuilder2(countryModel: CountryModel,timeDecayService:TimeDecayService,hyperParamsService:HyperParamsService) extends ClusterModelBuilder {

  def create(trainDatasource: ExDataSource, testClicks: Seq[Click], hyperParams: CompoundHyperParams): MarketModel = {
   
     /**
     * Process training set
     */
  val segmentSizeMap: Map[Int, Int] = testClicks.groupBy { c => c.marketId }.map { x => x._1 -> x._2.size }

   val clusterHistByMarket = MulticlassHistByKey[Int](100)
  testClicks.foreach { click => clusterHistByMarket.add(click.marketId, click.cluster, value = 0)
  }

   val countryByMarket: mutable.Map[Int, Int] = mutable.Map()
  testClicks.foreach(click => countryByMarket += click.marketId -> click.countryId)
    
   def onClick(click: Click) = {

    if (clusterHistByMarket.getMap.contains(click.marketId)) {
      val w = timeDecayService.getDecayForMarketId(click.dateTime,click.marketId)
      val beta1 = hyperParamsService.getParamValueForMarketId("expedia.model.marketmodel.beta1", click.marketId,hyperParams).toFloat
      val isBookingWeight = hyperParamsService.getParamValueForMarketId("expedia.model.marketmodel.isBookingWeight", click.marketId,hyperParams).toFloat

      if (click.isBooking == 1) clusterHistByMarket.add(click.marketId, click.cluster, value = w * isBookingWeight)
      else clusterHistByMarket.add(click.marketId, click.cluster, value = w * beta1)
    }
   }
    trainDatasource.foreach { click => onClick(click) }
  
     /**
     * Build model
     */
    
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

object MarketModelBuilder2 extends ClusterModelBuilderFactory {

  def build(trainDatasource: ExDataSource, testClicks: Seq[Click], modelHyperParamsMap: CompoundHyperParamsMap): MarketModelBuilder2 = {

    val hyperParams = modelHyperParamsMap.getModel("default")
    val hyperParamsService = HyperParamsService(testClicks)

    val timeDecayService = TimeDecayService(testClicks, hyperParamsService, hyperParams)

    val countryModelBuilder = CountryModelBuilder(testClicks, hyperParamsService, hyperParams, timeDecayService)

    def onClick(click: Click) = {
      countryModelBuilder.processCluster(click)
    }
    trainDatasource.foreach { click => onClick(click) }

    val countryModel = countryModelBuilder.create()

    MarketModelBuilder2(countryModel,timeDecayService,hyperParamsService)
  }
}