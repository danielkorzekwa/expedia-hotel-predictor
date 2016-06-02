package expedia.model.marketuserloc

import scala.collection.Seq
import scala.collection.mutable

import breeze.linalg.InjectNumericOps
import expedia.CompoundHyperParams
import expedia.HyperParamsService
import expedia.data.Click
import expedia.data.ExDataSource
import expedia.model.country.CountryModelBuilder
import expedia.model.countryuser.CountryUserModel
import expedia.model.countryuser.CountryUserModelBuilder
import expedia.model.marketmodel.MarketModel
import expedia.model.marketmodel.MarketModelBuilder
import expedia.stats.MulticlassHistByKey
import expedia.util.TimeDecayService

case class MarketUserLocModelBuilder(testClicks: Seq[Click], hyperParamsService:HyperParamsService,hyperParams: CompoundHyperParams, timeDecayService: TimeDecayService) {

  private val clusterHistByMarketUserLoc = MulticlassHistByKey[Tuple2[Int, Int]](100)
  testClicks.foreach(click => clusterHistByMarketUserLoc.add((click.marketId, click.userLoc), click.cluster, value = 0))

  private val countryByMarket: mutable.Map[Int, Int] = mutable.Map()
  testClicks.foreach(click => countryByMarket += click.marketId -> click.countryId)

  private val beta2 = 0.95f //hyperParams.getParamValue("expedia.model.marketuser.beta2").toFloat

  def processCluster(click: Click) = {

  

    val marketUserLocKey = (click.marketId, click.userLoc)
    if (clusterHistByMarketUserLoc.getMap.contains(marketUserLocKey)) {
      
        val beta1 = hyperParamsService.getParamValueForMarketId("expedia.model.marketuser.beta1", click.marketId,hyperParams).toFloat
    val isBookingWeight = hyperParamsService.getParamValueForMarketId("expedia.model.marketuser.isBookingWeight", click.marketId,hyperParams).toFloat

    val w = timeDecayService.getDecayForMarketId(click.dateTime,click.marketId)
      if (click.isBooking == 1) clusterHistByMarketUserLoc.add(marketUserLocKey, click.cluster, value = w * isBookingWeight)
      else clusterHistByMarketUserLoc.add(marketUserLocKey, click.cluster, value = w * beta1)
    }

  }

  def create(countryUserModel: CountryUserModel, marketModel: MarketModel): MarketUserLocModel = {

    clusterHistByMarketUserLoc.getMap.foreach {
      case ((marketId, userLoc), clusterCounts) =>
        val beta3 = hyperParamsService.getParamValueForMarketId("expedia.model.marketuser.beta3", marketId,hyperParams).toFloat

        //        if (countryUserModel.predictionExists(countryByMarket(marketId), userId)) {
        //          clusterCounts :+= beta3 * (beta2 * marketModel.predict(marketId) + (1 - beta2) * countryUserModel.predict(countryByMarket(marketId), userId)) 
        //        } else clusterCounts :+= beta3* marketModel.predict(marketId) 
        clusterCounts :+= 100f * marketModel.predict(marketId)
    }
    clusterHistByMarketUserLoc.normalise()

    MarketUserLocModel(clusterHistByMarketUserLoc)
  }

}

object MarketUserLocModelBuilder {

//  def buildFromTrainingSet(trainDatasource: ExDataSource, testClicks: Seq[Click], hyperParams: CompoundHyperParams): MarketUserLocModel = {
//
//    val timeDecayService = TimeDecayService(testClicks, hyperParams)
//
//    val countryModelBuilder = CountryModelBuilder(testClicks, hyperParams, timeDecayService)
//    val marketModelBuilder = MarketModelBuilder(testClicks, hyperParams, timeDecayService)
//    val marketUserLocModelBuilder = MarketUserLocModelBuilder(testClicks, hyperParams, timeDecayService)
//    val countryUserModelBuilder = CountryUserModelBuilder(testClicks, hyperParams)
//
//    def onClick(click: Click) = {
//
//      countryModelBuilder.processCluster(click)
//      countryUserModelBuilder.processCluster(click)
//      marketModelBuilder.processCluster(click)
//      marketUserLocModelBuilder.processCluster(click)
//
//    }
//    trainDatasource.foreach { click => onClick(click) }
//
//    val countryModel = countryModelBuilder.create()
//    val marketModel = marketModelBuilder.create(countryModel)
//    val countryUserModel = countryUserModelBuilder.create(countryModel)
//    val marketUserLocModel = marketUserLocModelBuilder.create(countryUserModel, marketModel)
//
//    marketUserLocModel
//
//  }
}