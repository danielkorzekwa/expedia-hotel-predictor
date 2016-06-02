package expedia.model.marketuser

import scala.collection.Seq
import scala.collection.mutable

import breeze.linalg.InjectNumericOps
import expedia.CompoundHyperParams
import expedia.HyperParamsService
import expedia.data.Click
import expedia.data.ExDataSource
import expedia.model.countryuser.CountryUserModel
import expedia.model.marketmodel.MarketModel
import expedia.stats.MulticlassHistByKey
import expedia.util.TimeDecayService

case class MarketUserModelBuilder(testClicks: Seq[Click],  hyperParamsService: HyperParamsService,hyperParams:CompoundHyperParams, timeDecayService: TimeDecayService) {

  private val clusterHistByMarketUser = MulticlassHistByKey[Tuple2[Int, Int]](100)
  testClicks.foreach(click => clusterHistByMarketUser.add((click.marketId, click.userId), click.cluster, value = 0))

  private val countryByMarket: mutable.Map[Int, Int] = mutable.Map()
  testClicks.foreach(click => countryByMarket += click.marketId -> click.countryId)

  private val beta2 = 0.95f //hyperParams.getParamValue("expedia.model.marketuser.beta2").toFloat

  def processCluster(click: Click) = {

  

    val marketUserKey = (click.marketId, click.userId)
    if (clusterHistByMarketUser.getMap.contains(marketUserKey)) {
      
        val beta1 = hyperParamsService.getParamValueForMarketId("expedia.model.marketuser.beta1", click.marketId,hyperParams).toFloat
    val isBookingWeight = hyperParamsService.getParamValueForMarketId("expedia.model.marketuser.isBookingWeight", click.marketId,hyperParams).toFloat

    val w = timeDecayService.getDecayForMarketId(click.dateTime,click.marketId)
      if (click.isBooking == 1) clusterHistByMarketUser.add(marketUserKey, click.cluster, value = w * isBookingWeight)
      else clusterHistByMarketUser.add(marketUserKey, click.cluster, value = w * beta1)
    }

  }

  def create(countryUserModel: CountryUserModel, marketModel: MarketModel): MarketUserModel = {

    clusterHistByMarketUser.getMap.foreach {
      case ((marketId, userId), clusterCounts) =>
        val beta3 = hyperParamsService.getParamValueForMarketId("expedia.model.marketuser.beta3", marketId,hyperParams).toFloat

        //        if (countryUserModel.predictionExists(countryByMarket(marketId), userId)) {
        //          clusterCounts :+= beta3 * (beta2 * marketModel.predict(marketId) + (1 - beta2) * countryUserModel.predict(countryByMarket(marketId), userId)) 
        //        } else clusterCounts :+= beta3* marketModel.predict(marketId) 

        clusterCounts :+= beta3 * marketModel.predict(marketId)
    }
    clusterHistByMarketUser.normalise()

    MarketUserModel(clusterHistByMarketUser)
  }

}

object MarketUserModelBuilder {

//  def buildFromTrainingSet(trainDatasource: ExDataSource, testClicks: Seq[Click], hyperParams: CompoundHyperParams): MarketUserModel = {
//
//    val timeDecayService = TimeDecayService(testClicks, hyperParams)
//
//    val countryModelBuilder = CountryModelBuilder(testClicks, hyperParams, timeDecayService)
//    val marketModelBuilder = MarketModelBuilder(testClicks, hyperParams, timeDecayService)
//    val marketUserModelBuilder = MarketUserModelBuilder(testClicks, hyperParams, timeDecayService)
//    val countryUserModelBuilder = CountryUserModelBuilder(testClicks, hyperParams)
//
//    def onClick(click: Click) = {
//
//      countryModelBuilder.processCluster(click)
//      countryUserModelBuilder.processCluster(click)
//      marketModelBuilder.processCluster(click)
//      marketUserModelBuilder.processCluster(click)
//
//    }
//    trainDatasource.foreach { click => onClick(click) }
//
//    val countryModel = countryModelBuilder.create()
//    val marketModel = marketModelBuilder.create(countryModel)
//    val countryUserModel = countryUserModelBuilder.create(countryModel)
//    val marketUserModel = marketUserModelBuilder.create(countryUserModel, marketModel)
//
//    marketUserModel
//
//  }
}