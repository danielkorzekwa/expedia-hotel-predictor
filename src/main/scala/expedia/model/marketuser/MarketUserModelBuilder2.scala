package expedia.model.marketuser

import expedia.CompoundHyperParams
import expedia.CompoundHyperParamsMap
import expedia.data.Click
import expedia.data.ExDataSource
import expedia.model.ClusterModelBuilder
import expedia.model.ClusterModelBuilderFactory
import expedia.HyperParamsService
import expedia.util.TimeDecayService
import expedia.stats.MulticlassHistByKey
import scala.collection._
import expedia.model.marketmodel.MarketModelBuilder2
import expedia.model.marketmodel.MarketModel

case class MarketUserModelBuilder2(marketModel:MarketModel,timeDecayService: TimeDecayService, hyperParamsService: HyperParamsService) extends ClusterModelBuilder {

  def create(trainDatasource: ExDataSource, testClicks: Seq[Click], hyperParams: CompoundHyperParams): MarketUserModel = {
    
      val clusterHistByMarketUser = MulticlassHistByKey[Tuple2[Int, Int]](100)
  testClicks.foreach(click => clusterHistByMarketUser.add((click.marketId, click.userId), click.cluster, value = 0))

   val countryByMarket: mutable.Map[Int, Int] = mutable.Map()
  testClicks.foreach(click => countryByMarket += click.marketId -> click.countryId)

   val beta2 = 0.95f //hyperParams.getParamValue("expedia.model.marketuser.beta2").toFloat

    
      /**
     * Process training set
     */
      def onClick(click: Click) = {


    val marketUserKey = (click.marketId, click.userId)
    if (clusterHistByMarketUser.getMap.contains(marketUserKey)) {
      
        val beta1 = hyperParamsService.getParamValueForMarketId("expedia.model.marketuser.beta1", click.marketId,hyperParams).toFloat
    val isBookingWeight = hyperParamsService.getParamValueForMarketId("expedia.model.marketuser.isBookingWeight", click.marketId,hyperParams).toFloat

      val decayFactor = hyperParamsService.getParamValueForMarketId("expedia.model.marketuser.decayFactor", click.marketId, hyperParams).toFloat
        val w = timeDecayService.getDecay(click.dateTime, decayFactor)
      if (click.isBooking == 1) clusterHistByMarketUser.add(marketUserKey, click.cluster, value = w * isBookingWeight)
      else clusterHistByMarketUser.add(marketUserKey, click.cluster, value = w * beta1)
    }

    }
    trainDatasource.foreach { click => onClick(click) }
    
     /**
     * Build model
     */
    
      clusterHistByMarketUser.getMap.foreach {
      case ((marketId, userId), clusterCounts) =>
        val beta3 = hyperParamsService.getParamValueForMarketId("expedia.model.marketuser.beta3", marketId,hyperParams).toFloat

        //        if (countryUserModel.predictionExists(countryByMarket(marketId), userId)) {
        //          clusterCounts :+= beta3 * (beta2 * marketModel.predict(marketId) + (1 - beta2) * countryUserModel.predict(countryByMarket(marketId), userId)) 
        //        } else clusterCounts :+= beta3* marketModel.predict(marketId) 

        clusterCounts :+= beta3 * marketModel.predict(marketId)
    }
    clusterHistByMarketUser.normalise()

    MarketUserModel(clusterHistByMarketUser,marketModel)
  }
}
object MarketUserModelBuilder2 extends ClusterModelBuilderFactory {

  def build(trainDatasource: ExDataSource, testClicks: Seq[Click], modelHyperParamsMap: CompoundHyperParamsMap): MarketUserModelBuilder2 = {
   
      val timeDecayService = TimeDecayService(testClicks)
    val hyperParamsService = HyperParamsService(testClicks)
    
       val marketModel = MarketModelBuilder2.build(trainDatasource,testClicks,  modelHyperParamsMap)
       .create(trainDatasource, testClicks, modelHyperParamsMap.getModel("market"))
   
    
    MarketUserModelBuilder2(marketModel,timeDecayService,hyperParamsService)
  }
}