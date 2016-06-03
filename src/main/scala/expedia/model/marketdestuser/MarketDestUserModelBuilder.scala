package expedia.model.marketdestuser

import expedia.CompoundHyperParams
import expedia.CompoundHyperParamsMap
import expedia.data.Click
import expedia.data.ExDataSource
import expedia.model.ClusterModelBuilder
import expedia.model.ClusterModelBuilderFactory
import expedia.HyperParamsService
import expedia.util.TimeDecayService
import scala.collection._
import expedia.stats.CounterMap
import expedia.stats.MulticlassHistByKey
import com.typesafe.scalalogging.slf4j.LazyLogging
import expedia.model.marketdest.MarketDestModel
import expedia.model.marketdest.MarketDestModelBuilder2
import expedia.model.marketdest.MarketDestModelBuilder2

case class MarketDestUserModelBuilder(marketDestModel: MarketDestModel, timeDecayService: TimeDecayService, hyperParamsService: HyperParamsService)
    extends ClusterModelBuilder with LazyLogging {

  def create(trainDatasource: ExDataSource, testClicks: Seq[Click], hyperParams: CompoundHyperParams): MarketDestUserPredictionModel = {

    //key ((destId, marketId,userId)
    val clusterHistByDestMarketUser = MulticlassHistByKey[Tuple3[Int, Int, Int]](100)
    testClicks.foreach { click =>
      clusterHistByDestMarketUser.add((click.destId, click.marketId, click.userId), click.cluster, value = 0)
    }

    val countryByMarket: mutable.Map[Int, Int] = mutable.Map()
    testClicks.foreach(click => countryByMarket += click.marketId -> click.countryId)

    val marketUserCounterMap = CounterMap[Tuple2[Int, Int]]()
    val userCounterMap = CounterMap[Int]()

    /**
     * Create counters
     */
    val destMarketCounterMap = CounterMap[Tuple2[Int, Int]]
    val destCounterMap = CounterMap[Int]()
    val marketCounterMap = CounterMap[Int]()
    def onClickCounters(click: Click) = {
      if (click.isBooking == 1) {
        destMarketCounterMap.add((click.destId, click.marketId))
        destCounterMap.add(click.destId)
        marketCounterMap.add(click.marketId)
      }
    }
    trainDatasource.foreach { click => onClickCounters(click) }

    /**
     * Process training set
     */
    def onClick(click: Click) = {
      val key = (click.destId, click.marketId, click.userId)
      if (clusterHistByDestMarketUser.getMap.contains(key)) {

        val isBookingWeight = hyperParamsService.getParamValueForMarketId("expedia.model.marketdestuser.isBookingWeight", click.marketId, hyperParams).toFloat
        val beta6 = hyperParamsService.getParamValueForMarketId("expedia.model.marketdestuser.beta6", click.marketId, hyperParams).toFloat

        val decayFactor = hyperParamsService.getParamValueForMarketId("expedia.model.marketdestuser.decayFactor", click.marketId, hyperParams).toFloat
        val w = timeDecayService.getDecay(click.dateTime, decayFactor)

        if (click.isBooking == 1) clusterHistByDestMarketUser.add(key, click.cluster, value = w * isBookingWeight)
        else clusterHistByDestMarketUser.add(key, click.cluster, value = w * beta6)
      }

      userCounterMap.add(click.userId)
      marketUserCounterMap.add((click.marketId, click.userId))

    }
    trainDatasource.foreach { click => onClick(click) }

    /**
     * Build model
     */

    logger.info("Add prior stats to clusterHistByDestMarketUser...")

    clusterHistByDestMarketUser.getMap.foreach {

      case ((destId, marketId, userId), userClusterProbs) =>

        //    private val beta1 = hyperParams.getParamValue("expedia.model.marketdestuser.beta1").toFloat
        //private val beta2 = hyperParams.getParamValue("expedia.model.marketdestuser.beta2").toFloat
        //private val beta3 = hyperParams.getParamValue("expedia.model.marketdestuser.beta3").toFloat
        //private val beta4 = hyperParams.getParamValue("expedia.model.marketdestuser.beta4").toFloat
        val beta5 = hyperParamsService.getParamValueForMarketId("expedia.model.marketdestuser.beta5", marketId, hyperParams).toFloat

        val marketCounts = marketCounterMap.getOrElse(marketId, 0)
        val destMarketCounts = destMarketCounterMap.getOrElse((destId, marketId), 0)
        val destCounts = destCounterMap.getOrElse(destId, 0)

        val (prior, factor) = (marketDestModel.predict(marketId, destId), beta5)

        //        val (prior, factor) = if (destMarketCounts < beta3) {
        //          val prior = (beta2 * marketDestModel.predict(marketId, destId) + (1 - beta2) * marketUserModel.predict(marketId, userId))
        //          val factor = beta4
        //          (prior, beta4)
        //        } else {
        //          val marketUserCounts = marketUserCounterMap.getOrElse((marketId, userId), 0)
        //          if (marketUserCounts == 0 && countryUserModel.predictionExists(countryByMarket(marketId), userId)) {
        //            val prior =  beta1 * marketDestModel.predict(marketId, destId) + (1 - beta1) * countryUserModel.predict(countryByMarket(marketId), userId)
        //            val factor=1f
        //            (prior,factor)
        //          } else {
        //            val prior = (beta2 * marketDestModel.predict(marketId, destId) + (1 - beta2) * marketUserModel.predict(marketId, userId))
        //            val factor = beta5
        //            (prior,factor)
        //          }
        //
        //        }

        userClusterProbs :+= factor * prior

    }
    clusterHistByDestMarketUser.normalise()
    logger.info("Add prior stats to clusterHistByDestMarketUser...done")

    MarketDestUserPredictionModel(clusterHistByDestMarketUser.getMap,marketDestModel)
  }
}
object MarketDestUserModelBuilder extends ClusterModelBuilderFactory {

  def build(trainDatasource: ExDataSource, testClicks: Seq[Click], modelHyperParamsMap: CompoundHyperParamsMap): MarketDestUserModelBuilder = {

    val timeDecayService = TimeDecayService(testClicks)
    val hyperParamsService = HyperParamsService(testClicks)

    val marketDestModel = MarketDestModelBuilder2.build(trainDatasource, testClicks, modelHyperParamsMap)
      .create(trainDatasource, testClicks, modelHyperParamsMap.getModel("marketdest"))

    MarketDestUserModelBuilder(marketDestModel, timeDecayService, hyperParamsService)
  }
}