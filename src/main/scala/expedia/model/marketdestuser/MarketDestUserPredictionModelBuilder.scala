package expedia.model.marketdestuser

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.Seq
import scala.collection.mutable
import com.typesafe.scalalogging.slf4j.LazyLogging
import breeze.linalg.InjectNumericOps
import breeze.linalg.sum
import expedia.data.Click
import expedia.data.ExDataSource
import expedia.model.country.CountryModel
import expedia.model.country.CountryModelBuilder
import expedia.model.countryuser.CountryUserModel
import expedia.model.countryuser.CountryUserModelBuilder
import expedia.model.dest.DestModel
import expedia.model.dest.DestModelBuilder
import expedia.model.marketmodel.MarketModel
import expedia.model.marketmodel.MarketModelBuilder
import expedia.model.regdest.RegDestModel
import expedia.model.regdest.RegDestModelBuilder
import expedia.stats.CounterMap
import expedia.stats.MulticlassHistByKey
import expedia.stats.OnlineAvg
import expedia.model.marketdest.MarketDestModel
import expedia.model.marketdest.MarketDestModelBuilder
import dk.bayes.math.linear.isIdentical
import expedia.model.clusterdistprox.ClusterDistProxModel
import expedia.model.clusterdistprox.ClusterDistProxModelBuilder
import expedia.model.destbydist.DestByDistModel
import expedia.model.destbydist.DestByDistModelBuilder
import expedia.model.destbydist.DestByDistModelBuilder
import expedia.model.marketuser.MarketUserModel
import expedia.model.marketuser.MarketUserModelBuilder
import expedia.model.marketuser.MarketUserModelBuilder
import expedia.model.ClusterModel
import expedia.HyperParams

/**
 * @param trainData mat[userId,dest,cluster]
 */
case class MarketDestUserPredictionModelBuilder(testClicks: Seq[Click], hyperParams: HyperParams) extends LazyLogging {

  //key ((destId, marketId,userId)
  private val clusterHistByDestMarketUser = MulticlassHistByKey[Tuple3[Int, Int, Int]](100)
  testClicks.foreach { click =>
    clusterHistByDestMarketUser.add((click.destId, click.marketId, click.userId), click.cluster, value = 0)
  }

  private val countryByMarket: mutable.Map[Int, Int] = mutable.Map()
  testClicks.foreach(click => countryByMarket += click.marketId -> click.countryId)

  private val marketUserCounterMap = CounterMap[Tuple2[Int, Int]]()
  private val userCounterMap = CounterMap[Int]()

  def processCluster(click: Click) = {

    val key = (click.destId, click.marketId, click.userId)
    if (clusterHistByDestMarketUser.getMap.contains(key)) {
      if (click.isBooking == 1) clusterHistByDestMarketUser.add(key, click.cluster)
      else clusterHistByDestMarketUser.add(key, click.cluster, value = 0.6f)
    }

    userCounterMap.add(click.userId)
    marketUserCounterMap.add((click.marketId, click.userId))

  }

  private val beta1 = hyperParams.getParamValue("expedia.model.marketdestuser.beta1").toFloat
  private val beta2 = hyperParams.getParamValue("expedia.model.marketdestuser.beta2").toFloat
  private val beta3 = hyperParams.getParamValue("expedia.model.marketdestuser.beta3").toFloat
  private val beta4 = hyperParams.getParamValue("expedia.model.marketdestuser.beta4").toFloat
  private val beta5 = hyperParams.getParamValue("expedia.model.marketdestuser.beta5").toFloat

  def create( countryModel: CountryModel, destMarketCounterMap: CounterMap[Tuple2[Int, Int]],
             destCounterMap: CounterMap[Int], marketCounterMap: CounterMap[Int],
            marketModel: MarketModel,
             countryUserModel: CountryUserModel, marketDestModel: MarketDestModel,
             marketUserModel: MarketUserModel): MarketDestUserPredictionModel = {

    logger.info("Add prior stats to clusterHistByDestMarketUser...")

    clusterHistByDestMarketUser.getMap.foreach {

      case ((destId, marketId, userId), userClusterProbs) =>

        val marketCounts = marketCounterMap.getOrElse(marketId, 0)
        val destMarketCounts = destMarketCounterMap.getOrElse((destId, marketId), 0)
        val destCounts = destCounterMap.getOrElse(destId, 0)


        if (destMarketCounts < beta3) {
          userClusterProbs :+= beta4 * (beta2 * marketDestModel.predict(marketId, destId) + (1 - beta2) * marketUserModel.predict(marketId, userId))
        } else {
          val marketUserCounts = marketUserCounterMap.getOrElse((marketId, userId), 0)
          if (marketUserCounts == 0 && countryUserModel.predictionExists(countryByMarket(marketId), userId)) {
            userClusterProbs :+= beta1 * marketDestModel.predict(marketId, destId) + (1 - beta1) * countryUserModel.predict(countryByMarket(marketId), userId)
          } else {
            userClusterProbs :+= beta5 * (beta2 * marketDestModel.predict(marketId, destId) + (1 - beta2) * marketUserModel.predict(marketId, userId))
          }

        }

    }
    clusterHistByDestMarketUser.normalise()
    logger.info("Add prior stats to clusterHistByDestMarketUser...done")

    MarketDestUserPredictionModel( clusterHistByDestMarketUser.getMap)
  }

}

object MarketDestUserPredictionModelBuilder {

  def buildFromTrainingSet(trainDatasource: ExDataSource, testClicks: Seq[Click], hyperParams: HyperParams): MarketDestUserPredictionModel = {

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
     * Create models
     */
    val marketModelBuilder = MarketModelBuilder(testClicks,hyperParams)
    val destModelBuilder = DestModelBuilder(testClicks,hyperParams)
    val countryModelBuilder = CountryModelBuilder(testClicks,hyperParams)

    val countryUserModelBuilder = CountryUserModelBuilder(testClicks,hyperParams)

    val marketDestModelBuilder = MarketDestModelBuilder(testClicks, destMarketCounterMap, destCounterMap, marketCounterMap, hyperParams)

    val marketUserModelBuilder = MarketUserModelBuilder(testClicks,hyperParams)
    val modelBuilder = MarketDestUserPredictionModelBuilder(testClicks, hyperParams)

    def onClick(click: Click) = {

      marketModelBuilder.processCluster(click)
      destModelBuilder.processCluster(click)
      countryModelBuilder.processCluster(click)
      countryUserModelBuilder.processCluster(click)
      marketDestModelBuilder.processCluster(click)
      marketUserModelBuilder.processCluster(click)
      modelBuilder.processCluster(click)

    }
    trainDatasource.foreach { click => onClick(click) }

    val countryModel = countryModelBuilder.create()
    val marketModel = marketModelBuilder.create(countryModel)
    val countryUserModel = countryUserModelBuilder.create(countryModel)
    val destModel = destModelBuilder.create(countryModel)
    val marketUserModel = marketUserModelBuilder.create(countryUserModel, marketModel)

    val marketDestModel = marketDestModelBuilder.create(destModel, marketModel, countryModel, destMarketCounterMap, destCounterMap, marketCounterMap)
    modelBuilder.create( countryModel, destMarketCounterMap, destCounterMap, marketCounterMap,  marketModel,
      countryUserModel, marketDestModel, marketUserModel)

  }
}