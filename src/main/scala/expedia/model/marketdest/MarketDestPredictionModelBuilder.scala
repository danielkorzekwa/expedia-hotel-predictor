package expedia.model.marketdest

import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix
import breeze.linalg._
import com.typesafe.scalalogging.slf4j.LazyLogging
import java.util.concurrent.atomic.AtomicInteger
import scala.collection._
import expedia.model.svm.loadClusterProbsByDestMap
import expedia.model.svm.SVMPredictionModel
import expedia.stats.CatStats
import expedia.stats.calcVectorProbsMutable
import expedia.stats.calcVectorMapProbsMutable
import expedia.stats.MulticlassHistByKey
import expedia.data.Click
import expedia.stats.MulticlassHistByKey
import expedia.stats.CounterMap
import expedia.model.dest.DestModel
import expedia.model.country.CountryModel
import expedia.stats.CounterMap
import expedia.stats.OnlineAvg
import expedia.model.clusterdist.ClusterDistPredictionModel
import expedia.model.clusterdistprox.ClusterDistProxModel
import expedia.model.country.CountryModelBuilder
import expedia.data.ExDataSource
import expedia.model.clusterdistprox.ClusterDistProxModelBuilder
import expedia.model.dest.DestModelBuilder

/**
 * @param trainData mat[userId,dest,cluster]
 */
case class MarketDestPredictionModelBuilder(testClicks: Seq[Click]) extends LazyLogging {

  private val userIds = testClicks.map { c => c.userId }.distinct.toSet

  //key: cont/market
  private val clusterHistMarket = MulticlassHistByKey[Int](100)
  testClicks.foreach(click => clusterHistMarket.add(click.marketId, click.cluster, value = 0))

  //key ((destId, marketId)
  private val clusterHistByDestMarket = MulticlassHistByKey[Tuple2[Int, Int]](100)
  testClicks.foreach(click => clusterHistByDestMarket.add((click.destId, click.marketId), click.cluster, value = 0))

  //key ((destId, marketId,userId)
  private val clusterHistByDestMarketUser = MulticlassHistByKey[Tuple3[Int, Int, Int]](100)
  testClicks.foreach(click => clusterHistByDestMarketUser.add((click.destId, click.marketId, click.userId), click.cluster, value = 0))

  private val continentByDest: mutable.Map[Int, Int] = mutable.Map()
  testClicks.foreach(click => continentByDest += click.destId -> click.continentId)

  private val countryByMarket: mutable.Map[Int, Int] = mutable.Map()
  testClicks.foreach(click => countryByMarket += click.marketId -> click.countryId)

  private val avgDaysStayByDestCust: mutable.Map[Tuple2[Int, Int], OnlineAvg] = mutable.Map()

  private val userCounterMap = CounterMap[Int]()

  def processCluster(click: Click) = {

    if (clusterHistMarket.getMap.contains(click.marketId)) {
      if (click.isBooking == 1) clusterHistMarket.add(click.marketId, click.cluster)
      else clusterHistMarket.add(click.marketId, click.cluster, value = 0.40f)
    }
    
    if (clusterHistByDestMarket.getMap.contains((click.destId, click.marketId))) {
      if (click.isBooking == 1) clusterHistByDestMarket.add((click.destId, click.marketId), click.cluster)
      else clusterHistByDestMarket.add((click.destId, click.marketId), click.cluster, value = 0.50f)
    }

    countryByMarket += click.marketId -> click.countryId
    continentByDest += click.destId -> click.continentId

    val destCustAvgStayDays = avgDaysStayByDestCust.getOrElseUpdate((click.destId, click.userId), OnlineAvg())
    destCustAvgStayDays.add(click.stayDays)

//    val key = (click.destId, click.marketId, click.userId)
//    if (clusterHistByDestMarketUser.getMap.contains(key)) {
//      if (click.isBooking == 1) clusterHistByDestMarketUser.add((click.destId, click.marketId, click.userId), click.cluster)
//      else clusterHistByDestMarketUser.add((click.destId, click.marketId, click.userId), click.cluster, value = 0.6f)
//    }

    userCounterMap.add(click.userId)
  }

  def create(destModel: DestModel, countryModel: CountryModel, destMarketCounterMap: CounterMap[Tuple2[Int, Int]],
             destCounterMap: CounterMap[Int], marketCounterMap: CounterMap[Int],
             clusterDistProxModel: ClusterDistProxModel): MarketDestPredictionModel = {

    clusterHistMarket.getMap.foreach { case (marketId, clusterCounts) => clusterCounts :+= countryModel.predict(countryByMarket(marketId)) }
    clusterHistMarket.getMap.foreach { case (key, stats) => calcVectorProbsMutable(stats) }

    logger.info("Add prior stats to clusterHistByDestMarket...")
    clusterHistByDestMarket.getMap.foreach {
      case ((destId, marketId), clusterProbs) =>
        val destMarketCounts = destMarketCounterMap.getOrElse((destId, marketId), 0)
        val destCounts = destCounterMap.getOrElse(destId, 0)
        val marketCounts = marketCounterMap.getOrElse(marketId, 0)

        if (destMarketCounts > 0 && destCounts > 0 && destCounts == destMarketCounts) clusterProbs :+= 1f * clusterHistMarket.getMap(marketId)
        else if (destMarketCounts > 0 && destCounts > 0 && marketCounts == destMarketCounts) clusterProbs :+= 5f * destModel.predict(destId, continentByDest(destId))
        else clusterProbs :+= 1f * clusterHistMarket.getMap(marketId)
    }
    logger.info("Add prior stats to clusterHistByDestMarket...done")

    logger.info("Normalise clusterHistByDestMarket...")
    clusterHistByDestMarket.getMap.foreach { case (key, stats) => calcVectorProbsMutable(stats) }
    logger.info("Normalise clusterHistByDestMarket...done")

    logger.info("Add prior stats to clusterHistByDestMarketUser...")

    val bigDestsCounter = CounterMap[Int]()

    clusterHistByDestMarketUser.getMap.foreach {
      case ((destId, marketId, userId), userClusterProbs) =>
        
        val destMarketCounts = destMarketCounterMap.getOrElse((destId, marketId), 0)
        val destCounts = destCounterMap.getOrElse(destId, 0)

        if (destMarketCounts < 300 || destCounts.toDouble / destMarketCounts > 1.3) {

          userClusterProbs :+= 4f * clusterHistByDestMarket.getMap((destId, marketId))
        }
        else {
          val avgStayDays = avgDaysStayByDestCust.get((destId, userId)) match {
            case Some(avgStayDays) if(destModel.svmDestIds.contains(destId) && avgStayDays.avg().toInt<3)  => {
              val custStayDays = avgStayDays.avg().toInt
              userClusterProbs :+= 7f * destModel.predict(destId, continentByDest(destId), custStayDays)
            }
            case _ => userClusterProbs :+= 7f * destModel.predict(destId, continentByDest(destId))
          }
        }

    }

    logger.info("Add prior stats to clusterHistByDestMarketUser...done")

    logger.info("Normalise clusterHistByDestMarketUser...")
    clusterHistByDestMarketUser.getMap.foreach { case (key, stats) => calcVectorProbsMutable(stats) }
    logger.info("Normalise clusterHistByDestMarketUser...done")

    MarketDestPredictionModel(destModel, clusterHistByDestMarketUser.getMap, clusterHistByDestMarket.getMap, clusterDistProxModel, userCounterMap, destCounterMap, destMarketCounterMap)
  }

}

object MarketDestPredictionModelBuilder {

  def buildFromTrainingSet(trainDatasource: ExDataSource, testClicks: Seq[Click]): MarketDestPredictionModel = {
    val clusterDistProxModelBuilder = ClusterDistProxModelBuilder(testClicks)

    val destModelBuilder = DestModelBuilder(testClicks)
    val countryModelBuilder = CountryModelBuilder(testClicks)
    val modelBuilder = MarketDestPredictionModelBuilder(testClicks)

    val destMarketCounterMap = CounterMap[Tuple2[Int, Int]]
    val destCounterMap = CounterMap[Int]()
    val marketCounterMap = CounterMap[Int]()

    def onClick(click: Click) = {
      clusterDistProxModelBuilder.processCluster(click)

      destModelBuilder.processCluster(click)
      countryModelBuilder.processCluster(click)
      modelBuilder.processCluster(click)

      if (click.isBooking == 1) {
        destMarketCounterMap.add((click.destId, click.marketId))
        destCounterMap.add(click.destId)
        marketCounterMap.add(click.marketId)
      }
    }
    trainDatasource.foreach { click => onClick(click) }

    val clusterDistProxModel = clusterDistProxModelBuilder.create()
    val countryModel = countryModelBuilder.create()
    val destModel = destModelBuilder.create(countryModel)
    modelBuilder.create(destModel, countryModel, destMarketCounterMap, destCounterMap, marketCounterMap, clusterDistProxModel)
  }
}