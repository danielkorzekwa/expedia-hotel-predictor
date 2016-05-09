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

  def processCluster(click: Click) = {

    clusterHistMarket.add(click.marketId, click.cluster)

    clusterHistByDestMarket.add((click.destId, click.marketId), click.cluster)

    countryByMarket += click.marketId -> click.countryId
    continentByDest += click.destId -> click.continentId

    val destCustAvgStayDays = avgDaysStayByDestCust.getOrElseUpdate((click.destId, click.userId), OnlineAvg())
    destCustAvgStayDays.add(click.stayDays)

    if (userIds.isEmpty || userIds.contains(click.userId)) {

      if (click.isBooking == 1) clusterHistByDestMarketUser.add((click.destId, click.marketId, click.userId), click.cluster)
      else clusterHistByDestMarketUser.add((click.destId, click.marketId, click.userId), click.cluster, value = 0.7f)

    }
  }

  def create(destModel: DestModel, countryModel: CountryModel, destMarketCounterMap: CounterMap[Tuple2[Int, Int]], destCounterMap: CounterMap[Int], marketCounterMap: CounterMap[Int]): MarketDestPredictionModel = {

    clusterHistMarket.getMap.foreach { case (marketId, clusterCounts) => clusterCounts :+= countryModel.predict(countryByMarket(marketId)) }
    clusterHistMarket.getMap.foreach { case (key, stats) => calcVectorProbsMutable(stats) }

    logger.info("Add prior stats to clusterHistByDestMarket...")
    clusterHistByDestMarket.getMap.foreach {
      case ((destId, marketId), clusterProbs) =>
        val destMarketCounts = destMarketCounterMap.getOrElse((destId, marketId), 0)
        val destCounts = destCounterMap.getOrElse(destId, 0)
        val marketCounts = marketCounterMap.getOrElse(marketId, 0)

        if (destMarketCounts > 0 && destCounts > 0 && destCounts == destMarketCounts) clusterProbs :+= 1f * clusterHistMarket.getMap(marketId)
        else if (destMarketCounts > 0 && destCounts > 0 && marketCounts == destMarketCounts) clusterProbs :+= 1f * destModel.predict(destId, continentByDest(destId))
        else clusterProbs :+= 1f * clusterHistMarket.getMap(marketId)
    }
    logger.info("Add prior stats to clusterHistByDestMarket...done")

    logger.info("Normalise clusterHistByDestMarket...")
    clusterHistByDestMarket.getMap.foreach { case (key, stats) => calcVectorProbsMutable(stats) }
    logger.info("Normalise clusterHistByDestMarket...done")

    logger.info("Add prior stats to clusterHistByDestMarketUser...")

    val bigDestsCounter = CounterMap[Int]()

    clusterHistByDestMarketUser.getMap.foreach {
      case ((destId, marketId, userId), clusterProbs) =>
        val destMarketCounts = destMarketCounterMap.getOrElse((destId, marketId), 0)
        val destCounts = destCounterMap.getOrElse(destId, 0)

        if (destMarketCounts < 300 || destCounts / destMarketCounts > 1.5) {
          clusterProbs :+= 1f * clusterHistByDestMarket.getMap((destId, marketId))
        } else {
          val avgStayDays = avgDaysStayByDestCust.get((destId, userId)) match {
            case Some(avgStayDays) if destModel.svmDestIds.contains(destId) => {
              val custStayDays = avgStayDays.avg().toInt
              clusterProbs :+= 10f * destModel.predict(destId, continentByDest(destId), custStayDays)
            }
            case _ => clusterProbs :+= 10f * destModel.predict(destId, continentByDest(destId))
          }
        }

    }

    logger.info("Add prior stats to clusterHistByDestMarketUser...done")

    logger.info("Normalise clusterHistByDestMarketUser...")
    clusterHistByDestMarketUser.getMap.foreach { case (key, stats) => calcVectorProbsMutable(stats) }
    logger.info("Normalise clusterHistByDestMarketUser...done")

    MarketDestPredictionModel(destModel, clusterHistByDestMarketUser.getMap, clusterHistByDestMarket.getMap)
  }

}