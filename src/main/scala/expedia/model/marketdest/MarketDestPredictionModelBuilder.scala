package expedia.model.marketdest

import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix
import breeze.linalg._
import com.typesafe.scalalogging.slf4j.LazyLogging
import java.util.concurrent.atomic.AtomicInteger
import scala.collection._
import expedia.model.svm.loadClusterProbsByDestMap
import expedia.model.svm.SVMPredictionModel
import expedia.stats.calcVectorProbsMutable
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
import expedia.model.regdest.RegDestModel
import expedia.model.regdest.RegDestModel
import expedia.model.regdest.RegDestModelBuilder
import expedia.model.marketmodel.MarketModel
import expedia.model.marketmodel.MarketModelBuilder

/**
 * @param trainData mat[userId,dest,cluster]
 */
case class MarketDestPredictionModelBuilder(testClicks: Seq[Click]) extends LazyLogging {

  private val userIds = testClicks.map { c => c.userId }.distinct.toSet

  //key ((destId, marketId)
  private val clusterHistByDestMarket = MulticlassHistByKey[Tuple2[Int, Int]](100)
  testClicks.foreach(click => clusterHistByDestMarket.add((click.destId, click.marketId), click.cluster, value = 0))

  //key ((destId, marketId,userId)
  private val clusterHistByDestMarketUser = MulticlassHistByKey[Tuple3[Int, Int, Int]](100)
  testClicks.foreach(click => clusterHistByDestMarketUser.add((click.destId, click.marketId, click.userId), click.cluster, value = 0))

  //key ((marketId,userId)
  private val clusterHistByMarketUser = MulticlassHistByKey[Tuple2[Int, Int]](100)
  testClicks.foreach(click => clusterHistByMarketUser.add((click.marketId, click.userId), click.cluster, value = 0))

  //key ((destId,userId)
  private val clusterHistByDestUser = MulticlassHistByKey[Tuple2[Int, Int]](100)
  testClicks.foreach(click => clusterHistByDestUser.add((click.destId, click.userId), click.cluster, value = 0))

   //key ((countryId,userId)
  private val clusterHistByCountryUser = MulticlassHistByKey[Tuple2[Int, Int]](100)
  testClicks.foreach(click => clusterHistByCountryUser.add((click.countryId, click.userId), click.cluster, value = 0))

  
  private val continentByDest: mutable.Map[Int, Int] = mutable.Map()
  testClicks.foreach(click => continentByDest += click.destId -> click.continentId)

  private val regionByUser: mutable.Map[Int, Int] = mutable.Map()
  testClicks.foreach(click => regionByUser += click.userId -> click.userRegion)

   private val countryByMarket: mutable.Map[Int, Int] = mutable.Map()
  testClicks.foreach(click => countryByMarket += click.marketId -> click.countryId)
  
  private val avgDaysStayByDestCust: mutable.Map[Tuple2[Int, Int], OnlineAvg] = mutable.Map()

  private val userCounterMap = CounterMap[Int]()

  def processCluster(click: Click) = {

    if (clusterHistByDestMarket.getMap.contains((click.destId, click.marketId))) {
      if (click.isBooking == 1) clusterHistByDestMarket.add((click.destId, click.marketId), click.cluster)
      else clusterHistByDestMarket.add((click.destId, click.marketId), click.cluster, value = 0.50f)
    }

    continentByDest += click.destId -> click.continentId
    regionByUser += click.userId -> click.userRegion

    val destCustAvgStayDays = avgDaysStayByDestCust.getOrElseUpdate((click.destId, click.userId), OnlineAvg())
    destCustAvgStayDays.add(click.stayDays)

    val key = (click.destId, click.marketId, click.userId)
    if (clusterHistByDestMarketUser.getMap.contains(key)) {
      if (click.isBooking == 1) clusterHistByDestMarketUser.add((click.destId, click.marketId, click.userId), click.cluster)
      else clusterHistByDestMarketUser.add((click.destId, click.marketId, click.userId), click.cluster, value = 0.6f)
    }

    val marketUserKey = (click.marketId, click.userId)
    if (clusterHistByMarketUser.getMap.contains(marketUserKey)) {
      if (click.isBooking == 1) clusterHistByMarketUser.add(marketUserKey, click.cluster)
      else clusterHistByMarketUser.add(marketUserKey, click.cluster, value = 0.6f)
    }

    val destUserKey = (click.destId, click.userId)
    if (clusterHistByDestUser.getMap.contains(destUserKey)) {
      if (click.isBooking == 1) clusterHistByDestUser.add(destUserKey, click.cluster)
      else clusterHistByDestUser.add(destUserKey, click.cluster, value = 0.6f)
    }
    
      val countryUserKey = (click.countryId, click.userId)
    if (clusterHistByCountryUser.getMap.contains(countryUserKey)) {
      if (click.isBooking == 1) clusterHistByCountryUser.add(countryUserKey, click.cluster)
      else clusterHistByCountryUser.add(countryUserKey, click.cluster, value = 0.6f)
    }

    userCounterMap.add(click.userId)
  }

  def create(destModel: DestModel, countryModel: CountryModel, destMarketCounterMap: CounterMap[Tuple2[Int, Int]],
             destCounterMap: CounterMap[Int], marketCounterMap: CounterMap[Int],
             regDestModel: RegDestModel, marketModel: MarketModel): MarketDestPredictionModel = {

    logger.info("Add prior stats to clusterHistByDestMarket...")
    clusterHistByDestMarket.getMap.foreach {
      case ((destId, marketId), clusterProbs) =>
        val destMarketCounts = destMarketCounterMap.getOrElse((destId, marketId), 0)
        val destCounts = destCounterMap.getOrElse(destId, 0)
        val marketCounts = marketCounterMap.getOrElse(marketId, 0)

        if (destMarketCounts > 0 && destCounts > 0 && destCounts == destMarketCounts) clusterProbs :+= 1f * marketModel.predict(marketId)
        else if (destMarketCounts > 0 && destCounts > 0 && marketCounts == destMarketCounts) clusterProbs :+= 5f * destModel.predict(destId)
        else clusterProbs :+= 1f * marketModel.predict(marketId)
    }
    logger.info("Add prior stats to clusterHistByDestMarket...done")

    logger.info("Normalise clusterHistByDestMarket...")
    clusterHistByDestMarket.getMap.foreach { case (key, stats) => calcVectorProbsMutable(stats) }
    logger.info("Normalise clusterHistByDestMarket...done")

    logger.info("Add prior stats to clusterHistByDestMarketUser...")

    val i = new AtomicInteger(0)

    clusterHistByDestMarketUser.getMap.foreach {

      case ((destId, marketId, userId), userClusterProbs) =>

        val marketCounts = marketCounterMap.getOrElse(marketId, 0)
        val destMarketCounts = destMarketCounterMap.getOrElse((destId, marketId), 0)
        val destCounts = destCounterMap.getOrElse(destId, 0)

        if (destMarketCounts < 300 || destCounts.toDouble / destMarketCounts > 1.3) {

          if (sum(userClusterProbs) == 0 && (destMarketCounts > 0 && marketCounts / destMarketCounts < 12)) {
            userClusterProbs :+= 4f * clusterHistByDestMarket.getMap((destId, marketId)) + clusterHistByMarketUser.getMap((marketId, userId))
          } else userClusterProbs :+= 4f * clusterHistByDestMarket.getMap((destId, marketId))

        } else {
          val avgStayDays = avgDaysStayByDestCust.get((destId, userId)) match {
            case Some(avgStayDays) if (destModel.svmDestIds.contains(destId) && avgStayDays.avg().toInt < 3 && !(regionByUser(userId) == 174 && destId == 8250 && marketId == 628)) => {
              val custStayDays = avgStayDays.avg().toInt
              userClusterProbs :+= 7f * destModel.predict(destId, continentByDest(destId), custStayDays)
            }
            case _ => userClusterProbs :+= {

              if (regDestModel.predictionExists(regionByUser(userId), destId)) 7f * regDestModel.predict(regionByUser(userId), destId)
              else {
                //println(marketCounts + ":" + destCounts + ":" + destMarketCounts)
                //   if (sum(userClusterProbs) == 0)  7f * destModel.predict(destId) +  clusterHistByDestUser.getMap((destId,userId))
                //   else 7f * destModel.predict(destId) 

                val marketUserCounts = clusterHistByMarketUser.getMap((marketId, userId))
                val destUserCounts = clusterHistByDestUser.getMap((destId, userId))
                if (sum(marketUserCounts) == 0) println(sum(marketUserCounts))

                if (sum(marketUserCounts) > 0)   7f * destModel.predict(destId) + marketUserCounts - userClusterProbs
                else if( clusterHistByCountryUser.getMap.contains((countryByMarket(marketId), userId))) 120f * destModel.predict(destId) + clusterHistByCountryUser.getMap((countryByMarket(marketId), userId)) - userClusterProbs
                else  7f * destModel.predict(destId) + marketUserCounts - userClusterProbs
              }
            }
          }
        }

    }

    logger.info("Add prior stats to clusterHistByDestMarketUser...done")

    logger.info("Normalise clusterHistByDestMarketUser...")
    clusterHistByDestMarketUser.getMap.foreach { case (key, stats) => calcVectorProbsMutable(stats) }
    logger.info("Normalise clusterHistByDestMarketUser...done")

    MarketDestPredictionModel(destModel, clusterHistByDestMarketUser.getMap, clusterHistByDestMarket.getMap, userCounterMap,
      destCounterMap, destMarketCounterMap, regDestModel)
  }

}

object MarketDestPredictionModelBuilder {

  def buildFromTrainingSet(trainDatasource: ExDataSource, testClicks: Seq[Click]): MarketDestPredictionModel = {

    val marketModelBuilder = MarketModelBuilder(testClicks)
    val destModelBuilder = DestModelBuilder(testClicks)
    val countryModelBuilder = CountryModelBuilder(testClicks)
    val regDestModelBuilder = RegDestModelBuilder()

    val modelBuilder = MarketDestPredictionModelBuilder(testClicks)

    val destMarketCounterMap = CounterMap[Tuple2[Int, Int]]
    val destCounterMap = CounterMap[Int]()
    val marketCounterMap = CounterMap[Int]()

    def onClick(click: Click) = {

      marketModelBuilder.processCluster(click)
      destModelBuilder.processCluster(click)
      countryModelBuilder.processCluster(click)
      regDestModelBuilder.processCluster(click)
      modelBuilder.processCluster(click)

      if (click.isBooking == 1) {
        destMarketCounterMap.add((click.destId, click.marketId))
        destCounterMap.add(click.destId)
        marketCounterMap.add(click.marketId)
      }
    }
    trainDatasource.foreach { click => onClick(click) }

    val countryModel = countryModelBuilder.create()
    val marketModel = marketModelBuilder.create(countryModel)
    val regDestModel = regDestModelBuilder.create()

    val destModel = destModelBuilder.create(countryModel)
    modelBuilder.create(destModel, countryModel, destMarketCounterMap, destCounterMap, marketCounterMap, regDestModel, marketModel)
  }
}