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

/**
 * @param trainData mat[userId,dest,cluster]
 */
case class MarketDestPredictionModelBuilder(svmPredictionsData: DenseMatrix[Double],userIds: Set[Int]) extends LazyLogging {

  val clusterStatMap = CatStats()

  val clusterHistByMarket = MulticlassHistByKey[Int](100)

  //key ((destId, marketId)
  val clusterHistByDestMarket = MulticlassHistByKey[Tuple2[Int, Int]](100)

  //key ((destId, marketId,userId)
  val clusterHistByDestMarketUser = MulticlassHistByKey[Tuple3[Int, Int, Int]](100)
  
   //key - (destId,marketId,userId)
  val clusterHistByDestMarketUser2 = MulticlassHistByKey[Tuple3[Int,Int, Int]](100)

  val continentByDest: mutable.Map[Int, Int] = mutable.Map()

  def processCluster(click: Click) = {
    clusterStatMap.add(click.cluster)
    clusterHistByMarket.add(click.market, click.cluster)

    clusterHistByDestMarket.add((click.destId, click.market), click.cluster)

    continentByDest += click.destId -> click.hotelContinent
    
     if (userIds.isEmpty || userIds.contains(click.userId)) {
      if (click.isBooking == 1) clusterHistByDestMarketUser2.add((click.destId, click.market,click.userId), click.cluster)
      else clusterHistByDestMarketUser2.add((click.destId, click.market,click.userId), click.cluster, value = 0.7f)
      
        if (click.isBooking == 1) clusterHistByDestMarketUser.add((click.destId, click.market,click.userId), click.cluster)
      else clusterHistByDestMarketUser.add((click.destId, click.market,click.userId), click.cluster, value = 0.7f)
      
    }
  }

  def create(destModel: DestModel, destMarketCounterMap: CounterMap[Tuple2[Int, Int]], destCounterMap: CounterMap[Int], marketCounterMap: CounterMap[Int]): MarketDestPredictionModel = {
    calcVectorProbsMutable(clusterStatMap.getItemVec)

    clusterHistByMarket.getMap().foreach { case (marketId, clusterCounts) => clusterCounts :+= clusterStatMap.getItemVec }
    calcVectorMapProbsMutable(clusterHistByMarket.getMap().toMap)

    logger.info("Add prior stats to clusterHistByDestMarket...")

    clusterHistByDestMarket.getMap().foreach {
      case ((destId, marketId), clusterProbs) =>
        val destMarketCounts = destMarketCounterMap.getOrElse((destId, marketId), 0)
        val destCounts = destCounterMap.getOrElse(destId, 0)
        val marketCounts = marketCounterMap.getOrElse(marketId, 0)

        if (destMarketCounts > 0 && destCounts > 0 && destCounts == destMarketCounts) clusterProbs :+= 1f * clusterHistByMarket.getMap()(marketId)
        else if (destMarketCounts > 0 && destCounts > 0 && marketCounts == destMarketCounts) clusterProbs :+= 1f * destModel.predict(destId, continentByDest(destId)) //clusterHistByDest.getMap()(destId)
        else clusterProbs :+= 1f * clusterHistByMarket.getMap()(marketId)
    }

    logger.info("Add prior stats to clusterHistByDestMarket...done")

    logger.info("Normalise clusterHistByDestMarket...")
    clusterHistByDestMarket.getMap.foreach { case (key, stats) => calcVectorProbsMutable(stats) }
    logger.info("Normalise clusterHistByDestMarket...done")

    
    
    logger.info("Add prior stats to clusterHistByDestMarketUser...")
    clusterHistByDestMarketUser.getMap().foreach {       case ((destId, marketId, userId), clusterProbs) =>
          val destMarketCounts = destMarketCounterMap.getOrElse((destId, marketId), 0)
           val destCounts = destCounterMap.getOrElse(destId, 0)
        if (destMarketCounts < 300) {
           clusterProbs :+= 1f * clusterHistByDestMarket.getMap()((destId, marketId)) 
        } else if (destCounts / destMarketCounts > 1.5) {
           clusterProbs :+= 1f * clusterHistByDestMarket.getMap()((destId, marketId)) 

        } else  clusterProbs :+= 10f * destModel.predict(destId, continentByDest(destId))
      
    }
    logger.info("Add prior stats to clusterHistByDestMarketUser...done")

    logger.info("Normalise clusterHistByDestMarketUser...")
    clusterHistByDestMarketUser.getMap.foreach { case (key, stats) => calcVectorProbsMutable(stats) }
    logger.info("Normalise clusterHistByDestMarketUser...done")

    
    
    
    
     logger.info("Calc clusterHistByDestMarketUser2 stats...")
    clusterHistByDestMarketUser2.getMap().foreach {
      case ((destId, marketId,userId), clusterProbs) =>
        clusterProbs :+= 10f * destModel.predict(destId, continentByDest(destId))

    }
    logger.info("Calc clusterHistByDestMarketUser2 stats...done")
    
    logger.info("Calc clusterHistByDestMarketUser2 probs...")
    clusterHistByDestMarketUser2.getMap.foreach { case (key, stats) => calcVectorProbsMutable(stats) }
    logger.info("Calc clusterHistByDestMarketUser2 probs...done")

    MarketDestPredictionModel(destModel, clusterHistByDestMarketUser.getMap(), clusterHistByDestMarket.getMap(),clusterHistByDestMarketUser2)
  }

}