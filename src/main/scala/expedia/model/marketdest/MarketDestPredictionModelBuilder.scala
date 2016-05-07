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
case class MarketDestPredictionModelBuilder(svmPredictionsData: DenseMatrix[Double]) extends LazyLogging {

  val clusterStatMap = CatStats()

  val clusterHistByContinent = MulticlassHistByKey[Int](100)

  val clusterHistByDest = MulticlassHistByKey[Int](100)
  val clusterHistByMarket = MulticlassHistByKey[Int](100)

  //key ((destId, marketId)
  val clusterHistByDestMarket = MulticlassHistByKey[Tuple2[Int, Int]](100)

  //key ((destId, marketId,userId)
  val clusterHistByDestMarketUser = MulticlassHistByKey[Tuple3[Int, Int, Int]](100)

  val clusterProbByDestMapSVM: Map[Int, DenseVector[Float]] = loadClusterProbsByDestMap(svmPredictionsData)

  val continentByDest: mutable.Map[Int, Int] = mutable.Map()

  def processCluster(click: Click) = {
    clusterStatMap.add(click.cluster)
    clusterHistByContinent.add(click.hotelContinent, click.cluster)
    clusterHistByDest.add(click.destId, click.cluster)
    clusterHistByMarket.add(click.market, click.cluster)

    clusterHistByDestMarket.add((click.destId, click.market), click.cluster)
    clusterHistByDestMarketUser.add((click.destId, click.market, click.userId), click.cluster)

    continentByDest += click.destId -> click.hotelContinent
  }

  def create(destModel:DestModel,destMarketCounterMap: CounterMap[Tuple2[Int, Int]], destCounterMap: CounterMap[Int], marketCounterMap: CounterMap[Int]): MarketDestPredictionModel = {
    calcVectorProbsMutable(clusterStatMap.getItemVec)

    calcVectorMapProbsMutable(clusterHistByContinent.getMap().toMap)

    clusterHistByDest.getMap().foreach { case (destId, clusterCounts) => clusterCounts :+= clusterProbByDestMapSVM.getOrElse(destId, clusterHistByContinent.getMap.getOrElse(continentByDest(destId), clusterStatMap.getItemVec)) }
    calcVectorMapProbsMutable(clusterHistByDest.getMap().toMap)

    clusterHistByMarket.getMap().foreach { case (marketId, clusterCounts) => clusterCounts :+= clusterStatMap.getItemVec }
    calcVectorMapProbsMutable(clusterHistByMarket.getMap().toMap)

    logger.info("Add prior stats to clusterHistByDestMarket...")

    clusterHistByDestMarket.getMap().foreach {
      case ((destId, marketId), clusterProbs) =>
        val destMarketCounts = destMarketCounterMap.getOrElse((destId, marketId), 0)
        val destCounts = destCounterMap.getOrElse(destId, 0)
        val marketCounts = marketCounterMap.getOrElse(marketId, 0)

        if (destMarketCounts > 0 && destCounts > 0 && destCounts == destMarketCounts) clusterProbs :+= 1f * clusterHistByMarket.getMap()(marketId)
        else if (destMarketCounts > 0 && destCounts > 0 && marketCounts == destMarketCounts) clusterProbs :+= 1f * destModel.predict(destId, continentByDest(destId))//clusterHistByDest.getMap()(destId)
        else clusterProbs :+= 1f * clusterHistByMarket.getMap()(marketId)
    }

    logger.info("Add prior stats to clusterHistByDestMarket...done")

    logger.info("Normalise clusterHistByDestMarket...")
    clusterHistByDestMarket.getMap.foreach { case (key, stats) => calcVectorProbsMutable(stats) }
    logger.info("Normalise clusterHistByDestMarket...done")

    logger.info("Add prior stats to clusterHistByDestMarketUser...")
    clusterHistByDestMarketUser.getMap().foreach { case ((destId, marketId, userId), clusterProbs) => clusterProbs :+= 1f * clusterHistByDestMarket.getMap()((destId, marketId)) }
    logger.info("Add prior stats to clusterHistByDestMarketUser...done")

    logger.info("Normalise clusterHistByDestMarketUser...")
    clusterHistByDestMarketUser.getMap.foreach { case (key, stats) => calcVectorProbsMutable(stats) }
    logger.info("Normalise clusterHistByDestMarketUser...done")

    MarketDestPredictionModel(clusterHistByDestMarketUser.getMap(), clusterHistByDestMarket.getMap(), clusterHistByDest.getMap(), clusterProbByDestMapSVM, clusterStatMap.getItemVec, clusterHistByContinent.getMap())
  }

}