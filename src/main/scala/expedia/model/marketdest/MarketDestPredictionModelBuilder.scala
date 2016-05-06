package expedia.model.marketdest

import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix
import breeze.linalg._
import expedia.stats.calcCatStatsMap
import expedia.stats.calcCatStats
import com.typesafe.scalalogging.slf4j.LazyLogging
import java.util.concurrent.atomic.AtomicInteger
import scala.collection._
import expedia.model.svm.loadClusterProbsByDestMap
import expedia.model.svm.SVMPredictionModel
import expedia.stats.CatStats
import expedia.stats.CatStatsMapNoPrior
import expedia.stats.CatStatsMap
import expedia.stats.calcVectorProbs
import expedia.stats.calcVectorMapProbs
import expedia.stats.calcVectorProbsMutable
import expedia.stats.calcVectorMapProbsMutable
import expedia.stats.MulticlassHistByKey
import expedia.data.Click

/**
 * @param trainData mat[userId,dest,cluster]
 */
case class MarketDestPredictionModelBuilder(svmPredictionsData: DenseMatrix[Double]) extends LazyLogging {

  val clusterStatMap = CatStats()

  val clusterHistByContinent = MulticlassHistByKey[Int](100)

  val clusterHistByDest = MulticlassHistByKey[Int](100)

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

    clusterHistByDestMarket.add((click.destId, click.market), click.cluster)
    clusterHistByDestMarketUser.add((click.destId, click.market, click.userId), click.cluster)

    continentByDest += click.destId -> click.hotelContinent
  }

  def toMarketDestPredictionModel(): MarketDestPredictionModel = {
    calcVectorProbsMutable(clusterStatMap.getItemVec)

    calcVectorMapProbsMutable(clusterHistByContinent.getMap().toMap)

    clusterHistByDest.getMap().foreach { case (destId, clusterCounts) => clusterCounts :+= clusterProbByDestMapSVM.getOrElse(destId, clusterHistByContinent.getMap.getOrElse(continentByDest(destId), clusterStatMap.getItemVec)) }
    calcVectorMapProbsMutable(clusterHistByDest.getMap().toMap)

    logger.info("Add prior stats to clusterHistByUserDest...")
    clusterHistByDestMarket.getMap().foreach { case ((destId, userId), clusterProbs) => clusterProbs :+= 1f * clusterHistByDest.getMap()(destId) }
    logger.info("Add prior stats to clusterHistByUserDest...done")

    logger.info("Normalise clusterHistByUserDest...")
    clusterHistByDestMarket.getMap.foreach { case (key, stats) => calcVectorProbsMutable(stats) }
    logger.info("Normalise clusterHistByUserDest...done")

    logger.info("Add prior stats to clusterHistByDestMarketUser...")
    clusterHistByDestMarketUser.getMap().foreach { case ((destId, marketId, userId), clusterProbs) => clusterProbs :+= 1f * clusterHistByDestMarket.getMap()((destId, marketId)) }
    logger.info("Add prior stats to clusterHistByDestMarketUser...done")

    logger.info("Normalise clusterHistByDestMarketUser...")
    clusterHistByDestMarketUser.getMap.foreach { case (key, stats) => calcVectorProbsMutable(stats) }
    logger.info("Normalise clusterHistByDestMarketUser...done")

    MarketDestPredictionModel(clusterHistByDestMarketUser.getMap(),clusterHistByDestMarket.getMap(), clusterHistByDest.getMap(), clusterProbByDestMapSVM, clusterStatMap.getItemVec, clusterHistByContinent.getMap())
  }

}