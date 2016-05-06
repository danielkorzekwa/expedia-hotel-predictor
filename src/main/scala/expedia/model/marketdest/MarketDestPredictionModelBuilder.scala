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
import expedia.stats.UserDestStatsMap
import expedia.stats.MulticlassHistByKey

/**
 * @param trainData mat[userId,dest,cluster]
 */
case class MarketDestPredictionModelBuilder(svmPredictionsData: DenseMatrix[Double], userIds: Set[Int]) extends LazyLogging {

  val clusterStatMap = CatStats()

  val clusterHistByContinent = MulticlassHistByKey[Int](100)
  
  val clusterHistByDest = MulticlassHistByKey[Int](100)

    val clusterHistByUserDest = MulticlassHistByKey[Tuple2[Int, Int]](100)

  val clusterProbByDestMapSVM: Map[Int, DenseVector[Float]] = loadClusterProbsByDestMap(svmPredictionsData)

  val continentByDest: mutable.Map[Int, Int] = mutable.Map()

  def processCluster(userId: Int, destId: Int, isBooking: Int, hotelContinent: Int, cluster: Int) = {
      clusterStatMap.add(cluster)

      clusterHistByContinent.add(hotelContinent, cluster)

      
         clusterHistByDest.add(destId, cluster)
      

      if (userIds.isEmpty || userIds.contains(userId.toInt)) {
       clusterHistByUserDest.add((destId, userId), cluster)
      }

      continentByDest += destId -> hotelContinent
  }

  def toMarketDestPredictionModel(): MarketDestPredictionModel = {
    calcVectorProbsMutable(clusterStatMap.getItemVec)

    calcVectorMapProbsMutable(clusterHistByContinent.getMap().toMap)

    clusterHistByDest.getMap().foreach { case (destId, clusterCounts) => clusterCounts :+= clusterProbByDestMapSVM.getOrElse(destId, clusterHistByContinent.getMap.getOrElse(continentByDest(destId), clusterStatMap.getItemVec)) }
  
    calcVectorMapProbsMutable(clusterHistByDest.getMap().toMap)

    logger.info("Calc clusterProbsByUser stats...")
    clusterHistByUserDest.getMap().foreach {
      case ((destId, userId), clusterProbs) =>
        clusterProbs :+= 1f * clusterHistByDest.getMap()(destId)

    }
    logger.info("Calc clusterProbsByUser stats...done")

    logger.info("Calc clusterProbsByUser probs...")
    clusterHistByUserDest.getMap.foreach { case (key, stats) => calcVectorProbsMutable(stats) }


    logger.info("Calc clusterProbsByUser probs...done")
    // val clusterProbsByUser: Map[Double, Map[Double, DenseVector[Double]]] = calcClusterProbsByUserMap(clusterProbByDestMap)

    MarketDestPredictionModel(clusterHistByUserDest.getMap(), clusterHistByDest.getMap(), clusterProbByDestMapSVM, clusterStatMap.getItemVec, clusterHistByContinent.getMap())
  }

}