package expedia.model.userdest

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
import expedia.stats.UserDestStatsMap
import expedia.stats.calcVectorProbsMutable
import expedia.stats.calcVectorMapProbsMutable

/**
 * @param trainData mat[userId,dest,cluster]
 */
case class UserDestPredictionModelBuilder(svmPredictionsData: DenseMatrix[Double], userIds: Set[Int]) extends LazyLogging {

  val clusterStatMap = CatStats()

  val clusterStatByContinentMapNoPrior = CatStatsMapNoPrior()

  val clusterStatByDestMapNoPrior = CatStatsMapNoPrior()

  val userDestStatsMap = UserDestStatsMap()

  val clusterProbByDestMapSVM: Map[Int, DenseVector[Float]] = loadClusterProbsByDestMap(svmPredictionsData)

  val continentByDest: mutable.Map[Int, Int] = mutable.Map()

  def processCluster(userId: Int, destId: Int, isBooking: Int, hotelContinent: Int, cluster: Int) = {
      clusterStatMap.add(cluster)

      clusterStatByContinentMapNoPrior.add(hotelContinent, cluster)

      clusterStatByDestMapNoPrior.add(destId, cluster)

      if (userIds.contains(userId.toInt)) {
           userDestStatsMap.add(userId, destId, cluster)
      }

      continentByDest += destId -> hotelContinent
  }

  def toUserDestPredictionModel(): UserDestPredictionModel = {
    calcVectorProbsMutable(clusterStatMap.getItemVec)

    calcVectorMapProbsMutable(clusterStatByContinentMapNoPrior.getMap().toMap)

    clusterStatByDestMapNoPrior.getMap().foreach { case (destId, clusterCounts) => clusterCounts :+= clusterProbByDestMapSVM.getOrElse(destId, clusterStatByContinentMapNoPrior.getMap.getOrElse(continentByDest(destId), clusterStatMap.getItemVec)) }
  
    calcVectorMapProbsMutable(clusterStatByDestMapNoPrior.getMap().toMap)

    logger.info("Calc clusterProbsByUser stats...")
    userDestStatsMap.getMap().foreach {
      case (userId, clusterByDestMapNoPrior) =>
        clusterByDestMapNoPrior.getMap().foreach { case (destId, clusterCounts) => clusterCounts :+= 10f * clusterStatByDestMapNoPrior.getMap()(destId) }
    }
    logger.info("Calc clusterProbsByUser stats...done")

    logger.info("Calc clusterProbsByUser probs...")
    userDestStatsMap.getMap.foreach { case (userId, stats) => calcVectorMapProbsMutable(stats.getMap.toMap) }
    val clusterProbsByUser: Map[Int, Map[Int, DenseVector[Float]]] = userDestStatsMap.getMap.map { case (userId, stats) => (userId, stats.getMap) }

    logger.info("Calc clusterProbsByUser probs...done")
    // val clusterProbsByUser: Map[Double, Map[Double, DenseVector[Double]]] = calcClusterProbsByUserMap(clusterProbByDestMap)

    UserDestPredictionModel(clusterProbsByUser, clusterStatByDestMapNoPrior.getMap(), clusterProbByDestMapSVM, clusterStatMap.getItemVec, clusterStatByContinentMapNoPrior.getMap(), continentByDest)
  }

}