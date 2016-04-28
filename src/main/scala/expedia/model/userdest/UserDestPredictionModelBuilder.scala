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

  val clusterStatByDestMapNoPrior = CatStatsMapNoPrior()

  val userDestStatsMap = UserDestStatsMap()

  val clusterProbByDestMapSVM: Map[Int, DenseVector[Float]] = loadClusterProbsByDestMap(svmPredictionsData)

  def processCluster(userId: Int, destId: Int, isBooking: Int, cluster: Int) = {
    clusterStatMap.add(cluster)

  clusterStatByDestMapNoPrior.add(destId, cluster)
 
    if (userIds.contains(userId.toInt)) {
    userDestStatsMap.add(userId, destId, cluster)
    }
  }

  def toUserDestPredictionModel(): UserDestPredictionModel = {
    calcVectorProbsMutable(clusterStatMap.getItemVec)


     clusterStatByDestMapNoPrior.getMap().foreach { case (destId, clusterCounts) => clusterCounts :+= clusterProbByDestMapSVM.getOrElse(destId, clusterStatMap.getItemVec) }
    calcVectorMapProbsMutable(clusterStatByDestMapNoPrior.getMap().toMap)

    logger.info("Calc clusterProbsByUser stats...")
    userDestStatsMap.getMap().foreach {
      case (userId, clusterByDestMapNoPrior) =>
        clusterByDestMapNoPrior.getMap().foreach { case (destId, clusterCounts) => clusterCounts :+=10f*clusterStatByDestMapNoPrior.getMap()(destId) }
    }
    logger.info("Calc clusterProbsByUser stats...done")

    logger.info("Calc clusterProbsByUser probs...")
    userDestStatsMap.getMap.foreach { case (userId, stats) => calcVectorMapProbsMutable(stats.getMap.toMap) }
    val clusterProbsByUser: Map[Int, Map[Int, DenseVector[Float]]] = userDestStatsMap.getMap.map { case (userId, stats) => (userId, stats.getMap) }

    logger.info("Calc clusterProbsByUser probs...done")
    // val clusterProbsByUser: Map[Double, Map[Double, DenseVector[Double]]] = calcClusterProbsByUserMap(clusterProbByDestMap)

    UserDestPredictionModel(clusterProbsByUser, clusterStatByDestMapNoPrior.getMap(), clusterProbByDestMapSVM, clusterStatMap.getItemVec)
  }

  //  def calcClusterProbsByUserMap(clusterProbByDestMap: Map[Double, DenseVector[Double]]): Map[Double, Map[Double, DenseVector[Double]]] = {
  //
  //    val clusterStatsByUserMap2: mutable.Map[Double, CatStatsMap] = mutable.Map()
  //
  //    val i = new AtomicInteger(0)
  //
  //    def prior(destId: Double) = clusterProbByDestMap(destId)
  //
  //    trainData(*, ::).foreach { row =>
  //      if (i.getAndIncrement % 1000 == 0) logger.info("UserDestPredict building=" + i.get)
  //      val userId = row(0)
  //      val destId = row(1)
  //      val cluster = row(2)
  //      clusterStatsByUserMap2.getOrElseUpdate(userId, CatStatsMap(prior)).add(destId, cluster)
  //    }
  //
  //    logger.info("Transforming user stats to user probs...")
  //    val clusterProbsByUserMap = clusterStatsByUserMap2.map { case (userId, stats) => (userId, calcVectorMapProbs(stats.toMap())) }
  //    logger.info("Transforming user stats to user probs...done")
  //    clusterProbsByUserMap
  //
  //  }
}