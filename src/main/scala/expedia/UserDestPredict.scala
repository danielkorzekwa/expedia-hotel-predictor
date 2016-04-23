package expedia

import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix
import breeze.linalg._
import expedia.similarity.calcCatProbs
import expedia.similarity.calcCatStatsMap
import expedia.similarity.calcCatStats
import dk.gp.util.filterRows
import com.typesafe.scalalogging.slf4j.LazyLogging
import java.util.concurrent.atomic.AtomicInteger

/**
 * @param trainData mat[userId,dest,cluster]
 */
case class UserDestPredict(trainData: DenseMatrix[Double]) extends LazyLogging {

  val clusterStatMap = calcCatStats(trainData(::, 2))
  var clusterProbMap: Map[Double, Double] = calcCatProbs(clusterStatMap)

  val clusterStatByDestMap = calcCatStatsMap(trainData(::, 1 to 2), destId => clusterProbMap)
  val clusterProbByDestMap: Map[Double, Map[Double, Double]] = calcCatProbs(clusterStatByDestMap)

  val clusterProbsByUser: Map[Double, Map[Double, Map[Double, Double]]] = calcClusterProbsByUserMap()

  /**
   * @param data [user_id,dest]
   * @param hotelCluster
   */
  def predict(data: DenseMatrix[Double], hotelCluster: Double): DenseVector[Double] = {

    data(*, ::).map { row =>

      val userId = row(0)
      val destId = row(1)
      val prob = clusterProbsByUser.getOrElse(userId, clusterProbByDestMap).getOrElse(destId, clusterProbByDestMap.getOrElse(destId, clusterProbMap))(hotelCluster)
      prob
    }

  }

  def calcClusterProbsByUserMap(): Map[Double, Map[Double, Map[Double, Double]]] = {

    val userIds = unique(trainData(::, 0)).toArray
    logger.info("all users:" + userIds.size)
    val i = new AtomicInteger(0)
    val clusterProbsByUserMap = userIds.map { userId =>
      logger.info("Processing user:" + i.getAndIncrement)
      val userTrainData = filterRows(trainData, 0, uId => uId == userId)

      val userClusterStats = calcCatStatsMap(userTrainData(::, 1 to 2), destId => clusterProbByDestMap(destId).map { case (dest, prob) => (dest, prob) })
      val userClusterProbs = calcCatProbs(userClusterStats)

      (userId, userClusterProbs)

    }.toMap

    println(clusterProbsByUserMap.size)
    clusterProbsByUserMap

    //    val userTrainData = filterRows(trainData, 0, userId => userId == 195876d)
    //
    //    val userClusterStats = calcCatStatsMap(userTrainData(::, 1 to 2), destId => clusterProbByDestMap(destId).map{ case (dest,prob) => (dest,prob)})
    //val userClusterProbs = calcCatProbs(userClusterStats)
    //    Map(195876d -> userClusterProbs)
    //  Map()
  }
}