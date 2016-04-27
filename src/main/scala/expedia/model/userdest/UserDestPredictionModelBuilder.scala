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

/**
 * @param trainData mat[userId,dest,cluster]
 */
case class UserDestPredictionModelBuilder(trainData: DenseMatrix[Double], svmPredictionsData: DenseMatrix[Double], svmPredictionModel: SVMPredictionModel) extends LazyLogging {

  val clusterStatMap = CatStats()
  val clusterStatByDestMapNoPrior = CatStatsMapNoPrior()
  val userDestStatsMap = UserDestStatsMap()

  val clusterProbByDestMapSVM: Map[Double, DenseVector[Double]] = loadClusterProbsByDestMap(svmPredictionsData)

  def processCluster(userId: Double, destId: Double, cluster: Double) = {
    clusterStatMap.add(cluster)

    clusterStatByDestMapNoPrior.add(destId, cluster)

    userDestStatsMap.add(userId, destId, cluster)
  }

  def toUserDestPredictionModel(): UserDestPredictionModel = {
    val clusterProbMap: DenseVector[Double] = calcVectorProbs(clusterStatMap.getItemVec)

    val clusterStatByDestMapWithPrior = clusterStatByDestMapNoPrior.toMap().map { case (destId, clusterCounts) => (destId, clusterCounts + clusterProbMap) }
    val clusterProbByDestMap: Map[Double, DenseVector[Double]] = calcVectorMapProbs(clusterStatByDestMapWithPrior)

    logger.info("Calc clusterProbsByUser stats...")
    val userDestStatsMapWithPrior = userDestStatsMap.toMap().map {
      case (userId, clusterByDestMapNoPrior) =>

        val clusterByDestMapWithPrior = clusterByDestMapNoPrior.toMap().map { case (destId, clusterCounts) => (destId, clusterCounts + clusterProbByDestMap(destId)) }
        (userId, clusterByDestMapWithPrior)
    }
    logger.info("Calc clusterProbsByUser stats...done")

    logger.info("Calc clusterProbsByUser probs...")
    val clusterProbsByUser: Map[Double, Map[Double, DenseVector[Double]]] = userDestStatsMapWithPrior.map { case (userId, stats) => (userId, calcVectorMapProbs(stats)) }
    logger.info("Calc clusterProbsByUser probs...done")
    // val clusterProbsByUser: Map[Double, Map[Double, DenseVector[Double]]] = calcClusterProbsByUserMap(clusterProbByDestMap)

    UserDestPredictionModel(clusterProbsByUser, clusterProbByDestMap, clusterProbByDestMapSVM, clusterProbMap)
  }

  def calcClusterProbsByUserMap(clusterProbByDestMap: Map[Double, DenseVector[Double]]): Map[Double, Map[Double, DenseVector[Double]]] = {

    val clusterStatsByUserMap2: mutable.Map[Double, CatStatsMap] = mutable.Map()

    val i = new AtomicInteger(0)

    def prior(destId: Double) = clusterProbByDestMap(destId)

    trainData(*, ::).foreach { row =>
      if (i.getAndIncrement % 1000 == 0) logger.info("UserDestPredict building=" + i.get)
      val userId = row(0)
      val destId = row(1)
      val cluster = row(2)
      clusterStatsByUserMap2.getOrElseUpdate(userId, CatStatsMap(prior)).add(destId, cluster)
    }

    logger.info("Transforming user stats to user probs...")
    val clusterProbsByUserMap = clusterStatsByUserMap2.map { case (userId, stats) => (userId, calcVectorMapProbs(stats.toMap())) }
    logger.info("Transforming user stats to user probs...done")
    clusterProbsByUserMap

  }
}