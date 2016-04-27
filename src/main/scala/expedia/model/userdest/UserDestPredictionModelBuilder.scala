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

/**
 * @param trainData mat[userId,dest,cluster]
 */
case class UserDestPredictionModelBuilder(trainData: DenseMatrix[Double], svmPredictionsData: DenseMatrix[Double], svmPredictionModel: SVMPredictionModel) {

  val clusterStatMap = CatStats()
  val clusterStatByDestMapNoPrior = CatStatsMapNoPrior()
  val clusterProbByDestMapSVM: Map[Double, DenseVector[Double]] = loadClusterProbsByDestMap(svmPredictionsData)

  def processCluster(destId: Double, cluster: Double) = {
    clusterStatMap.add(cluster)

    clusterStatByDestMapNoPrior.add(destId, cluster)
  }

  def toUserDestPredictionModel(): UserDestPredictionModel = {
    val clusterProbMap: DenseVector[Double] = calcVectorProbs(clusterStatMap.getItemVec)

    val clusterStatByDestMapWithPrior = clusterStatByDestMapNoPrior.toMap().map{case (destId,clusterCounts) => (destId,clusterCounts+clusterProbMap)}
    
    val clusterStatByDestMap = calcCatStatsMap(trainData(::, 1 to 2), destId => clusterProbByDestMapSVM.getOrElse(destId, clusterProbMap))
    val clusterProbByDestMap: Map[Double, DenseVector[Double]] = calcVectorMapProbs(clusterStatByDestMapWithPrior)

    val clusterProbsByUser: Map[Double, Map[Double, DenseVector[Double]]] = calcClusterProbsByUserMap(clusterProbByDestMap)

    UserDestPredictionModel(clusterProbsByUser, clusterProbByDestMap, clusterProbByDestMapSVM, clusterProbMap)
  }

  def calcClusterProbsByUserMap(clusterProbByDestMap: Map[Double, DenseVector[Double]]): Map[Double, Map[Double, DenseVector[Double]]] = {

    val clusterStatsByUserMap2: mutable.Map[Double, CatStatsMap] = mutable.Map()

    val i = new AtomicInteger(0)

    def prior(destId: Double) = clusterProbByDestMap(destId)

    trainData(*, ::).foreach { row =>
      if (i.getAndIncrement % 1000 == 0) println("UserDestPredict building=" + i.get)
      val userId = row(0)
      clusterStatsByUserMap2.getOrElseUpdate(userId, CatStatsMap(prior)).add(row(1 to 2).toDenseVector)
    }

    val clusterProbsByUserMap = clusterStatsByUserMap2.map { case (userId, stats) => (userId, calcVectorMapProbs(stats.toMap())) }

    clusterProbsByUserMap

  }
}