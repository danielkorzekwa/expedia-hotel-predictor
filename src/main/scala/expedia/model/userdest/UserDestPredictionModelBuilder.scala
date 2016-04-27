package expedia.model.userdest

import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix
import breeze.linalg._
import expedia.stats.calcCatProbs
import expedia.stats.calcCatStatsMap
import expedia.stats.calcCatStats
import com.typesafe.scalalogging.slf4j.LazyLogging
import java.util.concurrent.atomic.AtomicInteger
import scala.collection._
import expedia.stats.CatStatsMap3
import expedia.model.svm.loadClusterProbsByDestMap
import expedia.model.svm.SVMPredictionModel

/**
 * @param trainData mat[userId,dest,cluster]
 */
case class UserDestPredictionModelBuilder(trainData: DenseMatrix[Double], svmPredictionsData: DenseMatrix[Double], svmPredictionModel: SVMPredictionModel) {

  val clusterStatMap = calcCatStats(trainData(::, 2))
  var clusterProbMap: DenseVector[Double] = calcCatProbs(clusterStatMap)

  val clusterProbByDestMapSVM: Map[Double, DenseVector[Double]] = loadClusterProbsByDestMap(svmPredictionsData)

  // val clusterStatByDestMap = calcCatStatsMap(trainData(::, 1 to 2), destId => clusterProbMap)
  val clusterStatByDestMap = calcCatStatsMap(trainData(::, 1 to 2), destId => clusterProbByDestMapSVM.getOrElse(destId, clusterProbMap))

  val clusterProbByDestMap: Map[Double, DenseVector[Double]] = calcCatProbs(clusterStatByDestMap)

  val clusterProbsByUser: Map[Double, Map[Double, DenseVector[Double]]] = calcClusterProbsByUserMap()

  def processCluster() = {
    ???
  }

  def toUserDestPredictionModel(): UserDestPredictionModel = {
    UserDestPredictionModel(clusterProbsByUser, clusterProbByDestMap, clusterProbByDestMapSVM, clusterProbMap)
  }

  def calcClusterProbsByUserMap(): Map[Double, Map[Double, DenseVector[Double]]] = {

    val clusterStatsByUserMap2: mutable.Map[Double, CatStatsMap3] = mutable.Map()

    val i = new AtomicInteger(0)

    def prior(destId: Double) = clusterProbByDestMap(destId)

    trainData(*, ::).foreach { row =>
      if (i.getAndIncrement % 1000 == 0) println("UserDestPredict building=" + i.get)
      val userId = row(0)
      clusterStatsByUserMap2.getOrElseUpdate(userId, CatStatsMap3(prior)).add(row(1 to 2).toDenseVector)
    }

    val clusterProbsByUserMap = clusterStatsByUserMap2.map { case (userId, stats) => (userId, calcCatProbs(stats.toMap())) }

    clusterProbsByUserMap

  }
}