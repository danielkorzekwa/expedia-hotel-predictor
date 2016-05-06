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
import expedia.stats.calcVectorProbsMutable
import expedia.stats.calcVectorMapProbsMutable
import expedia.stats.MulticlassHistByKey
import expedia.stats.MulticlassHistByKey

/**
 * @param trainData mat[userId,dest,cluster]
 */
case class UserDestPredictionModelBuilder(svmPredictionsData: DenseMatrix[Double], userIds: Set[Int]) extends LazyLogging {

  val clusterStatMap = CatStats()

  val clusterHistByContinent = MulticlassHistByKey[Int](100)

  val clusterHistByDest = MulticlassHistByKey[Int](100)

  //key - (destId,userId)
  val clusterHistByUserDest = MulticlassHistByKey[Tuple2[Int, Int]](100)

  val clusterProbByDestMapSVM: Map[Int, DenseVector[Float]] = loadClusterProbsByDestMap(svmPredictionsData)

  val continentByDest: mutable.Map[Int, Int] = mutable.Map()

  def processCluster(userId: Int, destId: Int, isBooking: Int, hotelContinent: Int, cluster: Int) = {
    clusterStatMap.add(cluster)

    clusterHistByContinent.add(hotelContinent, cluster)

    if (isBooking == 1) clusterHistByDest.add(destId, cluster)
    else clusterHistByDest.add(destId, cluster, value = 0.05f)

    if (userIds.isEmpty || userIds.contains(userId.toInt)) {
      if (isBooking == 1) clusterHistByUserDest.add((destId, userId), cluster)
      else clusterHistByUserDest.add((destId, userId), cluster, value = 0.7f)
    }

    continentByDest += destId -> hotelContinent
  }

  def toUserDestPredictionModel(): UserDestPredictionModel = {
    calcVectorProbsMutable(clusterStatMap.getItemVec)

    calcVectorMapProbsMutable(clusterHistByContinent.getMap().toMap)

    clusterHistByDest.getMap().foreach { case (destId, clusterCounts) => clusterCounts :+= clusterProbByDestMapSVM.getOrElse(destId, clusterHistByContinent.getMap.getOrElse(continentByDest(destId), clusterStatMap.getItemVec)) }

    calcVectorMapProbsMutable(clusterHistByDest.getMap().toMap)

    logger.info("Calc clusterProbsByUser stats...")

    clusterHistByUserDest.getMap().foreach {
      case ((destId, userId), clusterProbs) =>
        clusterProbs :+= 10f * clusterHistByDest.getMap()(destId)

    }
    logger.info("Calc clusterProbsByUser stats...done")

    logger.info("Calc clusterProbsByUser probs...")
    clusterHistByUserDest.getMap.foreach { case (key, stats) => calcVectorProbsMutable(stats) }

    logger.info("Calc clusterProbsByUser probs...done")

    UserDestPredictionModel(clusterHistByUserDest.getMap(), clusterHistByDest.getMap(), clusterProbByDestMapSVM, clusterStatMap.getItemVec, clusterHistByContinent.getMap())
  }

}