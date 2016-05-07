package expedia.model.userdest

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
import expedia.stats.MulticlassHistByKey
import expedia.data.Click

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

  def processCluster(click:Click) = {
    clusterStatMap.add(click.cluster)

    clusterHistByContinent.add(click.hotelContinent, click.cluster)

    if (click.isBooking == 1) clusterHistByDest.add(click.destId, click.cluster)
    else clusterHistByDest.add(click.destId, click.cluster, value = 0.05f)

    if (userIds.isEmpty || userIds.contains(click.userId)) {
      if (click.isBooking == 1) clusterHistByUserDest.add((click.destId, click.userId), click.cluster)
      else clusterHistByUserDest.add((click.destId, click.userId), click.cluster, value = 0.7f)
    }

    continentByDest += click.destId -> click.hotelContinent
  }

  def create(): UserDestPredictionModel = {
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