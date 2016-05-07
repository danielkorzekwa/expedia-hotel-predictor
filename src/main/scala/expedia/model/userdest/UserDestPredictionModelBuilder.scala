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
import expedia.model.dest.DestModel

/**
 * @param trainData mat[userId,dest,cluster]
 */
case class UserDestPredictionModelBuilder(userIds: Set[Int]) extends LazyLogging {


  

  //key - (destId,userId)
  val clusterHistByUserDest = MulticlassHistByKey[Tuple2[Int, Int]](100)


  val continentByDest: mutable.Map[Int, Int] = mutable.Map()

  def processCluster(click:Click) = {



    if (userIds.isEmpty || userIds.contains(click.userId)) {
      if (click.isBooking == 1) clusterHistByUserDest.add((click.destId, click.userId), click.cluster)
      else clusterHistByUserDest.add((click.destId, click.userId), click.cluster, value = 0.7f)
    }

    continentByDest += click.destId -> click.hotelContinent
  }

  def create(destModel:DestModel): UserDestPredictionModel = {

    logger.info("Calc clusterProbsByUser stats...")

    clusterHistByUserDest.getMap().foreach {
      case ((destId, userId), clusterProbs) =>
        clusterProbs :+= 10f * destModel.predict(destId, continentByDest(destId))//clusterHistByDest.getMap()(destId)

    }
    logger.info("Calc clusterProbsByUser stats...done")

    logger.info("Calc clusterProbsByUser probs...")
    clusterHistByUserDest.getMap.foreach { case (key, stats) => calcVectorProbsMutable(stats) }

    logger.info("Calc clusterProbsByUser probs...done")

    UserDestPredictionModel(destModel,clusterHistByUserDest.getMap() )
  }

}