package expedia.model.dest

import expedia.data.Click
import expedia.stats.MulticlassHistByKey
import expedia.stats.calcVectorProbsMutable
import expedia.stats.calcVectorMapProbsMutable
import expedia.model.marketdest.MarketDestPredictionModel
import expedia.stats.CatStats
import expedia.model.svm.loadClusterProbsByDestMap
import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix
import scala.collection._
import com.typesafe.scalalogging.slf4j.LazyLogging
import scala.collection._
case class DestModelBuilder(svmPredictionsData: DenseMatrix[Double]) extends LazyLogging{
  val clusterStatMap = CatStats()

  val clusterHistByContinent = MulticlassHistByKey[Int](100)

  private val clusterHistByDest = MulticlassHistByKey[Int](100)

  val clusterProbByDestMapSVM: Map[Int, DenseVector[Float]] = loadClusterProbsByDestMap(svmPredictionsData)

  val continentByDest: mutable.Map[Int, Int] = mutable.Map()

  def processCluster(click: Click) = {
 clusterStatMap.add(click.cluster)
    clusterHistByContinent.add(click.hotelContinent, click.cluster)
  
    if (click.isBooking == 1) clusterHistByDest.add(click.destId, click.cluster)
    else clusterHistByDest.add(click.destId, click.cluster, value = 0.05f)
    
      continentByDest += click.destId -> click.hotelContinent
  }

  def create(): DestModel = {
    calcVectorProbsMutable(clusterStatMap.getItemVec)

    calcVectorMapProbsMutable(clusterHistByContinent.getMap().toMap)

    clusterHistByDest.getMap().foreach { case (destId, clusterCounts) => clusterCounts :+= clusterProbByDestMapSVM.getOrElse(destId, clusterHistByContinent.getMap.getOrElse(continentByDest(destId), clusterStatMap.getItemVec)) }
    calcVectorMapProbsMutable(clusterHistByDest.getMap().toMap)


    DestModel(clusterHistByDest, clusterProbByDestMapSVM, clusterHistByContinent,clusterStatMap)
  }

}