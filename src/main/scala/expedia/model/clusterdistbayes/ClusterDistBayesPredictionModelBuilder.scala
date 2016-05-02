package expedia.model.clusterdist

import scala.collection._
import breeze.linalg.DenseVector
import scala.collection.mutable.ListBuffer
import dk.bayes.dsl.variable.Categorical
import dk.bayes.dsl.infer
import com.typesafe.scalalogging.slf4j.LazyLogging
import java.util.concurrent.atomic.AtomicInteger
import breeze.linalg.DenseMatrix
import expedia.model.clusterdistbayes.calcClusterCPD
import expedia.model.clusterdistbayes.ClusterDistBayesPredictionModel

case class ClusterDistBayesPredictionModelBuilder() extends LazyLogging {

  //Map[(userLoc,dist,market),ListBuffer[cluster]]
  private val clusterMap: mutable.Map[Tuple3[Double, Double, Double], ListBuffer[Int]] = mutable.Map()

  def processCluster(userLoc: Double, dist: Double, market: Double, cluster: Int) = {

    if (dist != -1) {

      val key = (userLoc, dist, market)

      //Map[cluster,count]
      val clusters: ListBuffer[Int] = clusterMap.getOrElseUpdate(key, ListBuffer())
      clusters += cluster

    }

  }

  def toClusterDistPredictionModel( clusterByDistMap: Map[Tuple3[Double, Double, Double], DenseVector[Double]]): ClusterDistBayesPredictionModel = {

    logger.info("Predicting cluser dist bayes...")

    val priorProbs = DenseVector.fill(100)(1d / 100).toArray
    
    val clusterCPD = calcClusterCPD(clusterByDistMap)
    
    println(clusterCPD(13,::))
    
    val likProbs = clusterCPD.t.toArray//DenseVector.fill(100 * 100)(1d / (100)).toArray
    //Map[(userLoc,dist,market),[sorted clusters vector by cluster counts]]
    var i = new AtomicInteger(0)
    val sortedClusterMap: Map[Tuple3[Double, Double, Double], DenseVector[Double]] = clusterMap.toList.par.map {
      case (key, clusters) =>

        val prior = Categorical(priorProbs)
        val llVariables = clusters.map { cluster =>
          val llVariable = Categorical(prior, likProbs)
          llVariable.setValue(cluster)

        }

        val posterior = infer(prior)
       // println(posterior.cpd)
        if (i.incrementAndGet() % 1000 == 0) logger.info("Processed rows: %d".format(i.get))

        val sortedClusters = posterior.cpd.zipWithIndex.sortWith((a, b) => a._1 > b._1).map(_._2.toDouble).toArray
        
//        if(clusters.distinct.size>1) {
//        println(posterior.cpd)
//        println(sortedClusters.toList)
//        println(clusters)
//        println("--------------------------")
//        }
        key -> DenseVector(sortedClusters)//DenseVector(sortedClusters)
    }.toList.toMap
    logger.info("Predicting cluser dist bayes...done=" + sortedClusterMap.size)
   ClusterDistBayesPredictionModel(sortedClusterMap)
  }
}