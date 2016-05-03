package expedia.model.clusterdistbayes

import scala.collection._
import breeze.linalg.DenseVector
import scala.collection.mutable.ListBuffer
import dk.bayes.dsl.variable.Categorical
import dk.bayes.dsl.infer
import com.typesafe.scalalogging.slf4j.LazyLogging
import java.util.concurrent.atomic.AtomicInteger
import breeze.linalg.DenseMatrix
import expedia.stats.CatStats
import expedia.stats.calcVectorProbs
import expedia.stats.calcVectorProbsMutable
import breeze.numerics._
import dk.gp.gpr.GprModel
import dk.gp.gpr.predict

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

  def toClusterDistPredictionModel(clusterByDistMap: Map[Tuple3[Double, Double, Double], DenseVector[Double]]): ClusterDistBayesPredictionModel = {

    logger.info("Predicting cluser dist bayes...")

    /**
     * Compute prior
     */
    val clustersSeq = clusterByDistMap.map { case (key, clusters) => clusters }.toList
    val allClusters = DenseVector.vertcat(clustersSeq: _*)
    val catStats = CatStats()
    allClusters.foreach(cluster => catStats.add(cluster))
    calcVectorProbsMutable(catStats.getItemVec())
    val priorProbs = catStats.getItemVec().toArray.map(_.toDouble)
    println("prior: " + catStats.getItemVec())

    /**
     * Compute cluster CPD
     */
    val clusterCPD = calcClusterCPD(clusterByDistMap)

    var i = new AtomicInteger(0)
    //Map[(userLoc,dist,market),[sorted clusters vector by cluster counts]]
    val sortedClusterMap: Map[Tuple3[Double, Double, Double], DenseVector[Double]] = clusterMap.toList.par.map {
      case (key, clusters) =>

        if (true || key._2 == 98.8687) {

          val gpData = getDataForGP(DenseVector(clusters.toArray.map(_.toDouble)), clustersSize = 100)

          val dataX = gpData(::, 0 to 0)
          val dataY = gpData(::, 1)

          val covFunc = GpTestCovFunc()
          val covFuncParams = DenseVector[Double]()
          val noiseLogStdDev = log(1d)
          val mean = 0.33
          val gprModel = GprModel(dataX, dataY, covFunc, covFuncParams, noiseLogStdDev, mean)

          val predictionDataX = DenseVector.rangeD(0.0,100,1).toDenseMatrix.t
          val posterior = predict(predictionDataX, gprModel)(::,0).toArray
         // val posterior = DenseVector.fill(100)(1d / 100).toArray

          val sortedClusters = posterior.zipWithIndex.sortWith((a, b) => a._1 > b._1).map(_._2.toDouble).toArray

//          if(clusters.toList.distinct.size>1) {
//           println("post: " + posterior.toList)
//           println("cluster: " + clusters)
//            println("sortedClusters: " + sortedClusters.toList)
//            println("--------------------------")
//          }
          if (i.incrementAndGet() % 1000 == 0) logger.info("Processed rows: %d".format(i.get))

          key -> DenseVector(sortedClusters)
        } else key -> DenseVector(0d)
    }.toList.toMap
    logger.info("Predicting cluser dist bayes...done=" + sortedClusterMap.size)
    ClusterDistBayesPredictionModel(sortedClusterMap)
  }
}