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
import expedia.stats.calcVectorProbsMutable
import breeze.numerics._
import dk.gp.gpr.GprModel
import dk.gp.gpr.predict
import expedia.model.clusterdist.calcJacardSimMatrix
import expedia.model.clusterdistbayes.gprfast.GprFastModel
import expedia.model.clusterdistbayes.gprfast.GprFastModel
import expedia.model.clusterdistbayes.gprfast.gprFastPredict
import expedia.model.clusterdist.calcClusterCoExistMatrix

case class ClusterDistBayesPredictionModelBuilder() extends LazyLogging {

  //Map[(userLoc,dist,market),ListBuffer[cluster]]
  private val clustersByDistMap: mutable.Map[Tuple3[Double, Double, Double], ListBuffer[Double]] = mutable.Map()

  def processCluster(userLoc: Double, dist: Double, market: Double, cluster: Int) = {

    if (dist != -1) {

      val key = (userLoc, dist, market)

      //Map[cluster,count]
      val clusters: ListBuffer[Double] = clustersByDistMap.getOrElseUpdate(key, ListBuffer())
      clusters += cluster

    }

  }

  def toClusterDistPredictionModel(topClustersByDistMap: Map[Tuple3[Double, Double, Double], DenseVector[Int]]): ClusterDistBayesPredictionModel = {

    logger.info("Predicting cluser dist bayes...")

    /**
     * Compute prior
     */
    //    val clustersSeq = clusterByDistMap.map { case (key, clusters) => clusters }.toList
    //    val allClusters = DenseVector.vertcat(clustersSeq: _*)
    //    val catStats = CatStats()
    //    allClusters.foreach(cluster => catStats.add(cluster))
    //    calcVectorProbsMutable(catStats.getItemVec())
    //    val priorProbs = DenseVector(catStats.getItemVec().toArray.map(_.toDouble))
    //    println("prior: " + catStats.getItemVec())

    /**
     * Compute cluster similarities
     */
    val distClutersSeq = clustersByDistMap.map { case (key, clusters) => DenseVector(clusters.toList.distinct.toArray.map(_.toInt)) }.toList
    val clusterCoExistMat = calcClusterCoExistMatrix(distClutersSeq)
    val jacardSimMatrix = calcJacardSimMatrix(clusterCoExistMat)

    val jacardSimMatrixByCluster: Map[Double, DenseMatrix[Double]] = (0 until 100).map { cluster =>

      val distClutersSeqForCluster = distClutersSeq.filter { clusters => clusters.toArray.contains(cluster) }
      println(distClutersSeqForCluster.size + ":" + distClutersSeq.size)
      cluster.toDouble -> calcJacardSimMatrix(calcClusterCoExistMatrix(distClutersSeqForCluster))

    }.toMap

    val clusterCoExistMatString = jacardSimMatrixByCluster(81).map(x => "%.3f".format(x))
    println(clusterCoExistMatString.toDenseVector)
    println("---------")
    println(clusterCoExistMat(81, ::))
    println("---------")
    println(jacardSimMatrix(81, ::))

    var i = new AtomicInteger(0)
    //Map[(userLoc,dist,market),[sorted clusters vector by cluster counts]]

    // val predictedByClustersList: mutable.Map[List[Int], DenseVector[Double]] = mutable.Map()

    //key (num of clusters, top cluster)
    val gprFastModelsByClustersSize: mutable.Map[Tuple2[Int,Int], GprFastModel] = mutable.Map()
    clustersByDistMap.toList.foreach {
      case (key, clusters) =>
        if (clusters.size < 10) {
         
          val topCluster = topClustersByDistMap(key)(0)
          val gpModelKey = (clusters.size,topCluster)
          gprFastModelsByClustersSize.getOrElseUpdate(gpModelKey, createGPFastModel(gpModelKey,clusters, jacardSimMatrixByCluster(topCluster)))
        }
    }

    val sortedClusterMap: Map[Tuple3[Double, Double, Double], DenseVector[Double]] = clustersByDistMap.toList.par.map {
      case (key, clusters) =>

        if (clusters.size < 10 && (true || key._2 == 9734.0267)) {

           val gpModelKey = (clusters.size,topClustersByDistMap(key)(0))
          val gprFastModel = gprFastModelsByClustersSize(gpModelKey)

          val gpData = getDataForGP(DenseVector(clusters.toArray.map(_.toDouble)), clustersSize = 100)
          val dataY = gpData(::, 1)

          val posterior = gprFastPredict(dataY, gprFastModel).toArray
          //val posterior = predictedByClustersList.getOrElse(clusters.toList.sorted, gpPredict(clusters, jacardSimMatrix, priorProbs)).toArray

          val sortedClusters = posterior.zipWithIndex.sortWith((a, b) => a._1 > b._1).map(_._2.toDouble).toArray

          //          if(clusters.toList.distinct.size>1) {
        //  println("post: " + posterior.toList)
        //  println("cluster: " + clusters)
          //            println("--------------------------")
          //          }
          if (i.incrementAndGet() % 10000 == 0) logger.info("Processed rows: %d".format(i.get))

          key -> DenseVector(posterior)
        } else key -> DenseVector.fill(100)(Double.NaN)
    }.toList.toMap
    logger.info("Predicting cluser dist bayes...done=" + sortedClusterMap.size)
    ClusterDistBayesPredictionModel(sortedClusterMap)
  }

  var ii = new AtomicInteger(0)
  private def createGPFastModel(gpModelKey:Tuple2[Int,Int],clusters: Seq[Double], jacardSimMatrix: DenseMatrix[Double]): GprFastModel = {

     logger.info("Creating gp model...: " + gpModelKey + ":" + ii.incrementAndGet())
    val gpData = getDataForGP(DenseVector(clusters.toArray.map(_.toDouble)), clustersSize = 100)

    val dataX = gpData(::, 0 to 0)
    val predictionDataX = DenseVector.rangeD(0.0, 100, 1).toDenseMatrix.t
    val gprFastModel = createGPModelFast(dataX, jacardSimMatrix)
    gprFastModel
  }

}