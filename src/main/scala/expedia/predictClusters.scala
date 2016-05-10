package expedia

import expedia.data.ExDataSource
import breeze.linalg.DenseMatrix
import expedia.data.Click
import expedia.model.clusterdist.ClusterDistPredictionModelBuilder
import com.typesafe.scalalogging.slf4j.LazyLogging
import breeze.linalg.DenseVector
import java.util.concurrent.atomic.AtomicInteger
import expedia.model.clusterdistprox.ClusterDistProxModelBuilder
import expedia.model.marketdest.MarketDestPredictionModelBuilder
import expedia.model.country.CountryModelBuilder
import expedia.stats.CounterMap
import expedia.model.dest.DestModelBuilder

object predictClusters extends LazyLogging {

  /**
   * @return Top 5 predictions for three models[clusterDist,marketDest,clusterDistProx]. ClusterDist: [p1,p2,p3,p4,p5,c1,c2,c3,c4,c5]
   */
  def apply(trainDS: ExDataSource, testClicks: Seq[Click]): Tuple3[DenseMatrix[Double], DenseMatrix[Double], DenseMatrix[Double]] = {

    val clusterDistModelBuilder = ClusterDistPredictionModelBuilder()
    val clusterDistProxModelBuilder = ClusterDistProxModelBuilder(testClicks)

    val countryModelBuilder = CountryModelBuilder(testClicks)
    val destModelBuilder = DestModelBuilder(testClicks)
    val destMarketCounterMap = CounterMap[Tuple2[Int, Int]]
    val destCounterMap = CounterMap[Int]()
    val marketCounterMap = CounterMap[Int]()
    val userCounterMap = CounterMap[Int]()
    val marketDestPredictBuilder = MarketDestPredictionModelBuilder(testClicks)

    def onClick(click: Click) = {
      clusterDistModelBuilder.processCluster(click)
      clusterDistProxModelBuilder.processCluster(click)

      countryModelBuilder.processCluster(click)
      destModelBuilder.processCluster(click)
      marketDestPredictBuilder.processCluster(click)

      if (click.isBooking == 1) {
        destMarketCounterMap.add((click.destId, click.marketId))
        destCounterMap.add(click.destId)
        marketCounterMap.add(click.marketId)
      }

      userCounterMap.add(click.userId)

    }
    trainDS.foreach { click => onClick(click) }

    val clusterDistModel = clusterDistModelBuilder.create()
    val clusterDistProxModel = clusterDistProxModelBuilder.create()

    val countryModel = countryModelBuilder.create()
    val destModel = destModelBuilder.create(countryModel)
    val marketDestPredict = marketDestPredictBuilder.create(destModel, countryModel, destMarketCounterMap, destCounterMap, marketCounterMap)

    /**
     * Cluster dist
     */
    val c1 = new AtomicInteger(0)
    val predictionRecordsClusterDist = testClicks.par.map { click =>
      val predicted = clusterDistModel.predict(click.userLoc, click.dist, click.marketId)

      val predictedProbTuples = predicted.toArray.toList.zipWithIndex.sortWith((a, b) => a._1 > b._1).take(5).toArray

      val predictionProbs = predictedProbTuples.map(_._1.toDouble)
      val predictionRanks = predictedProbTuples.map(_._2.toDouble)

      if (c1.incrementAndGet() % 100000 == 0) logger.info("Predicting clusters: %d".format(c1.get))

      val record = DenseVector.vertcat(DenseVector(predictionProbs), DenseVector(predictionRanks))
      record
    }.toList

    val predictionMatrixClusterDist = DenseVector.horzcat(predictionRecordsClusterDist: _*).t

    /**
     * Cluster dist prox
     */
    val c2 = new AtomicInteger(0)
    val predictionRecordsClusterDistProx = testClicks.par.map { click =>
      val predicted = clusterDistProxModel.predict(click)

      val predictedProbTuples = predicted.toArray.toList.zipWithIndex.sortWith((a, b) => a._1 > b._1).take(5).toArray

      val predictionProbs = predictedProbTuples.map(_._1.toDouble)
      val predictionRanks = predictedProbTuples.map(_._2.toDouble)

      if (c2.incrementAndGet() % 100000 == 0) logger.info("Predicting clusters: %d".format(c2.get))

      val record = DenseVector.vertcat(DenseVector(predictionProbs), DenseVector(predictionRanks))
      record
    }.toList

    val predictionMatrixClusterDistProx = DenseVector.horzcat(predictionRecordsClusterDistProx: _*).t
    
      /**
     * Market dest
     */
    val c3 = new AtomicInteger(0)
    val predictionRecordsMarketDest = testClicks.par.map { click =>
      val predicted = marketDestPredict.predict(click.userId, click.marketId, click.destId, click.continentId)

      val predictedProbTuples = predicted.toArray.toList.zipWithIndex.sortWith((a, b) => a._1 > b._1).take(5).toArray

      val predictionProbs = predictedProbTuples.map(_._1.toDouble)
      val predictionRanks = predictedProbTuples.map(_._2.toDouble)

      if (c3.incrementAndGet() % 100000 == 0) logger.info("Predicting clusters: %d".format(c3.get))

      val record = DenseVector.vertcat(DenseVector(predictionProbs), DenseVector(predictionRanks))
      record
    }.toList

    val predictionMatrixMarketDest = DenseVector.horzcat(predictionRecordsMarketDest: _*).t

    (predictionMatrixClusterDist,predictionMatrixMarketDest,predictionMatrixClusterDistProx)
  }
}