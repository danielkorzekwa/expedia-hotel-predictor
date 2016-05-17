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
import expedia.model.clusterdist2.ClusterDist2ModelBuilder
import expedia.model.regdest.RegDestModelBuilder

object predictClusters extends LazyLogging {

  /**
   * @return Top 5 predictions for three models[clusterDist,marketDest,clusterDistProx]. ClusterDist: [p1,p2,p3,p4,p5,c1,c2,c3,c4,c5]
   */
  def apply(trainDS: ExDataSource, testClicks: Seq[Click]): Tuple3[DenseMatrix[Double], DenseMatrix[Double], DenseMatrix[Double]] = {

    val clusterDistModelBuilder = ClusterDistPredictionModelBuilder()
    val clusterDistProxModelBuilder = ClusterDistProxModelBuilder(testClicks)

    val countryModelBuilder = CountryModelBuilder(testClicks)
    val destModelBuilder = DestModelBuilder(testClicks)
    val regDestModelBuilder = RegDestModelBuilder()
    
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
      regDestModelBuilder.processCluster(click)
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
    val regDestModel = regDestModelBuilder.create()
    val marketDestPredict = marketDestPredictBuilder.create(destModel, countryModel, destMarketCounterMap, destCounterMap, marketCounterMap,regDestModel)

    /**
     * Cluster dist
     */
    val predictionMatrixClusterDist = clusterDistModel.predictTop5(testClicks)

    /**
     * Cluster dist prox
     */
    val predictionMatrixClusterDistProx = clusterDistProxModel.predictTop5(testClicks)
    
      /**
     * Market dest
     */
    val predictionMatrixMarketDest = marketDestPredict.predictTop5(testClicks)

    (predictionMatrixClusterDist,predictionMatrixMarketDest,predictionMatrixClusterDistProx)
  }
}