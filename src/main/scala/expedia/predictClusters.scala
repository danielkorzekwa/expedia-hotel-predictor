package expedia

import com.typesafe.scalalogging.slf4j.LazyLogging
import breeze.linalg.DenseMatrix
import expedia.data.Click
import expedia.data.ExDataSource
import expedia.model.clusterdist2.ClusterDist2ModelBuilder
import expedia.model.clusterdistprox.ClusterDistProxModelBuilder
import expedia.model.country.CountryModelBuilder
import expedia.model.dest.DestModelBuilder
import expedia.model.marketdestuser.MarketDestUserPredictionModelBuilder
import expedia.model.marketmodel.MarketModelBuilder
import expedia.model.regdest.RegDestModelBuilder
import expedia.stats.CounterMap
import expedia.model.clusterdistbayes.ClusterDistBayesPredictionModel
import expedia.model.clusterdist.ClusterDistPredictionModelBuilder
import expedia.model.countryuser.CountryUserModelBuilder
import expedia.model.marketdestuser.MarketDestUserPredictionModelBuilder
import expedia.model.marketdest.MarketDestModelBuilder
import expedia.model.destbydist.DestByDistModelBuilder

object predictClusters extends LazyLogging {

  /**
   * @return Top 5 predictions for three models[clusterDist,marketDest,clusterDistProx]. ClusterDist: [p1,p2,p3,p4,p5,c1,c2,c3,c4,c5]
   */
  def apply(trainDS: ExDataSource, testClicks: Seq[Click]): Tuple3[DenseMatrix[Double], DenseMatrix[Double], DenseMatrix[Double]] = {

    /**
     * Create counters
     */
    val destMarketCounterMap = CounterMap[Tuple2[Int, Int]]
    val destCounterMap = CounterMap[Int]()
    val marketCounterMap = CounterMap[Int]()
    def onClickCounters(click: Click) = {
      if (click.isBooking == 1) {
        destMarketCounterMap.add((click.destId, click.marketId))
        destCounterMap.add(click.destId)
        marketCounterMap.add(click.marketId)
      }
    }
    trainDS.foreach { click => onClickCounters(click) }

    /**
     * Create models
     */

    val clusterDistModelBuilder = ClusterDistPredictionModelBuilder(testClicks)
    val clusterDistProxModelBuilder = ClusterDistProxModelBuilder(testClicks)

    val countryModelBuilder = CountryModelBuilder(testClicks)
    val marketModelBuilder = MarketModelBuilder(testClicks)
    val destModelBuilder = DestModelBuilder(testClicks)
    val regDestModelBuilder = RegDestModelBuilder()
    val countryUserModelBuilder = CountryUserModelBuilder(testClicks)

     val destByDistModelBuilder = DestByDistModelBuilder(testClicks)
    
    val marketDestModelBuilder = MarketDestModelBuilder(testClicks,  destMarketCounterMap, destCounterMap, marketCounterMap)

    val marketDestUserPredictBuilder = MarketDestUserPredictionModelBuilder(testClicks)

    def onClick(click: Click) = {
      clusterDistModelBuilder.processCluster(click)
      clusterDistProxModelBuilder.processCluster(click)

      countryModelBuilder.processCluster(click)
      marketModelBuilder.processCluster(click)
      destModelBuilder.processCluster(click)
      regDestModelBuilder.processCluster(click)
      marketDestUserPredictBuilder.processCluster(click)
      countryUserModelBuilder.processCluster(click)
        destByDistModelBuilder.processCluster(click)
      
      marketDestModelBuilder.processCluster(click)
    }
    trainDS.foreach { click => onClick(click) }

    val clusterDistProxModel = clusterDistProxModelBuilder.create()

    val countryModel = countryModelBuilder.create()
    val marketModel = marketModelBuilder.create(countryModel)
    val clusterDistModel = clusterDistModelBuilder.create(marketModel)
    val destModel = destModelBuilder.create(countryModel)
    val regDestModel = regDestModelBuilder.create()
    val countryUserModel = countryUserModelBuilder.create(countryModel)
    val marketDestModel = marketDestModelBuilder.create(destModel, marketModel, countryModel, destMarketCounterMap, destCounterMap, marketCounterMap)
   val destByDistModel = destByDistModelBuilder.create()
    
    val marketDestUserPredict = marketDestUserPredictBuilder.create(
      destModel, countryModel, destMarketCounterMap, destCounterMap, marketCounterMap, regDestModel, marketModel, 
      countryUserModel, marketDestModel,clusterDistProxModel,destByDistModel)

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
    val predictionMatrixMarketDest = marketDestUserPredict.predictTop5(testClicks)

    (predictionMatrixClusterDist, predictionMatrixMarketDest, predictionMatrixClusterDistProx)
  }
}