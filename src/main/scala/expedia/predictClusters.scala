package expedia

import com.typesafe.scalalogging.slf4j.LazyLogging
import breeze.linalg.DenseMatrix
import expedia.data.Click
import expedia.data.ExDataSource
import expedia.model.clusterdist.ClusterDistPredictionModelBuilder
import expedia.model.clusterdistprox.ClusterDistProxModelBuilder
import expedia.model.country.CountryModelBuilder
import expedia.model.countryuser.CountryUserModelBuilder
import expedia.model.dest.DestModelBuilder
import expedia.model.marketdest.MarketDestModelBuilder
import expedia.model.marketdestuser.MarketDestUserPredictionModelBuilder
import expedia.model.marketmodel.MarketModelBuilder
import expedia.model.marketuser.MarketUserModelBuilder
import expedia.model.mdp.MdpModelBuilder
import expedia.model.mdpu.MdpuModelBuilder
import expedia.stats.CounterMap
import expedia.util.TimeDecayService
import expedia.model.cmu.CmuModelBuilder

object predictClusters extends LazyLogging {

  /**
   * @return Top 5 predictions for three models[clusterDist,marketDest,clusterDistProx]. ClusterDist: [p1,p2,p3,p4,p5,c1,c2,c3,c4,c5]
   */
  def apply(trainDS: ExDataSource, testClicks: Seq[Click],hyperParams:HyperParams): Tuple3[DenseMatrix[Double], DenseMatrix[Double], DenseMatrix[Double]] = {

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

    val timeDecayService = TimeDecayService(testClicks,hyperParams)
    /**
     * Create models
     */
    
    val clusterDistModelBuilder = ClusterDistPredictionModelBuilder(testClicks,hyperParams)
    val clusterDistProxModelBuilder = ClusterDistProxModelBuilder(testClicks)

    val countryModelBuilder = CountryModelBuilder(testClicks,hyperParams,timeDecayService)
    val marketModelBuilder = MarketModelBuilder(testClicks,hyperParams,timeDecayService)
    val destModelBuilder = DestModelBuilder(testClicks,hyperParams,timeDecayService)
    val countryUserModelBuilder = CountryUserModelBuilder(testClicks,hyperParams)

    val marketDestModelBuilder = MarketDestModelBuilder(testClicks, destMarketCounterMap, destCounterMap, marketCounterMap,hyperParams,timeDecayService)
    val marketUserModelBuilder = MarketUserModelBuilder(testClicks,hyperParams,timeDecayService)
    val mdpModelBuilder = MdpModelBuilder(testClicks,destMarketCounterMap, destCounterMap, marketCounterMap,hyperParams,timeDecayService)
    val marketDestUserPredictBuilder = MarketDestUserPredictionModelBuilder(testClicks,hyperParams,timeDecayService)
  //  val mdpuModelBuilder = MdpuModelBuilder(testClicks,hyperParams,timeDecayService)

    val cmuModelBuilder = CmuModelBuilder(testClicks,destMarketCounterMap,destCounterMap,marketCounterMap,hyperParams,timeDecayService)
    
    def onClick(click: Click) = {
      clusterDistModelBuilder.processCluster(click)
      clusterDistProxModelBuilder.processCluster(click)

      countryModelBuilder.processCluster(click)
      marketModelBuilder.processCluster(click)
      destModelBuilder.processCluster(click)
      marketDestUserPredictBuilder.processCluster(click)
      countryUserModelBuilder.processCluster(click)

      marketDestModelBuilder.processCluster(click)
      mdpModelBuilder.processCluster(click)
      marketUserModelBuilder.processCluster(click)
    //  mdpuModelBuilder.processCluster(click)
      cmuModelBuilder.processCluster(click)
    }
    trainDS.foreach { click => onClick(click) }

    val clusterDistProxModel = clusterDistProxModelBuilder.create()

    val countryModel = countryModelBuilder.create()
    val marketModel = marketModelBuilder.create(countryModel)
    val clusterDistModel = clusterDistModelBuilder.create(marketModel)
    val destModel = destModelBuilder.create(countryModel)
    val countryUserModel = countryUserModelBuilder.create(countryModel)
    val marketDestModel = marketDestModelBuilder.create(destModel, marketModel, countryModel, destMarketCounterMap, destCounterMap, marketCounterMap)

    val marketUserModel = marketUserModelBuilder.create(countryUserModel, marketModel)

    val marketDestUserModel = marketDestUserPredictBuilder.create(
       countryModel, destMarketCounterMap, destCounterMap, marketCounterMap,  marketModel,
      countryUserModel, marketDestModel, marketUserModel)

      val mdpModel = mdpModelBuilder.create(destModel, marketModel, countryModel, destMarketCounterMap, destCounterMap, marketCounterMap,marketDestModel)
    // val mdpuModel = mdpuModelBuilder.create(marketDestUserModel, marketDestModel, mdpModel, destCounterMap, destMarketCounterMap, destModel)
    val cmuModel = cmuModelBuilder.create(countryModel, destCounterMap, destMarketCounterMap, destModel)
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
    val predictionMatrixMarketDest = cmuModel.predictTop5(testClicks)

    (predictionMatrixClusterDist, predictionMatrixMarketDest, predictionMatrixClusterDistProx)
  }
}