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
import expedia.model.destcluster.DestClusterModelBuilder
import expedia.model.marketdestcluster.MarketDestClusterModelBuilder
import expedia.model.distsvm.DistSvmModel
import expedia.model.distgp.DistGpModel
import expedia.model.distsvm.DistSvmModel

object predictClustersCMU extends LazyLogging {

  /**
   * @return Top 5 predictions for four models[clusterDist,marketDest,clusterDistProx]. ClusterDist: [p1,p2,p3,p4,p5,c1,c2,c3,c4,c5]
   */
  def apply(trainDS: ExDataSource, testClicks: Seq[Click], hyperParams: HyperParams): Tuple4[DenseMatrix[Double], DenseMatrix[Double], DenseMatrix[Double],DenseMatrix[Double]] = {

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

    val timeDecayService = TimeDecayService(testClicks, hyperParams)
    /**
     * Create models
     */

    val clusterDistModelBuilder = ClusterDistPredictionModelBuilder(testClicks, hyperParams)
    val clusterDistProxModelBuilder = ClusterDistProxModelBuilder(testClicks)

    val countryModelBuilder = CountryModelBuilder(testClicks, hyperParams, timeDecayService)
    val marketModelBuilder = MarketModelBuilder(testClicks, hyperParams, timeDecayService)
    val destModelBuilder = DestModelBuilder(testClicks, hyperParams, timeDecayService)
    val destClusterModelBuilder = DestClusterModelBuilder(testClicks, hyperParams, timeDecayService)
    val marketDestModelBuilder = MarketDestModelBuilder(testClicks, destMarketCounterMap, destCounterMap, marketCounterMap, hyperParams, timeDecayService)
    val marketDestClusterModelBuilder = MarketDestClusterModelBuilder(testClicks, hyperParams, timeDecayService)
    val marketDestUserModelBuilder = MarketDestUserPredictionModelBuilder(testClicks, hyperParams, timeDecayService)
    val countryUserModelBuilder = CountryUserModelBuilder(testClicks, hyperParams)
    val marketUserModelBuilder = MarketUserModelBuilder(testClicks, hyperParams, timeDecayService)
    val mdpModelBuilder = MdpModelBuilder(testClicks, destMarketCounterMap, destCounterMap, marketCounterMap, hyperParams, timeDecayService)

    val cmuModelBuilder = CmuModelBuilder(testClicks, hyperParams, timeDecayService)

    def onClick(click: Click) = {
      clusterDistModelBuilder.processCluster(click)
      clusterDistProxModelBuilder.processCluster(click)

      countryModelBuilder.processCluster(click)
      marketModelBuilder.processCluster(click)
      destModelBuilder.processCluster(click)

      cmuModelBuilder.processCluster(click)
      destClusterModelBuilder.processCluster(click)
      marketDestModelBuilder.processCluster(click)
      marketDestClusterModelBuilder.processCluster(click)
      marketDestUserModelBuilder.processCluster(click)
      countryUserModelBuilder.processCluster(click)
      marketUserModelBuilder.processCluster(click)
      mdpModelBuilder.processCluster(click)
    }
    trainDS.foreach { click => onClick(click) }

    val clusterDistProxModel = clusterDistProxModelBuilder.create()

    val countryModel = countryModelBuilder.create()
    val marketModel = marketModelBuilder.create(countryModel)
    val clusterDistModel = clusterDistModelBuilder.create(marketModel)
    val destClusterModel = destClusterModelBuilder.create(countryModel, null)
    val destModel = destModelBuilder.create(countryModel, destClusterModel)
    val marketDestClusterModel = marketDestClusterModelBuilder.create(countryModel, marketModel)
    val marketDestModel = marketDestModelBuilder.create(
      destModel, marketModel, countryModel, destMarketCounterMap, destCounterMap, marketCounterMap, destClusterModel, marketDestClusterModel)

    val countryUserModel = countryUserModelBuilder.create(countryModel)
    val marketUserModel = marketUserModelBuilder.create(countryUserModel, marketModel)
    val marketDestUserModel = marketDestUserModelBuilder.create(countryModel, destMarketCounterMap, destCounterMap, marketCounterMap, marketModel, countryUserModel, marketDestModel, marketUserModel)

    val mdpModel = mdpModelBuilder.create(destModel, marketModel, countryModel, destMarketCounterMap, destCounterMap, marketCounterMap, marketDestModel)

    val cmuModel = cmuModelBuilder.create(countryModel, destCounterMap, destMarketCounterMap, destModel, marketDestModel, marketDestUserModel,
      countryUserModel, marketUserModel, marketModel, mdpModel)
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

    /**
     * Dist svm
     */
    val distSvmMatrix = DistSvmModel().predictTop5(testClicks)
    
    (predictionMatrixClusterDist, predictionMatrixMarketDest, predictionMatrixClusterDistProx,distSvmMatrix)
  }
}