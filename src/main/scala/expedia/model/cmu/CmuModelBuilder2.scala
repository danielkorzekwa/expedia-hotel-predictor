package expedia.model.cmu

import scala.collection._
import breeze.linalg._
import breeze.linalg.DenseVector
import expedia.CompoundHyperParams
import expedia.CompoundHyperParams
import expedia.HyperParamsService
import expedia.data.Click
import expedia.data.ExDataSource
import expedia.model.country.CountryModel
import expedia.model.country.CountryModelBuilder
import expedia.model.countryuser.CountryUserModel
import expedia.model.countryuser.CountryUserModelBuilder
import expedia.model.dest.DestModel
import expedia.model.dest.DestModelBuilder
import expedia.model.dest.DestModelBuilder
import expedia.model.dest.DestModelBuilder
import expedia.model.dest.DestModelBuilder
import expedia.model.dest.DestModelBuilder
import expedia.model.dest.DestModelBuilder
import expedia.model.dest.DestModelBuilder
import expedia.model.dest.DestModelBuilder
import expedia.model.destcluster.DestClusterModelBuilder
import expedia.model.marketdest.MarketDestModel
import expedia.model.marketdest.MarketDestModelBuilder
import expedia.model.marketdestcluster.MarketDestClusterModelBuilder
import expedia.model.marketdestuser.MarketDestUserPredictionModel
import expedia.model.marketdestuser.MarketDestUserPredictionModel
import expedia.model.marketdestuser.MarketDestUserPredictionModelBuilder
import expedia.model.marketmodel.MarketModel
import expedia.model.marketmodel.MarketModelBuilder
import expedia.model.marketuser.MarketUserModel
import expedia.model.marketuser.MarketUserModelBuilder
import expedia.model.mdp.MdpModel
import expedia.model.mdp.MdpModelBuilder
import expedia.stats.CounterMap
import expedia.stats.MulticlassHistByKey
import expedia.util.TimeDecayService
import expedia.model.ClusterModelBuilder
import expedia.model.ClusterModel
import expedia.model.ClusterModelBuilderFactory
import expedia.CompoundHyperParamsMap

case class CmuModelBuilder2(countryModel: CountryModel, destCounterMap: CounterMap[Int], destMarketCounterMap: CounterMap[Tuple2[Int, Int]],
                            destModel: DestModel, marketDestModel: MarketDestModel, marketDestUserModel: MarketDestUserPredictionModel,
                            countryUserModel: CountryUserModel, marketUserModel: MarketUserModel,
                            marketModel: MarketModel, mdpModel: MdpModel,hyperParamsService:HyperParamsService) extends ClusterModelBuilder{

  def create(trainDatasource: ExDataSource, testClicks: Seq[Click], hyperParams: CompoundHyperParams): CmuModel = {

    /**
     * Process training set
     */
    
    //key ((marketId,destId,isPackage,userId)
    val clusterHistByMDPU = MulticlassHistByKey[Tuple4[Int, Int, Int, Int]](100)
    testClicks.foreach { click =>
      val key = (click.marketId, click.destId, click.isPackage, click.userId)
      clusterHistByMDPU.add(key, click.cluster, value = 0)
    }

    val countryByMarket: mutable.Map[Int, Int] = mutable.Map()
    testClicks.foreach(click => countryByMarket += click.marketId -> click.countryId)

    val userCounterMap = CounterMap[Int]()

    def onClick(click: Click) = {
      userCounterMap.add(click.userId)
    }
    trainDatasource.foreach { click => onClick(click) }

    
    /**
     * Build model
     */
    
    val predictionMdpuMap = clusterHistByMDPU.getMap.map {
      case ((marketId, destId, isPackage, userId), clusterStat) =>

        //cmu params
        val cmuBeta1 = hyperParamsService.getParamValueForMarketId("expedia.model.cmu.beta1",marketId,hyperParams).toFloat
   val cmuBeta2 = hyperParamsService.getParamValueForMarketId("expedia.model.cmu.beta2",marketId,hyperParams).toFloat
   val cmuBeta3 = hyperParamsService.getParamValueForMarketId("expedia.model.cmu.beta3",marketId,hyperParams).toFloat
   val cmuBeta4 = hyperParamsService.getParamValueForMarketId("expedia.model.cmu.beta4",marketId,hyperParams).toFloat
   val cmuBeta5 = hyperParamsService.getParamValueForMarketId("expedia.model.cmu.beta5",marketId,hyperParams).toFloat
   val cmuBeta6 = hyperParamsService.getParamValueForMarketId("expedia.model.cmu.beta6",marketId,hyperParams).toFloat
   val cmuBeta7 = hyperParamsService.getParamValueForMarketId("expedia.model.cmu.beta7",marketId,hyperParams).toFloat
   val cmuBeta8 = hyperParamsService.getParamValueForMarketId("expedia.model.cmu.beta8",marketId,hyperParams).toFloat

        val c = countryModel.predict(countryByMarket(marketId))

        val cmPred = marketModel.predict(marketId)
        val cm = cmPred - c

        val muPred = marketUserModel.predict(marketId, userId)
        val mu = muPred - (c + cm)

        val cu = if (countryUserModel.predictionExists(countryByMarket(marketId), userId)) {
          val cuPred = countryUserModel.predict(countryByMarket(marketId), userId)
          val cu = cuPred - c
          cu
        } else DenseVector.fill(100)(1e-10f)

        val mdPred = marketDestModel.predict(marketId, destId)
        val md = mdPred - (c + cm)

        val mdpPred = mdpModel.predict(marketId, destId, isPackage)
        val mdp = mdpPred - mdPred

        val mduPred = marketDestUserModel.predict(marketId, destId, userId)
        val mdu2 = mduPred - mdPred
        val predicted = cmuBeta1 * c + cmuBeta2 * cm + cmuBeta3 * md + cmuBeta4 * mu + cmuBeta5 * cu + cmuBeta6 * mdu2 + cmuBeta7 * mdp
        (marketId, destId, isPackage, userId) -> predicted

    }
    CmuModel(predictionMdpuMap, userCounterMap, destCounterMap, destMarketCounterMap, destModel)
  }
}

object CmuModelBuilder2 extends ClusterModelBuilderFactory{

  def build(trainDatasource: ExDataSource, testClicks: Seq[Click], modelHyperParamsMap: CompoundHyperParamsMap): CmuModelBuilder2 = {
   
    val hyperParams = modelHyperParamsMap.getModel("default")
    val hyperParamsService = HyperParamsService(testClicks)
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
    trainDatasource.foreach { click => onClickCounters(click) }

    val timeDecayService = TimeDecayService(testClicks, hyperParamsService,hyperParams)

    val countryModelBuilder = CountryModelBuilder(testClicks, hyperParamsService, hyperParams,timeDecayService)

    val marketModelBuilder = MarketModelBuilder(testClicks, hyperParamsService,  modelHyperParamsMap.getModel("market"),timeDecayService)
    val destModelBuilder = DestModelBuilder(testClicks,  hyperParamsService,hyperParams, timeDecayService)

    val destClusterModelBuilder = DestClusterModelBuilder(testClicks,  hyperParamsService,hyperParams, timeDecayService)
    val marketDestModelBuilder = MarketDestModelBuilder(testClicks, destMarketCounterMap, destCounterMap, marketCounterMap,  hyperParamsService,hyperParams, timeDecayService)
    val marketDestClusterModelBuilder = MarketDestClusterModelBuilder(testClicks,  hyperParamsService,hyperParams, timeDecayService)

    val marketDestUserModelBuilder = MarketDestUserPredictionModelBuilder(testClicks,  hyperParamsService,hyperParams, timeDecayService)

    val countryUserModelBuilder = CountryUserModelBuilder(testClicks,  hyperParamsService,hyperParams)
    val marketUserModelBuilder = MarketUserModelBuilder(testClicks,  hyperParamsService,hyperParams, timeDecayService)

    val mdpModelBuilder = MdpModelBuilder(testClicks, destMarketCounterMap, destCounterMap, marketCounterMap,hyperParamsService, hyperParams, timeDecayService)

    def onClick(click: Click) = {
      marketModelBuilder.processCluster(click)
      countryModelBuilder.processCluster(click)
      destModelBuilder.processCluster(click)
      destClusterModelBuilder.processCluster(click)
      marketDestModelBuilder.processCluster(click)
      marketDestClusterModelBuilder.processCluster(click)
      marketDestUserModelBuilder.processCluster(click)
      countryUserModelBuilder.processCluster(click)
      marketUserModelBuilder.processCluster(click)
      mdpModelBuilder.processCluster(click)
    }
    trainDatasource.foreach { click => onClick(click) }

    val countryModel = countryModelBuilder.create()
    val marketModel = marketModelBuilder.create(countryModel)
    val destClusterModel = destClusterModelBuilder.create(countryModel, marketModel)
    val marketDestClusterModel = marketDestClusterModelBuilder.create(countryModel, marketModel)
    val destModel = destModelBuilder.create(countryModel, destClusterModel)
    val countryUserModel = countryUserModelBuilder.create(countryModel)
    val marketUserModel = marketUserModelBuilder.create(countryUserModel, marketModel)
    val marketDestModel = marketDestModelBuilder.create(
      destModel, marketModel, countryModel, destMarketCounterMap, destCounterMap, marketCounterMap, destClusterModel, marketDestClusterModel)

    val mdpModel = mdpModelBuilder.create(destModel, marketModel, countryModel, destMarketCounterMap, destCounterMap, marketCounterMap, marketDestModel)

    val marketDestUserModel = marketDestUserModelBuilder.create(countryModel, destMarketCounterMap, destCounterMap, marketCounterMap, marketModel, countryUserModel, marketDestModel, marketUserModel)

    val cmuModelBuilder = CmuModelBuilder2(countryModel, destCounterMap, destMarketCounterMap, destModel, marketDestModel, marketDestUserModel,
      countryUserModel, marketUserModel, marketModel, mdpModel,hyperParamsService)
    cmuModelBuilder
  }
}