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
import expedia.model.countryuser.CountryUserModel
import expedia.model.dest.DestModel
import expedia.model.marketdest.MarketDestModel
import expedia.model.marketdestuser.MarketDestUserPredictionModel
import expedia.model.marketdestuser.MarketDestUserPredictionModel
import expedia.model.marketmodel.MarketModel
import expedia.model.marketuser.MarketUserModel
import expedia.model.mdp.MdpModel
import expedia.stats.CounterMap
import expedia.stats.MulticlassHistByKey
import expedia.util.TimeDecayService
import expedia.model.ClusterModelBuilder
import expedia.model.ClusterModel
import expedia.model.ClusterModelBuilderFactory
import expedia.CompoundHyperParamsMap
import expedia.model.marketmodel.MarketModelBuilder2
import expedia.model.country.CountryModelBuilder2
import expedia.model.marketuser.MarketUserModelBuilder2
import expedia.model.marketdest.MarketDestModelBuilder2
import expedia.model.mdp.MdpModelBuilder2
import expedia.model.marketdest.MarketDestModelBuilder2
import expedia.model.marketdestuser.MarketDestUserModelBuilder
import expedia.model.marketdest.MarketDestModelBuilder2
import expedia.model.countryuser.CountryUserModelBuilder2
import expedia.model.destcluster.DestClusterModelBuilder2
import expedia.model.dest.DestModelBuilder2
import expedia.model.marketdestcluster.MarketDestClusterModelBuilder2
import expedia.model.dest.DestModelBuilder2
import expedia.model.dest.DestModelBuilder2

case class CmuModelBuilder2(countryModel: CountryModel,
                            destModel: DestModel, marketDestModel: MarketDestModel, marketDestUserModel: MarketDestUserPredictionModel,
                            countryUserModel: CountryUserModel, marketUserModel: MarketUserModel,
                            marketModel: MarketModel, mdpModel: MdpModel, hyperParamsService: HyperParamsService) extends ClusterModelBuilder {

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

    /**
     * Build model
     */

    val predictionMdpuMap = clusterHistByMDPU.getMap.map {
      case ((marketId, destId, isPackage, userId), clusterStat) =>

        //cmu params
        val cmuBeta1 = hyperParamsService.getParamValueForMarketId("expedia.model.cmu.beta1", marketId, hyperParams).toFloat
        val cmuBeta2 = hyperParamsService.getParamValueForMarketId("expedia.model.cmu.beta2", marketId, hyperParams).toFloat
        val cmuBeta3 = hyperParamsService.getParamValueForMarketId("expedia.model.cmu.beta3", marketId, hyperParams).toFloat
        val cmuBeta4 = hyperParamsService.getParamValueForMarketId("expedia.model.cmu.beta4", marketId, hyperParams).toFloat
        val cmuBeta5 = hyperParamsService.getParamValueForMarketId("expedia.model.cmu.beta5", marketId, hyperParams).toFloat
        val cmuBeta6 = hyperParamsService.getParamValueForMarketId("expedia.model.cmu.beta6", marketId, hyperParams).toFloat
        val cmuBeta7 = hyperParamsService.getParamValueForMarketId("expedia.model.cmu.beta7", marketId, hyperParams).toFloat
        val cmuBeta8 = hyperParamsService.getParamValueForMarketId("expedia.model.cmu.beta8", marketId, hyperParams).toFloat

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

object CmuModelBuilder2 extends ClusterModelBuilderFactory {

  def build(trainDatasource: ExDataSource, testClicks: Seq[Click], modelHyperParamsMap: CompoundHyperParamsMap): CmuModelBuilder2 = {

    val hyperParamsService = HyperParamsService(testClicks)
    val timeDecayService = TimeDecayService(testClicks)

    val destClusterModel = DestClusterModelBuilder2.build(trainDatasource, testClicks, modelHyperParamsMap).
      create(trainDatasource, testClicks, modelHyperParamsMap.getModel("destcluster"))

    val marketDestClusterModel = MarketDestClusterModelBuilder2.build(trainDatasource, testClicks, modelHyperParamsMap).
      create(trainDatasource, testClicks, modelHyperParamsMap.getModel("marketdestcluster"))

    val countryUserModel = CountryUserModelBuilder2.build(trainDatasource, testClicks, modelHyperParamsMap).
      create(trainDatasource, testClicks, modelHyperParamsMap.getModel("countryuser"))

    val marketUserModel = MarketUserModelBuilder2.build(trainDatasource, testClicks, modelHyperParamsMap).
      create(trainDatasource, testClicks, modelHyperParamsMap.getModel("marketuser"))

    val mdpModel = MdpModelBuilder2.build(trainDatasource, testClicks, modelHyperParamsMap)
      .create(trainDatasource, testClicks, modelHyperParamsMap.getModel("mdp"))

    val marketDestUserModel = MarketDestUserModelBuilder.build(trainDatasource, testClicks, modelHyperParamsMap)
      .create(trainDatasource, testClicks, modelHyperParamsMap.getModel("marketdestuser"))

    val marketModel = marketUserModel.marketModel
    val countryModel = marketModel.countryModel
    val marketDestModel = marketDestUserModel.marketDestModel
    val destModel = marketDestModel.destModel

    val cmuModelBuilder = CmuModelBuilder2(countryModel, destModel, marketDestModel, marketDestUserModel,
      countryUserModel, marketUserModel, marketModel, mdpModel, hyperParamsService)
    cmuModelBuilder
  }
}