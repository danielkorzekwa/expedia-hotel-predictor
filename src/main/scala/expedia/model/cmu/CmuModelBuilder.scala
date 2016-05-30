package expedia.model.cmu

import expedia.data.Click
import expedia.HyperParams
import expedia.data.ExDataSource
import expedia.util.TimeDecayService
import expedia.model.country.CountryModelBuilder
import expedia.model.marketuser.MarketUserModel
import expedia.model.country.CountryModel
import expedia.stats.MulticlassHistByKey
import scala.collection._
import breeze.linalg.DenseVector
import breeze.linalg._
import expedia.stats.CounterMap
import expedia.stats.MulticlassHist
import expedia.stats.normaliseMutable
import expedia.model.dest.DestModel
import expedia.model.dest.DestModelBuilder
import expedia.model.destcluster.DestClusterModelBuilder
import expedia.model.marketdest.MarketDestModel
import expedia.model.marketdest.MarketDestModelBuilder
import expedia.model.dest.DestModelBuilder
import expedia.model.dest.DestModelBuilder
import expedia.model.marketmodel.MarketModelBuilder
import expedia.model.dest.DestModelBuilder
import expedia.model.dest.DestModelBuilder
import expedia.model.dest.DestModelBuilder
import expedia.model.dest.DestModelBuilder
import expedia.model.dest.DestModelBuilder
import expedia.model.marketdestcluster.MarketDestClusterModelBuilder
import expedia.model.marketdestuser.MarketDestUserPredictionModel
import expedia.model.marketdestuser.MarketDestUserPredictionModel
import expedia.model.marketdestuser.MarketDestUserPredictionModelBuilder
import expedia.model.countryuser.CountryUserModelBuilder
import expedia.model.marketuser.MarketUserModelBuilder
import expedia.model.countryuser.CountryUserModel
import expedia.model.marketmodel.MarketModel
import expedia.model.mdp.MdpModel
import expedia.model.mdp.MdpModelBuilder

case class CmuModelBuilder(testClicks: Seq[Click], hyperParams: HyperParams, timeDecayService: TimeDecayService) {

  //key ((marketId,destId,isPackage,userId)
  private val clusterHistByMDPU = MulticlassHistByKey[Tuple4[Int, Int, Int, Int]](100)
  testClicks.foreach { click =>
    val key = (click.marketId, click.destId, click.isPackage, click.userId)
    clusterHistByMDPU.add(key, click.cluster, value = 0)
  }

  private val countryByMarket: mutable.Map[Int, Int] = mutable.Map()
  testClicks.foreach(click => countryByMarket += click.marketId -> click.countryId)

  private val userCounterMap = CounterMap[Int]()

  //market dest params
  private val destMarketCountsThreshold1 = hyperParams.getParamValue("expedia.model.marketdest.destMarketCountsThreshold1").toFloat
  private val destMarketCountsThresholdClickWeight1 = hyperParams.getParamValue("expedia.model.marketdest.destMarketCountsThresholdClickWeight1").toFloat
  private val destMarketCountsThreshold2 = hyperParams.getParamValue("expedia.model.marketdest.destMarketCountsThreshold2").toFloat
  private val destMarketCountsThresholdClickWeight2 = hyperParams.getParamValue("expedia.model.marketdest.destMarketCountsThresholdClickWeight2").toFloat
  private val destMarketCountsDefaultWeight = hyperParams.getParamValue("expedia.model.marketdest.destMarketCountsDefaultWeight").toFloat
  private val destMarketCountsDefaultWeightBeta3 = hyperParams.getParamValue("expedia.model.marketdest.beta3").toFloat

  //cmu params
  private val cmuBeta1 = hyperParams.getParamValue("expedia.model.cmu.beta1").toFloat
  private val cmuBeta2 = hyperParams.getParamValue("expedia.model.cmu.beta2").toFloat
  private val cmuBeta3 = hyperParams.getParamValue("expedia.model.cmu.beta3").toFloat
  private val cmuBeta4 = hyperParams.getParamValue("expedia.model.cmu.beta4").toFloat
  private val cmuBeta5 = hyperParams.getParamValue("expedia.model.cmu.beta5").toFloat
  private val cmuBeta6 = hyperParams.getParamValue("expedia.model.cmu.beta6").toFloat
  private val cmuBeta7 = hyperParams.getParamValue("expedia.model.cmu.beta7").toFloat
  private val cmuBeta8 = hyperParams.getParamValue("expedia.model.cmu.beta8").toFloat

  private val mdpuBeta1 = hyperParams.getParamValue("expedia.model.mdpu.beta1").toFloat

  def processCluster(click: Click) = {

    val w = timeDecayService.getDecay(click.dateTime)

    userCounterMap.add(click.userId)
  }

  def create(countryModel: CountryModel, destCounterMap: CounterMap[Int], destMarketCounterMap: CounterMap[Tuple2[Int, Int]],
             destModel: DestModel, marketDestModel: MarketDestModel, marketDestUserModel: MarketDestUserPredictionModel,
             countryUserModel: CountryUserModel, marketUserModel: MarketUserModel,
             marketModel: MarketModel, mdpModel: MdpModel): CmuModel = {

    val predictionMdpuMap = clusterHistByMDPU.getMap.map {
      case ((marketId, destId, isPackage, userId), clusterStat) =>
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
        //  val predicted =muPred
        (marketId, destId, isPackage, userId) -> predicted

    }
    CmuModel(predictionMdpuMap, userCounterMap, destCounterMap, destMarketCounterMap, destModel, hyperParams)
  }
}

object CmuModelBuilder {

  def buildFromTrainingSet(trainDatasource: ExDataSource, testClicks: Seq[Click], hyperParams: HyperParams): CmuModel = {
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

    val timeDecayService = TimeDecayService(testClicks, hyperParams)

    val countryModelBuilder = CountryModelBuilder(testClicks, hyperParams, timeDecayService)

    val marketModelBuilder = MarketModelBuilder(testClicks, hyperParams, timeDecayService)
    val destModelBuilder = DestModelBuilder(testClicks, hyperParams, timeDecayService)

    val destClusterModelBuilder = DestClusterModelBuilder(testClicks, hyperParams, timeDecayService)
    val marketDestModelBuilder = MarketDestModelBuilder(testClicks, destMarketCounterMap, destCounterMap, marketCounterMap, hyperParams, timeDecayService)
    val marketDestClusterModelBuilder = MarketDestClusterModelBuilder(testClicks, hyperParams, timeDecayService)

    val cmuModelBuilder = CmuModelBuilder(testClicks,  hyperParams, timeDecayService)

    val marketDestUserModelBuilder = MarketDestUserPredictionModelBuilder(testClicks, hyperParams, timeDecayService)

    val countryUserModelBuilder = CountryUserModelBuilder(testClicks, hyperParams)
    val marketUserModelBuilder = MarketUserModelBuilder(testClicks, hyperParams, timeDecayService)

    val mdpModelBuilder = MdpModelBuilder(testClicks, destMarketCounterMap, destCounterMap, marketCounterMap, hyperParams, timeDecayService)

    def onClick(click: Click) = {
      marketModelBuilder.processCluster(click)
      countryModelBuilder.processCluster(click)
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
    val cmuModel = cmuModelBuilder.create(countryModel, destCounterMap, destMarketCounterMap, destModel, marketDestModel, marketDestUserModel,
      countryUserModel, marketUserModel, marketModel, mdpModel)
    cmuModel
  }
}