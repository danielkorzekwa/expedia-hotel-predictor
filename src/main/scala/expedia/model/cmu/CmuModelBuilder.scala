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

case class CmuModelBuilder(testClicks: Seq[Click],
                           destMarketCounterMap: CounterMap[Tuple2[Int, Int]],
                           destCounterMap: CounterMap[Int], marketCounterMap: CounterMap[Int],
                           hyperParams: HyperParams, timeDecayService: TimeDecayService) {

  private val clusterHistByMarket = MulticlassHistByKey[Int](100)
  testClicks.foreach { click => clusterHistByMarket.add(click.marketId, click.cluster, value = 0)
  }

  //key ((countryId,userId)
  private val clusterHistByCountryUser = MulticlassHistByKey[Tuple2[Int, Int]](100)
  testClicks.foreach(click => clusterHistByCountryUser.add((click.countryId, click.userId), click.cluster, value = 0))

  private val clusterHistByMarketUser = MulticlassHistByKey[Tuple2[Int, Int]](100)
  testClicks.foreach(click => clusterHistByMarketUser.add((click.marketId, click.userId), click.cluster, value = 0))


  //key ((marketId,destId,isPackage)
  private val clusterHistByMDP = MulticlassHistByKey[Tuple3[Int, Int, Int]](100)
  testClicks.foreach { click =>
    clusterHistByMDP.add((click.marketId, click.destId, click.isPackage), click.cluster, value = 0)
  }

  //key ((destId, marketId,userId)
  private val clusterHistByDestMarketUser = MulticlassHistByKey[Tuple3[Int, Int, Int]](100)
  testClicks.foreach { click =>
    clusterHistByDestMarketUser.add((click.destId, click.marketId, click.userId), click.cluster, value = 0)
  }

  //key ((marketId,destId,isPackage,userId)
  private val clusterHistByMDPU = MulticlassHistByKey[Tuple4[Int, Int, Int, Int]](100)
  testClicks.foreach { click =>
    val key = (click.marketId, click.destId, click.isPackage, click.userId)
    clusterHistByMDPU.add(key, click.cluster, value = 0)
  }

  private val countryByMarket: mutable.Map[Int, Int] = mutable.Map()
  testClicks.foreach(click => countryByMarket += click.marketId -> click.countryId)

  private val clusterHistByMarketBeta1 = hyperParams.getParamValue("expedia.model.marketmodel.beta1").toFloat

  private val clusterHistByCountryUserBeta1 = hyperParams.getParamValue("expedia.model.countryuser.beta1").toFloat
  private val clusterHistByCountryUserBeta2 = hyperParams.getParamValue("expedia.model.countryuser.beta2").toFloat

  private val clusterHistByMarketUserBeta1 = hyperParams.getParamValue("expedia.model.marketuser.beta1").toFloat

  private val userCounterMap = CounterMap[Int]()

  //market dest params
  private val destMarketCountsThreshold1 = hyperParams.getParamValue("expedia.model.marketdest.destMarketCountsThreshold1").toFloat
  private val destMarketCountsThresholdClickWeight1 = hyperParams.getParamValue("expedia.model.marketdest.destMarketCountsThresholdClickWeight1").toFloat
  private val destMarketCountsThreshold2 = hyperParams.getParamValue("expedia.model.marketdest.destMarketCountsThreshold2").toFloat
  private val destMarketCountsThresholdClickWeight2 = hyperParams.getParamValue("expedia.model.marketdest.destMarketCountsThresholdClickWeight2").toFloat
  private val destMarketCountsDefaultWeight = hyperParams.getParamValue("expedia.model.marketdest.destMarketCountsDefaultWeight").toFloat
  private val destMarketCountsDefaultWeightBeta3 = hyperParams.getParamValue("expedia.model.marketdest.beta3").toFloat

  //mdp params
  private val clusterHistByMDPBeta1 = hyperParams.getParamValue("expedia.model.mdp.beta1").toFloat
  private val clusterHistByMDPBeta2 = hyperParams.getParamValue("expedia.model.mdp.beta2").toFloat
  private val clusterHistByMDPBeta3 = hyperParams.getParamValue("expedia.model.mdp.beta3").toFloat
  private val clusterHistByMDPBeta4 = hyperParams.getParamValue("expedia.model.mdp.beta4").toFloat
  private val clusterHistByMDPBeta5 = hyperParams.getParamValue("expedia.model.mdp.beta5").toFloat
  private val clusterHistByMDPBeta8 = hyperParams.getParamValue("expedia.model.mdp.beta8").toFloat

  //dest market user params
  private val clusterHistByDestMarketUserBeta2 = hyperParams.getParamValue("expedia.model.marketdestuser.beta2").toFloat
  private val clusterHistByDestMarketUserBeta5 = hyperParams.getParamValue("expedia.model.marketdestuser.beta5").toFloat
  private val clusterHistByDestMarketUserBeta6 = hyperParams.getParamValue("expedia.model.marketdestuser.beta6").toFloat

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

    //market
    if (clusterHistByMarket.getMap.contains(click.marketId)) {
      if (click.isBooking == 1) clusterHistByMarket.add(click.marketId, click.cluster, value = w)
      else clusterHistByMarket.add(click.marketId, click.cluster, value = w * clusterHistByMarketBeta1)
    }

    //marketDest
    val marketCounts = marketCounterMap.getOrElse(click.marketId, 0)
    val destMarketCounts = destMarketCounterMap.getOrElse((click.destId, click.marketId), 0)
    val destCounts = destCounterMap.getOrElse(click.destId, 0)
    val clickWeightMd =
      if (destMarketCounts < destMarketCountsThreshold1) destMarketCountsThresholdClickWeight1
      else if (destMarketCounts < destMarketCountsThreshold2) destMarketCountsThresholdClickWeight2
      else destMarketCountsDefaultWeight


    //mdp
    val clickWeightMdp =
      if (destMarketCounts < clusterHistByMDPBeta1) clusterHistByMDPBeta2
      else if (destMarketCounts < clusterHistByMDPBeta3) clusterHistByMDPBeta4
      else clusterHistByMDPBeta5
    val keyMdp = (click.marketId, click.destId, click.isPackage)
    if (clusterHistByMDP.getMap.contains(keyMdp)) {
      if (click.isBooking == 1) clusterHistByMDP.add(keyMdp, click.cluster, value = w)
      else clusterHistByMDP.add(keyMdp, click.cluster, value = w * clickWeightMdp)
    }

    //market user
    val marketUserKey = (click.marketId, click.userId)
    if (clusterHistByMarketUser.getMap.contains(marketUserKey)) {
      if (click.isBooking == 1) clusterHistByMarketUser.add(marketUserKey, click.cluster, value = w)
      else clusterHistByMarketUser.add(marketUserKey, click.cluster, value = w * clusterHistByMarketUserBeta1)
    }

    //country user
    val countryUserKey = (click.countryId, click.userId)
    if (clusterHistByCountryUser.getMap.contains(countryUserKey)) {
      if (click.isBooking == 1) clusterHistByCountryUser.add(countryUserKey, click.cluster)
      else clusterHistByCountryUser.add(countryUserKey, click.cluster, value = clusterHistByCountryUserBeta1)
    }

    //market dest user
    val keyMdu = (click.destId, click.marketId, click.userId)
    if (clusterHistByDestMarketUser.getMap.contains(keyMdu)) {
      if (click.isBooking == 1) clusterHistByDestMarketUser.add(keyMdu, click.cluster, value = w)
      else clusterHistByDestMarketUser.add(keyMdu, click.cluster, value = w * clusterHistByDestMarketUserBeta6)
    }

    //mdpu
    val keyMdpu = (click.marketId, click.destId, click.isPackage, click.userId)
    if (clusterHistByMDPU.getMap.contains(keyMdpu)) {
      if (click.isBooking == 1) clusterHistByMDPU.add(keyMdpu, click.cluster, value = w)
      else clusterHistByMDPU.add(keyMdpu, click.cluster, value = w * mdpuBeta1)
    }

    userCounterMap.add(click.userId)
  }

  def create(countryModel: CountryModel, destCounterMap: CounterMap[Int], destMarketCounterMap: CounterMap[Tuple2[Int, Int]],
             destModel: DestModel,marketDestModel:MarketDestModel): CmuModel = {

    //mc
    clusterHistByMarket.getMap.foreach {
      case (marketId, clusterCounts) =>
        val prior = countryModel.predict(countryByMarket(marketId))
        val mc = normaliseMutable(clusterCounts + prior) - prior
        clusterCounts :+= mc - clusterCounts
    }

    //cu
    clusterHistByCountryUser.getMap.foreach {
      case ((countryId, userId), clusterCounts) =>
        val prior = countryModel.predict(countryId)
        val cu = normaliseMutable(clusterCounts + clusterHistByCountryUserBeta2 * prior) - prior
        clusterCounts :+= cu - clusterCounts
    }


    //mdp
    clusterHistByMDP.getMap.foreach {
      case ((marketId, destId, isPackage), clusterCounts) =>

        val prior = marketDestModel.predict(marketId, destId)
        val mdp = normaliseMutable(clusterCounts + clusterHistByMDPBeta8 * prior) - prior
        clusterCounts :+= mdp - clusterCounts

    }

    //mu
    clusterHistByMarketUser.getMap.foreach {
      case ((marketId, userId), clusterCounts) =>
        val c = countryModel.predict(countryByMarket(marketId))
        val cm = clusterHistByMarket.getMap(marketId)
        val cu = clusterHistByCountryUser.getMap.getOrElse((countryByMarket(marketId), userId), DenseVector.fill(100)(1e-10f))

        val prior = c + cm
        val mu = normaliseMutable(clusterCounts + prior) - prior
        clusterCounts :+= mu - clusterCounts
    }

    //mdu
    clusterHistByDestMarketUser.getMap.foreach {

      case ((destId, marketId, userId), clusterCounts) =>
        val c = countryModel.predict(countryByMarket(marketId))
        val cm = clusterHistByMarket.getMap(marketId)
     //   val md = clusterHistByMarketDest.getMap((marketId, destId))
        val mu = clusterHistByMarketUser.getMap((marketId, userId))
        val cu = clusterHistByCountryUser.getMap.getOrElse((countryByMarket(marketId), userId), DenseVector.fill(100)(1e-10f))

        val prior = marketDestModel.predict(marketId, destId)
        val mdu = normaliseMutable(clusterCounts + 8f * prior) - prior
        clusterCounts :+= mdu - clusterCounts

    }

    //mdpu
    clusterHistByMDPU.getMap.map {
      case ((marketId, destId, isPackage, userId), clusterCounts) =>
        val c = countryModel.predict(countryByMarket(marketId))
        val cm = clusterHistByMarket.getMap(marketId)
        val mdp = clusterHistByMDP.getMap((marketId, destId, isPackage))
        val prior =  marketDestModel.predict(marketId, destId) + mdp
        val mdpu = normaliseMutable(clusterCounts + 8f * prior) - prior
        clusterCounts :+= mdpu - clusterCounts
    }

    val predictionMdpuMap = clusterHistByMDPU.getMap.map {
      case ((marketId, destId, isPackage, userId), clusterStat) =>
        val c = countryModel.predict(countryByMarket(marketId))
        val cm = clusterHistByMarket.getMap(marketId)
        val mdp = clusterHistByMDP.getMap((marketId, destId, isPackage))
        val mu = clusterHistByMarketUser.getMap((marketId, userId))
        val cu = clusterHistByCountryUser.getMap.getOrElse((countryByMarket(marketId), userId), DenseVector.fill(100)(1e-10f))
        val mdu = clusterHistByDestMarketUser.getMap((destId, marketId, userId))
        val mdpu = clusterHistByMDPU.getMap((marketId, destId, isPackage, userId))
       
        val mdPred = marketDestModel.predict(marketId, destId)
        val md2 = mdPred - (c+cm)
        val predicted = cmuBeta1 * c + cmuBeta2 * cm + cmuBeta3 * md2 + cmuBeta4 * mu + cmuBeta5 * cu + cmuBeta6 * mdu + cmuBeta7 * mdp
        (marketId, destId, isPackage, userId) -> predicted

    }
    CmuModel(predictionMdpuMap, userCounterMap, destCounterMap, destMarketCounterMap, destModel)
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
  val marketDestModelBuilder = MarketDestModelBuilder(testClicks, destMarketCounterMap, destCounterMap, marketCounterMap, hyperParams,timeDecayService)
 
    val cmuModelBuilder = CmuModelBuilder(testClicks, destMarketCounterMap, destCounterMap, marketCounterMap, hyperParams, timeDecayService)

    def onClick(click: Click) = {
marketModelBuilder.processCluster(click)
      countryModelBuilder.processCluster(click)
      destModelBuilder.processCluster(click)
      cmuModelBuilder.processCluster(click)
      destClusterModelBuilder.processCluster(click)
marketDestModelBuilder.processCluster(click)
    }
    trainDatasource.foreach { click => onClick(click) }

    val countryModel = countryModelBuilder.create()
    val destClusterModel = destClusterModelBuilder.create(countryModel)
    val destModel = destModelBuilder.create(countryModel, destClusterModel)
    val marketModel = marketModelBuilder.create(countryModel)
    val marketDestModel = marketDestModelBuilder.create(destModel, marketModel, countryModel, destMarketCounterMap, destCounterMap, marketCounterMap, destClusterModel)
    val cmuModel = cmuModelBuilder.create(countryModel, destCounterMap, destMarketCounterMap, destModel,marketDestModel)
    cmuModel
  }
}