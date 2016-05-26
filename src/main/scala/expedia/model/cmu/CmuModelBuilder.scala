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

  //key ((marketId,destId)
  private val clusterHistByMarketDest = MulticlassHistByKey[Tuple2[Int, Int]](100)
  testClicks.foreach { click =>
    clusterHistByMarketDest.add((click.marketId, click.destId), click.cluster, value = 0)
  }

  //key ((destId, marketId,userId)
  private val clusterHistByDestMarketUser = MulticlassHistByKey[Tuple3[Int, Int, Int]](100)
  testClicks.foreach { click =>
    clusterHistByDestMarketUser.add((click.destId, click.marketId, click.userId), click.cluster, value = 0)
  }

  private val countryByMarket: mutable.Map[Int, Int] = mutable.Map()
  testClicks.foreach(click => countryByMarket += click.marketId -> click.countryId)

  private val clusterHistByMarketBeta1 = hyperParams.getParamValue("expedia.model.marketmodel.beta1").toFloat

  private val clusterHistByCountryUserBeta1 = hyperParams.getParamValue("expedia.model.countryuser.beta1").toFloat
  private val clusterHistByCountryUserBeta2 = hyperParams.getParamValue("expedia.model.countryuser.beta2").toFloat

  private val clusterHistByMarketUserBeta1 = hyperParams.getParamValue("expedia.model.marketuser.beta1").toFloat

  //market dest params
  private val destMarketCountsThreshold1 = hyperParams.getParamValue("expedia.model.marketdest.destMarketCountsThreshold1").toFloat
  private val destMarketCountsThresholdClickWeight1 = hyperParams.getParamValue("expedia.model.marketdest.destMarketCountsThresholdClickWeight1").toFloat
  private val destMarketCountsThreshold2 = hyperParams.getParamValue("expedia.model.marketdest.destMarketCountsThreshold2").toFloat
  private val destMarketCountsThresholdClickWeight2 = hyperParams.getParamValue("expedia.model.marketdest.destMarketCountsThresholdClickWeight2").toFloat
  private val destMarketCountsDefaultWeight = hyperParams.getParamValue("expedia.model.marketdest.destMarketCountsDefaultWeight").toFloat
  private val destMarketCountsDefaultWeightBeta3 = hyperParams.getParamValue("expedia.model.marketdest.beta3").toFloat

  //dest market user params
  private val clusterHistByDestMarketUserBeta2 = hyperParams.getParamValue("expedia.model.marketdestuser.beta2").toFloat
  private val clusterHistByDestMarketUserBeta5 = hyperParams.getParamValue("expedia.model.marketdestuser.beta5").toFloat
  private val clusterHistByDestMarketUserBeta6 = hyperParams.getParamValue("expedia.model.marketdestuser.beta6").toFloat
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
    val clickWeight =
      if (destMarketCounts < destMarketCountsThreshold1) destMarketCountsThresholdClickWeight1
      else if (destMarketCounts < destMarketCountsThreshold2) destMarketCountsThresholdClickWeight2
      else destMarketCountsDefaultWeight

    if (clusterHistByMarketDest.getMap.contains((click.marketId, click.destId))) {
      if (click.isBooking == 1) clusterHistByMarketDest.add((click.marketId, click.destId), click.cluster, value = w)
      else clusterHistByMarketDest.add((click.marketId, click.destId), click.cluster, value = w * clickWeight)
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
    val key = (click.destId, click.marketId, click.userId)
    if (clusterHistByDestMarketUser.getMap.contains(key)) {
      if (click.isBooking == 1) clusterHistByDestMarketUser.add(key, click.cluster, value = w)
      else clusterHistByDestMarketUser.add(key, click.cluster, value = w * clusterHistByDestMarketUserBeta6)
    }

  }

  def create(countryModel: CountryModel): CmuModel = {

    //mc
    clusterHistByMarket.getMap.foreach { case (marketId, clusterCounts) => clusterCounts :+= countryModel.predict(countryByMarket(marketId)) }
    clusterHistByMarket.normalise()
    clusterHistByMarket.getMap.foreach { case (marketId, clusterCounts) => clusterCounts :-= countryModel.predict(countryByMarket(marketId)) }

    //cu
    clusterHistByCountryUser.getMap.foreach { case ((countryId, userId), clusterCounts) => clusterCounts :+= clusterHistByCountryUserBeta2 * countryModel.predict(countryId) }
    clusterHistByCountryUser.normalise()
    clusterHistByCountryUser.getMap.foreach { case ((countryId, userId), clusterCounts) => clusterCounts :-= countryModel.predict(countryId) }

    //dm
    clusterHistByMarketDest.getMap.foreach {
      case ((marketId, destId), clusterCounts) =>

        val c = countryModel.predict(countryByMarket(marketId))
        val cm = clusterHistByMarket.getMap(marketId)

        clusterCounts :+= destMarketCountsDefaultWeightBeta3 * (c + cm)

    }
    clusterHistByMarketDest.normalise()
    clusterHistByMarketDest.getMap.foreach {
      case ((marketId, destId), clusterCounts) =>
        val c = countryModel.predict(countryByMarket(marketId))
        val cm = clusterHistByMarket.getMap(marketId)
        clusterCounts :-= (c + cm)
    }

    //mu
    clusterHistByMarketUser.getMap.foreach {
      case ((marketId, userId), clusterCounts) =>
        val c = countryModel.predict(countryByMarket(marketId))
        val cm = clusterHistByMarket.getMap(marketId)
        val cu = clusterHistByCountryUser.getMap.getOrElse((countryByMarket(marketId), userId), DenseVector.fill(100)(1e-10f))

        val b = 0.7f
     //   clusterCounts :+= 1f*(c + b*cm + (1-b)*cu)
        clusterCounts :+= c + cm
    }
    clusterHistByMarketUser.normalise()

    clusterHistByMarketUser.getMap.foreach {
      case ((marketId, userId), clusterCounts) =>
        val c = countryModel.predict(countryByMarket(marketId))
        val cm = clusterHistByMarket.getMap(marketId)
        val cu = clusterHistByCountryUser.getMap.getOrElse((countryByMarket(marketId), userId), DenseVector.fill(100)(1e-10f))

         val b = 0.7f
      //  clusterCounts :-= (c + b*cm + (1-b)*cu)
        
        clusterCounts :-= c + cm
    }

    clusterHistByDestMarketUser.getMap.foreach {

      case ((destId, marketId, userId), userClusterProbs) =>
        val c = countryModel.predict(countryByMarket(marketId))
        val cm = clusterHistByMarket.getMap(marketId)
        val md = clusterHistByMarketDest.getMap((marketId, destId))
        val mu = clusterHistByMarketUser.getMap((marketId, userId))
          val cu = clusterHistByCountryUser.getMap.getOrElse((countryByMarket(marketId), userId), DenseVector.fill(100)(1e-10f))

        val b = 0.8f
        //userClusterProbs :+= 8f*(b*(c +cm + md) +  (1 - b) * (c + cm + mu) ) 
      //  userClusterProbs :+= 8f * (c + cm + b * md + (1 - b) * mu)
        
           userClusterProbs :+= 8f * (c + cm + b * md + (1 - b) * mu)

    }
    clusterHistByDestMarketUser.normalise()

    CmuModel(clusterHistByMarketUser, clusterHistByDestMarketUser.getMap)
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
    val cmuModelBuilder = CmuModelBuilder(testClicks, destMarketCounterMap, destCounterMap, marketCounterMap, hyperParams, timeDecayService)

    def onClick(click: Click) = {

      countryModelBuilder.processCluster(click)
      cmuModelBuilder.processCluster(click)

    }
    trainDatasource.foreach { click => onClick(click) }

    val countryModel = countryModelBuilder.create()
    val cmuModel = cmuModelBuilder.create(countryModel)
    cmuModel
  }
}