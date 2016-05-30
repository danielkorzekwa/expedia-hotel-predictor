package expedia.model.mdp

import breeze.linalg.InjectNumericOps
import expedia.HyperParams
import expedia.data.Click
import expedia.data.ExDataSource
import expedia.model.country.CountryModel
import expedia.model.country.CountryModelBuilder
import expedia.model.dest.DestModel
import expedia.model.dest.DestModelBuilder
import expedia.model.marketdest.MarketDestModel
import expedia.model.marketdest.MarketDestModelBuilder
import expedia.model.marketmodel.MarketModel
import expedia.model.marketmodel.MarketModelBuilder
import expedia.stats.CounterMap
import expedia.stats.MulticlassHistByKey
import expedia.util.TimeDecayService

case class MdpModelBuilder(testClicks: Seq[Click],
                           destMarketCounterMap: CounterMap[Tuple2[Int, Int]],
                           destCounterMap: CounterMap[Int], marketCounterMap: CounterMap[Int], hyperParams: HyperParams, timeDecayService: TimeDecayService) {

  //key ((marketId,destId,isPackage)
  private val clusterHistByMDP = MulticlassHistByKey[Tuple3[Int, Int, Int]](100)
  testClicks.foreach { click =>
    clusterHistByMDP.add((click.marketId, click.destId, click.isPackage), click.cluster, value = 0)
  }

  private val beta1 = hyperParams.getParamValue("expedia.model.mdp.beta1").toFloat
  private val beta2 = hyperParams.getParamValue("expedia.model.mdp.beta2").toFloat
  private val beta3 = hyperParams.getParamValue("expedia.model.mdp.beta3").toFloat
  private val beta4 = hyperParams.getParamValue("expedia.model.mdp.beta4").toFloat
  private val beta5 = hyperParams.getParamValue("expedia.model.mdp.beta5").toFloat

  private val beta6 = hyperParams.getParamValue("expedia.model.mdp.beta6").toFloat
  private val beta7 = hyperParams.getParamValue("expedia.model.mdp.beta7").toFloat
  private val beta8 = hyperParams.getParamValue("expedia.model.mdp.beta8").toFloat

  def processCluster(click: Click) = {

    val w = timeDecayService.getDecay(click.dateTime)

    val marketCounts = marketCounterMap.getOrElse(click.marketId, 0)
    val destMarketCounts = destMarketCounterMap.getOrElse((click.destId, click.marketId), 0)
    val destCounts = destCounterMap.getOrElse(click.destId, 0)
    val clickWeight =
      if (destMarketCounts < beta1) beta2
      else if (destMarketCounts < beta3) beta4
      else beta5

    val key = (click.marketId, click.destId, click.isPackage)
    if (clusterHistByMDP.getMap.contains(key)) {
      if (click.isBooking == 1) clusterHistByMDP.add(key, click.cluster, value = w)
      else clusterHistByMDP.add(key, click.cluster, value = w * clickWeight)
    }

  }

  def create(destModel: DestModel, marketModel: MarketModel, countryModel: CountryModel, destMarketCounterMap: CounterMap[Tuple2[Int, Int]],
             destCounterMap: CounterMap[Int], marketCounterMap: CounterMap[Int],
             marketDestModel: MarketDestModel): MdpModel = {

    clusterHistByMDP.getMap.foreach {
      case ((marketId, destId, isPackage), clusterCounts) =>

        val destMarketCounts = destMarketCounterMap.getOrElse((destId, marketId), 0)
        val destCounts = destCounterMap.getOrElse(destId, 0)
        val marketCounts = marketCounterMap.getOrElse(marketId, 0)

        if (destMarketCounts > 0 && destCounts > 0 && destCounts == destMarketCounts) clusterCounts :+= beta6 * marketModel.predict(marketId)
        else if (destMarketCounts > 0 && destCounts > 0 && marketCounts == destMarketCounts) clusterCounts :+= beta7 * destModel.predict(destId)
        else clusterCounts :+= beta8 * marketModel.predict(marketId)

    }

    //     clusterHistByMDP.getMap.foreach {
    //      case ((marketId, destId, isPackage), clusterCounts) =>
    //
    //       clusterCounts :+= 128f*marketDestModel.predict(marketId, destId)
    //
    //    }

    clusterHistByMDP.normalise()

    MdpModel(clusterHistByMDP)
  }
}

object MdpModelBuilder {

  def buildFromTrainingSet(trainDatasource: ExDataSource, testClicks: Seq[Click], hyperParams: HyperParams): MdpModel = {

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
    val destModelBuilder = DestModelBuilder(testClicks, hyperParams, timeDecayService)
    val marketModelBuilder = MarketModelBuilder(testClicks, hyperParams, timeDecayService)
    val marketDestModelBuilder = MarketDestModelBuilder(testClicks, destMarketCounterMap, destCounterMap, marketCounterMap, hyperParams, timeDecayService)

    val mdpModelBuilder = MdpModelBuilder(testClicks, destMarketCounterMap, destCounterMap, marketCounterMap, hyperParams, timeDecayService)

    def onClick(click: Click) = {
      destModelBuilder.processCluster(click)
      marketModelBuilder.processCluster(click)
      countryModelBuilder.processCluster(click)
      marketDestModelBuilder.processCluster(click)
      mdpModelBuilder.processCluster(click)
    }
    trainDatasource.foreach { click => onClick(click) }

    val countryModel = countryModelBuilder.create()
    val destModel = destModelBuilder.create(countryModel,null)

    val marketModel = marketModelBuilder.create(countryModel)
    val marketDestModel = marketDestModelBuilder.create(
        destModel, marketModel, countryModel, destMarketCounterMap, destCounterMap, marketCounterMap,null,null)

    val mdpModel = mdpModelBuilder.create(destModel, marketModel, countryModel, destMarketCounterMap, destCounterMap, marketCounterMap, marketDestModel)

    mdpModel
  }

}