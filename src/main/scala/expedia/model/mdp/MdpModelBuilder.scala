package expedia.model.mdp

import expedia.data.Click
import expedia.stats.CounterMap
import expedia.stats.MulticlassHistByKey
import expedia.model.dest.DestModel
import expedia.model.marketmodel.MarketModel
import expedia.model.country.CountryModel
import expedia.model.country.CountryModelBuilder
import expedia.data.ExDataSource
import expedia.model.marketmodel.MarketModelBuilder
import expedia.model.dest.DestModelBuilder

case class MdpModelBuilder(testClicks: Seq[Click],
                      destMarketCounterMap: CounterMap[Tuple2[Int, Int]],
                      destCounterMap: CounterMap[Int], marketCounterMap: CounterMap[Int]) {

  
  //key ((marketId,destId,isPackage)
  private val clusterHistByMDP = MulticlassHistByKey[Tuple3[Int,Int, Int]](100)
  testClicks.foreach { click =>
    clusterHistByMDP.add((click.marketId, click.destId,click.isPackage), click.cluster, value = 0)
  }

  
  def processCluster(click: Click) = {

    val marketCounts = marketCounterMap.getOrElse(click.marketId, 0)
    val destMarketCounts = destMarketCounterMap.getOrElse((click.destId, click.marketId), 0)
    val destCounts = destCounterMap.getOrElse(click.destId, 0)
    val clickWeight =
      if (destMarketCounts < 300) 0.5f
      else if (destMarketCounts < 500) 0.1f
      else 0.05f

      val key = (click.marketId, click.destId,click.isPackage)
    if (clusterHistByMDP.getMap.contains(key)) {
      if (click.isBooking == 1) clusterHistByMDP.add(key, click.cluster)
      else clusterHistByMDP.add(key, click.cluster, value = clickWeight)
    }

  }
  
   def create(destModel: DestModel, marketModel: MarketModel, countryModel: CountryModel, destMarketCounterMap: CounterMap[Tuple2[Int, Int]],
             destCounterMap: CounterMap[Int], marketCounterMap: CounterMap[Int]): MdpModel = {

    clusterHistByMDP.getMap.foreach {
      case ((marketId, destId,isPackage), clusterCounts) =>

        val destMarketCounts = destMarketCounterMap.getOrElse((destId, marketId), 0)
        val destCounts = destCounterMap.getOrElse(destId, 0)
        val marketCounts = marketCounterMap.getOrElse(marketId, 0)

        if (destMarketCounts > 0 && destCounts > 0 && destCounts == destMarketCounts) clusterCounts :+= 1f * marketModel.predict(marketId)
        else if (destMarketCounts > 0 && destCounts > 0 && marketCounts == destMarketCounts) clusterCounts :+= 5f * destModel.predict(destId)
        else clusterCounts :+= 1f * marketModel.predict(marketId)

    }
    clusterHistByMDP.normalise()

    MdpModel(clusterHistByMDP)
  }
}

object MdpModelBuilder {
  
   def buildFromTrainingSet(trainDatasource: ExDataSource, testClicks: Seq[Click]): MdpModel = {

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

    val countryModelBuilder = CountryModelBuilder(testClicks)
    val destModelBuilder = DestModelBuilder(testClicks)
    val marketModelBuilder = MarketModelBuilder(testClicks)
    val mdpModelBuilder = MdpModelBuilder(testClicks, destMarketCounterMap, destCounterMap, marketCounterMap)

    def onClick(click: Click) = {
      destModelBuilder.processCluster(click)
      marketModelBuilder.processCluster(click)
      countryModelBuilder.processCluster(click)
      mdpModelBuilder.processCluster(click)
    }
    trainDatasource.foreach { click => onClick(click) }

    val countryModel = countryModelBuilder.create()
    val destModel = destModelBuilder.create(countryModel)

    val marketModel = marketModelBuilder.create(countryModel)
    val mdpModel = mdpModelBuilder.create(destModel, marketModel, countryModel, destMarketCounterMap, destCounterMap, marketCounterMap)

    mdpModel
  }
  
  
}