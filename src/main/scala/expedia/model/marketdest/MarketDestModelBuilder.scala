package expedia.model.marketdest

import expedia.data.Click
import expedia.stats.MulticlassHistByKey
import expedia.model.country.CountryModel
import scala.collection._
import expedia.stats.CounterMap
import expedia.model.marketmodel.MarketModel
import expedia.model.dest.DestModel

case class MarketDestModelBuilder(testClicks: Seq[Click],clickWeight:Float) {

  //key ((marketId,destId)
  private val clusterHistByMarketDest = MulticlassHistByKey[Tuple2[Int, Int]](100)
  testClicks.foreach(click => clusterHistByMarketDest.add((click.marketId, click.destId), click.cluster, value = 0))

  private val countryByDest: mutable.Map[Int, Int] = mutable.Map()
  testClicks.foreach(click => countryByDest += click.destId -> click.countryId)

  def processCluster(click: Click) = {
    if (clusterHistByMarketDest.getMap.contains((click.marketId, click.destId))) {
      if (click.isBooking == 1) clusterHistByMarketDest.add((click.marketId, click.destId), click.cluster)
      else clusterHistByMarketDest.add((click.marketId, click.destId), click.cluster, value = clickWeight)
    }

  }

  def create(destModel: DestModel, marketModel: MarketModel, countryModel: CountryModel, destMarketCounterMap: CounterMap[Tuple2[Int, Int]],
             destCounterMap: CounterMap[Int], marketCounterMap: CounterMap[Int]): MarketDestModel = {

    clusterHistByMarketDest.getMap.foreach {
      case ((marketId, destId), clusterCounts) =>

        val destMarketCounts = destMarketCounterMap.getOrElse((destId, marketId), 0)
        val destCounts = destCounterMap.getOrElse(destId, 0)
        val marketCounts = marketCounterMap.getOrElse(marketId, 0)

        if (destMarketCounts > 0 && destCounts > 0 && destCounts == destMarketCounts) clusterCounts :+= 1f * marketModel.predict(marketId)
        else if (destMarketCounts > 0 && destCounts > 0 && marketCounts == destMarketCounts) clusterCounts :+= 5f * destModel.predict(destId)
        else clusterCounts :+= 1f * marketModel.predict(marketId)

    }
    clusterHistByMarketDest.normalise()

    MarketDestModel(clusterHistByMarketDest)
  }
}