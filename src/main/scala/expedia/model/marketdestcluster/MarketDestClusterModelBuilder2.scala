package expedia.model.marketdestcluster

import java.io.File
import scala.collection.Map
import scala.collection.Seq
import scala.collection.mutable
import breeze.linalg.csvread
import expedia.CompoundHyperParams
import expedia.CompoundHyperParamsMap
import expedia.HyperParamsService
import expedia.data.Click
import expedia.data.ExDataSource
import expedia.model.ClusterModelBuilder
import expedia.model.ClusterModelBuilderFactory
import expedia.stats.CounterMap
import expedia.stats.MulticlassHistByKey
import expedia.util.TimeDecayService
import expedia.model.country.CountryModel
import expedia.model.country.CountryModelBuilder2
import expedia.model.marketmodel.MarketModel
import expedia.model.marketmodel.MarketModelBuilder2

case class MarketDestClusterModelBuilder2(countryModel:CountryModel,marketModel:MarketModel,timeDecayService: TimeDecayService, hyperParamsService: HyperParamsService) extends ClusterModelBuilder {
  def create(trainDatasource: ExDataSource, testClicks: Seq[Click], hyperParams: CompoundHyperParams): MarketDestClusterModel = {


    val destCounterMap = CounterMap[Int]()

    val countryByDestCluster: mutable.Map[Int, Int] = mutable.Map()
    testClicks.foreach { click =>
      if (MarketDestClusterModelBuilder2.destClusterByDestMap.contains(click.destId)) countryByDestCluster += MarketDestClusterModelBuilder2.destClusterByDestMap(click.destId) -> click.countryId
    }

    val destClusterHistByMarketDestCluster = MulticlassHistByKey[Tuple2[Int, Int]](100)
    testClicks.foreach { click =>
      if (MarketDestClusterModelBuilder2.destClusterByDestMap.contains(click.destId)) {
        val key = (click.marketId, MarketDestClusterModelBuilder2.destClusterByDestMap(click.destId))
        destClusterHistByMarketDestCluster.add(key, click.cluster, value = 0)
      }
    }

    /**
     * Process training set
     */
    def onClick(click: Click) = {

      if (MarketDestClusterModelBuilder2.destClusterByDestMap.contains(click.destId)) countryByDestCluster += MarketDestClusterModelBuilder2.destClusterByDestMap(click.destId) -> click.countryId

      if (click.isBooking == 1) {
        destCounterMap.add(click.destId)
      }

      MarketDestClusterModelBuilder2.destClusterByDestMap.get(click.destId) match {
        case Some(destCluster) => {
          val key = (click.marketId, MarketDestClusterModelBuilder2.destClusterByDestMap(click.destId))
          if (destClusterHistByMarketDestCluster.getMap.contains(key)) {

            val beta1 = hyperParamsService.getParamValueForMarketId("expedia.model.marketdestcluster.beta1", click.marketId, hyperParams).toFloat
            val isBookingWeight = hyperParamsService.getParamValueForMarketId("expedia.model.marketdestcluster.isBookingWeight", click.marketId, hyperParams).toFloat
            val decayFactor = hyperParamsService.getParamValueForMarketId("expedia.model.marketdestcluster.decayFactor", click.marketId, hyperParams).toFloat
            val w = timeDecayService.getDecay(click.dateTime, decayFactor)

            if (click.isBooking == 1) destClusterHistByMarketDestCluster.add(key, click.cluster, value = w * isBookingWeight)
            else destClusterHistByMarketDestCluster.add(key, click.cluster, value = w * beta1)
          }
        }
        case None => //do nothing
      }

    }
    trainDatasource.foreach { click => onClick(click) }

    /**
     * Build model
     */
    
    destClusterHistByMarketDestCluster.getMap.foreach {
      case ((marketId, destCluster), clusterCounts) =>

        val beta2 = hyperParamsService.getParamValueForMarketId("expedia.model.marketdestcluster.beta2", marketId,hyperParams).toFloat

        clusterCounts :+= beta2 * marketModel.predict(marketId)
    }
    destClusterHistByMarketDestCluster.normalise()

    MarketDestClusterModel(destClusterHistByMarketDestCluster, MarketDestClusterModelBuilder2.destClusterByDestMap, countryModel)
  }
}

object MarketDestClusterModelBuilder2
    extends ClusterModelBuilderFactory {

  def build(trainDatasource: ExDataSource, testClicks: Seq[Click], modelHyperParamsMap: CompoundHyperParamsMap): MarketDestClusterModelBuilder2 = {
    val timeDecayService = TimeDecayService(testClicks)
    val hyperParamsService = HyperParamsService(testClicks)

     val countryModel = CountryModelBuilder2.build(trainDatasource, testClicks, modelHyperParamsMap).
      create(trainDatasource, testClicks, modelHyperParamsMap.getModel("country"))
    
        val marketModel = MarketModelBuilder2.build(trainDatasource,testClicks,  modelHyperParamsMap)
        .create(trainDatasource, testClicks, modelHyperParamsMap.getModel("market"))
   
      
    MarketDestClusterModelBuilder2(countryModel,marketModel,timeDecayService, hyperParamsService)
  }
  
  
    val destClusterByDestMat = csvread(new File("c:/perforce/daniel/ex/statistics/clusterByDest_30K.csv"), skipLines = 1)
    val destClusterByDestMap: Map[Int, Int] = (0 until destClusterByDestMat.rows).map { i =>
      val destId = destClusterByDestMat(i, 0).toInt
      val clusterId = destClusterByDestMat(i, 1).toInt
      destId -> clusterId
    }.toMap
}