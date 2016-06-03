package expedia.model.destcluster

import expedia.data.Click
import expedia.model.ClusterModelBuilderFactory
import expedia.util.TimeDecayService
import expedia.HyperParamsService
import expedia.data.ExDataSource
import expedia.CompoundHyperParamsMap
import expedia.model.ClusterModelBuilder
import expedia.CompoundHyperParams
import breeze.linalg._
import expedia.stats.CounterMap
import expedia.stats.MulticlassHistByKey
import java.io.File
import scala.collection._
import expedia.model.country.CountryModel
import expedia.model.country.CountryModelBuilder2

case class DestClusterModelBuilder2(countryModel:CountryModel,timeDecayService: TimeDecayService, hyperParamsService: HyperParamsService) extends ClusterModelBuilder{
  
   def create(trainDatasource: ExDataSource, testClicks: Seq[Click], hyperParams: CompoundHyperParams): DestClusterModel = {
      val destClusterByDestMat = csvread(new File("c:/perforce/daniel/ex/statistics/clusterByDest_30K.csv"), skipLines = 1)
  val destClusterByDestMap: Map[Int, Int] = (0 until destClusterByDestMat.rows).map { i =>
    val destId = destClusterByDestMat(i, 0).toInt
    val clusterId = destClusterByDestMat(i, 1).toInt
    destId -> clusterId
  }.toMap

  val destCounterMap = CounterMap[Int]()

   val countryByDestCluster: mutable.Map[Int, Int] = mutable.Map()
  testClicks.foreach { click =>
    if (destClusterByDestMap.contains(click.destId)) countryByDestCluster += destClusterByDestMap(click.destId) -> click.countryId
  }

   val destClusterHistByDestCluster = MulticlassHistByKey[Int](100)
     
     /**
     * Process training set
     */
    def onClick(click: Click) = {

    if (destClusterByDestMap.contains(click.destId)) countryByDestCluster += destClusterByDestMap(click.destId) -> click.countryId

    if (click.isBooking == 1) {
      destCounterMap.add(click.destId)
    }

    destClusterByDestMap.get(click.destId) match {
      case Some(destCluster) => {

        if (destClusterHistByDestCluster.getMap.contains(destCluster)) {
          val isBookingWeight = hyperParamsService.getParamValueForDestId("expedia.model.destcluster.isBookingWeight", click.destId,hyperParams).toFloat
          val beta1 = hyperParamsService.getParamValueForDestId("expedia.model.destcluster.beta1", click.destId,hyperParams).toFloat
          val decayFactor = hyperParamsService.getParamValueForDestId("expedia.model.destcluster.decayFactor", click.destId, hyperParams).toFloat
        val w = timeDecayService.getDecay(click.dateTime, decayFactor)

          if (click.isBooking == 1) destClusterHistByDestCluster.add(destCluster, click.cluster, value = w * isBookingWeight)
          else destClusterHistByDestCluster.add(destCluster, click.cluster, value = w * beta1)
        }
      }
      case None => //do nothing
    }
    
    }
    trainDatasource.foreach { click => onClick(click) }
    
     /**
     * Build model
     */
    
     destClusterHistByDestCluster.getMap.foreach {
      case (destCluster, clusterCounts) =>

        val beta3 = hyperParamsService.getParamValueForCountryId("expedia.model.destcluster.beta3", countryByDestCluster(destCluster),hyperParams).toFloat

        clusterCounts :+= beta3 * countryModel.predict(countryByDestCluster(destCluster))
    }
    destClusterHistByDestCluster.normalise()

    DestClusterModel(destClusterHistByDestCluster, destClusterByDestMap, countryModel)
   }
}

object DestClusterModelBuilder2 extends ClusterModelBuilderFactory {

  def build(trainDatasource: ExDataSource, testClicks: Seq[Click], modelHyperParamsMap: CompoundHyperParamsMap): DestClusterModelBuilder2 = {

    val timeDecayService = TimeDecayService(testClicks)
    val hyperParamsService = HyperParamsService(testClicks)

     val countryModel = CountryModelBuilder2.build(trainDatasource, testClicks, modelHyperParamsMap).
      create(trainDatasource, testClicks, modelHyperParamsMap.getModel("country"))
    
    DestClusterModelBuilder2(countryModel,timeDecayService,hyperParamsService)
  }
}