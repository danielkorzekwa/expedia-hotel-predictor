package expedia.model.old

import scala.collection.Seq
import scala.collection.mutable
import com.typesafe.scalalogging.slf4j.LazyLogging
import breeze.linalg.InjectNumericOps
import expedia.HyperParams
import expedia.data.Click
import expedia.data.ExDataSource
import expedia.model.country.CountryModel
import expedia.model.country.CountryModelBuilder
import expedia.stats.MulticlassHistByKey
import expedia.util.TimeDecayService
import expedia.model.marketmodel.MarketModel
import expedia.model.marketmodel.MarketModelBuilder
import scala.collection._
import expedia.stats.CounterMap
import expedia.stats.normaliseMutable
import expedia.model.dest.DestModel
import expedia.model.old.destsmapfordest.DestsMapForDestBuilder
import expedia.model.old.destsfordest.DestsForDestMapBuilder

/**
 * The idea was to reconstruct the hierarchy of destinations and use parent dest for a prior.
 */
case class DestModelBuilder2(testClicks: Seq[Click], hyperParams: HyperParams, timeDecayService: TimeDecayService) extends LazyLogging {

  private val clusterHistByDest = MulticlassHistByKey[Int](100)
  testClicks.foreach(click => clusterHistByDest.add(click.destId, click.cluster, value = 0))

  private val countryByDest: mutable.Map[Int, Int] = mutable.Map()
  testClicks.foreach(click => countryByDest += click.destId -> click.countryId)

  private val marketByDest: mutable.Map[Int, Int] = mutable.Map()
  testClicks.foreach(click => marketByDest += click.destId -> click.marketId)

  val destCounterMap = CounterMap[Int]()
  val marketCounterMap = CounterMap[Int]()
  val destMarketCounterMap = CounterMap[Tuple2[Int, Int]]

  private val beta1 = hyperParams.getParamValue("expedia.model.dest.beta1").toFloat
  private val beta2 = hyperParams.getParamValue("expedia.model.dest.beta2").toFloat

  def processCluster(click: Click) = {

    val w = timeDecayService.getDecay(click.dateTime)

    //  if (clusterHistByDest.getMap.contains(click.destId)) {

    if (click.isBooking == 1) clusterHistByDest.add(click.destId, click.cluster, value = w)
    else clusterHistByDest.add(click.destId, click.cluster, value = w * beta1)

    //  }

    if (click.isBooking == 1) {
      destMarketCounterMap.add((click.destId, click.marketId))
      destCounterMap.add(click.destId)
      marketCounterMap.add(click.marketId)
    }

    countryByDest += click.destId -> click.countryId
  }

  /**
   * @param destsByDestMap  [destId,other destIds that coexists with destId]
   * @param destsMapByDest [destId, Map[destId,coexistence counts]]
   */
  def create(countryModel: CountryModel, marketModel: MarketModel, destsByDestMap: Map[Int, Set[Int]],
             destsMapByDest: Map[Int, Map[Int, Int]]): DestModel = {

    val processedDestIds: mutable.Set[Int] = mutable.HashSet()

    while (processedDestIds.size < clusterHistByDest.getMap.size) {
      println("processes/all = %d / %d".format(processedDestIds.size, clusterHistByDest.getMap.size))
      clusterHistByDest.getMap.foreach {
        case (destId, clusterCounts) =>

          if (!processedDestIds.contains(destId)) {

            if (destsByDestMap.contains(destId)) {

              val coexistsDestIds = destsByDestMap(destId)
              val destCounts = destCounterMap.getOrElse(destId, 0)

              val sortedDestIdsByDestCount = coexistsDestIds.map(otherDestId => (otherDestId, destCounterMap.getOrElse(otherDestId, 0))).toList.sortWith((a, b) => a._2 > b._2)
              val sortedDestIdsByCoexist = destsMapByDest(destId).toList.sortWith((a, b) => a._2 > b._2)

           //   val parentDestId = sortedDestIdsByDestCount.find(destIdWithCounts => destIdWithCounts._2 > destCounts)
              val parentDestId:Option[Tuple2[Int,Int]] = 
                if(sortedDestIdsByCoexist.size>1 && destCounterMap.getOrElse(sortedDestIdsByCoexist(1)._1,0)>destCounts) Some(sortedDestIdsByCoexist(1)) else None


              parentDestId match {
                case Some(parentDestId) => {
                  if (processedDestIds.contains(parentDestId._1)) {

                    clusterCounts :+= beta2 * clusterHistByDest.getMap(parentDestId._1)
                    normaliseMutable(clusterCounts)
                    processedDestIds += destId

                  } else {
                    
//                      clusterCounts :+= beta2 * countryModel.predict(countryByDest(destId))
//                  normaliseMutable(clusterCounts)
//                  processedDestIds += destId
//                    
//                    if(destId==8235) {
//              println(destId + ":" + parentDestId + ":" + sortedDestIdsByDestCount)
//              println(sortedDestIdsByCoexist)
//              println("--------------")
//                    }
                  }

                }
                case None => {
                  clusterCounts :+= beta2 * countryModel.predict(countryByDest(destId))
                  normaliseMutable(clusterCounts)
                  processedDestIds += destId
                }
              }

            } else {
              clusterCounts :+= beta2 * countryModel.predict(countryByDest(destId))
              normaliseMutable(clusterCounts)
              processedDestIds += destId
            }
          }
      }
    }

    DestModel(clusterHistByDest)
  }

}

object DestModelBuilder2 {
  def buildFromTrainingSet(trainDatasource: ExDataSource, testClicks: Seq[Click], hyperParams: HyperParams): DestModel = {

    val timeDecayService = TimeDecayService(testClicks, hyperParams)

    val destModelBuilder = DestModelBuilder2(testClicks, hyperParams, timeDecayService)
    val marketModelBuilder = MarketModelBuilder(testClicks, hyperParams, timeDecayService)
    val countryModelBuilder = CountryModelBuilder(testClicks, hyperParams, timeDecayService)

    val destsByDestMapBuilder = DestsForDestMapBuilder()
    val destsMapByDestBuilder = DestsMapForDestBuilder()

    def onClick(click: Click) = {

      destModelBuilder.processCluster(click)
      marketModelBuilder.processCluster(click)
      countryModelBuilder.processCluster(click)

      destsByDestMapBuilder.processCluster(click)
      destsMapByDestBuilder.processCluster(click)
    }
    trainDatasource.foreach { click => onClick(click) }

    val countryModel = countryModelBuilder.create()
    val marketModel = marketModelBuilder.create(countryModel)
    val destsByDestMap = destsByDestMapBuilder.create()
    val destsMapByDest = destsMapByDestBuilder.create()
    val destModel = destModelBuilder.create(countryModel, marketModel, destsByDestMap, destsMapByDest)

    destModel
  }
}