package expedia.model.marketuser

import scala.collection.Seq
import scala.collection.mutable

import breeze.linalg.InjectNumericOps
import expedia.CompoundHyperParams
import expedia.HyperParamsService
import expedia.data.Click
import expedia.data.ExDataSource
import expedia.model.countryuser.CountryUserModel
import expedia.model.marketmodel.MarketModel
import expedia.stats.MulticlassHistByKey
import expedia.util.TimeDecayService

case class MarketUserModelBuilder(testClicks: Seq[Click],  hyperParamsService: HyperParamsService,hyperParams:CompoundHyperParams, timeDecayService: TimeDecayService) {

  private val clusterHistByMarketUser = MulticlassHistByKey[Tuple2[Int, Int]](100)
  testClicks.foreach(click => clusterHistByMarketUser.add((click.marketId, click.userId), click.cluster, value = 0))

  private val countryByMarket: mutable.Map[Int, Int] = mutable.Map()
  testClicks.foreach(click => countryByMarket += click.marketId -> click.countryId)

  private val beta2 = 0.95f //hyperParams.getParamValue("expedia.model.marketuser.beta2").toFloat

  

}

object MarketUserModelBuilder {

//  def buildFromTrainingSet(trainDatasource: ExDataSource, testClicks: Seq[Click], hyperParams: CompoundHyperParams): MarketUserModel = {
//
//    val timeDecayService = TimeDecayService(testClicks, hyperParams)
//
//    val countryModelBuilder = CountryModelBuilder(testClicks, hyperParams, timeDecayService)
//    val marketModelBuilder = MarketModelBuilder(testClicks, hyperParams, timeDecayService)
//    val marketUserModelBuilder = MarketUserModelBuilder(testClicks, hyperParams, timeDecayService)
//    val countryUserModelBuilder = CountryUserModelBuilder(testClicks, hyperParams)
//
//    def onClick(click: Click) = {
//
//      countryModelBuilder.processCluster(click)
//      countryUserModelBuilder.processCluster(click)
//      marketModelBuilder.processCluster(click)
//      marketUserModelBuilder.processCluster(click)
//
//    }
//    trainDatasource.foreach { click => onClick(click) }
//
//    val countryModel = countryModelBuilder.create()
//    val marketModel = marketModelBuilder.create(countryModel)
//    val countryUserModel = countryUserModelBuilder.create(countryModel)
//    val marketUserModel = marketUserModelBuilder.create(countryUserModel, marketModel)
//
//    marketUserModel
//
//  }
}