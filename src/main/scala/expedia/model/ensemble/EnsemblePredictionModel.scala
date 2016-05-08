package expedia.model.ensemble

import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import breeze.linalg._
import expedia.model.svm.SVMPredictionModel
import scala.io.Source
import java.io.File
import com.typesafe.scalalogging.slf4j.LazyLogging
import expedia.model.clusterdist.ClusterDistPredictionModelBuilder
import scala.collection._
import expedia.model.marketdest.MarketDestPredictionModelBuilder
import expedia.model.marketdest.MarketDestPredictionModel
import expedia.data.ExDataSource
import expedia.data.Click
import expedia.model.clusterdist.ClusterDistPredictionModel
import expedia.stats.CounterMap
import expedia.model.dest.DestModelBuilder
import expedia.model.country.CountryModelBuilder
import expedia.model.country.CountryModelBuilder

/**
 * @param trainData ('user_location_city','orig_destination_distance','user_id','srch_destination_id','hotel_market','hotel_cluster')
 */
object EnsemblePredictionModel extends LazyLogging {
  def apply(expediaTrainFile: String, svmPredictionsData: DenseMatrix[Double], userIds: Set[Int], testClicks: Seq[Click]): EnsemblePredictionModel = {
   val countryModelBuilder = CountryModelBuilder(testClicks)
    val destModelBuilder = DestModelBuilder(svmPredictionsData)
    val clusterDistPredictBuilder = ClusterDistPredictionModelBuilder()
    //  val userDestPredictBuilder = UserDestPredictionModelBuilder(userIds)
    val marketDestPredictBuilder = MarketDestPredictionModelBuilder(svmPredictionsData, userIds, testClicks)

    val destMarketCounterMap = CounterMap[Tuple2[Int, Int]]
    val destCounterMap = CounterMap[Int]()
    val marketCounterMap = CounterMap[Int]()

    def onClick(click: Click) = {
     countryModelBuilder.processCluster(click)
      destModelBuilder.processCluster(click)
      clusterDistPredictBuilder.processCluster(click)
      //   userDestPredictBuilder.processCluster(click)
      marketDestPredictBuilder.processCluster(click)

      if (click.isBooking == 1) {
        destMarketCounterMap.add((click.destId, click.marketId))
        destCounterMap.add(click.destId)
        marketCounterMap.add(click.marketId)
      }
    }
    ExDataSource(expediaTrainFile).foreach { click => onClick(click) }

    val countryModel = countryModelBuilder.create()
    val destModel = destModelBuilder.create()
    val clusterDistPredict = clusterDistPredictBuilder.create()
    val marketDestPredict = marketDestPredictBuilder.create(destModel, countryModel,destMarketCounterMap, destCounterMap, marketCounterMap)
    new EnsemblePredictionModel(clusterDistPredict, marketDestPredict, destMarketCounterMap, destCounterMap)

  }

}

case class EnsemblePredictionModel(clusterDistPredict: ClusterDistPredictionModel,
                                   marketDestPredict: MarketDestPredictionModel,
                                   destMarketCounterMap: CounterMap[Tuple2[Int, Int]],
                                   destCounterMap: CounterMap[Int])
    extends LazyLogging {

  /**
   * @param data ['user_location_city','orig_destination_distance','user_id','srch_destination_id','hotel_market']
   */
  def predict(userLoc: Int, dist: Double, userId: Int, destId: Int, hotelContinent: Int, market: Int): DenseVector[Double] = {

    val destMarketCounts = destMarketCounterMap.getOrElse((destId, market), 0)
    val destCounts = destCounterMap.getOrElse(destId, 0)

    val clustDistProbs = clusterDistPredict.predict(userLoc, dist, market)
    val marketDestProbs = marketDestPredict.predict(userId, market, destId, hotelContinent)
    val clustersProbVector = DenseVector.fill(100)(0d)
    (0 until 100).foreach { hotelCluster =>
      val leakProb = clustDistProbs(hotelCluster)

      val prob = if (leakProb.isNaN()) {
        marketDestProbs(hotelCluster)
      } else leakProb
      clustersProbVector(hotelCluster) = prob
    }
    clustersProbVector
  }

}