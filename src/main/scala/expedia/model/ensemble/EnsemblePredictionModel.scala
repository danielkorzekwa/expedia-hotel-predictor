package expedia.model.ensemble

import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import breeze.linalg._
import expedia.model.userdest.UserDestPredictionModel
import expedia.model.svm.SVMPredictionModel
import scala.io.Source
import java.io.File
import com.typesafe.scalalogging.slf4j.LazyLogging
import expedia.model.clusterdist.ClusterDistPredictionModelBuilder
import expedia.model.userdest.UserDestPredictionModelBuilder
import expedia.model.userdest.UserDestPredictionModelBuilder
import expedia.model.singlecatmodel.SingleCatPredictionModel
import expedia.model.singlecatmodel.SingleCatPredictionModelBuilder
import expedia.model.singlecatmodel.SingleCatPredictionModel
import expedia.model.singlecatmodel.SingleCatPredictionModel
import expedia.model.userdest.UserDestPredictionModel
import scala.collection._
import expedia.model.marketdest.MarketDestPredictionModelBuilder
import expedia.model.marketdest.MarketDestPredictionModel
import expedia.data.ExDataSource
import expedia.data.Click
import expedia.model.clusterdist.ClusterDistPredictionModel
import expedia.stats.CounterMap

/**
 * @param trainData ('user_location_city','orig_destination_distance','user_id','srch_destination_id','hotel_market','hotel_cluster')
 */
object EnsemblePredictionModel extends LazyLogging {
  def apply(expediaTrainFile: String, svmPredictionsData: DenseMatrix[Double], userIds: Set[Int]): EnsemblePredictionModel = {

    val clusterDistPredictBuilder = ClusterDistPredictionModelBuilder()
    val userDestPredictBuilder = UserDestPredictionModelBuilder(svmPredictionsData, userIds)
    val marketDestPredictBuilder = MarketDestPredictionModelBuilder(svmPredictionsData, Set())

    val singleCatPredictBuilder = SingleCatPredictionModelBuilder()

    val destMarketCounterMap = CounterMap[Tuple2[Int, Int]]
    val destCounterMap = CounterMap[Int]()

    def onClick(click: Click) = {
      val key = (click.userLoc, click.dist, click.market)
      clusterDistPredictBuilder.processCluster(click)
      userDestPredictBuilder.processCluster(click.userId, click.destId, click.isBooking, click.hotelContinent, click.cluster)
      marketDestPredictBuilder.processCluster(click.market, click.destId, click.isBooking, click.hotelContinent, click.cluster)
      singleCatPredictBuilder.processCluster(click.market, click.cluster)

      if (click.isBooking == 1) {
        destMarketCounterMap.add((click.destId, click.market))
        destCounterMap.add(click.destId)
      }
    }
    ExDataSource(expediaTrainFile).foreach { click => onClick(click) }

    val clusterDistPredict = clusterDistPredictBuilder.create()
    val userDestPredict = userDestPredictBuilder.toUserDestPredictionModel()
    val marketDestPredict = marketDestPredictBuilder.toMarketDestPredictionModel()
    val singleCatPredict = singleCatPredictBuilder.toSingleCatPredictionModel()
    new EnsemblePredictionModel(clusterDistPredict, userDestPredict, singleCatPredict, marketDestPredict, destMarketCounterMap, destCounterMap)

  }

}

case class EnsemblePredictionModel(clusterDistPredict: ClusterDistPredictionModel, userDestPredict: UserDestPredictionModel, singleCatPredictionModel: SingleCatPredictionModel,
                                   marketDestPredict: MarketDestPredictionModel,
                                   destMarketCounterMap: CounterMap[Tuple2[Int, Int]],
                                   destCounterMap: CounterMap[Int])
    extends LazyLogging {

  /**
   * @param data ['user_location_city','orig_destination_distance','user_id','srch_destination_id','hotel_market']
   */
  def predict(userLoc: Int, dist: Double, userId: Int, destId: Int, hotelContinent: Int, market: Int): DenseVector[Double] = {

    val clustDistProbs = clusterDistPredict.predict(userLoc, dist, market)
    val userDestProbs = userDestPredict.predict(userId, destId, hotelContinent)
    val marketDestProbs = marketDestPredict.predict(market, destId, hotelContinent)
    val clustersProbVector = DenseVector.fill(100)(0d)
    (0 until 100).foreach { hotelCluster =>
      val leakProb = clustDistProbs(hotelCluster)

      val prob = if (leakProb.isNaN()) {
        val destMarketCounts = destMarketCounterMap.getOrElse((destId, market), 0)
        val destCounts = destCounterMap.getOrElse(destId, 0)
        if (destMarketCounts < 150) {
          //   logger.info("dm=%d, d=%d".format(destMarketCounts, destCounts))
          marketDestProbs(hotelCluster)
        } else if (destCounts / destMarketCounts > 1.5f) {
          //  logger.info("..")
          marketDestProbs(hotelCluster)

        } else userDestProbs(hotelCluster)
      } else leakProb
      clustersProbVector(hotelCluster) = prob
    }
    clustersProbVector
  }

}