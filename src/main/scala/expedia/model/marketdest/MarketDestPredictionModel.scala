package expedia.model.marketdest

import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix
import breeze.linalg._
import com.typesafe.scalalogging.slf4j.LazyLogging
import java.util.concurrent.atomic.AtomicInteger
import scala.collection._
import expedia.model.svm.loadClusterProbsByDestMap
import expedia.model.svm.SVMPredictionModel
import expedia.data.ExDataSource
import expedia.data.Click
import expedia.stats.CounterMap
import expedia.model.dest.DestModelBuilder
import expedia.model.dest.DestModel
import expedia.stats.MulticlassHistByKey

case class MarketDestPredictionModel(
    destModel: DestModel,
    clusterHistByDestMarketUser: Map[Tuple3[Int, Int, Int], DenseVector[Float]],
    clusterProbsByDestMarket: Map[Tuple2[Int, Int], DenseVector[Float]]) extends LazyLogging {

  /**
   * @param data [user_id,dest]
   * @param hotelCluster
   */
  def predict(userId: Int, marketId: Int, destId: Int, continent: Int): DenseVector[Float] = {

    val userProb = clusterHistByDestMarketUser((destId, marketId, userId))
    userProb
  }

}

object MarketDestPredictionModel {
  def apply(expediaTrainFile: String, svmPredictionsData: DenseMatrix[Double], testClicks: Seq[Click]): MarketDestPredictionModel = {

    val destModelBuilder = DestModelBuilder(svmPredictionsData)
    val modelBuilder = MarketDestPredictionModelBuilder(svmPredictionsData, Set(), testClicks)

    val destMarketCounterMap = CounterMap[Tuple2[Int, Int]]
    val destCounterMap = CounterMap[Int]()
    val marketCounterMap = CounterMap[Int]()

    def onClick(click: Click) = {
      destModelBuilder.create()
      modelBuilder.processCluster(click)

      if (click.isBooking == 1) {
        destMarketCounterMap.add((click.destId, click.market))
        destCounterMap.add(click.destId)
        marketCounterMap.add(click.market)
      }
    }
    ExDataSource(expediaTrainFile).foreach { click => onClick(click) }

    val destModel = destModelBuilder.create()
    modelBuilder.create(destModel, destMarketCounterMap, destCounterMap, marketCounterMap)
  }
}