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

case class MarketDestPredictionModel(
    clusterHistByDestMarketUser: Map[Tuple3[Int, Int, Int], DenseVector[Float]],
    clusterProbsByDestMarket: Map[Tuple2[Int, Int], DenseVector[Float]],
    clusterProbByDestMap: Map[Int, DenseVector[Float]],
    clusterProbByDestMapSVM: Map[Int, DenseVector[Float]],
    clusterProbMap: DenseVector[Float],
    clusterStatByContinentMapNoPrior: Map[Int, DenseVector[Float]]) extends LazyLogging {

  /**
   * @param data [user_id,dest]
   * @param hotelCluster
   */
  def predict(userId: Int, marketId: Int, destId: Int, continent: Int): DenseVector[Float] = {

    def userProbDefault(destId: Int): DenseVector[Float] = {
      clusterProbByDestMap.getOrElse(destId, clusterProbByDestMap.getOrElse(destId, clusterProbByDestMapSVM.getOrElse(destId, clusterStatByContinentMapNoPrior.getOrElse(continent, clusterProbMap))))
    }

    val userProb = clusterHistByDestMarketUser.getOrElse((destId, marketId, userId), clusterProbsByDestMarket.getOrElse((destId, marketId), userProbDefault(destId)))
    userProb

  }

}

object MarketDestPredictionModel {
  def apply(expediaTrainFile: String, svmPredictionsData: DenseMatrix[Double]): MarketDestPredictionModel = {

    val modelBuilder = MarketDestPredictionModelBuilder(svmPredictionsData: DenseMatrix[Double])

    val destMarketCounterMap = CounterMap[Tuple2[Int, Int]]
    val destCounterMap = CounterMap[Int]()
val marketCounterMap = CounterMap[Int]()

    def onClick(click: Click) = {
      modelBuilder.processCluster(click)

      if (click.isBooking == 1) {
        destMarketCounterMap.add((click.destId, click.market))
        destCounterMap.add(click.destId)
        marketCounterMap.add(click.market)
      }
    }
    ExDataSource(expediaTrainFile).foreach { click => onClick(click) }


    modelBuilder.create(destMarketCounterMap,destCounterMap,marketCounterMap)
  }
}