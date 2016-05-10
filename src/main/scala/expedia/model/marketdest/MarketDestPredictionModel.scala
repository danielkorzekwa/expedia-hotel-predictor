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
import expedia.model.country.CountryModelBuilder
import expedia.model.country.CountryModelBuilder
import expedia.model.country.CountryModelBuilder
import expedia.model.clusterdist.ClusterDistPredictionModel
import expedia.model.clusterdistprox.ClusterDistProxModelBuilder
import expedia.model.clusterdistprox.ClusterDistProxModel

case class MarketDestPredictionModel(
    destModel: DestModel,
    clusterHistByDestMarketUser: Map[Tuple3[Int, Int, Int], DenseVector[Float]],
    clusterProbsByDestMarket: Map[Tuple2[Int, Int], DenseVector[Float]],
    clusterDistProxModel: ClusterDistProxModel) extends LazyLogging {

  /**
   * @param data [user_id,dest]
   * @param hotelCluster
   */
  def predict(click: Click): DenseVector[Float] = {
    val userProb = clusterHistByDestMarketUser((click.destId, click.marketId, click.userId))

    val clusterDistProxProbs = clusterDistProxModel.predict(click)

    // clusterDistProxProbs.foreachPair{(index,prob) => 
    //   if(prob<0.005) {
    //     
    //     if(click.userLoc==24103 && click.marketId==628 && click.dist==227.5322) {
    //       println("...")
    //     }
    //    userProb(index)=prob
    //  }
    // }

    userProb
  }

  def predictTop5(clicks: Seq[Click]): DenseMatrix[Double] = {
    val i = new AtomicInteger(0)
    val predictionRecordsMarketDest = clicks.par.map { click =>
      val predicted = predict(click)

      val predictedProbTuples = predicted.toArray.toList.zipWithIndex.sortWith((a, b) => a._1 > b._1).take(5).toArray

      val predictionProbs = predictedProbTuples.map(_._1.toDouble)
      val predictionRanks = predictedProbTuples.map(_._2.toDouble)

      if (i.incrementAndGet() % 100000 == 0) logger.info("Predicting clusters: %d".format(i.get))

      val record = DenseVector.vertcat(DenseVector(predictionProbs), DenseVector(predictionRanks))
      record
    }.toList

    val predictionMatrixMarketDest = DenseVector.horzcat(predictionRecordsMarketDest: _*).t
    predictionMatrixMarketDest
  }
}

object MarketDestPredictionModel {
  def apply(trainDatasource: ExDataSource, testClicks: Seq[Click],param:Double=300): MarketDestPredictionModel = {

    val clusterDistProxModelBuilder = ClusterDistProxModelBuilder(testClicks)

    val destModelBuilder = DestModelBuilder(testClicks)
    val countryModelBuilder = CountryModelBuilder(testClicks)
    val modelBuilder = MarketDestPredictionModelBuilder(testClicks,param)

    val destMarketCounterMap = CounterMap[Tuple2[Int, Int]]
    val destCounterMap = CounterMap[Int]()
    val marketCounterMap = CounterMap[Int]()

    def onClick(click: Click) = {
      clusterDistProxModelBuilder.processCluster(click)

      destModelBuilder.processCluster(click)
      countryModelBuilder.processCluster(click)
      modelBuilder.processCluster(click)

      if (click.isBooking == 1) {
        destMarketCounterMap.add((click.destId, click.marketId))
        destCounterMap.add(click.destId)
        marketCounterMap.add(click.marketId)
      }
    }
    trainDatasource.foreach { click => onClick(click) }

    val clusterDistProxModel = clusterDistProxModelBuilder.create()
    val countryModel = countryModelBuilder.create()
    val destModel = destModelBuilder.create(countryModel)
    modelBuilder.create(destModel, countryModel, destMarketCounterMap, destCounterMap, marketCounterMap, clusterDistProxModel)
  }
}