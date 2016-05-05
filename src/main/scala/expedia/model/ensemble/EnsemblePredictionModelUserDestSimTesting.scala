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
import expedia.model.clusterdist.ClusterDistPredictionModel3
import expedia.model.singlecatmodel.SingleCatPredictionModel
import expedia.model.singlecatmodel.SingleCatPredictionModelBuilder
import expedia.model.singlecatmodel.SingleCatPredictionModel
import expedia.model.singlecatmodel.SingleCatPredictionModel
import expedia.model.userdest.UserDestPredictionModel
import scala.collection._
import expedia.model.marketdest.MarketDestPredictionModelBuilder
import expedia.model.marketdest.MarketDestPredictionModel
import expedia.model.clusterdistbayes.ClusterDistBayesPredictionModel
import expedia.model.clusterdistbayes.ClusterDistBayesPredictionModelBuilder

/**
 * @param trainData ('user_location_city','orig_destination_distance','user_id','srch_destination_id','hotel_market','hotel_cluster')
 */
object EnsemblePredictionModelUserDestSimTesting extends LazyLogging {
  def apply(expediaTrainFile: String, svmPredictionsData: DenseMatrix[Double], userIds: Set[Int]): EnsemblePredictionModelUserDestSimTesting = {

    val clusterDistPredictBuilder = ClusterDistPredictionModelBuilder()

    processExpediaTrainFile(expediaTrainFile, clusterDistPredictBuilder)

    val clusterDistPredict = clusterDistPredictBuilder.toClusterDistPredictionModel()
    new EnsemblePredictionModelUserDestSimTesting(clusterDistPredict)

  }

  private def processExpediaTrainFile(expediaTrainFile: String, clusterDistPredictBuilder: ClusterDistPredictionModelBuilder) = {

    var i = 0
    Source.fromFile(new File(expediaTrainFile)).getLines().drop(1).foreach { l =>
      val lArray = l.split(",")

      val userLoc = lArray(5).toDouble
      val dist = if (lArray(6).equals("NA") || lArray(6).isEmpty()) -1d else lArray(6).toDouble
      val userId = lArray(7).toInt
      val destId = lArray(16).toInt
      val isBooking = lArray(18).toInt
      val hotelContinent = lArray(20).toInt
      val market = lArray(22).toDouble
      val cluster = lArray(23).toInt

      if (dist == 9734.0267) println(l)

      val key = (userLoc, dist, market)
      clusterDistPredictBuilder.processCluster(userLoc, dist, market, cluster)
      i += 1
      if (i % 1000000 == 0) logger.info("Processed expedia rows: %d".format(i))
    }

  }
}

case class EnsemblePredictionModelUserDestSimTesting(clusterDistPredict: ClusterDistPredictionModel3)
    extends LazyLogging {

  /**
   * @param data ['user_location_city','orig_destination_distance','user_id','srch_destination_id','hotel_market']
   */
  def predict(userLoc: Int, dist: Double, userId: Int, destId: Int, hotelContinent: Int, market: Int): DenseVector[Double] = {

    val clustersProbVector = DenseVector.fill(100)(0d)
    (0 until 100).foreach { hotelCluster =>
      val leakProb = clusterDistPredict.predict(userLoc, dist, market, hotelCluster)

      val prob = if (leakProb.isNaN()) {

        0d
      } else leakProb
      clustersProbVector(hotelCluster) = prob
    }
    clustersProbVector
  }

}