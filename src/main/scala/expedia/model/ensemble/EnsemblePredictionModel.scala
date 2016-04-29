package expedia.model.ensemble

import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import breeze.linalg._
import expedia.model.userdest.UserDestPredictionModel
import expedia.model.clusterdist.ClusterDistPredictionModel
import expedia.model.svm.SVMPredictionModel
import scala.io.Source
import java.io.File
import com.typesafe.scalalogging.slf4j.LazyLogging
import expedia.model.clusterdist.ClusterDistPredictionModelBuilder
import expedia.model.userdest.UserDestPredictionModelBuilder
import expedia.model.userdest.UserDestPredictionModelBuilder
import expedia.model.clusterdist.ClusterDistPredictionModelBuilder2
import expedia.model.clusterdist.ClusterDistPredictionModelBuilder2
import expedia.model.clusterdist.ClusterDistPredictionModel3
/**
 * @param trainData ('user_location_city','orig_destination_distance','user_id','srch_destination_id','hotel_market','hotel_cluster')
 */
object EnsemblePredictionModel extends LazyLogging {
  def apply(expediaTrainFile: String, svmPredictionsData: DenseMatrix[Double], userIds: Set[Int]): EnsemblePredictionModel = {

    val clusterDistPredictBuilder = ClusterDistPredictionModelBuilder()
    val userDestPredictBuilder = UserDestPredictionModelBuilder(svmPredictionsData, userIds)

      val clusterDistPredictBuilder2 = ClusterDistPredictionModelBuilder2()
    
    processExpediaTrainFile(expediaTrainFile, clusterDistPredictBuilder, userDestPredictBuilder,clusterDistPredictBuilder2)

    val clusterDistPredict = clusterDistPredictBuilder.toClusterDistPredictionModel()
    val userDestPredict = userDestPredictBuilder.toUserDestPredictionModel()

    new EnsemblePredictionModel(clusterDistPredict, userDestPredict)

  }

  private def processExpediaTrainFile(expediaTrainFile: String, clusterDistPredictBuilder: ClusterDistPredictionModelBuilder,
                                      userDestPredictBuilder: UserDestPredictionModelBuilder,clusterDistPredictBuilder2:ClusterDistPredictionModelBuilder2) = {

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

      val key = (userLoc, dist, market)

      clusterDistPredictBuilder.processCluster(userLoc, dist, market, cluster)
      userDestPredictBuilder.processCluster(userId, destId, isBooking,hotelContinent,cluster)
 //clusterDistPredictBuilder2.processCluster(userLoc, dist, market, cluster)
      i += 1
      if (i % 10000 == 0) logger.info("Processed expedia rows: %d".format(i))
    }

  }
}

case class EnsemblePredictionModel(clusterDistPredict: ClusterDistPredictionModel3, userDestPredict: UserDestPredictionModel)
    extends LazyLogging {

  /**
   * @param data ['user_location_city','orig_destination_distance','user_id','srch_destination_id','hotel_market']
   */
  def predict(data: DenseMatrix[Double], hotelCluster: Double): DenseVector[Double] = {

    data(*, ::).map { row =>

      val userLoc = row(0)
      val distId = row(1)
      val market = row(4)

      val leakProb = clusterDistPredict.predict(userLoc, distId, market, hotelCluster)

      if (leakProb.isNaN()) userDestPredict.predict(row(2 to 3), hotelCluster) else leakProb
    }
  }

}