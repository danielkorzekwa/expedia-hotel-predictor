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
import expedia.model.singlecatmodel.SingleCatPredictionModel
import expedia.model.singlecatmodel.SingleCatPredictionModelBuilder
import expedia.model.singlecatmodel.SingleCatPredictionModel
import expedia.model.singlecatmodel.SingleCatPredictionModel
import expedia.model.userdest.UserDestPredictionModel
import scala.collection._

/**
 * @param trainData ('user_location_city','orig_destination_distance','user_id','srch_destination_id','hotel_market','hotel_cluster')
 */
object EnsemblePredictionModel extends LazyLogging {
  def apply(expediaTrainFile: String, svmPredictionsData: DenseMatrix[Double], userIds: Set[Int]): EnsemblePredictionModel = {

    val clusterDistPredictBuilder = ClusterDistPredictionModelBuilder()
    val userDestPredictBuilder = UserDestPredictionModelBuilder(svmPredictionsData, userIds)
    val marketDestPredictBuilder = UserDestPredictionModelBuilder(svmPredictionsData, Set())

    val singleCatPredictBuilder = SingleCatPredictionModelBuilder()

    val clusterDistPredictBuilder2 = ClusterDistPredictionModelBuilder2()

    val countsByDestMarketMap:mutable.Map[Tuple2[Int,Int],Int] = mutable.Map()
    
    processExpediaTrainFile(expediaTrainFile, clusterDistPredictBuilder, userDestPredictBuilder, clusterDistPredictBuilder2, singleCatPredictBuilder, marketDestPredictBuilder,
        countsByDestMarketMap)

    val clusterDistPredict = clusterDistPredictBuilder.toClusterDistPredictionModel()
    val userDestPredict = userDestPredictBuilder.toUserDestPredictionModel()
    val marketDestPredict = marketDestPredictBuilder.toUserDestPredictionModel()
    val singleCatPredict = singleCatPredictBuilder.toSingleCatPredictionModel()
    new EnsemblePredictionModel(clusterDistPredict, userDestPredict, singleCatPredict, marketDestPredict,countsByDestMarketMap)

  }

  private def processExpediaTrainFile(expediaTrainFile: String, clusterDistPredictBuilder: ClusterDistPredictionModelBuilder,
                                      userDestPredictBuilder: UserDestPredictionModelBuilder, clusterDistPredictBuilder2: ClusterDistPredictionModelBuilder2,
                                      singleCatPredictBuilder: SingleCatPredictionModelBuilder, marketDestPredictBuilder: UserDestPredictionModelBuilder,
                                      countsByDestMarketMap:mutable.Map[Tuple2[Int,Int],Int]) = {

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
      userDestPredictBuilder.processCluster(userId.toInt, destId, isBooking, hotelContinent, cluster)
      marketDestPredictBuilder.processCluster(market.toInt, destId, isBooking, hotelContinent, cluster)
      singleCatPredictBuilder.processCluster(market.toInt, cluster)
      
      if(isBooking==1) {
      val currDestMarketCount = countsByDestMarketMap.getOrElseUpdate((destId,market.toInt),0)
      countsByDestMarketMap.update((destId,market.toInt),currDestMarketCount+1)
      }
      //clusterDistPredictBuilder2.processCluster(userLoc, dist, market, cluster)
      i += 1
      if (i % 10000 == 0) logger.info("Processed expedia rows: %d".format(i))
    }

  }
}

case class EnsemblePredictionModel(clusterDistPredict: ClusterDistPredictionModel3, userDestPredict: UserDestPredictionModel, singleCatPredictionModel: SingleCatPredictionModel,
                                   marketDestPredict: UserDestPredictionModel,countsByDestMarketMap:mutable.Map[Tuple2[Int,Int],Int])
    extends LazyLogging {

  /**
   * @param data ['user_location_city','orig_destination_distance','user_id','srch_destination_id','hotel_market']
   */
  def predict(userLoc:Int,dist:Double,userId:Int,destId:Int,hotelContinent:Int,market:Int): DenseVector[Double] = {


    val clustersProbVector = DenseVector.fill(100)(0d)
    (0 until 100).foreach { hotelCluster =>
      val leakProb = clusterDistPredict.predict(userLoc, dist, market, hotelCluster)

      val prob = if (leakProb.isNaN()) {
        val distMarketCounts = countsByDestMarketMap.getOrElse((destId,market),0)
        
        
        if(distMarketCounts<150) marketDestPredict.predict(market,destId, hotelContinent,hotelCluster)
        else userDestPredict.predict(userId, destId, hotelContinent,hotelCluster)
      } else leakProb
      // val prob =   if (leakProb.isNaN()) singleCatPredictionModel.predict(market.toInt, hotelCluster.toInt) else leakProb
      clustersProbVector(hotelCluster) = prob
    }
    clustersProbVector
  }

}