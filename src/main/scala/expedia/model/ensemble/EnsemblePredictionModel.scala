package expedia.model.ensemble

import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import breeze.linalg._
import expedia.model.userdest.UserDestPredictionModel
import expedia.model.clusterdist.ClusterDistPredictionModel
import expedia.model.svm.SVMPredictionModel

/**
 * @param trainData ('user_location_city','orig_destination_distance','user_id','srch_destination_id','hotel_market','hotel_cluster')
 */
case class EnsemblePredictionModel(trainBookedData: DenseMatrix[Double], expediaTrainFile: String,svmPredictionsData:DenseMatrix[Double],svmPredictionModel: SVMPredictionModel) {

  val userDestPredict = UserDestPredictionModel(trainBookedData(::, List(2, 3, 5)).toDenseMatrix,svmPredictionsData,svmPredictionModel)
  val clusterDistPredict = ClusterDistPredictionModel(expediaTrainFile)

  def predict(data: DenseMatrix[Double], hotelCluster: Double): DenseVector[Double] = {

    data(*, ::).map { row =>

      val leakProb = clusterDistPredict.predict(row(List(0, 1, 4)).toDenseVector, hotelCluster)

      if (leakProb.isNaN()) userDestPredict.predict(row(2 to 3), hotelCluster) else leakProb
    }
  }
}