package expedia

import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import breeze.linalg._

/**
 * @param trainData ('user_location_city','orig_destination_distance','user_id','srch_destination_id','hotel_market','hotel_cluster')
 */
case class EnsemblePredict(trainData: DenseMatrix[Double]) {

  val userDestPredict = UserDestPredict(trainData(::, List(2, 3, 5)).toDenseMatrix)
  val leakPredict = UserLocMarketDistClusterPredict(trainData(::, List(0, 1, 4, 5)).toDenseMatrix)

  def predict(data: DenseMatrix[Double], hotelCluster: Double): DenseVector[Double] = {

    data(*, ::).map { row =>

      val leakProb = leakPredict.predict(row(List(0, 1, 4)).toDenseVector, hotelCluster)

      //if (leakProb.isNaN()) userDestPredict.predict(row(2 to 3), hotelCluster) else leakProb
      if (leakProb.isNaN()) 0d else leakProb
    }
  }
}