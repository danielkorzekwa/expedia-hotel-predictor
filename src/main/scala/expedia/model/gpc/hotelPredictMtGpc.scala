

package expedia.model.gpc

import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix
import breeze.linalg._
import breeze.stats._
import dk.gp.mtgpc.MtgpcModel
import dk.gp.cov.CovNoise
import breeze.numerics._
import dk.gp.mtgpc.mtgpcTrain
import dk.gp.mtgpc.mtgpcPredict
import com.typesafe.scalalogging.slf4j.LazyLogging

object hotelPredictMtGpc extends LazyLogging {

  /**
   * @param trainHotelData [category,booked(0/1)]
   * @param tData [category]
   */
  def apply(trainHotelData: DenseMatrix[Double], testData: DenseMatrix[Double], hotelCluster: Int): DenseVector[Double] = {

    logger.info("Predicting for hotelCluster=" + hotelCluster)
  val categories = unique(trainHotelData(::, 0))
    println("hotelCluster=%d, Categories=%d".format(hotelCluster, categories.size))
    
    val x = trainHotelData(::, 0 to 0)
    val y = trainHotelData(::, 1)
  
    val covFunc = CovNoise()
    val covFuncParams = DenseVector(log(1))
    val gpMean = 0d
    val gpcModel = MtgpcModel(x, y, covFunc, covFuncParams, gpMean)
    val trainedModel = mtgpcTrain(gpcModel, maxIter = 10)

    val predicted = mtgpcPredict(testData(::, 0 to 0), trainedModel)
 
    predicted
  }
}