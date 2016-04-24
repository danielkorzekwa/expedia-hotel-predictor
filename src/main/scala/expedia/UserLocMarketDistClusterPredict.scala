package expedia

import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import breeze.linalg._
import expedia.similarity.CalcUserLocMarketDistCluster
import java.io.FileInputStream
import java.io.ObjectInputStream
import com.typesafe.scalalogging.slf4j.LazyLogging

/**
 * @param data [user_location_city,orig_destination_distance,hotel_market,hotel_cluster]
 */
case class UserLocMarketDistClusterPredict(trainData: DenseMatrix[Double]) extends LazyLogging{

 // val clustMap = CalcUserLocMarketDistCluster(trainData).toMap

  logger.info("Loading distClusterMap...")
//  val fileIn = new FileInputStream("target/distClusterMap_all.obj")
//  val objectInt = new ObjectInputStream(fileIn)
//  val clustMap = objectInt.readObject().asInstanceOf[Map[Tuple3[Double, Double, Double], Double]]
  
  //  val dataFile = "c:/perforce/daniel/ex/orig_data/train.csv"
  val dataFile = "c:/perforce/daniel/ex/train_small.csv"
  val clustMap = CalcUserLocMarketDistCluster(dataFile).toMap
  
   logger.info("Loading distClusterMap...done")
  println("distClusterMap size=" + clustMap.size)
  /**
   * @param data [user_location_city,orig_destination_distance,hotel_market]
   */
  def predict(data: DenseMatrix[Double], hotelCluster: Double): DenseVector[Double] = {

    val probsVector = data(*, ::).map { row =>
      predict(row, hotelCluster)
    }

    probsVector
  }

  /**
   * @param data [user_location_city,orig_destination_distance,hotel_market]
   */
  def predict(row: DenseVector[Double], hotelCluster: Double): Double = {

    val key = (row(0), row(1), row(2))
    val cluster = clustMap.getOrElse(key, -1d)

    if (cluster == hotelCluster) 1d else Double.NaN

  }
}