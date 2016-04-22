package expedia

import breeze.linalg._
import java.io.File

case class SVMFromCSVPredict() {
  
  val predictionMat = csvread(new File("c:/perforce/daniel/ex/svm_predictions.csv"),skipLines=1)
   def predict(data: DenseMatrix[Double], hotelCluster: Double): DenseVector[Double] = {

   val predicted = (0 until data.rows).map{i =>
     val prob = predictionMat(i,hotelCluster.toInt)
       prob
       
   }.toArray
   DenseVector(predicted)

  }
}