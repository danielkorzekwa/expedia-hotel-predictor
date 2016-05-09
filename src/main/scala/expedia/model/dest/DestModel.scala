package expedia.model.dest

import java.io.File

import scala.collection.Map
import scala.collection.Seq

import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import breeze.linalg.csvread
import expedia.data.Click
import expedia.data.ExDataSource
import expedia.model.country.CountryModelBuilder
import expedia.model.svm.loadClusterProbsByKeyMap
import expedia.stats.MulticlassHistByKey

case class DestModel(
    clusterHistByDest: MulticlassHistByKey[Int],
    clusterProbByDestMapSVM: Map[Int, DenseVector[Float]]) {

   val svmPredictionsByStaydaysData = csvread(new File("c:/perforce/daniel/ex/svm/svm_predictions_8250_628_by_staylength.csv"), skipLines = 1)
  val clusterProbsByStaydays: Map[Int, DenseVector[Float]] = loadClusterProbsByKeyMap(svmPredictionsByStaydaysData)

  val zeroProbs =  DenseVector.fill(100)(0f)
  
  def predict(destId: Int, continentId: Int,stayDays:Int): DenseVector[Float] = {

     
     if(destId==8250) {
     //   clusterHistByDest.getMap(destId)
       clusterProbsByStaydays.getOrElse(stayDays,clusterHistByDest.getMap(destId))
     }
     else zeroProbs
    
  }
   
    def predict(destId: Int, continentId: Int): DenseVector[Float] = {

    clusterHistByDest.getMap(destId)
    
  }
}

object DestModel {

  def apply(expediaTrainFile: String, svmPredictionsData: DenseMatrix[Double], testClicks: Seq[Click]): DestModel = {

    val countryModelBuilder = CountryModelBuilder(testClicks)
    val destModelBuilder = DestModelBuilder(svmPredictionsData, testClicks)

    def onClick(click: Click) = {
      countryModelBuilder.processCluster(click)
      destModelBuilder.processCluster(click)

    }
    ExDataSource(expediaTrainFile).foreach { click => onClick(click) }

    val countryModel = countryModelBuilder.create()
    val destModel = destModelBuilder.create(countryModel)
    destModel
  }

}