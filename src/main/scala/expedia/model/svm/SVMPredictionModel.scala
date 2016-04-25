package expedia.model.svm

import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix
import expedia.model.svm.libsvm.LibSvmModel
import expedia.model.svm.libsvm.libSvmTrain
import breeze.linalg._
import scala.collection.mutable.ListBuffer
import expedia.model.svm.libsvm.libSvmPredict

/**
 * @param destMatrix [destId,d1,d2,...d149]
 * @trainData [dest,cluster]
 */
case class SVMPredictionModel(destMatrix: DenseMatrix[Double], trainData: DenseMatrix[Double]) {

  private val d149ByDestMap = calcD149ByDestMap(destMatrix)

  private val svmModel = trainSVM()
  /**
   * @param row [destId]
   *
   * @param [cluster0 prob, cluster 1 prob,...,cluster99 prob]
   */
  def predict(destId: Double): DenseVector[Double] = {
println(destId)
    val d149Vec = d149ByDestMap(destId)
    
    val probs = libSvmPredict(d149Vec.toDenseMatrix.t,svmModel)
    probs(0,::).t
  }

  private def trainSVM(): LibSvmModel = {

    val trainDataSeq: ListBuffer[DenseVector[Double]] = ListBuffer()

    (0 until 100).foreach {
      case i =>

        val row = trainData(i, ::)
        val destId = row(0)
        val cluster = row(1)

        d149ByDestMap.get(destId) match {
          case Some(d149Vec) =>
            trainDataSeq += DenseVector(d149Vec.toArray :+ cluster)
          case _ => //no d149 for destId found
        }

    }

    val svmTrainData = DenseVector.horzcat(trainDataSeq :_*).t
    val svmTrainDataX = svmTrainData(::,0 until svmTrainData.cols-1)
     val svmTrainDataY = svmTrainData(::,svmTrainData.cols-1)
   val svmModel = libSvmTrain(svmTrainDataX,svmTrainDataY)
   svmModel
  }
}