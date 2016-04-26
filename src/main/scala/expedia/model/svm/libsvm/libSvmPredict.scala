package expedia.model.svm.libsvm

import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import breeze.linalg._
import libsvm.svm

object libSvmPredict {

  def apply(z: DenseMatrix[Double], model: LibSvmModel): DenseMatrix[Double] = {

    val predictionMatrix = z(*, ::).map { z =>

      val nodes = toSvmNodes(z)
      val probsArray = Array.fill(model.svm_model.label.size)(0d)
      svm.svm_predict_probability(model.svm_model, nodes, probsArray)

      val predictedSorted = Array.fill(model.svm_model.label.max+1)(0d)
      model.svm_model.label.zipWithIndex.foreach { case (l, index) => predictedSorted(l) = probsArray(index) }

      DenseVector(predictedSorted)
    }

    predictionMatrix
  }
}