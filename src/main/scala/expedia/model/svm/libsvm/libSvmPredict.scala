package expedia.model.svm.libsvm

import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import breeze.linalg._
import libsvm.svm

object libSvmPredict {

  def apply(z: DenseMatrix[Double], model: LibSvmModel): DenseMatrix[Double] = {

    val predictionMatrix = z(*, ::).map { z =>

      val nodes = toSvmNodes(z)
      val probsArray =  Array.fill(model.svm_model.label.size)(0d)
      val predicted = svm.svm_predict_probability(model.svm_model, nodes, probsArray)
      DenseVector(probsArray)
    }

   predictionMatrix
  }
}