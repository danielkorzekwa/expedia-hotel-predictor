package expedia.model.svm.libsvm

import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import breeze.linalg._
import libsvm.svm

object libSvmPredict {

  def apply(z: DenseMatrix[Double], model: LibSvmModel): DenseVector[Double] = {

    val predictionVector = z(*, ::).map { z =>

      val nodes = toSvmNodes(z)
      val predicted = svm.svm_predict(model.svm_model, nodes)
      predicted
    }

    predictionVector
  }
}