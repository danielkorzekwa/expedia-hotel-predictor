package expedia.model.svm.libsvm.svr

import breeze.linalg.{ * => * }
import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import expedia.model.svm.libsvm.toSvmNodes
import libsvm.svm

object svrPredict {

  def apply(z: DenseMatrix[Double], model: SvrModel): DenseVector[Double] = {

    val predictionVector = z(*, ::).map { z =>

      val nodes = toSvmNodes(z)
      val predicted = svm.svm_predict(model.svm_model, nodes)
      predicted
    }

    predictionVector
  }

}