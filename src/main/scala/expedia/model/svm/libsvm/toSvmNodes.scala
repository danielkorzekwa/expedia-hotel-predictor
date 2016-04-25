package expedia.model.svm.libsvm

import libsvm.svm_node
import breeze.linalg.DenseVector


object toSvmNodes {

  def apply(x: DenseVector[Double]): Array[svm_node] = {

    val nodes = x.toArray.zipWithIndex.map {
      case (v, index) =>
        val node = new svm_node()
        node.index = index + 1
        node.value = v
        node
    }

    nodes
  }
}