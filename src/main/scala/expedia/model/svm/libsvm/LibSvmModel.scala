package expedia.model.svm.libsvm

import libsvm.svm_model
import libsvm.svm

case class LibSvmModel(svm_model: svm_model) {

  def save(file: String) = svm.svm_save_model(file, svm_model)
}

object LibSvmModel {
  def loadFromFile(file: String) = {
    val svmModel = svm.svm_load_model(file)
    LibSvmModel(svmModel)
  }
}