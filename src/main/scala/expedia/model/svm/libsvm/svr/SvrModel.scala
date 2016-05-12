package expedia.model.svm.libsvm.svr

import libsvm.svm
import libsvm.svm_model

case class SvrModel(svm_model: svm_model) {
  def save(file: String) = svm.svm_save_model(file, svm_model)
}

object SvrModel {
  def loadFromFile(file: String) = {
    val svmModel = svm.svm_load_model(file)
    SvrModel(svmModel)
  }
}