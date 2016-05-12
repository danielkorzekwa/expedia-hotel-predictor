package expedia.model.svm.libsvm.svc

import libsvm.svm_model
import libsvm.svm

case class SvcModel(svm_model: svm_model) {

  def save(file: String) = svm.svm_save_model(file, svm_model)
}

object SvcModel {
  def loadFromFile(file: String) = {
    val svmModel = svm.svm_load_model(file)
    SvcModel(svmModel)
  }
}