package expedia.model.usermodel

import breeze.linalg.DenseVector
import expedia.data.Click
import expedia.model.ClusterModel
import expedia.stats.MulticlassHistByKey

case class UserModel(clusterHistByUser: MulticlassHistByKey[Int]) extends ClusterModel {
  
   def predict(click:Click): DenseVector[Float] = {

    clusterHistByUser.getMap(click.userId)

  }
}