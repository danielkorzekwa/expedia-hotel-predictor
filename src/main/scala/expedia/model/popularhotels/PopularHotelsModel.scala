package expedia.model.popularhotels

import expedia.data.Click
import expedia.data.ExDataSource
import breeze.linalg.DenseVector

case class PopularHotelsModel(clusterProbs:DenseVector[Float]) {

  def predict(): DenseVector[Float]  = clusterProbs
}

object PopularHotelsModel {

  def apply(expediaTrainFile: String): PopularHotelsModel = {

    val modelBuilder = PopularHotelsModelBuilder()

    ExDataSource(expediaTrainFile).foreach { click =>modelBuilder.processCluster(click) }

    modelBuilder.create()
  }
}