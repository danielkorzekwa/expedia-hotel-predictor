package expedia.model.old.clusterdistbayes.gprfast

import breeze.linalg.DenseVector

object gprFastPredict {

  def apply(y: DenseVector[Double], model: GprFastModel): DenseVector[Double] = {

    val predMean = model.kXZ.t * (model.kXXInv * y)
    predMean
  }
}