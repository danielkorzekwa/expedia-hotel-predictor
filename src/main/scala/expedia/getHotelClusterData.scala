package expedia

import breeze.linalg.DenseMatrix
import breeze.linalg._

object getHotelClusterData {

	def apply(data: DenseMatrix[Double], hotelCluster: Int): DenseMatrix[Double] = {

    data(*, ::).map { r =>
      val rCopy = r.copy
      rCopy(rCopy.size - 1) = if (rCopy(rCopy.size - 1) == hotelCluster.toDouble) 1d else 0
      rCopy
    }
  }
}