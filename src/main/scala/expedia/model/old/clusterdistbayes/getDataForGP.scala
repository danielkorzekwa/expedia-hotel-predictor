package expedia.model.old.clusterdistbayes

import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector

object getDataForGP {

  /**
   * @param clusters
   * @return [clusterId, active(0/1)
   */
  def apply(clusters: DenseVector[Double],clustersSize:Int): DenseMatrix[Double] = {

    val m = DenseMatrix.fill(clusters.size * clustersSize, 2)(0d)

    clusters.toArray.zipWithIndex.foreach{ case(c,i) =>
      
      (0 until clustersSize).foreach{c =>   m(i*clustersSize+c.toInt,0)=c}
      
      m(i*clustersSize+c.toInt,1)=1
    }
    m
  }
}