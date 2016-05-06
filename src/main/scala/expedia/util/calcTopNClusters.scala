package expedia.util

import breeze.linalg.DenseVector

object calcTopNClusters {

  def apply(clusterProbs: DenseVector[Float], n: Int,minProb:Double): DenseVector[Int] = {
    val topNArray = clusterProbs.toArray.toList.zipWithIndex.filter{case (prob,cluster) => prob>minProb}.sortWith((a, b) => a._1 > b._1).take(n).map(_._2).toArray

    DenseVector(topNArray)
  }
}