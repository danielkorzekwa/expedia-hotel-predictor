package expedia.util

import breeze.linalg.DenseVector

object getTopNClusters {

  /**
   * @return  [p1,p2,p3,p4,p5...,c1,c2,c3,c4,c5,...]
   */
  def apply(clusterProbs: DenseVector[Float],n:Int): DenseVector[Double] = {

    val predictedProbTuples = clusterProbs.toArray.toList.zipWithIndex.sortWith((a, b) => a._1 > b._1).take(n).toArray

    val predictionProbs = predictedProbTuples.map(_._1.toDouble)
    val predictionRanks = predictedProbTuples.map(_._2.toDouble)

    val record = DenseVector.vertcat(DenseVector(predictionProbs), DenseVector(predictionRanks))

    record
  }

}