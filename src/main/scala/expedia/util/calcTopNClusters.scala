package expedia.util

import breeze.linalg.DenseVector
import scala.util.Random

object calcTopNClusters {

  def apply(clusterProbs: DenseVector[Float], n: Int, minProb: Option[Double]=None): DenseVector[Int] = {

    val topNArray = if (minProb.isDefined) {
      clusterProbs.toArray.toList.zipWithIndex.filter { case (prob, cluster) => prob > minProb.get }.sortWith((a, b) => a._1 > b._1).take(n).map(_._2).toArray
   
    }
    else clusterProbs.toArray.toList.zipWithIndex.sortWith((a, b) => a._1 > b._1).take(n).map(_._2).toArray
    DenseVector(topNArray)
  }

}
  
