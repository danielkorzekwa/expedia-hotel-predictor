

package expedia

import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix
import breeze.linalg._
import breeze.stats._

/**Simple statistics model*/
object hotelPredict {

  /**
   * @param trainHotelData [category,booked(0/1)]
   * @param tData [category]
   */
  def apply(trainHotelData: DenseMatrix[Double], testData: DenseMatrix[Double], hotelCluster: Int): DenseVector[Double] = {

    val categories = unique(trainHotelData(::, 0))
    println("hotelCluster=%d, Categories=%d".format(hotelCluster, categories.size))

    val probByCategory: Map[Double, Double] = categories.toArray.zipWithIndex.map {
      case (category, index) =>
        val idx = trainHotelData(::, 0).findAll { c => c == category }
        //  println(index)
        val categoryData: DenseMatrix[Double] = trainHotelData(idx, ::).toDenseMatrix

        //  println(idx.size + ":" +    sum(categoryData(::,1)) +":"+ mean(categoryData(::, 1)))
        val categoryHotelClusterProb = mean(categoryData(::, categoryData.cols - 1))
        (category, categoryHotelClusterProb)
    }.filter(x => x._2 > 0).toMap

    val hotelClusterProb = mean(trainHotelData(::, trainHotelData.cols - 1))

    testData(*, ::).map { row =>

      val prob = if (probByCategory.contains(row(0))) probByCategory(row(0))
      else hotelClusterProb

      prob
    }
    //  testData(*, ::).map(row => hotelClusterProb)
  }
}