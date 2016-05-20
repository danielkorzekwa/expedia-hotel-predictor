package expedia.rankgpr

import org.junit.Test
import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import expedia.data.ExDataSource
import breeze.numerics._
import dk.gp.cov.CovSEiso

class rankGprPredictTest {

  @Test def test_two_classes = {

    val cluster1 = 19
    val cluster2 = 23

    val allClicks = ExDataSource(dsName = "test", "c:/perforce/daniel/ex/segments/dest_12217/train_2013_dest12217.csv").getAllClicks()
    val filteredClicks = allClicks.filter { c => c.isBooking == 1 && (c.cluster == cluster1 || c.cluster == cluster2) && c.checkinMonth > -1 }

    val dataX = DenseVector(filteredClicks.map(c => c.checkinMonth.toDouble).toArray).toDenseMatrix.t
    val dataY = DenseVector(filteredClicks.map(c => c.cluster.toDouble).toArray)

      val covFunc = CovSEiso()
      val covFuncParams = DenseVector[Double](log(1), log(1))
      val noiseLogStdDev = log(1d)
    val model = RankGprModel(dataX, dataY,covFunc,covFuncParams,noiseLogStdDev)

    val xTest = DenseVector.rangeD(1d, 13, 1).toDenseMatrix.t
    val predicted = RankGprPredict(model).predict(xTest)

    println(DenseMatrix.horzcat(xTest, predicted))

  }

  @Test def test_three_classes = {

    val clusterSet = Set(19, 21, 23)

    val allClicks = ExDataSource(dsName = "test", "c:/perforce/daniel/ex/segments/dest_12217/train_2013_dest12217.csv").getAllClicks()
    val filteredClicks = allClicks.filter { c => c.isBooking == 1 && (clusterSet.contains(c.cluster)) && c.checkinMonth > -1 }

    val dataX = DenseVector(filteredClicks.map(c => c.checkinMonth.toDouble).toArray).toDenseMatrix.t
    val dataY = DenseVector(filteredClicks.map(c => c.cluster.toDouble).toArray)

      val covFunc = CovSEiso()
      val covFuncParams = DenseVector[Double](log(1), log(1))
      val noiseLogStdDev = log(1d)
    val model = RankGprModel(dataX, dataY,covFunc,covFuncParams,noiseLogStdDev)

    val xTest = DenseVector.rangeD(1d, 13, 1).toDenseMatrix.t
    val predicted =  RankGprPredict(model).predict(xTest)


    println(DenseMatrix.horzcat(xTest, predicted))

  }

  @Test def test_seven_classes = {

    val clusterSet = Set(49, 4, 19, 59, 23, 13, 21)

    val allClicks = ExDataSource(dsName = "test", "c:/perforce/daniel/ex/segments/dest_12217/train_2013_dest12217.csv").getAllClicks()
    val filteredClicks = allClicks.filter { c => c.isBooking == 1 && (clusterSet.contains(c.cluster)) && c.checkinMonth > -1 }

    val dataX = DenseVector(filteredClicks.map(c => c.checkinMonth.toDouble).toArray).toDenseMatrix.t
    val dataY = DenseVector(filteredClicks.map(c => c.cluster.toDouble).toArray)

     val covFunc = CovSEiso()
      val covFuncParams = DenseVector[Double](log(1), log(1))
      val noiseLogStdDev = log(1d)
    val model = RankGprModel(dataX, dataY,covFunc,covFuncParams,noiseLogStdDev)

    val xTest = DenseVector.rangeD(1d, 13, 1).toDenseMatrix.t
    val predicted =  RankGprPredict(model).predict(xTest)


    println(DenseMatrix.horzcat(xTest, predicted))

  }
}