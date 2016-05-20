package expedia.rankgpr

import org.junit.Test

import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import expedia.data.ExDataSource

class rankGprPredictTest {

  @Test def test_two_classes = {

    val cluster1 = 19
    val cluster2 = 21

    val allClicks = ExDataSource(dsName = "test", "c:/perforce/daniel/ex/segments/dest_12217/train_2013_dest12217.csv").getAllClicks()
    val filteredClicks = allClicks.filter { c => c.isBooking == 1 && (c.cluster == cluster1 || c.cluster == cluster2) && c.checkinMonth > -1 }

    val dataX = DenseVector(filteredClicks.map(c => c.checkinMonth.toDouble).toArray).toDenseMatrix.t
    val dataY = DenseVector(filteredClicks.map(c => c.cluster.toDouble).toArray)

    val model = RankGprModel(dataX, dataY)

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

    val model = RankGprModel(dataX, dataY)

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

    val model = RankGprModel(dataX, dataY)

    val xTest = DenseVector.rangeD(1d, 13, 1).toDenseMatrix.t
    val predicted =  RankGprPredict(model).predict(xTest)


    println(DenseMatrix.horzcat(xTest, predicted))

  }
}