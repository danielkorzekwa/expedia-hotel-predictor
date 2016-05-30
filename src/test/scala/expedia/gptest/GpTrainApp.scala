package expedia.gptest

import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import breeze.numerics.log
import dk.gp.cov.CovFunc
import dk.gp.cov.CovSEiso
import expedia.data.ExCSVDataSource
import expedia.rankgpr.RankGprModel
import expedia.rankgpr.rankGprTrain
import expedia.stats.MulticlassHist
import expedia.util.calcTopNClusters

object GpTrainApp {

  val DAY = (1000L * 3600 * 24).toDouble

  def main(args: Array[String]): Unit = {

    val allClicks = ExCSVDataSource(dsName = "test", "c:/perforce/daniel/ex/segments/loc_market_dest/train_2013_loc_2096_market_675_dest_8267.csv").getAllClicks()

    
      val clusterHist = MulticlassHist(100)
      allClicks.foreach(click => clusterHist.add(click.cluster))
      val clusterSet = calcTopNClusters(clusterHist.getHistogram, n = 5).toArray.toSet

      val filteredTrainClicks = allClicks.filter { c => (clusterSet.contains(c.cluster)) }
    
   
    val dataX = DenseVector(filteredTrainClicks.map(c => c.dist).toArray).toDenseMatrix.t
    val dataY = DenseVector(filteredTrainClicks.map(c => c.cluster.toDouble).toArray)

    val covFunc = TestCovFunc()
    val covFuncParams = DenseVector[Double](-0.794353361706918, -11.251867145609713, -1.1522147378772258, 0.28935151100974615)
    val noiseLogStdDev = log(-3.0328025222890753)
    val model = RankGprModel(dataX, dataY, TestCovFunc(), covFuncParams, noiseLogStdDev)

    val trainedModel = rankGprTrain(model)
    println("learned covFuncParams=" + trainedModel.covFuncParams)
    println("learned noiseLogStdDev=" + trainedModel.noiseLogStdDev)
  }

 

}