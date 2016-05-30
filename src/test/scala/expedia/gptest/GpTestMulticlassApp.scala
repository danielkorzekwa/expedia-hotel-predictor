package expedia.gptest

import breeze.linalg._
import java.io.File
import dk.gp.gpr.GprModel
import breeze.linalg._
import breeze.numerics._
import dk.gp.gpr.predict
import dk.gp.cov.CovSEiso
import dk.gp.gpr.gpr
import expedia.data.ExDataSource
import breeze.stats.mean
import dk.gp.gpc.GpcModel
import dk.gp.gpc.gpcTrain
import dk.gp.gpc.gpcPredict
import org.joda.time.LocalTime
import java.text.SimpleDateFormat
import org.joda.time.LocalDate
import expedia.data.ExCSVDataSource
import expedia.rankgpr.RankGprModel
import expedia.rankgpr.RankGprPredict

object GpTestMulticlassApp {

  def main(args: Array[String]): Unit = {

    val allClicks = ExCSVDataSource(dsName = "test", "c:/perforce/daniel/ex/segments/loc_market_dest/train_2013_loc_2096_market_675_dest_8267.csv").getAllClicks()

    val filteredClicks = allClicks //.filter { c => (clusterSet.contains(c.cluster))}

    val dataX = DenseVector(filteredClicks.map(c => c.dist).toArray).toDenseMatrix.t
    val dataY = DenseVector(filteredClicks.map(c => c.cluster.toDouble).toArray)

    val covFunc = CovSEiso()
    val covFuncParams = DenseVector[Double](-0.6503421631104198, 0.36622005354726767)
    val noiseLogStdDev = -0.9264392185311553
    val model = RankGprModel(dataX, dataY, covFunc, covFuncParams, noiseLogStdDev)
    val modelPredict = RankGprPredict(model)

    val testX = DenseVector.rangeD(4015.0, 4025.0, 0.1).toDenseMatrix.t
    val predicted = modelPredict.predict(testX)

    println(DenseMatrix.horzcat(testX, predicted).toString(300, 100))
  }

}