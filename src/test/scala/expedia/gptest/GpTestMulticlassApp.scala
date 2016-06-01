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
import expedia.model.distgp.DistGPCovFunc

object GpTestMulticlassApp {

  def main(args: Array[String]): Unit = {

    val allClicks = ExCSVDataSource(dsName = "test", "c:/perforce/daniel/ex/segments/loc_market_dest/more_than_1000/train_2013_loc_2096_market_675_dest_8267.csv").getAllClicks()
    val clusterSet = Set(56, 41)
    val filteredClicks = allClicks // .filter { c => (clusterSet.contains(c.cluster))}

    val dataX = DenseVector(filteredClicks.map(c => c.dist).toArray).toDenseMatrix.t
    val dataY = DenseVector(filteredClicks.map(c => c.cluster.toDouble).toArray)

    val covFunc = DistGPCovFunc()
    val covFuncParams = DenseVector[Double](-0.794353361706918, -11.251867145609713, -1.1522147378772258, 0.28935151100974615)
    val noiseLogStdDev = -3.0328025222890753
    val model = RankGprModel(dataX, dataY, covFunc, covFuncParams, noiseLogStdDev)
    val modelPredict = RankGprPredict(model)

    val testX = DenseVector.rangeD(4015.0, 4025.0, 0.1).toDenseMatrix.t
    val predicted = modelPredict.predict(testX)// (::,List(model.classes.toArray.indexOf(41),model.classes.toArray.indexOf(56)))
    println(model.classes)
    println(DenseMatrix.horzcat(testX, predicted).toString(300, 100))
  }

}