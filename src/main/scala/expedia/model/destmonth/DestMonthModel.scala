package expedia.model.destmonth

import breeze.linalg.DenseVector
import expedia.data.Click
import expedia.rankgpr.RankGprPredict
import expedia.rankgpr.RankGprModel
import expedia.rankgpr.RankGprPredict
import breeze.linalg._
import java.io.File
import expedia.data.ExDataSource
import expedia.stats.MulticlassHist
import expedia.util.calcTopNClusters
import com.typesafe.scalalogging.slf4j.LazyLogging
import breeze.numerics._
import dk.gp.cov.CovSEiso
import expedia.rankgpr.rankGprTrain
import java.util.Date
import breeze.stats._

case class DestMonthModel(rankGprPredict: RankGprPredict) {

  val DAY = (1000L * 3600 * 24).toDouble

  def predictRankedClasses(checkinDate: Date): DenseVector[Double] = {
    val rankedClasses = rankGprPredict.predict(DenseVector(checkinDate.getTime / DAY))
    rankedClasses
  }
}

object DestMonthModel extends LazyLogging {

  val DAY = (1000L * 3600 * 24).toDouble

  /**
   * key - destId
   */
  def build(): Map[Int, DestMonthModel] = {

    val destIds = csvread(new File("c:/perforce/daniel/ex/segments/destmonthdata/destIds.csv"), skipLines = 1).toDenseVector

    val destMonthModelMap = (0 until destIds.size).par.map { r =>
      val destId = destIds(r).toInt

      val trainDS = ExDataSource(dsName = "trainDS", "c:/perforce/daniel/ex/segments/destmonthdata/train_2013_dest%d_booked_only.csv".format(destId))
      val trainClicks = trainDS.getAllClicks()

      val clusterHist = MulticlassHist(100)
      trainClicks.foreach(click => clusterHist.add(click.cluster))
      val clusterSet = calcTopNClusters(clusterHist.getHistogram, n = 5).toArray.toSet

      val filteredTrainClicks = trainClicks.filter { c => (clusterSet.contains(c.cluster)) && c.checkinDate.getTime > 0 }

      val dataX = DenseVector(filteredTrainClicks.map(c => c.checkinDate.getTime / DAY).toArray).toDenseMatrix.t
      val dataY = DenseVector(filteredTrainClicks.map(c => c.cluster.toDouble).toArray)

      val covFunc = CovSEiso()
      val covFuncParams = DenseVector[Double](log(1),log(10000))
      val noiseLogStdDev = log(1d)
      val model = RankGprModel(dataX, dataY, covFunc, covFuncParams, noiseLogStdDev)
      val trainedModel = rankGprTrain(model)
      logger.info("Trained covFuncParams=" + trainedModel.covFuncParams)
      val rankGprPredict = try {
        RankGprPredict(trainedModel)
      } catch {
        case e: Exception => {
          logger.error("Creating rankGprPredict model for destId=%d failed".format(destId))
          throw e
        }
      }

      destId -> DestMonthModel(rankGprPredict)
    }.toList.toMap

    destMonthModelMap
  }
}