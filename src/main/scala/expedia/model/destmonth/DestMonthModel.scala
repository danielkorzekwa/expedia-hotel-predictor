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

case class DestMonthModel(rankGprPredict: RankGprPredict) {

  def predictRankedClasses(checkInMonth: Int): DenseVector[Double] = {
    val rankedClasses = rankGprPredict.predict(DenseVector(checkInMonth.toDouble))
    rankedClasses
  }
}

object DestMonthModel {

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
      val clusterSet = calcTopNClusters(clusterHist.getHistogram, n = 7).toArray.toSet

      val filteredTrainClicks = trainClicks.filter { c => (clusterSet.contains(c.cluster)) && c.checkinMonth > -1 }

      val dataX = DenseVector(filteredTrainClicks.map(c => c.checkinMonth.toDouble).toArray).toDenseMatrix.t
      val dataY = DenseVector(filteredTrainClicks.map(c => c.cluster.toDouble).toArray)

      val model = RankGprModel(dataX, dataY)
      val rankGprPredict = RankGprPredict(model)

      destId -> DestMonthModel(rankGprPredict)
    }.toList.toMap

    destMonthModelMap
  }
}