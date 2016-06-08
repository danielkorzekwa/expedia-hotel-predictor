package expedia.modelgp.countryuser

import expedia.rankgpr.rankGprTrain
import expedia.data.ExCSVDataSource
import expedia.rankgpr.RankGprModel
import breeze.linalg.DenseVector
import dk.gp.cov.CovSEiso
import breeze.numerics._

object CountryUserGPModelTrain {

  def main(args: Array[String]): Unit = {

    val expediaTrainGP13 = "c:/perforce/daniel/ex/segments/cont_3_14smallest_countries/train_2013_booked_only.csv"
    val trainDS = ExCSVDataSource(dsName = "trainDS", expediaTrainGP13)

    val trainClicks = trainDS.getAllClicks()

    val xSeq = trainClicks.map(c => DenseVector(c.userId,c.countryId.toDouble))
    val x = DenseVector.horzcat(xSeq :_*).t
    val y = DenseVector(trainClicks.map(c => c.cluster.toDouble).toArray)

    val covFuncParams = DenseVector(log(1), log(1),log(1), log(1))
    val noiseLogStdDev = log(1)
    
    val covFunc = CountryUserCovFunc()

    val rankGprModel = RankGprModel(x, y, covFunc, covFuncParams, noiseLogStdDev)
    val trainedModel = rankGprTrain(rankGprModel, tolerance = 1e-3)

    println("newCovParams:" + trainedModel.covFuncParams)
    println("trained noiseLogStdDev:" + trainedModel.noiseLogStdDev)

  }
}