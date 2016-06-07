package expedia.modelgp.country

import breeze.linalg.DenseVector
import breeze.numerics._
import expedia.data.Click
import expedia.data.ExDataSource
import expedia.model.ClusterModel
import expedia.rankgpr.RankGprModel
import dk.gp.cov.CovSEiso
import expedia.rankgpr.RankGprPredict
import expedia.rankgpr.RankGprPredict

case class CountryGPModel(rankGprPredict: RankGprPredict) extends ClusterModel {

  def predict(click: Click): DenseVector[Float] = {
    val xTest = DenseVector(click.countryId.toDouble)
    val predictedClassProbs = rankGprPredict.predict(xTest)

    val probsVec = DenseVector.fill(100)(0d)
    rankGprPredict.model.classes.zipWithIndex.foreach { case (c, i) => probsVec(c.toInt) = predictedClassProbs(i) }

    probsVec.map(_.toFloat)
  }
}

object CountryGPModel {

  def apply(trainDS: ExDataSource): CountryGPModel = {

    val trainClicks = trainDS.getAllClicks()

    val x = DenseVector(trainClicks.map(c => c.countryId.toDouble).toArray).toDenseMatrix.t
    val y = DenseVector(trainClicks.map(c => c.cluster.toDouble).toArray)

    //val covFuncParams = DenseVector(log(1),log(0.00001))
    // val noiseLogStdDev = log(1)

    val covFuncParams = DenseVector(-1.0780525412946214, -1.0136412651304236)
    val noiseLogStdDev = -1.3174576979091912
    val covFunc = CountryCovFunc()

    val rankGprModel = RankGprModel(x, y, covFunc, covFuncParams, noiseLogStdDev)
    val rankGprPredict = RankGprPredict(rankGprModel)
    CountryGPModel(rankGprPredict)
  }
}