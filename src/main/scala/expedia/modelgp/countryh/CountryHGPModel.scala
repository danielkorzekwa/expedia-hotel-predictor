package expedia.modelgp.countryh

import breeze.linalg.DenseVector
import breeze.numerics._
import expedia.data.Click
import expedia.data.ExDataSource
import expedia.model.ClusterModel
import expedia.rankgpr.RankGprModel
import dk.gp.cov.CovSEiso
import expedia.rankgpr.RankGprPredict
import expedia.rankgpr.RankGprPredict
import breeze.linalg.DenseMatrix
import expedia.util.getTopNClusters
import java.util.concurrent.atomic.AtomicInteger
import breeze.linalg._
import expedia.rankhgpr.RankHgprModel
import expedia.rankhgpr.rankHgprPredict

case class CountryHGPModel(trainDS: ExDataSource) extends ClusterModel {

  def predict(click: Click): DenseVector[Float] = {
    ???
  }

  override def predict(clicks: Seq[Click]): DenseMatrix[Float] = {

    val trainClicks = trainDS.getAllClicks()

    val x = DenseVector(trainClicks.map(c => c.countryId.toDouble).toArray).toDenseMatrix.t
    val y = DenseVector(trainClicks.map(c => c.cluster.toDouble).toArray)
    def calcU(x: DenseMatrix[Double]): DenseMatrix[Double] = DenseMatrix(-1d)

    val covFuncParams = DenseVector(-1.0780525412946214, -1.0136412651304236)
    val noiseLogStdDev = -1.3174576979091912
    val covFunc = CountryHCovFunc()

    val rankHgprModel = RankHgprModel(x, y, calcU, covFunc, covFuncParams, noiseLogStdDev)

    val xTest = DenseMatrix(clicks.map(c => c.countryId.toDouble).toArray).t

    val predictedClassProbs = rankHgprPredict(xTest, rankHgprModel).map(_.toFloat)

    val predictedClusterProbs = predictedClassProbs(*, ::).map { r =>
      val probsVec = DenseVector.fill(100)(0d)
      rankHgprModel.classes.zipWithIndex.foreach { case (c, i) => probsVec(c.toInt) = r(i) }
      probsVec
    }
    predictedClusterProbs.map(_.toFloat)
  }

  override def predictTop5(clicks: Seq[Click]): DenseMatrix[Double] = {

    val predictedProbs = predict(clicks)
    val top5 = predictedProbs(*, ::).map(r => getTopNClusters(r, 5))
    top5
  }

}

