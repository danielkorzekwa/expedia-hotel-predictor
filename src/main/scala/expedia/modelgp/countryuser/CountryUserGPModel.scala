package expedia.modelgp.countryuser

import breeze.linalg.DenseVector
import breeze.numerics._
import expedia.data.Click
import expedia.data.ExDataSource
import expedia.model.ClusterModel
import expedia.rankgpr.RankGprModel
import dk.gp.cov.CovSEiso
import expedia.rankgpr.RankGprPredict
import expedia.rankgpr.RankGprPredict

case class CountryUserGPModel(rankGprPredict:RankGprPredict) extends ClusterModel{
  
    def predict(click:Click): DenseVector[Float] = {
      val xTest = DenseVector(click.userId,click.countryId.toDouble)
      val predictedClassProbs = rankGprPredict.predict(xTest)
     
         val probsVec = DenseVector.fill(100)(0d)
            rankGprPredict.model.classes.zipWithIndex.foreach { case (c, i) => probsVec(c.toInt) = predictedClassProbs(i) }
            
      probsVec.map(_.toFloat)
    }
}

object CountryUserGPModel {
  
  def apply(trainDS:ExDataSource):CountryUserGPModel = {
    
    val trainClicks = trainDS.getAllClicks()
    
     val xSeq = trainClicks.map(c => DenseVector(c.userId,c.countryId.toDouble))
    val x = DenseVector.horzcat(xSeq :_*).t
    val y = DenseVector(trainClicks.map(c => c.cluster.toDouble).toArray)
    
     
    val covFuncParams = DenseVector(-1.39683204464794, -0.9566853310794947, -1.1021606360567275, -1.5870231803891186)
    val noiseLogStdDev =  -2.0696267229544376
    val covFunc = CountryUserCovFunc()
    
   
    
    val rankGprModel = RankGprModel(x,y,covFunc,covFuncParams,noiseLogStdDev)
    val rankGprPredict = RankGprPredict(rankGprModel)
    CountryUserGPModel(rankGprPredict)
  }
}