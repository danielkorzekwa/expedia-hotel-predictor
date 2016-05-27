package expedia.stats

import org.junit._
import Assert._
import breeze.linalg.DenseVector
import dk.bayes.math.Beta
import breeze.linalg._
import breeze.numerics._

class DirichletTest {

  @Test def test_getMean = {
 
    
    
    println("old way:")
    println("low precision:" + 5d*(DenseVector(2.0, 3.0, 5.0)/sum(DenseVector(2.0, 3.0, 5.0))))
     println("low precision:" + 5d*(DenseVector(20.0, 30.0, 50.0)/sum(DenseVector(20.0, 30.0, 50.0))))
     
     println("new way:")
     
      val alphaL = DenseVector(2.0, 3.0, 5.0)
    val mL = Dirichlet.calcMean(alphaL)
    val vL = Dirichlet.calcVariance(alphaL)
    val sL = Dirichlet.calcPrecision(mL,vL + DenseVector.fill(3)(0.0138))
    val alphaLWithNoise =  Dirichlet.calcAlpha(mL, sL)
      println("low precision:" + alphaLWithNoise)
      
        val alphaH = DenseVector(2000.0, 3000.0, 5000.0)
    val mH = Dirichlet.calcMean(alphaH)
    val vH = Dirichlet.calcVariance(alphaH)
    val sH = Dirichlet.calcPrecision(mH,vH + DenseVector.fill(3)(0.0138))
    val alphaHWithNoise =  Dirichlet.calcAlpha(mH, sH)
      println("low precision:" + alphaHWithNoise)
  }
}