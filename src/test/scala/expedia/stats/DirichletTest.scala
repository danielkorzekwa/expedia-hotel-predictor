package expedia.stats

import org.junit._

import breeze.linalg.DenseVector
import breeze.stats._
import breeze.stats.distributions.Dirichlet

class DirichletTest {

  @Test def test_getMean: Unit = {

    val data = Seq(
      DenseVector(0.9, 0.01, 0.09),
      DenseVector(0.9, 0.09, 0.01),
      DenseVector(0.9, 0.09, 0.01),
      DenseVector(0.1, 0.1, 0.8))

    val dataMat = DenseVector.horzcat(data: _*).t

    val meanVec: DenseVector[Double] = DenseVector.tabulate(dataMat.cols) { colIndex =>
      val colVec = dataMat(::, colIndex)
      mean(colVec)
    }

    val varVec: DenseVector[Double] = DenseVector.tabulate(dataMat.cols) { colIndex =>
      val colVec = dataMat(::, colIndex)
      variance(colVec)
    }
    
    val s = Dirichlet.calcPrecision(meanVec,varVec)
    val a = Dirichlet.calcAlpha(meanVec,s)

    val expFam = new breeze.stats.distributions.Dirichlet.ExpFam(DenseVector.zeros[Double](3))

    val suffStat = data.foldLeft(expFam.emptySufficientStatistic) { (a, x) =>
      a + expFam.sufficientStatisticFor(x)
    }

    val alphaHat = expFam.mle(suffStat)
    println(alphaHat)
    println(a)
    //DenseVector(2.9803000577558274, 2.325871404559782, 5.850530402841005)

  }
}