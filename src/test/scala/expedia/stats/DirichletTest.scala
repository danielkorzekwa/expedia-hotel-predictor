package expedia.stats

import org.junit._

import breeze.linalg.DenseVector
import breeze.stats._
import breeze.stats.distributions.Dirichlet
import breeze.linalg._

class DirichletTest {

  @Test def test_getMean: Unit = {

    val data = Seq(
      DenseVector(0.6, 0.2, 0.2),
       DenseVector(0.4, 0.5, 0.1),
      DenseVector(0.2, 0.4, 0.4))

    val dataMat = DenseVector.horzcat(data: _*).t

    val meanVec: DenseVector[Double] = DenseVector.tabulate(dataMat.cols) { colIndex =>
      val colVec = dataMat(::, colIndex)
      mean(colVec)
    }

    val varVec: DenseVector[Double] = DenseVector.tabulate(dataMat.cols) { colIndex =>
      val colVec = dataMat(::, colIndex)
      variance(colVec)
    }

    val s = Dirichlet.calcPrecision(meanVec, varVec)
    val a = Dirichlet.calcAlpha(meanVec, s)

    val expFam = new breeze.stats.distributions.Dirichlet.ExpFam(DenseVector.zeros[Double](3))

    val suffStat = data.foldLeft(expFam.emptySufficientStatistic) { (a, x) =>
      a + expFam.sufficientStatisticFor(x)
    }

    val alphaHat = expFam.mle(suffStat)
    println("alphaHat norm:" + alphaHat / sum(alphaHat))
    println("a norm:" + a / sum(a))
    println("meanVec:" + meanVec / sum(meanVec))
    //DenseVector(2.9803000577558274, 2.325871404559782, 5.850530402841005)

  }
}