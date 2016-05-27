package expedia.model.clusterdistbayes

import org.junit._
import Assert._
import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix
import dk.bayes.math.linear.isIdentical
import expedia.model.old.clusterdistbayes.getDataForGP

class getDataForGPTest {

  @Test def test_simple: Unit = {
    val clusters = DenseVector[Double](1, 1, 2)

    val gpData = getDataForGP(clusters,3)

    val expectedDPData = DenseMatrix(
      (0.0, 0.0),
      (1.0, 1.0),
      (2.0, 0.0),
      (0.0, 0.0),
      (1.0, 1.0),
      (2.0, 0.0),
      (0.0, 0.0),
      (1.0, 0.0),
      (2.0, 1.0))

    assertTrue("gpData is incorrect", isIdentical(expectedDPData, gpData, 0.0001))
  }
  
  @Test def test_lots_of_72: Unit = {
    val clusters = DenseVector[Double](72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 16, 16, 72, 72)

    val gpData = getDataForGP(clusters,100)

  println(gpData.toString(1000,1000))

  //  assertTrue("gpData is incorrect", isIdentical(expectedDPData, gpData, 0.0001))
  }
}