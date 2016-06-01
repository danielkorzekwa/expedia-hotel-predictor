package expedia.rankgpr.util

import org.junit._
import Assert._
import breeze.linalg.DenseMatrix
import dk.bayes.math.linear.isIdentical
import breeze.linalg.DenseVector

class calcMultiClassProbsPKPDTest {

  @Test def test_2_classes = {
    val probsMat = DenseMatrix((0.0, 0.7), (0.3, 0.0))
    val probsVec = calcMultiClassProbsPKPD(probsMat)

    assertTrue(isIdentical(DenseVector(0.7, 0.3), probsVec, 0.0001))
  }

  @Test def test_3_classes = {
    val probsMat = DenseMatrix((0.0, 3d / 5, 3.0 / 4), (2.0 / 5, 0.0, 2.0 / 3), (1.0 / 4, 1.0 / 3, 0.0))
    val probsVec = calcMultiClassProbsPKPD(probsMat)
    println(probsVec)
    assertTrue(isIdentical(DenseVector(0.5000, 0.33333, 0.16666), probsVec, 0.0001))
  }
}