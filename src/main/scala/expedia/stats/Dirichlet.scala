package expedia.stats

import breeze.linalg.DenseVector
import breeze.linalg._
import breeze.numerics._

/**
 * http://www.msr-waypoint.com/en-us/um/people/minka/papers/dirichlet/minka-dirichlet.pdf
 *
 * Estimating a Dirichlet distribution
 * Thomas P. Minka
 * 2000 (revised 2003, 2009, 2012)
 */
object Dirichlet {

  def calcMean(alpha: DenseVector[Double]): DenseVector[Double] = {
    val s = sum(alpha)
    alpha / s
  }

  def calcVariance(alpha: DenseVector[Double]): DenseVector[Double] = {
    val m = calcMean(alpha)
    val s = sum(alpha)
    (m :* (1d - m)) / (1 + s) //or  (alpha:*(s-alpha))/((s*s)*(s+1))
  }

  def calcPrecision(m: DenseVector[Double], v: DenseVector[Double]): Double = {

    val s = (1d / (m.size - 1)) * (0 until m.size - 1).map(i => log((m(i) * (1 - m(i))) / v(i) - 1)).sum
    exp(s)
  }
  
  def calcAlpha(m:DenseVector[Double],s:Double):DenseVector[Double] = {
    s*m
  }

}