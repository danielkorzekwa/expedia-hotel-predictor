package expedia.learn

import scala.collection.Seq
import scala.util.Random

import com.typesafe.scalalogging.slf4j.LazyLogging

import breeze.linalg.DenseVector
import breeze.stats.mean
import breeze.stats.mean.reduce_Double
import dk.gp.util.averagePrecision
import expedia.CompoundHyperParams
import expedia.SimpleHyperParams
import expedia.data.Click
import expedia.data.ExDataSource
import expedia.model.ClusterModelBuilder

object trainSimpleModelParams extends LazyLogging {

  def apply(modelBuilder: ClusterModelBuilder, initialHyperParams: SimpleHyperParams, trainDS: ExDataSource, testClicks: Seq[Click]): SimpleHyperParams = {

    val initialMapk = computeMapk(initialHyperParams, trainDS, testClicks, modelBuilder)
    var bestMapk = initialMapk
    var bestHyperParams = initialHyperParams

    val params = initialHyperParams.getParams() //.filter(p => p.startsWith("expedia.model.marketuser.beta3") || p.startsWith("expedia.model.cmu."))
    val rand = new Random()
    for (i <- 1 to 2) {
      Random.shuffle(params).zipWithIndex.foreach {
        case (param, paramIndex) =>

          val bestParamValue = bestHyperParams.getParamValue(param)
          (-3 to 3).filter(x => x != 0).foreach { i =>
            val currParamValue = bestParamValue + i * bestParamValue * 0.05 * (1 + rand.nextInt(10))
            val currHyperParams = bestHyperParams.withParamValue(param, currParamValue)
            val currMapk = computeMapk(currHyperParams, trainDS, testClicks, modelBuilder)

            if (currMapk > bestMapk) {
              logger.info("Best!!!, param=%s %d/%d, curr=%.8f ,best=%.8f, initial=%.8f".format(param, paramIndex + 1, params.size, currMapk, bestMapk, initialMapk))
              bestMapk = currMapk
              bestHyperParams = currHyperParams
            } else logger.info(" param=%s %d/%d, curr=%.8f ,best=%.8f, initial=%.8f".format(param, paramIndex + 1, params.size, currMapk, bestMapk, initialMapk))

          }
      }
    }
    bestHyperParams
  }

  private def computeMapk(hyperParams: SimpleHyperParams, trainDS: ExDataSource, testClicks: Seq[Click], modelBuilder: ClusterModelBuilder): Double = {

    val segmentCompoundHyperParams = CompoundHyperParams(List(hyperParams))

    val top5predictions = modelBuilder.create(trainDS, testClicks, segmentCompoundHyperParams).predictTop5(testClicks)
    val actual = DenseVector(testClicks.map(c => c.cluster.toDouble).toArray)
    val mapk = mean(averagePrecision(top5predictions(::, 5 to 9), actual, k = 5))
    mapk
  }
}