package expedia

import com.typesafe.scalalogging.slf4j.LazyLogging
import breeze.linalg.DenseMatrix
import expedia.data.Click
import expedia.data.ExDataSource
import expedia.model.clusterdist.ClusterDistPredictionModelBuilder2
import expedia.model.cmu.CmuModelBuilder2
import expedia.model.distgp.DistGpModel
import expedia.model.distsvm.DistSvmModel
import expedia.model.clusterdistprox.ClusterDistProxModelBuilder2

object predictClustersCMU extends LazyLogging {

  /**
   * @return Top 5 predictions for four models[clusterDist,marketDest,clusterDistProx]. ClusterDist: [p1,p2,p3,p4,p5,c1,c2,c3,c4,c5]
   */
  def apply(trainDatasource: ExDataSource, testClicks: Seq[Click], modelHyperParamsMap: CompoundHyperParamsMap): Tuple5[DenseMatrix[Double], DenseMatrix[Double], DenseMatrix[Double], DenseMatrix[Double], DenseMatrix[Double]] = {

    val clusterDistModel = ClusterDistPredictionModelBuilder2.build(trainDatasource, testClicks, modelHyperParamsMap).
      create(trainDatasource, testClicks, modelHyperParamsMap.getModel("clusterdist"))

    val clusterDistProxModel = ClusterDistProxModelBuilder2.build(trainDatasource, testClicks, modelHyperParamsMap).
      create(trainDatasource, testClicks, modelHyperParamsMap.getModel("clusterdistprox"))

    val cmuModel = CmuModelBuilder2.build(trainDatasource, testClicks, modelHyperParamsMap).
      create(trainDatasource, testClicks, modelHyperParamsMap.getModel("cmu"))

    /**
     * Cluster dist
     */
    val predictionMatrixClusterDist = clusterDistModel.predictTop5(testClicks)

    /**
     * Cluster dist prox
     */
    val predictionMatrixClusterDistProx = clusterDistProxModel.predictTop5(testClicks)

    /**
     * cmu
     */
    val predictionMatrixMarketDest = cmuModel.predictTop5(testClicks)

    /**
     * Dist svm/gp
     */
    val distSvmMatrix = DistSvmModel().predictTop5(testClicks)
    //   val distGPMatrix = LocMarketDistGpModel.build2().predictTop5(testClicks)
    val distGPMatrix = DistGpModel.build2().predictTop5(testClicks)

    (predictionMatrixClusterDist, predictionMatrixMarketDest, predictionMatrixClusterDistProx, distSvmMatrix, distGPMatrix)
  }
}