package expedia.model.singlecatmodel

import breeze.linalg.DenseVector
import expedia.stats.CatStats
import expedia.stats.CatStatsMapNoPrior
import expedia.stats.calcVectorProbsMutable
import expedia.stats.calcVectorMapProbsMutable

case class SingleCatPredictionModelBuilder() {

  private val globalCusterStat = CatStats()
  private val clusterStatByCat = CatStatsMapNoPrior()

  def processCluster(categoryId: Int, cluster: Int) = {
    globalCusterStat.add(cluster)

    clusterStatByCat.add(categoryId, cluster)

  }

  def toSingleCatPredictionModel(): SingleCatPredictionModel = {
    calcVectorProbsMutable(globalCusterStat.getItemVec)

    clusterStatByCat.getMap().foreach { case (categoryId, clusterCounts) => clusterCounts :+= globalCusterStat.getItemVec }

    calcVectorMapProbsMutable(clusterStatByCat.getMap().toMap)

    SingleCatPredictionModel(globalCusterStat.getItemVec(), clusterStatByCat.getMap())

  }
}