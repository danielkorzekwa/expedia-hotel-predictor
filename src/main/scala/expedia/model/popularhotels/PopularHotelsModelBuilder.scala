package expedia.model.popularhotels

import expedia.stats.CatStats
import expedia.stats.calcVectorProbsMutable
import expedia.data.Click

case class PopularHotelsModelBuilder() {

  private val clusterStatMap = CatStats()

  def processCluster(click: Click) = {
    if (click.isBooking == 1) clusterStatMap.add(click.cluster)
  }

  def create(): PopularHotelsModel = {

    calcVectorProbsMutable(clusterStatMap.getItemVec)

    PopularHotelsModel(clusterStatMap.getItemVec)
  }

}