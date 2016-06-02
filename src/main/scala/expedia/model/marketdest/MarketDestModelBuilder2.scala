package expedia.model.marketdest

import expedia.CompoundHyperParams
import expedia.CompoundHyperParamsMap
import expedia.data.Click
import expedia.data.ExDataSource
import expedia.model.ClusterModelBuilder
import expedia.model.ClusterModelBuilderFactory

case class MarketDestModelBuilder2() extends ClusterModelBuilder {

  def create(trainDatasource: ExDataSource, testClicks: Seq[Click], hyperParams: CompoundHyperParams):MarketDestModel = {
    ???
  }
}
object MarketDestModelBuilder2 extends ClusterModelBuilderFactory {

  def build(trainDatasource: ExDataSource, testClicks: Seq[Click], modelHyperParamsMap: CompoundHyperParamsMap): MarketDestModelBuilder2 = {
    ???
  }
}