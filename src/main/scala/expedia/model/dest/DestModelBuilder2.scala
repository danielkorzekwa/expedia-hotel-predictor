package expedia.model.dest

import expedia.CompoundHyperParams
import expedia.data.Click
import expedia.data.ExDataSource
import expedia.model.ClusterModelBuilder
import expedia.model.ClusterModelBuilderFactory
import expedia.CompoundHyperParamsMap

case class DestModelBuilder2() extends ClusterModelBuilder {

  def create(trainDatasource: ExDataSource, testClicks: Seq[Click], hyperParams: CompoundHyperParams):DestModel = {
    ???
  }
}
object DestModelBuilder2 extends ClusterModelBuilderFactory {

  def build(trainDatasource: ExDataSource, testClicks: Seq[Click], modelHyperParamsMap: CompoundHyperParamsMap): DestModelBuilder2 = {
    ???
  }
}