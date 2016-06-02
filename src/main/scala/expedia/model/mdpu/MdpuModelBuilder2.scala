package expedia.model.mdpu

import expedia.CompoundHyperParams
import expedia.CompoundHyperParamsMap
import expedia.data.Click
import expedia.data.ExDataSource
import expedia.model.ClusterModelBuilder
import expedia.model.ClusterModelBuilderFactory

case class MdpuModelBuilder2() extends ClusterModelBuilder {

  def create(trainDatasource: ExDataSource, testClicks: Seq[Click], hyperParams: CompoundHyperParams): MdpuModel = {
    ???
  }
}
object CountryModelBuilder2 extends ClusterModelBuilderFactory {

  def build(trainDatasource: ExDataSource, testClicks: Seq[Click], modelHyperParamsMap: CompoundHyperParamsMap): MdpuModelBuilder2 = {
    ???
  }
}