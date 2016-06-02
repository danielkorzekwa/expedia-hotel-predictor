package expedia.model.mdp

import expedia.CompoundHyperParams
import expedia.CompoundHyperParamsMap
import expedia.data.Click
import expedia.data.ExDataSource
import expedia.model.ClusterModelBuilder
import expedia.model.ClusterModelBuilderFactory

case class MdpModelBuilder2() extends ClusterModelBuilder {

  def create(trainDatasource: ExDataSource, testClicks: Seq[Click], hyperParams: CompoundHyperParams): MdpModel = {
    ???
  }
}
object CountryModelBuilder2 extends ClusterModelBuilderFactory {

  def build(trainDatasource: ExDataSource, testClicks: Seq[Click], modelHyperParamsMap: CompoundHyperParamsMap): MdpModelBuilder2 = {
    ???
  }
}