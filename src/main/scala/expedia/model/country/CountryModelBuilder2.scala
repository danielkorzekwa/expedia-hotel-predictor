package expedia.model.country

import expedia.model.ClusterModelBuilder
import expedia.data.Click
import expedia.model.ClusterModelBuilderFactory
import expedia.CompoundHyperParams
import expedia.data.ExDataSource
import expedia.CompoundHyperParamsMap

case class CountryModelBuilder2() extends ClusterModelBuilder {

  def create(trainDatasource: ExDataSource, testClicks: Seq[Click], hyperParams: CompoundHyperParams): CountryModel = {
    ???
  }
}
object CountryModelBuilder2 extends ClusterModelBuilderFactory {

  def build(trainDatasource: ExDataSource, testClicks: Seq[Click], modelHyperParamsMap: CompoundHyperParamsMap): CountryModelBuilder2 = {
    ???
  }
}