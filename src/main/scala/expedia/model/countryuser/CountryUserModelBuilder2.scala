package expedia.model.countryuser

import expedia.data.Click
import expedia.model.ClusterModelBuilderFactory
import expedia.model.ClusterModelBuilder
import expedia.CompoundHyperParams
import expedia.data.ExDataSource
import expedia.CompoundHyperParamsMap

case class CountryUserModelBuilder2() extends ClusterModelBuilder {

  def create(trainDatasource: ExDataSource, testClicks: Seq[Click], hyperParams: CompoundHyperParams): CountryUserModel = {
    ???
  }
}
object CountryUserModelBuilder2 extends ClusterModelBuilderFactory {

  def build(trainDatasource: ExDataSource, testClicks: Seq[Click], modelHyperParamsMap: CompoundHyperParamsMap): CountryUserModelBuilder2 = {
    ???
  }
}