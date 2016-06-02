package expedia.model

import expedia.data.Click
import expedia.data.ExDataSource
import expedia.CompoundHyperParams

trait ClusterModelBuilder {

def create(trainDatasource: ExDataSource, testClicks: Seq[Click], hyperParams: CompoundHyperParams): ClusterModel

  
}