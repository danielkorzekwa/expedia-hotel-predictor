package expedia.model

import expedia.model.cmu.CmuModel
import expedia.data.Click
import expedia.data.ExDataSource
import expedia.CompoundHyperParams
import scala.collection._
import expedia.CompoundHyperParamsMap

trait ClusterModelBuilderFactory {
  
   def build(trainDatasource: ExDataSource, testClicks: Seq[Click], modelHyperParamsMap: CompoundHyperParamsMap): ClusterModelBuilder
  
}