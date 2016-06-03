package expedia.model.marketmodel

import expedia.stats.MulticlassHistByKey
import breeze.linalg.DenseVector
import expedia.data.Click
import breeze.linalg.DenseMatrix
import expedia.util.getTop5Clusters
import java.util.concurrent.atomic.AtomicInteger
import com.typesafe.scalalogging.slf4j.LazyLogging
import expedia.model.ClusterModel
import expedia.model.country.CountryModel

case class MarketModel(clusterHistByMarket: MulticlassHistByKey[Int],countryModel:CountryModel) extends ClusterModel with LazyLogging{

  def predict(marketId:Int): DenseVector[Float] = {
    clusterHistByMarket.getMap(marketId)
  }
  
  def predict(click: Click): DenseVector[Float] =predict(click.marketId)
  
 
}