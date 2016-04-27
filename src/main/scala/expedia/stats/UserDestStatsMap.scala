package expedia.stats

import scala.collection._

import com.typesafe.scalalogging.slf4j.LazyLogging

case class UserDestStatsMap() {
  
   private val clusterStatsByUserMap2: mutable.Map[Double, CatStatsMapNoPrior] = mutable.Map()
  
  def add(userId:Double, destId:Double,cluster:Double): Unit = {
    
      clusterStatsByUserMap2.getOrElseUpdate(userId, CatStatsMapNoPrior()).add(destId,cluster)
  }
   
    def toMap(): immutable.Map[Double, CatStatsMapNoPrior] = clusterStatsByUserMap2.toMap
}