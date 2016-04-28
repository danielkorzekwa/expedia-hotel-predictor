package expedia.stats

import scala.collection._

import com.typesafe.scalalogging.slf4j.LazyLogging

case class UserDestStatsMap() {
  
   private val clusterStatsByUserMap2: mutable.Map[Int, CatStatsMapNoPrior] = mutable.Map()
  
  def add(userId:Int, destId:Int,cluster:Int,value:Float=1f): Unit = {
    
      clusterStatsByUserMap2.getOrElseUpdate(userId, CatStatsMapNoPrior()).add(destId,cluster,value)
  }
   
    def getMap(): mutable.Map[Int, CatStatsMapNoPrior] = clusterStatsByUserMap2
}