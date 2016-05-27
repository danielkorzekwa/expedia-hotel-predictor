package expedia.model.usermodel

import expedia.HyperParams
import expedia.data.Click
import expedia.data.ExDataSource
import expedia.stats.MulticlassHistByKey
import breeze.numerics._

case class UserModelBuilder(testClicks: Seq[Click], hyperParams: HyperParams) {
  
  
 
  private val clusterHistByUser = MulticlassHistByKey[Int](100)
  testClicks.foreach(click => clusterHistByUser.add( click.userId, click.cluster, value = 0))

  

  def processCluster(click: Click) = {

    if (clusterHistByUser.getMap.contains(click.userId)) {
      if (click.isBooking == 1) clusterHistByUser.add(click.userId, click.cluster)
      else {
         val cnt = click.cnt
          val cntWeight = (1-exp(-4f*cnt)).toFloat
        clusterHistByUser.add(click.userId, click.cluster, value = cntWeight)
      }
      
//       val cnt = click.cnt
//        val cntWeight = (1-exp(-4f*cnt)).toFloat
//      if (click.isBooking == 0) clusterHistByUser.add(click.userId, click.cluster,value=cntWeight)
    }
  }

  def create():  UserModel = {

   
    clusterHistByUser.normalise()

     UserModel(clusterHistByUser)
  }
  
}

object UserModelBuilder {
  
   def buildFromTrainingSet(trainDatasource: ExDataSource, testClicks: Seq[Click],hyperParams:HyperParams): UserModel = {
    
    val userModelBuilder = UserModelBuilder(testClicks,hyperParams)

    def onClick(click: Click) = {
      userModelBuilder.processCluster(click)
    }
    trainDatasource.foreach { click => onClick(click) }

    val userModel = userModelBuilder.create()

    userModel
  }
  
}