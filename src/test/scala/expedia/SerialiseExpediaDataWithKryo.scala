package expedia

import dk.gp.util.loadObject
import dk.gp.util.saveObject
import expedia.data.Click
import expedia.data.ExCSVDataSource

object SerialiseExpediaDataWithKryo {

  def main(args: Array[String]): Unit = {

    val expediaTrainFile = "c:/perforce/daniel/ex/segments/continent2_country198/train_2014_booked_only"
    val trainClicks = ExCSVDataSource(dsName = "testDS", expediaTrainFile + ".csv").getAllClicks().toList
    saveObject(trainClicks, expediaTrainFile + ".kryo")
    
  
   
  }
}