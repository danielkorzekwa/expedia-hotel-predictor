package expedia

import dk.gp.util.loadObject
import dk.gp.util.saveObject
import expedia.data.Click
import expedia.data.ExCSVDataSource

object SerialiseExpediaDataWithKryo {

  def main(args: Array[String]): Unit = {

    val expediaTrainFile = "c:/perforce/daniel/ex/segments/continent_6/train_2013_continent6"
    val trainClicks = ExCSVDataSource(dsName = "testDS", expediaTrainFile + ".csv").getAllClicks().toList
    saveObject(trainClicks, expediaTrainFile + ".kryo")
    
  
   
  }
}