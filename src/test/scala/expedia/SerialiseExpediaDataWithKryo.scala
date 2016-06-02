package expedia

import dk.gp.util.loadObject
import dk.gp.util.saveObject
import expedia.data.Click
import expedia.data.ExCSVDataSource

object SerialiseExpediaDataWithKryo {

  def main(args: Array[String]): Unit = {

    val expediaTrainFile = "c:/perforce/daniel/ex/segments/all/train_2013"
    val trainClicks = ExCSVDataSource(dsName = "testDS", expediaTrainFile + ".csv").getAllClicks().toList
    saveObject(trainClicks, expediaTrainFile + ".kryo")
    
  
   
  }
}