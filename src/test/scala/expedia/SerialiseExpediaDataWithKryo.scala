package expedia

import dk.gp.util.loadObject
import dk.gp.util.saveObject
import expedia.data.Click
import expedia.data.ExCSVDataSource

object SerialiseExpediaDataWithKryo {

  def main(args: Array[String]): Unit = {

    val expediaTrainFile = "c:/perforce/daniel/ex/segments/continent_2/train_2013_continent2"
    val trainClicks = ExCSVDataSource(dsName = "testDS", expediaTrainFile + ".csv").getAllClicks().toList
    saveObject(trainClicks, expediaTrainFile + ".kryo")
    
     val expediaTestFile = "c:/perforce/daniel/ex/segments/continent_2/train_2014_continent2_booked_only"
    val testClicks = ExCSVDataSource(dsName = "testDS", expediaTestFile + ".csv").getAllClicks().toList
    saveObject(testClicks, expediaTestFile + ".kryo")
   
  }
}