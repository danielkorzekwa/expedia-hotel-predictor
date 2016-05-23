package expedia

import expedia.data.ExDataSource
import dk.gp.util.saveObject
import dk.gp.util.loadObject
import expedia.data.Click

object SerialiseExpediaDataWithKryo {

  def main(args: Array[String]): Unit = {

    val expediaTestFile = "c:/perforce/daniel/ex/segments/continent_3/train_2014_continent3_booked_only.csv"
    val expediaTestFileKryo = "c:/perforce/daniel/ex/segments/continent_3/train_2014_continent3_booked_only.kryo"
    
     val now = System.currentTimeMillis()
    val testClicks = ExDataSource(dsName = "testDS", expediaTestFile).getAllClicks().toList
     println(System.currentTimeMillis()-now)
    
    saveObject(testClicks, expediaTestFileKryo)
    val now2 = System.currentTimeMillis()
    val testClicks2 = loadObject[List[Click]](expediaTestFileKryo)
    println(System.currentTimeMillis()-now2 + ":" + testClicks2.size)
  }
}