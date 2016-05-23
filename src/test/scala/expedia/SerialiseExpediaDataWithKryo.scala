package expedia

import dk.gp.util.loadObject
import dk.gp.util.saveObject
import expedia.data.Click
import expedia.data.ExDataSource
import expedia.data.ExCSVDataSource

object SerialiseExpediaDataWithKryo {

  def main(args: Array[String]): Unit = {

    val expediaTestFile = "c:/perforce/daniel/ex/segments/continent_3/train_2013_continent3.csv"
    val expediaTestFileKryo = "c:/perforce/daniel/ex/segments/continent_3/train_2013_continent3.kryo"

    val now = System.currentTimeMillis()
    val testClicks = ExCSVDataSource(dsName = "testDS", expediaTestFile).getAllClicks().toList
    println("Loading from csv file time=" + (System.currentTimeMillis() - now))

    saveObject(testClicks, expediaTestFileKryo)
    val now2 = System.currentTimeMillis()
    val testClicks2 = loadObject[List[Click]](expediaTestFileKryo)
    println("Loading from kryo file time=" + (System.currentTimeMillis() - now2 + ":" + testClicks2.size))
  }
}