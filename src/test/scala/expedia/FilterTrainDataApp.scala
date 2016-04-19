package expedia

import scala.io.Source
import java.io.File
import org.apache.commons.io.FileUtils
import scala.collection.JavaConversions._
object FilterTrainDataApp {

  def main(args: Array[String]): Unit = {

    var c = 0
    var a = 0
    val dataFile = "c:/perforce/daniel/expedia/train.csv"
    Source.fromFile(new File(dataFile)).getLines().foreach { l =>
      val isBooking = l.split(",")(18)
      if (isBooking.size != 1 || (isBooking.size == 1 && isBooking.toInt == 1)) {
        FileUtils.writeLines(new File("target/train_booked.csv"), List(l), true)
        c += 1
      }
      a += 1

      println(a + ":" + c)
    }
  }
}