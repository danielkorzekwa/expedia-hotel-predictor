package expedia

import scala.io.Source
import java.io.File
import org.apache.commons.io.FileUtils
import scala.collection.JavaConversions._
object FilterTrainDataApp {

  def main(args: Array[String]): Unit = {

    var all = 0
    var booked = 0
    val dataFile = "c:/perforce/daniel/ex/orig_data/train.csv"
    Source.fromFile(new File(dataFile)).getLines().foreach { l =>
      val isBooking = l.split(",")(18)
      if (true ||isBooking.size != 1 || (isBooking.size == 1 && isBooking.toInt == 1)) {
        FileUtils.writeLines(new File("target/train_sample.csv"), List(l), true)
        booked += 1
      }
      all += 1

      println(all + ":" + booked)
      if(all>500000) System.exit(0)
    }
  }
}