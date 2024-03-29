package expedia

import scala.io.Source
import java.io.File
import org.apache.commons.io.FileUtils
import scala.collection.JavaConversions._
import java.text.SimpleDateFormat
import java.util.TimeZone
import com.typesafe.scalalogging.slf4j.LazyLogging
object FilterTrainDataApp extends LazyLogging{

  val df = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
  df.setTimeZone(TimeZone.getTimeZone("UTC"))
  val testSetStart = df.parse("2014-01-01 00:00:00")

  def main(args: Array[String]): Unit = {

    var all = 0
    var filtered = 0
    val dataFile = "c:/perforce/daniel/ex/orig_data/train.csv"
    Source.fromFile(new File(dataFile)).getLines().foreach { l =>
      val lArray = l.split(",")
      val datetimeString = lArray(0)
      val isBooking = lArray(18)

      if (all==0 || (all > 0 && isBooking.size == 1 && df.parse(datetimeString).getTime < testSetStart.getTime)) {
        FileUtils.writeLines(new File("target/train_sample.csv"), List(l), true)
        filtered += 1
      }
      all += 1

     if (all % 10000 == 0) logger.info(all + ":" + filtered)
 
     // if (all > 500000) System.exit(0)
    }
  }
}