package expedia

import breeze.linalg._
import java.io.File
import com.typesafe.scalalogging.slf4j.LazyLogging
import scala.io.Source
import expedia.similarity.CalcUserLocMarketDistCluster
import java.io.FileOutputStream
import java.io.ObjectOutputStream
import java.text.SimpleDateFormat
import java.util.TimeZone

object ComputeSrcDestDistStats extends LazyLogging {

  val distClusterMap = new CalcUserLocMarketDistCluster()

  def main(args: Array[String]): Unit = {

    val df = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
    df.setTimeZone(TimeZone.getTimeZone("UTC"))

    val testSetStart = df.parse("2014-01-01 00:00:00").getTime
    var i = 0
    val dataFile = "c:/perforce/daniel/ex/orig_data/train.csv"
    //val dataFile = "c:/perforce/daniel/ex/train_small.csv"
    Source.fromFile(new File(dataFile)).getLines().drop(1).foreach { l =>
      val lArray = l.split(",")

      val datetime = df.parse(lArray(0).drop(1).dropRight(1))

      if (datetime.getTime < testSetStart) {
        val userLoc = lArray(5).toDouble
        val dist = if (lArray(6).equals("NA") || lArray(6).isEmpty()) -1d else lArray(6).toDouble
        val market = lArray(22).toDouble
        val cluster = lArray(23).toDouble

        val row = DenseVector(userLoc, dist, market, cluster)
        distClusterMap.add(row)

      }
      i += 1
      if (i % 100000 == 0) println(i)
    }

    val fileOut = new FileOutputStream("target/distClusterMap.obj")
    val objectOut = new ObjectOutputStream(fileOut)
    objectOut.writeObject(distClusterMap.toMap)
  }
}