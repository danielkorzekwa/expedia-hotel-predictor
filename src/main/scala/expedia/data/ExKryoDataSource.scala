package expedia.data

import java.io.File
import scala.io.Source
import com.typesafe.scalalogging.slf4j.LazyLogging
import java.text.SimpleDateFormat
import java.util.TimeZone
import org.joda.time.LocalDate
import java.util.Date
import dk.gp.util.loadObject

case class ExKryoDataSource(dsName: String, expediaFile: String, filter: (Click) => Boolean = (click) => true) extends ExDataSource with LazyLogging {

  private val clicks = loadObject[List[Click]](expediaFile)

  def getAllClicks(): Seq[Click] = {

    clicks.filter(c => filter(c)).toList
  }

  def foreach(onClick: (Click) => Unit) = {

    logger.info("Processing %s...".format(dsName))

    var i = 0
    clicks.foreach { click =>

      if (filter(click)) onClick(click)
      i += 1
      if (i % 1000000 == 0) logger.info("Processed expedia rows(%s): %d".format(dsName, i))
    }

  }

 
}