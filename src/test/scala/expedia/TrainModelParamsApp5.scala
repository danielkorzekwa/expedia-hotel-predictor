package expedia

import scala.util.Random
import com.typesafe.scalalogging.slf4j.LazyLogging
import breeze.linalg.DenseVector
import breeze.stats.mean
import dk.gp.util.averagePrecision
import dk.gp.util.loadObject
import dk.gp.util.saveObject
import expedia.data.Click
import expedia.data.ExDataSource
import expedia.data.ExKryoDataSource
import expedia.model.cmu.CmuModelBuilder2
import expedia.learn.learnModelParams

object TrainModelParamsApp5 extends LazyLogging {

  def main(args: Array[String]): Unit = {
    logger.info("Learning hyper params...")

    val now = System.currentTimeMillis()

    val expediaTrainFileKryo = "c:/perforce/daniel/ex/segments/all/train_2013.kryo"
    val trainDS = ExKryoDataSource(dsName = "trainDS", expediaTrainFileKryo)
    val expediaTestFileKryo = "c:/perforce/daniel/ex/segments/all/train_2014_booked_only.kryo"
    val testClicks = ExKryoDataSource(dsName = "testDS", expediaTestFileKryo).getAllClicks()

    //    val expediaTrainFileKryo = "c:/perforce/daniel/ex/segments/continent_3/train_2013_continent3.kryo"
    //    val trainDS = ExKryoDataSource(dsName = "trainDS", expediaTrainFileKryo)
    //    val expediaTestFileKryo = "c:/perforce/daniel/ex/segments/continent_3/train_2014_continent3_booked_only.kryo"
    //    val testClicks = ExKryoDataSource(dsName = "testDS", expediaTestFileKryo).getAllClicks()

    // val modelsToLearn = List("market")
  //  val modelsToLearn = List("country", "countryuser", "marketuser", "market", "dest", "destcluster", "marketdestcluster", "marketdest", "mdp", "marketdestuser", "mdpu", "cmu")
   val modelsToLearn = List("cmu")
    
    val hyperParamsMapFile = "target/hyperParamsMap_trained.kryo"

    for (i <- 1 to 100) {
      val hyperParams = loadObject[CompoundHyperParamsMap](hyperParamsMapFile)
      val newHyperParamsMap = learnModelParams(trainDS, testClicks, hyperParams, modelsToLearn, hyperParamsMapFile)
      saveObject(newHyperParamsMap, hyperParamsMapFile)
    }

    logger.info("Learning hyper params...done:" + (System.currentTimeMillis() - now) / 1000 + " sec.")
  }

}