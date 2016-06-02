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

    //    val expediaTrainFileKryo = "c:/perforce/daniel/ex/segments/all/train_2013.kryo"
    //    val trainDS = ExKryoDataSource(dsName = "trainDS", expediaTrainFileKryo)
    //    val expediaTestFileKryo = "c:/perforce/daniel/ex/segments/all/train_2014_booked_only.kryo"
    //    val testClicks = ExKryoDataSource(dsName = "testDS", expediaTestFileKryo).getAllClicks()

    val expediaTrainFileKryo = "c:/perforce/daniel/ex/segments/continent_3/train_2013_continent3.kryo"
    val trainDS = ExKryoDataSource(dsName = "trainDS", expediaTrainFileKryo)
    val expediaTestFileKryo = "c:/perforce/daniel/ex/segments/continent_3/train_2014_continent3_booked_only.kryo"
    val testClicks = ExKryoDataSource(dsName = "testDS", expediaTestFileKryo).getAllClicks()

    //  val hyperParamsListFromDisk = loadObject[List[SimpleHyperParams]]("c:/perforce/daniel/ex/hyperparams/hyperParams_best_020616_test14.kryo")
    val hyperParams = CompoundHyperParams.createHyperParamsByModel()

    learnModelParams(trainDS, testClicks, hyperParams)

    logger.info("Learning hyper params...done:" + (System.currentTimeMillis() - now) / 1000 + " sec.")
  }

 

 

}