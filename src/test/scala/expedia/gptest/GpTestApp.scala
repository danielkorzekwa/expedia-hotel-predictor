package expedia.gptest

import breeze.linalg._
import java.io.File
import dk.gp.gpr.GprModel
import breeze.linalg._
import breeze.numerics._
import dk.gp.gpr.predict

object GpTestApp {

  def main(args: Array[String]): Unit = {

    val data = csvread(new File("src/test/resources/gptest/gptest.csv"), skipLines = 1)
    
    val dataX = data(::,1 to 1)
    val dataY = data(::,2)
    
    val covFunc = GpTestCovFunc()
    val covFuncParams = DenseVector[Double]()
    val noiseLogStdDev = log(1d)
    val mean = 0.33
    val gprModel = GprModel(dataX,dataY,covFunc,covFuncParams,noiseLogStdDev,mean)
    
    println(covFunc.cov(dataX, dataX, covFuncParams))
    
   val predicted = predict(dataX,gprModel)
   
   println(predicted)
  }
}