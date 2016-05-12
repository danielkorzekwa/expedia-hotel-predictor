package expedia

import java.io.File
import breeze.linalg.csvread
import java.io.FileOutputStream
import java.io.ObjectOutputStream
import expedia.model.svm.libsvm.svc.svcTrain
import expedia.model.svm.libsvm.svc.svcPredict

object TrainSvmModelApp {

  def main(args: Array[String]): Unit = {

    //[cluster,p1,p2,...pn]
    val svmTrainDataA = csvread(new File("c:/perforce/daniel/ex/data_booked/svmData_a.csv"), skipLines = 1) //(0 to 99, ::)
    val svmTrainDataB = csvread(new File("c:/perforce/daniel/ex/data_booked/svmData_b.csv"), skipLines = 1) //(0 to 99, ::)
    val x = svmTrainDataA(::, 1 to svmTrainDataA.cols - 1)
    val y = svmTrainDataA(::, 0)

    val svmModel = svcTrain(x, y)

    val fileOut = new FileOutputStream("target/svm_model.obj")
    val out = new ObjectOutputStream(fileOut).writeObject(svmModel)

    svmModel.save("target/svm_model.libsvm")
    val testX = svmTrainDataB(::, 1 to svmTrainDataB.cols - 1)
    println(svmModel.svm_model.label.toList)
    val predicted = svcPredict(testX, svmModel)
    println(predicted.cols)

    println(predicted.toString(10, 900))

  }
}