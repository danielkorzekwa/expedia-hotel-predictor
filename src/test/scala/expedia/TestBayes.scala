package expedia

import dk.bayes.dsl.variable.Categorical
import dk.bayes.dsl._
import breeze.linalg._
import java.io.File

object TestBayes {

  def main(args: Array[String]): Unit = {

    val priorCPD = Array(0.0077562244, 0.004675486, 0.009273565, 0.008696205, 0.008642935, 0.018043015, 0.0106799295, 0.008826048, 0.008745589, 0.010009902, 0.011718125, 0.008658195, 0.0037238533, 0.010238516, 0.006818186, 0.00828642, 0.016298724, 0.009543519, 0.018868133, 0.0060574347, 0.0072493344, 0.012840386, 0.00791298, 0.0073525435, 0.0017656536, 0.012650336, 0.008493393, 0.0020178503, 0.013938509, 0.009557113, 0.012615101, 0.0071783084, 0.009496076, 0.012513001, 0.0077914596, 0.004612229, 0.0077254283, 0.013312041, 0.0061997636, 0.011301959, 0.0137895215, 0.023709254, 0.019831419, 0.007089249, 0.008866833, 0.002115788, 0.00941201, 0.017320551, 0.029769741, 0.005444561, 0.017158246, 0.010821148, 0.008753358, 0.0035840215, 0.005446226, 0.009473602, 0.0067296815, 0.0059742015, 0.011193201, 0.01513346, 0.0064059044, 0.008414599, 0.011327206, 0.0055341753, 0.013353657, 0.014630177, 0.005484513, 0.0058704377, 0.014495062, 0.009581806, 0.013597253, 0.0054329084, 0.014322769, 0.010445766, 0.0012043844, 0.0036863985, 0.009703326, 0.011253684, 0.0095568355, 0.0042007794, 0.0056135245, 0.006190608, 0.010653017, 0.016219376, 0.0070884167, 0.008003149, 0.005764731, 0.007445487, 0.0017578852, 0.013558411, 0.013371414, 0.040325653, 0.0065659895, 0.007787853, 0.012402302, 0.013362813, 0.009873122, 0.0101827495, 0.011604372, 0.010027936)

    val llCPD = csvread(new File("target/cpd.csv")).toArray
    val llMat = DenseMatrix(llCPD).reshape(100, 100).t

    println(llMat(33, 16))
    // val cpd = cpdText.split(",").map(_.toDouble)
    //   val prior
    val prior = Categorical(priorCPD)
    Categorical(prior, llCPD).setValue(33)
    Categorical(prior, llCPD).setValue(33)
    Categorical(prior, llCPD).setValue(33)
    Categorical(prior, llCPD).setValue(33)
    Categorical(prior, llCPD).setValue(33)
    Categorical(prior, llCPD).setValue(33)
    Categorical(prior, llCPD).setValue(33)
    Categorical(prior, llCPD).setValue(33)

    Categorical(prior, llCPD).setValue(70)
    Categorical(prior, llCPD).setValue(70)
    Categorical(prior, llCPD).setValue(70)
    Categorical(prior, llCPD).setValue(70)
    Categorical(prior, llCPD).setValue(70)
    Categorical(prior, llCPD).setValue(70)
    Categorical(prior, llCPD).setValue(70)

    val predictionVar = Categorical(prior, llCPD)
    println(infer(predictionVar).cpd(70))
  }
}