package expedia.model.svm.libsvm

import breeze.linalg.DenseMatrix
import libsvm.svm_parameter
import libsvm.svm
import breeze.linalg.DenseVector
import libsvm.svm_problem
import breeze.linalg._

object libSvmTrain {

  def apply(x: DenseMatrix[Double], y: DenseVector[Double]): LibSvmModel = {

    val problem = new svm_problem()
    problem.l = x.rows
    problem.x = x(*, ::).map(xVal => toSvmNodes(xVal)).toArray
    problem.y = y.toArray
    

    val param = getSvnParameter(problem.x(0).size)

    val model = svm.svm_train(problem, param)
    LibSvmModel(model)
  }

  private def getSvnParameter(featuresNum: Int): svm_parameter = {
    val param = new svm_parameter();

    param.svm_type = svm_parameter.C_SVC

    // default values
    param.kernel_type = svm_parameter.RBF;
    param.degree = 3;
    param.gamma = 1d / featuresNum
    param.coef0 = 0;
    param.nu = 0.5;
    param.cache_size = 100;
    param.C = 1;
    param.eps = 1e-3;
    param.p = 0.1;
    param.shrinking = 1;
    param.probability = 1;
    param.nr_weight = 0;
    param.weight_label = Array(0);
    param.weight = Array(0);

    param
  }
}