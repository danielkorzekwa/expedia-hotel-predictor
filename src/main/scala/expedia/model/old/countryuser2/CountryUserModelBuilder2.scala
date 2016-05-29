package expedia.model.old.countryuser2

import expedia.data.Click
import expedia.stats.MulticlassHistByKey
import expedia.model.marketmodel.MarketModel
import expedia.model.country.CountryModel
import expedia.HyperParams
import expedia.model.country.CountryModelBuilder
import expedia.data.ExDataSource
import expedia.util.TimeDecayService
import scala.collection._
import breeze.linalg._
import breeze.stats._
import expedia.stats.normaliseMutable

/**
 * Compute country prior from user click proportions instead of using clicks, directly. Didn't work....
 */
case class CountryUserModelBuilder2(testClicks: Seq[Click], hyperParams: HyperParams) {

  private val clusterHistByCountryUser: mutable.Map[Int, MulticlassHistByKey[Int]] = mutable.Map()

  private val beta1 = hyperParams.getParamValue("expedia.model.countryuser.beta1").toFloat
  private val beta2 = hyperParams.getParamValue("expedia.model.countryuser.beta2").toFloat

  def processCluster(click: Click) = {

    val userHist = clusterHistByCountryUser.getOrElseUpdate(click.countryId, MulticlassHistByKey[Int](100))

    if (click.isBooking == 1) userHist.add(click.userId, click.cluster)
    else userHist.add(click.userId, click.cluster, value = beta1)
  }

  def create(countryModel: CountryModel): CountryUserModel2 = {

    val b = beta2
    for (i <- 0 to 2) {
      countryModel.clusterHistByCountry.getMap.foreach {
        case (countryId, countryClusterCounts) =>

          if (countryId == 182) println(countryId + ":" + countryClusterCounts)
          val countryUserHist = clusterHistByCountryUser(countryId)
          val userProportions = countryUserHist.getMap.map {
            case (userId, userClusterCounts) =>
              val proportions = (userClusterCounts + b * countryClusterCounts + 1e-7f) / sum(userClusterCounts + b * countryClusterCounts + 1e-7f)
              proportions.map(x => x.toDouble)
          }.toList

          val userProportionsMat = DenseVector.horzcat(userProportions: _*).t

          val meanVec: DenseVector[Float] = DenseVector.tabulate(userProportionsMat.cols) { colIndex =>
            val colVec = userProportionsMat(::, colIndex)
            mean(colVec).toFloat
          }
          
               val expFam = new breeze.stats.distributions.Dirichlet.ExpFam(DenseVector.zeros[Double](100))
        val suffStat = userProportions.foldLeft(expFam.emptySufficientStatistic) { (a, x) => a + expFam.sufficientStatisticFor(x)}
        val alphaHat = expFam.mle(suffStat).map(x => x.toFloat)
          normaliseMutable(alphaHat)

          countryClusterCounts :+= alphaHat - countryClusterCounts
      }
    }

    countryModel.clusterHistByCountry.getMap.foreach {
      case (countryId, countryClusterCounts) =>

        val countryUserHist = clusterHistByCountryUser(countryId)
        val userProportions = countryUserHist.getMap.foreach {
          case (userId, userClusterCounts) =>

            userClusterCounts :+= beta2 * countryClusterCounts
normaliseMutable(userClusterCounts)
        }
    }

    //      var prior = DenseVector.fill(100)(0f)
    //    (0 until 1).foreach{i =>
    //    val userProportions = clusterHistByCountryUser.getMap.filter { x => sum(x._2) > 0 }.map {
    //      case ((countryId, userId), clusterCounts) =>
    //        val proportions = (clusterCounts + prior + 1e-7f) / sum(clusterCounts + prior + 1e-7f)
    //        proportions.map(x => x.toDouble)
    //    }.toList
    //
    //    val expFam = new breeze.stats.distributions.Dirichlet.ExpFam(DenseVector.zeros[Double](100))
    //    val suffStat = userProportions.foldLeft(expFam.emptySufficientStatistic) { (a, x) => a + expFam.sufficientStatisticFor(x)}
    //    val alphaHat = expFam.mle(suffStat).map(x => x.toFloat)
    //    prior = alphaHat
    //    println("alpha:" + sum(alphaHat) + ":" + alphaHat)
    //    
    //    }

    //
    //    clusterHistByCountryUser.getMap.foreach { case ((countryId, userId), clusterCounts) => clusterCounts :+= beta2 * countryModel.predict(countryId) }
    //    clusterHistByCountryUser.normalise()
    println("countries:" + countryModel.clusterHistByCountry.getMap.size)
    CountryUserModel2(clusterHistByCountryUser, countryModel.clusterHistByCountry)
  }
}

object CountryUserModelBuilder2 {
  def buildFromTrainingSet(trainDatasource: ExDataSource, testClicks: Seq[Click], hyperParams: HyperParams): CountryUserModel2 = {

    val timeDecayService = TimeDecayService(testClicks, hyperParams)

    val countryModelBuilder = CountryModelBuilder(testClicks, hyperParams, timeDecayService)
    val countryUserModelBuilder = CountryUserModelBuilder2(testClicks, hyperParams)
    def onClick(click: Click) = {
     //   countryModelBuilder.processCluster(click)
      countryUserModelBuilder.processCluster(click)
    }
    trainDatasource.foreach { click => onClick(click) }

    val countryModel = countryModelBuilder.create()
    val countryUserModel = countryUserModelBuilder.create(countryModel)
    countryUserModel
  }
}