package expedia.model.countryuser

import expedia.CompoundHyperParams
import expedia.CompoundHyperParamsMap
import expedia.HyperParamsService
import expedia.data.Click
import expedia.data.ExDataSource
import expedia.model.ClusterModelBuilder
import expedia.model.ClusterModelBuilderFactory
import expedia.stats.MulticlassHistByKey
import expedia.util.TimeDecayService
import expedia.model.country.CountryModel
import expedia.model.country.CountryModelBuilder2

case class CountryUserModelBuilder2(countryModel: CountryModel, timeDecayService: TimeDecayService, hyperParamsService: HyperParamsService) extends ClusterModelBuilder {

  def create(trainDatasource: ExDataSource, testClicks: Seq[Click], hyperParams: CompoundHyperParams): CountryUserModel = {

    //key ((countryId,userId)
    val clusterHistByCountryUser = MulticlassHistByKey[Tuple2[Int, Int]](100)
    testClicks.foreach(click => clusterHistByCountryUser.add((click.countryId, click.userId), click.cluster, value = 0))

    /**
     * Process training set
     */
    def onClick(click: Click) = {

      val countryUserKey = (click.countryId, click.userId)
      if (clusterHistByCountryUser.getMap.contains(countryUserKey)) {

        val isBookingWeight = hyperParamsService.getParamValueForCountryId("expedia.model.countryuser.isBookingWeight", click.countryId, hyperParams).toFloat
        val beta1 = hyperParamsService.getParamValueForCountryId("expedia.model.countryuser.beta1", click.countryId, hyperParams).toFloat

        if (click.isBooking == 1) clusterHistByCountryUser.add(countryUserKey, click.cluster, value = isBookingWeight)
        else clusterHistByCountryUser.add(countryUserKey, click.cluster, value = beta1)
      }
    }
    trainDatasource.foreach { click => onClick(click) }

    /**
     * Build model
     */

    clusterHistByCountryUser.getMap.foreach {
      case ((countryId, userId), clusterCounts) =>

        val beta2 = hyperParamsService.getParamValueForCountryId("expedia.model.countryuser.beta2", countryId, hyperParams).toFloat

        clusterCounts :+= beta2 * countryModel.predict(countryId)
    }
    clusterHistByCountryUser.normalise()

    CountryUserModel(clusterHistByCountryUser)
  }
}
object CountryUserModelBuilder2 extends ClusterModelBuilderFactory {

  def build(trainDatasource: ExDataSource, testClicks: Seq[Click], modelHyperParamsMap: CompoundHyperParamsMap): CountryUserModelBuilder2 = {

    val timeDecayService = TimeDecayService(testClicks)
    val hyperParamsService = HyperParamsService(testClicks)

    val countryModel = CountryModelBuilder2.build(trainDatasource, testClicks, modelHyperParamsMap).
      create(trainDatasource, testClicks, modelHyperParamsMap.getModel("country"))

    CountryUserModelBuilder2(countryModel, timeDecayService, hyperParamsService)
  }
}