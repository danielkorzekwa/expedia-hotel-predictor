package expedia

case class CompoundHyperParamsMap(hyperParamsMap: Map[String, CompoundHyperParams]) {

  def getModel(model: String): CompoundHyperParams = {
    hyperParamsMap.getOrElse(model, getDefaultCompoundHyperParams(model))
  }

  def addModel(model: String, hyperParams: CompoundHyperParams): CompoundHyperParamsMap = {
    CompoundHyperParamsMap(hyperParamsMap + (model -> hyperParams))
  }

  def deleteModelPArams(model: String): CompoundHyperParamsMap = {
    CompoundHyperParamsMap(hyperParamsMap - model)
  }

  def deleteModelParams(paramsIndex: Int): CompoundHyperParamsMap = {
    val newHyperParamsMap = hyperParamsMap.map {
      case (key, hyperParams) =>
        val newhyperParamsList = hyperParams.prioritizedHyperParams.zipWithIndex.filter(_._2 != paramsIndex).map(_._1)

        key -> hyperParams.copy(prioritizedHyperParams = newhyperParamsList)
    }

    this.copy(hyperParamsMap = newHyperParamsMap)
  }

  def addModelParams(continentIdMatcher: Option[Int] = None, countryIdMatcher: Option[Int] = None): CompoundHyperParamsMap = {
    val newHyperParamsMap = hyperParamsMap.map {
      case (model, hyperParams) =>

        val newhyperParamsList = hyperParams.prioritizedHyperParams :+ SimpleHyperParams(getDefaultParams(model), continentIdMatcher, countryIdMatcher)
        model -> hyperParams.copy(prioritizedHyperParams = newhyperParamsList)
    }

    this.copy(hyperParamsMap = newHyperParamsMap)
  }

  private def getDefaultCompoundHyperParams(model: String): CompoundHyperParams = {

    val simpleHyperParams = List(
      SimpleHyperParams(getDefaultParams(model), None, Some(198)),
      SimpleHyperParams(getDefaultParams(model), None, Some(50)),
      SimpleHyperParams(getDefaultParams(model), Some(0), None),
      SimpleHyperParams(getDefaultParams(model), Some(1), None),
      SimpleHyperParams(getDefaultParams(model), Some(3), None),
      SimpleHyperParams(getDefaultParams(model), Some(4), None),
      SimpleHyperParams(getDefaultParams(model), Some(5), None),
      SimpleHyperParams(getDefaultParams(model), Some(6), None),
      SimpleHyperParams(getDefaultParams(model), None, None))

    CompoundHyperParams(simpleHyperParams)
  }

  private def getDefaultParams(model: String): Map[String, Double] = {

    model match {
      case "cmu" => {
        Map(
          "expedia.model.cmu.beta1" -> 1,
          "expedia.model.cmu.beta2" -> 1,
          "expedia.model.cmu.beta3" -> 0.8,
          "expedia.model.cmu.beta4" -> 0.21000000312924386,
          "expedia.model.cmu.beta5" -> 0.07999999821186066,
          "expedia.model.cmu.beta6" -> 0.882,
          "expedia.model.cmu.beta7" -> 0.6,
          "expedia.model.cmu.beta8" -> 0.035)
      }
      case "country" => {
        Map(
          "expedia.model.country.isBookingWeight" -> 1,
          "expedia.model.country.beta1" -> 0.04709250070173292,
          "expedia.model.country.beta2" -> 501.8112,
          "expedia.model.country.decayFactor" -> -0.07)
      }
      case "countryuser" => {
        Map(
          "expedia.model.countryuser.isBookingWeight" -> 1,
          "expedia.model.countryuser.beta1" -> 0.39599999999999996,
          "expedia.model.countryuser.beta2" -> 1.430000000000000,
          "expedia.model.countryuser.decayFactor" -> -0.07)
      }
      case "dest" => {
        Map(
          "expedia.model.dest.isBookingWeight" -> 1,
          "expedia.model.dest.beta1" -> 0.050000000745,
          "expedia.model.dest.beta2" -> 1,
          "expedia.model.dest.decayFactor" -> -0.07)
      }
      case "destcluster" => {
        Map(
          "expedia.model.destcluster.isBookingWeight" -> 1,
          "expedia.model.destcluster.beta1" -> 0.054625,
          "expedia.model.destcluster.beta3" -> 1.155,
          "expedia.model.destcluster.decayFactor" -> -0.07)
      }
      case "marketdestcluster" => {
        Map(
          "expedia.model.marketdestcluster.beta1" -> 0.054625,
          "expedia.model.marketdestcluster.isBookingWeight" -> 1,
          "expedia.model.marketdestcluster.beta2" -> 1,
          "expedia.model.marketdestcluster.decayFactor" -> -0.07)
      }
      case "marketdest" => {
        Map(
          "expedia.model.marketdest.destMarketCountsThreshold1" -> 132.3,
          "expedia.model.marketdest.destMarketCountsThresholdClickWeight1" -> 1.053,
          "expedia.model.marketdest.destMarketCountsThreshold2" -> 245.0,
          "expedia.model.marketdest.destMarketCountsThresholdClickWeight2" -> 0.01547787,
          "expedia.model.marketdest.destMarketCountsDefaultWeight" -> 0.050000000745,
          "expedia.model.marketdest.isBookingWeight" -> 1,
          "expedia.model.marketdest.beta1" -> 2.88999999,
          "expedia.model.marketdest.beta2" -> 1,
          "expedia.model.marketdest.beta3" -> 0.918,
          "expedia.model.marketdest.beta4" -> 0.918,
          "expedia.model.marketdest.segmentSizeWeight" -> 1,
          "expedia.model.marketdest.decayFactor" -> -0.07)
      }
      case "marketdestuser" => {
        Map(
          "expedia.model.marketdestuser.isBookingWeight" -> 1,
          "expedia.model.marketdestuser.beta6" -> 0.45,
          "expedia.model.marketdestuser.beta5" -> 8.0,
          "expedia.model.marketdestuser.decayFactor" -> -0.07)
      }
      case "marketuser" => {
        Map(
          "expedia.model.marketuser.beta1" -> 0.55,
          "expedia.model.marketuser.isBookingWeight" -> 1,
          "expedia.model.marketuser.beta3" -> 1,
          "expedia.model.marketuser.decayFactor" -> -0.07)
      }
      case "mdp" => {
        Map(
          "expedia.model.mdp.isBookingWeight" -> 1,
          "expedia.model.mdp.beta1" -> 323.4,
          "expedia.model.mdp.beta2" -> 0.729000000,
          "expedia.model.mdp.beta3" -> 679.25,
          "expedia.model.mdp.beta4" -> 0.1056000,
          "expedia.model.mdp.beta5" -> 0.0350000,
          "expedia.model.mdp.beta6" -> 2.0,
          "expedia.model.mdp.beta7" -> 5.0,
          "expedia.model.mdp.beta8" -> 73.92,
          "expedia.model.mdp.segmentSizeWeight" -> 1,
          "expedia.model.mdp.decayFactor" -> -0.07)
      }
      case "mdpu" => {
        Map(
          "expedia.model.mdpu.beta1" -> 0.1,
          "expedia.model.mdpu.isBookingWeight" -> 1,
          "expedia.model.mdpu.beta2" -> 0.1,
            "expedia.model.mdpu.beta3" -> 1,
          "expedia.model.mdpu.decayFactor" -> -0.07)
      }
      case "market" => {
        Map(
          "expedia.model.marketmodel.decayFactor" -> -0.07,
          "expedia.model.marketmodel.beta1" -> 0.0350,
          "expedia.model.marketmodel.isBookingWeight" -> 1,
          "expedia.model.marketmodel.beta2" -> 1,
          "expedia.model.marketmodel.segmentSizeWeight" -> 0)
      }
      case "clusterdist" => {
        Map(
          "expedia.model.clusterdist.beta1" -> 1,
          "expedia.model.clusterdist.beta2" -> 7.0)
      }
      case "clusterdistprox" => {
        Map( //none
        )
      }
        case "marketdestuser2" => {
        Map(
            "expedia.model.marketdestuser2.beta1" -> 0.949999988079071,
            "expedia.model.marketdestuser2.beta2" -> 0.8,
            "expedia.model.marketdestuser2.beta3" -> 400.0,
            "expedia.model.marketdestuser2.beta4"-> 4.0,
          "expedia.model.marketdestuser2.isBookingWeight" -> 1,
          "expedia.model.marketdestuser2.beta6" -> 0.45,
          "expedia.model.marketdestuser2.beta5" -> 8.0,
          "expedia.model.marketdestuser2.decayFactor" -> -0.07)
      }
    }
  }

}