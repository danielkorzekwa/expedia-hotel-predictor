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

  private def getDefaultCompoundHyperParams(model: String): CompoundHyperParams = {

    val simpleHyperParams = List(
      SimpleHyperParams(getDefaultParams(model), None, Some(198)),
      SimpleHyperParams(getDefaultParams(model), Some(3), None),
      SimpleHyperParams(getDefaultParams(model), Some(4), None),
      SimpleHyperParams(getDefaultParams(model), Some(6), None),
      SimpleHyperParams(getDefaultParams(model), None, None))

    CompoundHyperParams(simpleHyperParams)
  }

  private def getDefaultParams(model: String): Map[String, Double] = {

    model match {
      case "country" => {
        Map(
          "expedia.model.country.isBookingWeight" -> 1,
          "expedia.model.country.beta1" -> 1,
          "expedia.model.country.beta2" -> 1,
          "expedia.model.country.decayFactor" -> -0.07)
      }
      case "countryuser" => {
        Map(
          "expedia.model.countryuser.isBookingWeight" -> 1,
          "expedia.model.countryuser.beta1" -> 1,
          "expedia.model.countryuser.beta2" -> 1,
          "expedia.model.countryuser.decayFactor" -> -0.07)
      }
      case "dest" => {
        Map(
          "expedia.model.dest.isBookingWeight" -> 1,
          "expedia.model.dest.beta1" -> 1,
          "expedia.model.dest.beta2" -> 1,
          "expedia.model.dest.decayFactor" -> -0.07)
      }
      case "destcluster" => {
        Map(
          "expedia.model.destcluster.isBookingWeight" -> 1,
          "expedia.model.destcluster.beta1" -> 1,
          "expedia.model.destcluster.beta3" -> 1,
          "expedia.model.destcluster.decayFactor" -> -0.07)
      }
      case "marketdestcluster" => {
        Map(
          "expedia.model.marketdestcluster.beta1" -> 1,
          "expedia.model.marketdestcluster.isBookingWeight" -> 1,
          "expedia.model.marketdestcluster.beta2" -> 1,
          "expedia.model.marketdestcluster.decayFactor" -> -0.07)
      }
      case "marketdest" => {
        Map(
          "expedia.model.marketdest.destMarketCountsThreshold1" -> 1,
          "expedia.model.marketdest.destMarketCountsThresholdClickWeight1" -> 1,
          "expedia.model.marketdest.destMarketCountsThreshold2" -> 1,
          "expedia.model.marketdest.destMarketCountsThresholdClickWeight2" -> 1,
          "expedia.model.marketdest.destMarketCountsDefaultWeight" -> 1,
          "expedia.model.marketdest.isBookingWeight" -> 1,
          "expedia.model.marketdest.beta1" -> 1,
          "expedia.model.marketdest.beta2" -> 1,
          "expedia.model.marketdest.beta3" -> 1,
          "expedia.model.marketdest.beta4" -> 1,
          "expedia.model.marketdest.segmentSizeWeight" -> 1,
          "expedia.model.marketdest.decayFactor" -> -0.07)
      }
      case "marketdestuser" => {
        Map(
          "expedia.model.marketdestuser.isBookingWeight" -> 1,
          "expedia.model.marketdestuser.beta6" -> 1,
          "expedia.model.marketdestuser.beta5" -> 1,
          "expedia.model.marketdestuser.decayFactor" -> -0.07)
      }
      case "marketuser" => {
        Map(
          "expedia.model.marketuser.beta1" -> 1,
          "expedia.model.marketuser.isBookingWeight" -> 1,
          "expedia.model.marketuser.beta3" -> 1,
          "expedia.model.marketuser.decayFactor" -> -0.07)
      }
      case "mdp" => {
        Map(
          "expedia.model.mdp.isBookingWeight" -> 1,
          "expedia.model.mdp.beta1" -> 1,
          "expedia.model.mdp.beta2" -> 1,
          "expedia.model.mdp.beta3" -> 1,
          "expedia.model.mdp.beta4" -> 1,
          "expedia.model.mdp.beta5" -> 1,
          "expedia.model.mdp.beta6" -> 1,
          "expedia.model.mdp.beta7" -> 1,
          "expedia.model.mdp.beta8" -> 1,
          "expedia.model.mdp.segmentSizeWeight" -> 1,
          "expedia.model.mdp.decayFactor" -> -0.07)
      }
      case "mdpu" => {
        Map(
          "expedia.model.mdpu.beta1" -> 1,
          "expedia.model.mdpu.isBookingWeight" -> 1,
          "expedia.model.mdpu.beta2" -> 1,
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
    }
  }

}