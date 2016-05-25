package expedia

case class HyperParams(paramsMap: Map[String, Double]) {

  def getParams(): Seq[String] = paramsMap.keys.toList

  def getParamValues(param: String): Seq[Double] = param match {

    case "timeDecay" => List(-0.1, -0.09, -0.08, -0.07, -0.06, -0.05, -0.04, -0.03, -0.02, -0.01)

    case "expedia.model.marketdest.destMarketCountsThreshold1" => List(100, 200, 300, 400, 500, 600)
    case "expedia.model.marketdest.destMarketCountsThresholdClickWeight1" => List(0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
    case "expedia.model.marketdest.destMarketCountsThreshold2" => List(100, 200, 300, 400, 500, 600)
    case "expedia.model.marketdest.destMarketCountsThresholdClickWeight2" => List(0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
    case "expedia.model.marketdest.destMarketCountsDefaultWeight" => List(0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)

    case "expedia.model.marketdest.beta1" => List(1, 2, 4, 8, 16, 32, 64)
    case "expedia.model.marketdest.beta2" => List(1, 2, 4, 8, 16, 32, 64)
    case "expedia.model.marketdest.beta3" => List(1, 2, 4, 8, 16, 32, 64)

    case "expedia.model.marketdestuser.beta1" => List(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95)
    case "expedia.model.marketdestuser.beta2" => List(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95)
    case "expedia.model.marketdestuser.beta3" => List(100, 200, 300, 400, 500, 600)
    case "expedia.model.marketdestuser.beta4" => List(1, 2, 4, 8, 14, 32, 64)
    case "expedia.model.marketdestuser.beta5" => List(1, 2, 4, 8, 14, 32, 64)
    case "expedia.model.marketdestuser.beta6" => List(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95)

    case "expedia.model.mdp.beta1" => List(100, 200, 300, 400, 500, 600)
    case "expedia.model.mdp.beta2" => List(0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
    case "expedia.model.mdp.beta3" => List(100, 200, 300, 400, 500, 600)
    case "expedia.model.mdp.beta4" => List(0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
    case "expedia.model.mdp.beta5" => List(0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
    case "expedia.model.mdp.beta6" => List(1, 2, 4, 8, 16, 32, 64)
    case "expedia.model.mdp.beta7" => List(1, 2, 4, 5, 8, 16, 32, 64)
    case "expedia.model.mdp.beta8" => List(1, 2, 4, 8, 16, 32, 64)

    case "expedia.model.dest.beta1" => List(0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
    case "expedia.model.dest.beta2" => List(1, 2, 4, 8, 16, 32, 64)

    case "expedia.model.mdpu.beta1" => List(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95)
    case "expedia.model.mdpu.beta2" => List(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95)

    case "expedia.model.marketuser.beta1" => List(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95)
    case "expedia.model.marketuser.beta2" => List(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95)
    case "expedia.model.marketuser.beta3" => List(1, 2, 4, 8, 16, 32, 64)

    case "expedia.model.marketmodel.beta1" => List(0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)

    case "expedia.model.countryuser.beta1" => List(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95)
    case "expedia.model.countryuser.beta2" => List(1, 2, 4, 8, 14, 32, 64)

    case "expedia.model.country.beta1" => List(0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
    case "expedia.model.country.beta2" => List(1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024)

    case "expedia.model.clusterdist.beta1" => List(0.5, 0.7, 1, 1.5, 2, 4, 8)
    case "expedia.model.clusterdist.beta2" => List(1, 2, 4, 7, 16, 32, 64)
  }

  def getParamValue(param: String): Double = paramsMap(param)

  def copy(param: String, newValue: Double): HyperParams = {
    val newParamMap = paramsMap + (param -> newValue)
    HyperParams(newParamMap)
  }

}

object HyperParams {
  
  def createBestParams3():HyperParams = {
    HyperParams(Map(
        "expedia.model.marketdest.beta2" -> 1.0, 
        "expedia.model.marketuser.beta2" -> 0.6, 
        "expedia.model.marketdest.destMarketCountsThreshold2" -> 500.0, "timeDecay" -> -0.07, "expedia.model.mdp.beta1" -> 600.0, "expedia.model.marketdestuser.beta1" -> 0.949999988079071, "expedia.model.mdp.beta6" -> 2.0, "expedia.model.countryuser.beta1" -> 0.4, "expedia.model.marketdest.destMarketCountsDefaultWeight" -> 0.05000000074505806, "expedia.model.marketdest.destMarketCountsThresholdClickWeight2" -> 0.05, "expedia.model.mdp.beta7" -> 5.0, "expedia.model.marketdest.destMarketCountsThresholdClickWeight1" -> 0.9, "expedia.model.marketuser.beta1" -> 0.5, "expedia.model.mdp.beta4" -> 0.10000000149011612, "expedia.model.country.beta1" -> 0.05000000074505806, "expedia.model.mdpu.beta2" -> 0.1, "expedia.model.marketdestuser.beta4" -> 4.0, "expedia.model.clusterdist.beta1" -> 1.0, "expedia.model.mdp.beta8" -> 64.0, "expedia.model.mdp.beta3" -> 500.0, "expedia.model.marketdestuser.beta6" -> 0.5, "expedia.model.marketdest.beta1" -> 4.0, "expedia.model.mdpu.beta1" -> 0.1, "expedia.model.dest.beta1" -> 0.05000000074505806, "expedia.model.country.beta2" -> 512.0, "expedia.model.marketdestuser.beta3" -> 400.0, "expedia.model.mdp.beta2" -> 0.9, "expedia.model.marketdest.destMarketCountsThreshold1" -> 300.0, "expedia.model.marketdest.beta3" -> 1.0, "expedia.model.marketuser.beta3" -> 1.0, "expedia.model.dest.beta2" -> 1.0, "expedia.model.marketdestuser.beta2" -> 0.8, "expedia.model.countryuser.beta2" -> 1.0, "expedia.model.mdp.beta5" -> 0.05000000074505806, "expedia.model.marketmodel.beta1" -> 0.05000000074505806, "expedia.model.marketdestuser.beta5" -> 8.0, "expedia.model.clusterdist.beta2" -> 7.0)) 
  }

}