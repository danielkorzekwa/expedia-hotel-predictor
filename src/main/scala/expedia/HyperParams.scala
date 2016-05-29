package expedia

case class HyperParams(paramsMap: Map[String, Double]) {

  def getParams(): Seq[String] = paramsMap.keys.toList
  
  def getParamValue(param: String): Double = paramsMap(param)

  def copy(param: String, newValue: Double): HyperParams = {
    val newParamMap = paramsMap + (param -> newValue)
    HyperParams(newParamMap)
  }

}

object HyperParams {

  //  def createBestParams3(): HyperParams = {
  //    HyperParams(Map(
  //      "expedia.model.cmu.beta1" -> 1f,
  //      "expedia.model.cmu.beta2" -> 1f,
  //      "expedia.model.cmu.beta3" -> 0.8f,
  //      "expedia.model.cmu.beta4" -> 0.2f,
  //      "expedia.model.cmu.beta5" -> 0.08f,
  //      "expedia.model.cmu.beta6" -> 0.9f,
  //      "expedia.model.cmu.beta7" -> 1f,
  //
  //      "expedia.model.marketdest.beta2" -> 1.0,
  //      "expedia.model.marketuser.beta2" -> 0.6,
  //      "expedia.model.marketdest.destMarketCountsThreshold2" -> 500.0, "timeDecay" -> -0.07, "expedia.model.mdp.beta1" -> 600.0, "expedia.model.marketdestuser.beta1" -> 0.949999988079071, "expedia.model.mdp.beta6" -> 2.0, "expedia.model.countryuser.beta1" -> 0.4, "expedia.model.marketdest.destMarketCountsDefaultWeight" -> 0.05000000074505806, "expedia.model.marketdest.destMarketCountsThresholdClickWeight2" -> 0.05, "expedia.model.mdp.beta7" -> 5.0, "expedia.model.marketdest.destMarketCountsThresholdClickWeight1" -> 0.9, "expedia.model.marketuser.beta1" -> 0.5, "expedia.model.mdp.beta4" -> 0.10000000149011612, "expedia.model.country.beta1" -> 0.05000000074505806, "expedia.model.mdpu.beta2" -> 0.1, "expedia.model.marketdestuser.beta4" -> 4.0, "expedia.model.clusterdist.beta1" -> 1.0, "expedia.model.mdp.beta8" -> 64.0, "expedia.model.mdp.beta3" -> 500.0, "expedia.model.marketdestuser.beta6" -> 0.5, "expedia.model.marketdest.beta1" -> 4.0, "expedia.model.mdpu.beta1" -> 0.1, "expedia.model.dest.beta1" -> 0.05000000074505806, "expedia.model.country.beta2" -> 512.0, "expedia.model.marketdestuser.beta3" -> 400.0, "expedia.model.mdp.beta2" -> 0.9, "expedia.model.marketdest.destMarketCountsThreshold1" -> 300.0, "expedia.model.marketdest.beta3" -> 1.0, "expedia.model.marketuser.beta3" -> 1.0, "expedia.model.dest.beta2" -> 1.0, "expedia.model.marketdestuser.beta2" -> 0.8, "expedia.model.countryuser.beta2" -> 1.0, "expedia.model.mdp.beta5" -> 0.05000000074505806, "expedia.model.marketmodel.beta1" -> 0.05000000074505806, "expedia.model.marketdestuser.beta5" -> 8.0, "expedia.model.clusterdist.beta2" -> 7.0))
  //  }

  def createParamsCMU1(): HyperParams = {
    HyperParams(Map("expedia.model.marketdest.beta2" -> 1.0,
      "expedia.model.marketuser.beta2" -> 0.6,
      "expedia.model.marketdest.destMarketCountsThreshold2" -> 500.0,
      "expedia.timeDecay" -> -0.07,
      "expedia.model.mdp.beta1" -> 600.0,
      "expedia.model.marketdestuser.beta1" -> 0.949999988079071,
      "expedia.model.mdp.beta6" -> 2.0,
      "expedia.model.cmu.beta6" -> 0.7,
      "expedia.model.countryuser.beta1" -> 0.4,
      "expedia.model.marketdest.destMarketCountsDefaultWeight" -> 0.05000000074505806,
      "expedia.model.marketdest.destMarketCountsThresholdClickWeight2" -> 0.05,
      "expedia.model.mdp.beta7" -> 5.0,
      "expedia.model.marketdest.destMarketCountsThresholdClickWeight1" -> 0.9,
      "expedia.model.marketuser.beta1" -> 0.5, "expedia.model.cmu.beta3" -> 0.8,
      "expedia.model.mdp.beta4" -> 0.10000000149011612,
      "expedia.model.country.beta1" -> 0.05000000074505806,
      "expedia.model.mdpu.beta2" -> 0.1,
      "expedia.model.marketdestuser.beta4" -> 4.0,
      "expedia.model.clusterdist.beta1" -> 1.0,
      "expedia.model.cmu.beta4" -> 0.20000000298023224,
      "expedia.model.mdp.beta8" -> 64.0,
      "expedia.model.mdp.beta3" -> 500.0,
      "expedia.model.marketdestuser.beta6" -> 0.5,
      "expedia.model.cmu.beta7" -> 0.6,
      "expedia.model.marketdest.beta1" -> 4.0,
      "expedia.model.mdpu.beta1" -> 0.1,
      "expedia.model.dest.beta1" -> 0.05000000074505806,
      "expedia.model.cmu.beta2" -> 1.0,
      "expedia.model.country.beta2" -> 512.0,
      "expedia.model.marketdestuser.beta3" -> 400.0,
      "expedia.model.mdp.beta2" -> 0.9,
      "expedia.model.marketdest.destMarketCountsThreshold1" -> 300.0,
      "expedia.model.marketdest.beta3" -> 1.0,
      "expedia.model.marketuser.beta3" -> 1.0,
      "expedia.model.dest.beta2" -> 1.0,
      "expedia.model.cmu.beta5" -> 0.07999999821186066,
      "expedia.model.marketdestuser.beta2" -> 0.8,
      "expedia.model.countryuser.beta2" -> 1.0,
      "expedia.model.mdp.beta5" -> 0.05000000074505806,
      "expedia.model.marketmodel.beta1" -> 0.05000000074505806,
      "expedia.model.marketdestuser.beta5" -> 8.0,
      "expedia.model.cmu.beta1" -> 1.0,
      "expedia.model.clusterdist.beta2" -> 7.0,
      "expedia.model.cmu.beta8" -> 0.05))

  }
  
  
  def createParamsCMU2():HyperParams = {
HyperParams(Map(
    "expedia.model.destcluster.beta1" -> 0.05,
    "expedia.model.destcluster.beta2" -> 1,
    "expedia.model.marketdest.beta2" -> 1.0, "expedia.model.marketuser.beta2" -> 0.6, "expedia.model.marketdest.destMarketCountsThreshold2" -> 245.0, "expedia.model.mdp.beta1" -> 323.4, "expedia.model.marketdestuser.beta1" -> 0.949999988079071, "expedia.model.mdp.beta6" -> 2.0, "expedia.model.cmu.beta6" -> 0.84, "expedia.model.countryuser.beta1" -> 0.39599999999999996, "expedia.model.marketdest.destMarketCountsDefaultWeight" -> 0.05000000074505806, "expedia.model.marketdest.destMarketCountsThresholdClickWeight2" -> 0.01715, "expedia.timeDecay" -> -0.07, "expedia.model.mdp.beta7" -> 5.0, "expedia.model.marketdest.destMarketCountsThresholdClickWeight1" -> 1.053, "expedia.model.marketuser.beta1" -> 0.55, "expedia.model.cmu.beta3" -> 0.8, "expedia.model.mdp.beta4" -> 0.10560000157356263, "expedia.model.country.beta1" -> 0.04095000061020254, "expedia.model.mdpu.beta2" -> 0.1, "expedia.model.marketdestuser.beta4" -> 4.0, "expedia.model.clusterdist.beta1" -> 1.0, "expedia.model.cmu.beta4" -> 0.20000000298023224, "expedia.model.mdp.beta8" -> 70.4, "expedia.model.mdp.beta3" -> 715.0, "expedia.model.cmu.beta8" -> 0.035, "expedia.model.marketdestuser.beta6" -> 0.45, "expedia.model.cmu.beta7" -> 0.6, "expedia.model.marketdest.beta1" -> 4.0, "expedia.model.mdpu.beta1" -> 0.1, "expedia.model.dest.beta1" -> 0.05000000074505806, "expedia.model.cmu.beta2" -> 1.0, "expedia.model.country.beta2" -> 414.72, "expedia.model.marketdestuser.beta3" -> 400.0, "expedia.model.mdp.beta2" -> 0.7290000000000001, "expedia.model.marketdest.destMarketCountsThreshold1" -> 132.3, "expedia.model.marketdest.beta3" -> 1.2, "expedia.model.marketuser.beta3" -> 1.0, "expedia.model.dest.beta2" -> 1.0, "expedia.model.cmu.beta5" -> 0.07999999821186066, "expedia.model.marketdestuser.beta2" -> 0.8, "expedia.model.countryuser.beta2" -> 1.4300000000000002, "expedia.model.mdp.beta5" -> 0.03500000052154064, "expedia.model.marketmodel.beta1" -> 0.03500000052154064, "expedia.model.marketdestuser.beta5" -> 8.0, "expedia.model.cmu.beta1" -> 1.0, "expedia.model.clusterdist.beta2" -> 7.0)) 

  }

}