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
    "expedia.model.cmumodel.beta1" -> 0.83,
    "expedia.model.destcluster.beta1" -> 0.05,
    "expedia.model.destcluster.beta2" -> 1,
    "expedia.model.destcluster.beta3" -> 1,
    "expedia.model.marketdest.beta2" -> 1.0, "expedia.model.marketuser.beta2" -> 0.6, "expedia.model.marketdest.destMarketCountsThreshold2" -> 245.0, "expedia.model.mdp.beta1" -> 323.4, "expedia.model.marketdestuser.beta1" -> 0.949999988079071, "expedia.model.mdp.beta6" -> 2.0, "expedia.model.cmu.beta6" -> 0.84, "expedia.model.countryuser.beta1" -> 0.39599999999999996, "expedia.model.marketdest.destMarketCountsDefaultWeight" -> 0.05000000074505806, "expedia.model.marketdest.destMarketCountsThresholdClickWeight2" -> 0.01715, "expedia.timeDecay" -> -0.07, "expedia.model.mdp.beta7" -> 5.0, "expedia.model.marketdest.destMarketCountsThresholdClickWeight1" -> 1.053, "expedia.model.marketuser.beta1" -> 0.55, "expedia.model.cmu.beta3" -> 0.8, "expedia.model.mdp.beta4" -> 0.10560000157356263, "expedia.model.country.beta1" -> 0.04095000061020254, "expedia.model.mdpu.beta2" -> 0.1, "expedia.model.marketdestuser.beta4" -> 4.0, "expedia.model.clusterdist.beta1" -> 1.0, "expedia.model.cmu.beta4" -> 0.20000000298023224, "expedia.model.mdp.beta8" -> 70.4, "expedia.model.mdp.beta3" -> 715.0, "expedia.model.cmu.beta8" -> 0.035, "expedia.model.marketdestuser.beta6" -> 0.45, "expedia.model.cmu.beta7" -> 0.6, "expedia.model.marketdest.beta1" -> 4.0, "expedia.model.mdpu.beta1" -> 0.1, "expedia.model.dest.beta1" -> 0.05000000074505806, "expedia.model.cmu.beta2" -> 1.0, "expedia.model.country.beta2" -> 414.72, "expedia.model.marketdestuser.beta3" -> 400.0, "expedia.model.mdp.beta2" -> 0.7290000000000001, "expedia.model.marketdest.destMarketCountsThreshold1" -> 132.3, "expedia.model.marketdest.beta3" -> 1.2, "expedia.model.marketuser.beta3" -> 1.0, "expedia.model.dest.beta2" -> 1.0, "expedia.model.cmu.beta5" -> 0.07999999821186066, "expedia.model.marketdestuser.beta2" -> 0.8, "expedia.model.countryuser.beta2" -> 1.4300000000000002, "expedia.model.mdp.beta5" -> 0.03500000052154064, "expedia.model.marketmodel.beta1" -> 0.03500000052154064, "expedia.model.marketdestuser.beta5" -> 8.0, "expedia.model.cmu.beta1" -> 1.0, "expedia.model.clusterdist.beta2" -> 7.0)) 

  }
  
  def createParamsCMU3():HyperParams = {
    HyperParams(Map(
        "expedia.model.marketdest.beta4" -> 0.918,
        "expedia.model.marketdestcluster.beta1" -> 0.054625,
        "expedia.model.marketdestcluster.beta2" -> 1,
        "expedia.model.marketdest.beta2" -> 1.0, "expedia.model.marketuser.beta2" -> 0.6, "expedia.model.marketdest.destMarketCountsThreshold2" -> 245.0, "expedia.model.mdp.beta1" -> 323.4, "expedia.model.marketdestuser.beta1" -> 0.949999988079071, "expedia.model.mdp.beta6" -> 2.0, "expedia.model.cmu.beta6" -> 0.882, "expedia.model.countryuser.beta1" -> 0.39599999999999996, "expedia.model.marketdest.destMarketCountsDefaultWeight" -> 0.05000000074505806, "expedia.model.marketdest.destMarketCountsThresholdClickWeight2" -> 0.015477874999999999, "expedia.timeDecay" -> -0.07, "expedia.model.mdp.beta7" -> 5.0, "expedia.model.marketdest.destMarketCountsThresholdClickWeight1" -> 1.053, "expedia.model.marketuser.beta1" -> 0.55, "expedia.model.cmu.beta3" -> 0.8, "expedia.model.mdp.beta4" -> 0.10560000157356263, "expedia.model.destcluster.beta3" -> 1.155, "expedia.model.country.beta1" -> 0.047092500701732924, "expedia.model.mdpu.beta2" -> 0.1, "expedia.model.marketdestuser.beta4" -> 4.0, "expedia.model.cmumodel.beta1" -> 0.7885, "expedia.model.clusterdist.beta1" -> 1.0, "expedia.model.cmu.beta4" -> 0.21000000312924386, "expedia.model.mdp.beta8" -> 73.92, "expedia.model.mdp.beta3" -> 679.25, "expedia.model.cmu.beta8" -> 0.035, "expedia.model.marketdestuser.beta6" -> 0.45, "expedia.model.cmu.beta7" -> 0.6, "expedia.model.marketdest.beta1" -> 2.8899999999999997, "expedia.model.mdpu.beta1" -> 0.1, "expedia.model.dest.beta1" -> 0.05000000074505806, "expedia.model.cmu.beta2" -> 1.0, "expedia.model.country.beta2" -> 501.8112, "expedia.model.marketdestuser.beta3" -> 400.0, "expedia.model.mdp.beta2" -> 0.7290000000000001, "expedia.model.marketdest.destMarketCountsThreshold1" -> 132.3, "expedia.model.marketdest.beta3" -> 0.918, "expedia.model.marketuser.beta3" -> 1.0, "expedia.model.dest.beta2" -> 1.0, "expedia.model.cmu.beta5" -> 0.07999999821186066, "expedia.model.marketdestuser.beta2" -> 0.8, "expedia.model.countryuser.beta2" -> 1.4300000000000002, "expedia.model.mdp.beta5" -> 0.03500000052154064, "expedia.model.destcluster.beta1" -> 0.054625, "expedia.model.marketmodel.beta1" -> 0.03500000052154064, "expedia.model.marketdestuser.beta5" -> 8.0, "expedia.model.cmu.beta1" -> 1.0, "expedia.model.clusterdist.beta2" -> 7.0)) 

  }
  
  def createParamsCont3():HyperParams = {
    HyperParams(Map("expedia.model.marketdest.beta2" -> 1.0, "expedia.model.marketuser.beta2" -> 0.6, "expedia.model.marketdest.destMarketCountsThreshold2" -> 266.25374999999997, "expedia.model.mdp.beta1" -> 355.73999999999995, "expedia.model.marketdestuser.beta1" -> 0.949999988079071, "expedia.model.mdp.beta6" -> 2.0, "expedia.model.cmu.beta6" -> 0.71442, "expedia.model.countryuser.beta1" -> 0.47817, "expedia.model.marketdest.destMarketCountsDefaultWeight" -> 0.057356250854674724, "expedia.model.marketdestcluster.beta1" -> 0.07224156250000001, "expedia.model.marketdestcluster.beta2" -> 1.0924999999999998, "expedia.model.marketdest.destMarketCountsThresholdClickWeight2" -> 0.011741902921875, "expedia.timeDecay" -> -0.10183250000000002, "expedia.model.mdp.beta7" -> 5.0, "expedia.model.marketdest.destMarketCountsThresholdClickWeight1" -> 1.053, "expedia.model.marketuser.beta1" -> 0.55, "expedia.model.cmu.beta3" -> 0.8, "expedia.model.mdp.beta4" -> 0.10383120154720545, "expedia.model.destcluster.beta3" -> 1.155, "expedia.model.country.beta1" -> 0.04002862559647299, "expedia.model.mdpu.beta2" -> 0.1, "expedia.model.marketdest.beta4" -> 0.9639000000000001, "expedia.model.marketdestuser.beta4" -> 4.0, "expedia.model.cmumodel.beta1" -> 0.7885, "expedia.model.clusterdist.beta1" -> 1.0, "expedia.model.cmu.beta4" -> 0.21000000312924386, "expedia.model.mdp.beta8" -> 85.00800000000001, "expedia.model.mdp.beta3" -> 747.175, "expedia.model.cmu.beta8" -> 0.035, "expedia.model.marketdestuser.beta6" -> 0.405, "expedia.model.cmu.beta7" -> 0.51, "expedia.model.marketdest.beta1" -> 3.4896749999999996, "expedia.model.mdpu.beta1" -> 0.1, "expedia.model.dest.beta1" -> 0.05000000074505806, "expedia.model.cmu.beta2" -> 0.85, "expedia.model.country.beta2" -> 577.0828799999999, "expedia.model.marketdestuser.beta3" -> 400.0, "expedia.model.mdp.beta2" -> 0.5298007500000002, "expedia.model.marketdest.destMarketCountsThreshold1" -> 112.45500000000001, "expedia.model.marketdest.beta3" -> 0.95931, "expedia.model.marketuser.beta3" -> 0.9, "expedia.model.dest.beta2" -> 0.85, "expedia.model.cmu.beta5" -> 0.10579999763518572, "expedia.model.marketdestuser.beta2" -> 0.8, "expedia.model.countryuser.beta2" -> 1.4800500000000003, "expedia.model.mdp.beta5" -> 0.03675000054761767, "expedia.model.destcluster.beta1" -> 0.054625, "expedia.model.marketmodel.beta1" -> 0.034212500509805975, "expedia.model.marketdestuser.beta5" -> 10.579999999999998, "expedia.model.cmu.beta1" -> 1.15, "expedia.model.clusterdist.beta2" -> 7.0)) 

  }
  
  def createParamsCont6():HyperParams = {
    HyperParams(Map("expedia.model.marketdest.beta2" -> 1.0, "expedia.model.marketuser.beta2" -> 0.6, "expedia.model.marketdest.destMarketCountsThreshold2" -> 220.5, "expedia.model.mdp.beta1" -> 261.14549999999997, "expedia.model.marketdestuser.beta1" -> 0.949999988079071, "expedia.model.mdp.beta6" -> 1.7, "expedia.model.cmu.beta6" -> 0.7497, "expedia.model.countryuser.beta1" -> 0.30294, "expedia.model.marketdest.destMarketCountsDefaultWeight" -> 0.04488750066887587, "expedia.model.marketdestcluster.beta1" -> 0.06281875, "expedia.model.marketdestcluster.beta2" -> 0.8075, "expedia.model.marketdest.destMarketCountsThresholdClickWeight2" -> 0.0138140034375, "expedia.timeDecay" -> -0.07350000000000001, "expedia.model.mdp.beta7" -> 5.0, "expedia.model.marketdest.destMarketCountsThresholdClickWeight1" -> 1.6850369250000001, "expedia.model.marketuser.beta1" -> 0.606375, "expedia.model.cmu.beta3" -> 0.8, "expedia.model.mdp.beta4" -> 0.10560000157356263, "expedia.model.destcluster.beta3" -> 1.155, "expedia.model.country.beta1" -> 0.05605184896023762, "expedia.model.mdpu.beta2" -> 0.1, "expedia.model.marketdest.beta4" -> 1.2193335000000003, "expedia.model.marketdestuser.beta4" -> 4.0, "expedia.model.cmumodel.beta1" -> 0.7885, "expedia.model.clusterdist.beta1" -> 1.0, "expedia.model.cmu.beta4" -> 0.18900000281631946, "expedia.model.mdp.beta8" -> 77.616, "expedia.model.mdp.beta3" -> 679.25, "expedia.model.cmu.beta8" -> 0.035, "expedia.model.marketdestuser.beta6" -> 0.3825, "expedia.model.cmu.beta7" -> 0.6599999999999999, "expedia.model.marketdest.beta1" -> 1.9836237499999996, "expedia.model.mdpu.beta1" -> 0.1, "expedia.model.dest.beta1" -> 0.05000000074505806, "expedia.model.cmu.beta2" -> 1.0, "expedia.model.country.beta2" -> 474.21158399999996, "expedia.model.marketdestuser.beta3" -> 400.0, "expedia.model.mdp.beta2" -> 0.5003673750000001, "expedia.model.marketdest.destMarketCountsThreshold1" -> 112.45500000000001, "expedia.model.marketdest.beta3" -> 0.7803, "expedia.model.marketuser.beta3" -> 1.0, "expedia.model.dest.beta2" -> 1.0, "expedia.model.cmu.beta5" -> 0.07199999839067459, "expedia.model.marketdestuser.beta2" -> 0.8, "expedia.model.countryuser.beta2" -> 1.6445000000000003, "expedia.model.mdp.beta5" -> 0.0385000005736947, "expedia.model.destcluster.beta1" -> 0.054625, "expedia.model.marketmodel.beta1" -> 0.03325000049546361, "expedia.model.marketdestuser.beta5" -> 8.0, "expedia.model.cmu.beta1" -> 1.0, "expedia.model.clusterdist.beta2" -> 7.0)) 

  }

}