package expedia

case class SimpleHyperParams(paramsMap: Map[String, Double], continentIdMatcher: Option[Int] = None, countryIdMatcher: Option[Int] = None) {

  def getParams(): Seq[String] = paramsMap.keys.toList

  def getParamValue(param: String): Double = paramsMap(param)

  def copy(param: String, newValue: Double): SimpleHyperParams = {
    val newParamMap = paramsMap + (param -> newValue)
    SimpleHyperParams(newParamMap)
  }

  def containsClick(contId: Int, countryId: Int): Boolean = {
    val continentMatched = continentIdMatcher.isEmpty || continentIdMatcher.get == contId
    val countryMatched = countryIdMatcher.isEmpty || countryIdMatcher.get == countryId

    continentMatched && countryMatched
  }
}

object SimpleHyperParams {

  def createParamsCMU3(): SimpleHyperParams = {
    SimpleHyperParams(Map(
      "expedia.model.marketmodel.segmentSizeWeight" -> 0,
      "expedia.model.marketdest.segmentSizeWeight" -> 0,
      "expedia.model.marketdest.segmentSizeWeight" -> 0,

      "expedia.model.country.isBookingWeight" -> 1,
      "expedia.model.countryuser.isBookingWeight" -> 1,
      "expedia.model.dest.isBookingWeight" -> 1,
      "expedia.model.destcluster.isBookingWeight" -> 1,
      "expedia.model.marketdest.isBookingWeight" -> 1,
      "expedia.model.marketdestcluster.isBookingWeight" -> 1,
      "expedia.model.marketdestuser.isBookingWeight" -> 1,
      "expedia.model.marketmodel.isBookingWeight" -> 1,
      "expedia.model.marketuser.isBookingWeight" -> 1,
      "expedia.model.mdpu.isBookingWeight" -> 1,
      "expedia.model.mdp.isBookingWeight" -> 1,

      "expedia.model.marketmodel.beta2" -> 1,
      "expedia.model.marketdest.beta4" -> 0.918,
      "expedia.model.marketdestcluster.beta1" -> 0.054625,
      "expedia.model.marketdestcluster.beta2" -> 1,
      "expedia.model.marketdest.beta2" -> 1.0, "expedia.model.marketuser.beta2" -> 0.6, "expedia.model.marketdest.destMarketCountsThreshold2" -> 245.0, "expedia.model.mdp.beta1" -> 323.4, "expedia.model.marketdestuser.beta1" -> 0.949999988079071, "expedia.model.mdp.beta6" -> 2.0, "expedia.model.cmu.beta6" -> 0.882, "expedia.model.countryuser.beta1" -> 0.39599999999999996, "expedia.model.marketdest.destMarketCountsDefaultWeight" -> 0.05000000074505806, "expedia.model.marketdest.destMarketCountsThresholdClickWeight2" -> 0.015477874999999999, "expedia.timeDecay" -> -0.07, "expedia.model.mdp.beta7" -> 5.0, "expedia.model.marketdest.destMarketCountsThresholdClickWeight1" -> 1.053, "expedia.model.marketuser.beta1" -> 0.55, "expedia.model.cmu.beta3" -> 0.8, "expedia.model.mdp.beta4" -> 0.10560000157356263, "expedia.model.destcluster.beta3" -> 1.155, "expedia.model.country.beta1" -> 0.047092500701732924, "expedia.model.mdpu.beta2" -> 0.1, "expedia.model.marketdestuser.beta4" -> 4.0, "expedia.model.cmumodel.beta1" -> 0.7885, "expedia.model.clusterdist.beta1" -> 1.0, "expedia.model.cmu.beta4" -> 0.21000000312924386, "expedia.model.mdp.beta8" -> 73.92, "expedia.model.mdp.beta3" -> 679.25, "expedia.model.cmu.beta8" -> 0.035, "expedia.model.marketdestuser.beta6" -> 0.45, "expedia.model.cmu.beta7" -> 0.6, "expedia.model.marketdest.beta1" -> 2.8899999999999997, "expedia.model.mdpu.beta1" -> 0.1, "expedia.model.dest.beta1" -> 0.05000000074505806, "expedia.model.cmu.beta2" -> 1.0, "expedia.model.country.beta2" -> 501.8112, "expedia.model.marketdestuser.beta3" -> 400.0, "expedia.model.mdp.beta2" -> 0.7290000000000001, "expedia.model.marketdest.destMarketCountsThreshold1" -> 132.3, "expedia.model.marketdest.beta3" -> 0.918, "expedia.model.marketuser.beta3" -> 1.0, "expedia.model.dest.beta2" -> 1.0, "expedia.model.cmu.beta5" -> 0.07999999821186066, "expedia.model.marketdestuser.beta2" -> 0.8, "expedia.model.countryuser.beta2" -> 1.4300000000000002, "expedia.model.mdp.beta5" -> 0.03500000052154064, "expedia.model.destcluster.beta1" -> 0.054625, "expedia.model.marketmodel.beta1" -> 0.03500000052154064, "expedia.model.marketdestuser.beta5" -> 8.0, "expedia.model.cmu.beta1" -> 1.0, "expedia.model.clusterdist.beta2" -> 7.0))

  }

  def createParamsCont3(): SimpleHyperParams = {

    SimpleHyperParams(Map("expedia.model.marketdest.beta2" -> 1.0, "expedia.model.marketmodel.segmentSizeWeight" -> 1.2075, "expedia.model.marketuser.beta2" -> 0.6, "expedia.model.marketdest.destMarketCountsThreshold2" -> 226.31568749999997, "expedia.model.mdp.beta1" -> 392.20334999999994, "expedia.model.marketdestuser.beta1" -> 0.949999988079071, "expedia.model.mdp.beta6" -> 2.0, "expedia.model.cmu.beta6" -> 0.71442, "expedia.model.countryuser.beta1" -> 0.4064445, "expedia.model.marketdestuser.isBookingWeight" -> 1.0, "expedia.model.marketdest.destMarketCountsDefaultWeight" -> 0.05448843831194099, "expedia.model.marketdestcluster.beta1" -> 0.08723168671875, "expedia.model.marketmodel.beta2" -> 1.0117124999999998, "expedia.model.marketdestcluster.beta2" -> 1.3820124999999999, "expedia.model.marketdest.destMarketCountsThresholdClickWeight2" -> 0.008488515169796484, "expedia.model.destcluster.isBookingWeight" -> 1.0, "expedia.model.marketuser.isBookingWeight" -> 1.0, "expedia.timeDecay" -> -0.10183250000000002, "expedia.model.dest.isBookingWeight" -> 1.0, "expedia.model.mdp.beta7" -> 5.0, "expedia.model.marketdest.destMarketCountsThresholdClickWeight1" -> 1.0003499999999999, "expedia.model.marketuser.beta1" -> 0.55, "expedia.model.cmu.beta3" -> 0.8, "expedia.model.mdp.beta4" -> 0.13191754156572452, "expedia.model.destcluster.beta3" -> 1.155, "expedia.model.country.beta1" -> 0.04002862559647299, "expedia.model.marketdest.segmentSizeWeight" -> 1.0, "expedia.model.mdpu.beta2" -> 0.1, "expedia.model.marketdest.beta4" -> 0.9157050000000001, "expedia.model.marketdest.isBookingWeight" -> 0.95, "expedia.model.marketdestuser.beta4" -> 4.0, "expedia.model.marketmodel.isBookingWeight" -> 1.0, "expedia.model.cmumodel.beta1" -> 0.7885, "expedia.model.clusterdist.beta1" -> 1.0, "expedia.model.cmu.beta4" -> 0.22050000328570604, "expedia.model.mdp.beta8" -> 93.50880000000001, "expedia.model.mdp.beta3" -> 512.842240625, "expedia.model.country.isBookingWeight" -> 1.0, "expedia.model.cmu.beta8" -> 0.035, "expedia.model.marketdestuser.beta6" -> 0.405, "expedia.model.cmu.beta7" -> 0.4845, "expedia.model.marketdest.beta1" -> 3.1407074999999995, "expedia.model.mdpu.beta1" -> 0.1, "expedia.model.dest.beta1" -> 0.05000000074505806, "expedia.model.cmu.beta2" -> 0.85, "expedia.model.country.beta2" -> 548.2287359999999, "expedia.model.marketdestuser.beta3" -> 400.0, "expedia.model.mdp.beta2" -> 0.4278141056250001, "expedia.model.marketdest.destMarketCountsThreshold1" -> 112.45500000000001, "expedia.model.marketdest.beta3" -> 1.465334033625, "expedia.model.marketuser.beta3" -> 0.9, "expedia.model.dest.beta2" -> 0.85, "expedia.model.cmu.beta5" -> 0.10579999763518572, "expedia.model.marketdestuser.beta2" -> 0.8, "expedia.model.countryuser.beta2" -> 1.4800500000000003, "expedia.model.mdp.beta5" -> 0.03491250052023679, "expedia.model.destcluster.beta1" -> 0.054625, "expedia.model.marketmodel.beta1" -> 0.03737715680696303, "expedia.model.marketdestuser.beta5" -> 10.579999999999998, "expedia.model.cmu.beta1" -> 1.15, "expedia.model.clusterdist.beta2" -> 7.0, "expedia.model.mdp.isBookingWeight" -> 1.0, "expedia.model.marketdestcluster.isBookingWeight" -> 0.7224999999999999, "expedia.model.mdpu.isBookingWeight" -> 1.0, "expedia.model.countryuser.isBookingWeight" -> 1.0),
        Some(3),None)

  }

  def createParamsCont6(): SimpleHyperParams = {
    SimpleHyperParams(Map(
      "expedia.model.marketmodel.segmentSizeWeight" -> 0,
      "expedia.model.marketdest.segmentSizeWeight" -> 0,
      "expedia.model.marketdest.segmentSizeWeight" -> 0,

      "expedia.model.country.isBookingWeight" -> 1,
      "expedia.model.countryuser.isBookingWeight" -> 1,
      "expedia.model.dest.isBookingWeight" -> 1,
      "expedia.model.destcluster.isBookingWeight" -> 1,
      "expedia.model.marketdest.isBookingWeight" -> 1,
      "expedia.model.marketdestcluster.isBookingWeight" -> 1,
      "expedia.model.marketdestuser.isBookingWeight" -> 1,
      "expedia.model.marketmodel.isBookingWeight" -> 1,
      "expedia.model.marketuser.isBookingWeight" -> 1,
      "expedia.model.mdpu.isBookingWeight" -> 1,
      "expedia.model.mdp.isBookingWeight" -> 1,
      "expedia.model.marketmodel.beta2" -> 1,
      "expedia.model.marketdest.beta2" -> 1.0, "expedia.model.marketuser.beta2" -> 0.6, "expedia.model.marketdest.destMarketCountsThreshold2" -> 220.5, "expedia.model.mdp.beta1" -> 261.14549999999997, "expedia.model.marketdestuser.beta1" -> 0.949999988079071, "expedia.model.mdp.beta6" -> 1.7, "expedia.model.cmu.beta6" -> 0.7497, "expedia.model.countryuser.beta1" -> 0.30294, "expedia.model.marketdest.destMarketCountsDefaultWeight" -> 0.04488750066887587, "expedia.model.marketdestcluster.beta1" -> 0.06281875, "expedia.model.marketdestcluster.beta2" -> 0.8075, "expedia.model.marketdest.destMarketCountsThresholdClickWeight2" -> 0.0138140034375, "expedia.timeDecay" -> -0.07350000000000001, "expedia.model.mdp.beta7" -> 5.0, "expedia.model.marketdest.destMarketCountsThresholdClickWeight1" -> 1.6850369250000001, "expedia.model.marketuser.beta1" -> 0.606375, "expedia.model.cmu.beta3" -> 0.8, "expedia.model.mdp.beta4" -> 0.10560000157356263, "expedia.model.destcluster.beta3" -> 1.155, "expedia.model.country.beta1" -> 0.05605184896023762, "expedia.model.mdpu.beta2" -> 0.1, "expedia.model.marketdest.beta4" -> 1.2193335000000003, "expedia.model.marketdestuser.beta4" -> 4.0, "expedia.model.cmumodel.beta1" -> 0.7885, "expedia.model.clusterdist.beta1" -> 1.0, "expedia.model.cmu.beta4" -> 0.18900000281631946, "expedia.model.mdp.beta8" -> 77.616, "expedia.model.mdp.beta3" -> 679.25, "expedia.model.cmu.beta8" -> 0.035, "expedia.model.marketdestuser.beta6" -> 0.3825, "expedia.model.cmu.beta7" -> 0.6599999999999999, "expedia.model.marketdest.beta1" -> 1.9836237499999996, "expedia.model.mdpu.beta1" -> 0.1, "expedia.model.dest.beta1" -> 0.05000000074505806, "expedia.model.cmu.beta2" -> 1.0, "expedia.model.country.beta2" -> 474.21158399999996, "expedia.model.marketdestuser.beta3" -> 400.0, "expedia.model.mdp.beta2" -> 0.5003673750000001, "expedia.model.marketdest.destMarketCountsThreshold1" -> 112.45500000000001, "expedia.model.marketdest.beta3" -> 0.7803, "expedia.model.marketuser.beta3" -> 1.0, "expedia.model.dest.beta2" -> 1.0, "expedia.model.cmu.beta5" -> 0.07199999839067459, "expedia.model.marketdestuser.beta2" -> 0.8, "expedia.model.countryuser.beta2" -> 1.6445000000000003, "expedia.model.mdp.beta5" -> 0.0385000005736947, "expedia.model.destcluster.beta1" -> 0.054625, "expedia.model.marketmodel.beta1" -> 0.03325000049546361, "expedia.model.marketdestuser.beta5" -> 8.0, "expedia.model.cmu.beta1" -> 1.0, "expedia.model.clusterdist.beta2" -> 7.0),
      Some(6),None)
  }

  def createParamsCont4(): SimpleHyperParams = {
    SimpleHyperParams(Map(
      "expedia.model.marketmodel.segmentSizeWeight" -> 0,
      "expedia.model.marketdest.segmentSizeWeight" -> 0,
      "expedia.model.marketdest.segmentSizeWeight" -> 0,

      "expedia.model.country.isBookingWeight" -> 1,
      "expedia.model.countryuser.isBookingWeight" -> 1,
      "expedia.model.dest.isBookingWeight" -> 1,
      "expedia.model.destcluster.isBookingWeight" -> 1,
      "expedia.model.marketdest.isBookingWeight" -> 1,
      "expedia.model.marketdestcluster.isBookingWeight" -> 1,
      "expedia.model.marketdestuser.isBookingWeight" -> 1,
      "expedia.model.marketmodel.isBookingWeight" -> 1,
      "expedia.model.marketuser.isBookingWeight" -> 1,
      "expedia.model.mdpu.isBookingWeight" -> 1,
      "expedia.model.mdp.isBookingWeight" -> 1,
      "expedia.model.marketmodel.beta2" -> 1,
      "expedia.model.marketdest.beta2" -> 1.0, "expedia.model.marketuser.beta2" -> 0.6, "expedia.model.marketdest.destMarketCountsThreshold2" -> 245.0, "expedia.model.mdp.beta1" -> 323.4, "expedia.model.marketdestuser.beta1" -> 0.949999988079071, "expedia.model.mdp.beta6" -> 2.0, "expedia.model.cmu.beta6" -> 0.882, "expedia.model.countryuser.beta1" -> 0.39599999999999996, "expedia.model.marketdest.destMarketCountsDefaultWeight" -> 0.05225000077858567, "expedia.model.marketdestcluster.beta1" -> 0.0596778125, "expedia.model.marketdestcluster.beta2" -> 1.0, "expedia.model.marketdest.destMarketCountsThresholdClickWeight2" -> 0.0124983840625, "expedia.timeDecay" -> -0.07, "expedia.model.mdp.beta7" -> 5.0, "expedia.model.marketdest.destMarketCountsThresholdClickWeight1" -> 1.053, "expedia.model.marketuser.beta1" -> 0.6641250000000001, "expedia.model.cmu.beta3" -> 0.8, "expedia.model.mdp.beta4" -> 0.08976000133752823, "expedia.model.destcluster.beta3" -> 1.155, "expedia.model.country.beta1" -> 0.05117777513760825, "expedia.model.mdpu.beta2" -> 0.1, "expedia.model.marketdest.beta4" -> 1.0120950000000002, "expedia.model.marketdestuser.beta4" -> 4.0, "expedia.model.cmumodel.beta1" -> 0.7885, "expedia.model.clusterdist.beta1" -> 1.0, "expedia.model.cmu.beta4" -> 0.21000000312924386, "expedia.model.mdp.beta8" -> 66.528, "expedia.model.mdp.beta3" -> 645.2875, "expedia.model.cmu.beta8" -> 0.035, "expedia.model.marketdestuser.beta6" -> 0.495, "expedia.model.cmu.beta7" -> 0.51, "expedia.model.marketdest.beta1" -> 3.3379499999999998, "expedia.model.mdpu.beta1" -> 0.1, "expedia.model.dest.beta1" -> 0.05000000074505806, "expedia.model.cmu.beta2" -> 1.0, "expedia.model.country.beta2" -> 539.5724928, "expedia.model.marketdestuser.beta3" -> 400.0, "expedia.model.mdp.beta2" -> 0.7654500000000001, "expedia.model.marketdest.destMarketCountsThreshold1" -> 106.83225000000002, "expedia.model.marketdest.beta3" -> 1.0098, "expedia.model.marketuser.beta3" -> 1.05, "expedia.model.dest.beta2" -> 1.0, "expedia.model.cmu.beta5" -> 0.07999999821186066, "expedia.model.marketdestuser.beta2" -> 0.8, "expedia.model.countryuser.beta2" -> 1.5730000000000002, "expedia.model.mdp.beta5" -> 0.0327250004876405, "expedia.model.destcluster.beta1" -> 0.054625, "expedia.model.marketmodel.beta1" -> 0.024023125357972457, "expedia.model.marketdestuser.beta5" -> 8.4, "expedia.model.cmu.beta1" -> 1.0, "expedia.model.clusterdist.beta2" -> 5.95),
      Some(4),None)
  }

  def createParamsCont2Country198(): SimpleHyperParams = {
    SimpleHyperParams(Map(
      "expedia.model.marketmodel.segmentSizeWeight" -> 0,
      "expedia.model.marketdest.segmentSizeWeight" -> 0,
      "expedia.model.marketdest.segmentSizeWeight" -> 0,

      "expedia.model.country.isBookingWeight" -> 1,
      "expedia.model.countryuser.isBookingWeight" -> 1,
      "expedia.model.dest.isBookingWeight" -> 1,
      "expedia.model.destcluster.isBookingWeight" -> 1,
      "expedia.model.marketdest.isBookingWeight" -> 1,
      "expedia.model.marketdestcluster.isBookingWeight" -> 1,
      "expedia.model.marketdestuser.isBookingWeight" -> 1,
      "expedia.model.marketmodel.isBookingWeight" -> 1,
      "expedia.model.marketuser.isBookingWeight" -> 1,
      "expedia.model.mdpu.isBookingWeight" -> 1,
      "expedia.model.mdp.isBookingWeight" -> 1,
      "expedia.model.marketmodel.beta2" -> 1,
      "expedia.model.marketdest.beta2" -> 1.0, "expedia.model.marketuser.beta2" -> 0.6, "expedia.model.marketdest.destMarketCountsThreshold2" -> 159.31125, "expedia.model.mdp.beta1" -> 371.90999999999997, "expedia.model.marketdestuser.beta1" -> 0.949999988079071, "expedia.model.mdp.beta6" -> 2.0, "expedia.model.cmu.beta6" -> 0.9702, "expedia.model.countryuser.beta1" -> 0.39599999999999996, "expedia.model.marketdest.destMarketCountsDefaultWeight" -> 0.036551250544656064, "expedia.model.marketdestcluster.beta1" -> 0.039466562499999996, "expedia.model.marketmodel.beta2" -> 0.61965, "expedia.model.marketdestcluster.beta2" -> 0.65025, "expedia.model.marketdest.destMarketCountsThresholdClickWeight2" -> 0.015810455839062498, "expedia.timeDecay" -> -0.08050000000000002, "expedia.model.mdp.beta7" -> 5.0, "expedia.model.marketdest.destMarketCountsThresholdClickWeight1" -> 1.0003499999999999, "expedia.model.marketuser.beta1" -> 0.42075000000000007, "expedia.model.cmu.beta3" -> 0.8, "expedia.model.mdp.beta4" -> 0.09424800140440465, "expedia.model.destcluster.beta3" -> 0.98175, "expedia.model.country.beta1" -> 0.046187942061691446, "expedia.model.mdpu.beta2" -> 0.1, "expedia.model.marketdest.beta4" -> 0.70227, "expedia.model.marketdestuser.beta4" -> 4.0, "expedia.model.cmumodel.beta1" -> 0.7885, "expedia.model.clusterdist.beta1" -> 1.0, "expedia.model.cmu.beta4" -> 0.19950000297278167, "expedia.model.mdp.beta8" -> 60.04152, "expedia.model.mdp.beta3" -> 548.494375, "expedia.model.cmu.beta8" -> 0.035, "expedia.model.marketdestuser.beta6" -> 0.4275, "expedia.model.cmu.beta7" -> 0.69, "expedia.model.marketdest.beta1" -> 2.2053228749999993, "expedia.model.mdpu.beta1" -> 0.1, "expedia.model.dest.beta1" -> 0.05000000074505806, "expedia.model.cmu.beta2" -> 1.0, "expedia.model.country.beta2" -> 526.90176, "expedia.model.marketdestuser.beta3" -> 400.0, "expedia.model.mdp.beta2" -> 0.7654500000000001, "expedia.model.marketdest.destMarketCountsThreshold1" -> 125.68500000000002, "expedia.model.marketdest.beta3" -> 0.8989933837500002, "expedia.model.marketuser.beta3" -> 0.9, "expedia.model.dest.beta2" -> 1.0, "expedia.model.cmu.beta5" -> 0.06459999855607748, "expedia.model.marketdestuser.beta2" -> 0.8, "expedia.model.countryuser.beta2" -> 1.576575, "expedia.model.mdp.beta5" -> 0.03250187548431568, "expedia.model.destcluster.beta1" -> 0.054625, "expedia.model.marketmodel.beta1" -> 0.03675000054761767, "expedia.model.marketdestuser.beta5" -> 8.82, "expedia.model.cmu.beta1" -> 1.15, "expedia.model.clusterdist.beta2" -> 7.0),
      None,Some(198))

  }

}