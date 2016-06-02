package expedia.model.cmu

import expedia.SimpleHyperParams

object CmuModelParams {

  def getPrioritizedHyperParams(): Seq[SimpleHyperParams] = {
    val prioritizedHyperParams = List(
      createParamsCont2Country198(),
      createParamsCont3(),
      createParamsCont4(),
      createParamsCont6(),
      createParamsCMU3())

    prioritizedHyperParams
  }

 private def createParamsCMU3(): SimpleHyperParams = {
    SimpleHyperParams(Map(
      "expedia.model.cmu.beta6" -> 0.882,
      "expedia.model.cmu.beta3" -> 0.8,
      "expedia.model.cmumodel.beta1" -> 0.7885,
      "expedia.model.cmu.beta4" -> 0.21000000312924386,
      "expedia.model.cmu.beta8" -> 0.035,
      "expedia.model.cmu.beta7" -> 0.6,
      "expedia.model.cmu.beta2" -> 1.0,
      "expedia.model.cmu.beta5" -> 0.07999999821186066,
      "expedia.model.cmu.beta1" -> 1.0))

  }

 private def createParamsCont3(): SimpleHyperParams = {

    SimpleHyperParams(Map(
      "expedia.model.cmu.beta6" -> 0.882,
      "expedia.model.cmu.beta3" -> 0.8,
      "expedia.model.cmumodel.beta1" -> 0.7885,
      "expedia.model.cmu.beta4" -> 0.21000000312924386,
      "expedia.model.cmu.beta8" -> 0.035,
      "expedia.model.cmu.beta7" -> 0.6,
      "expedia.model.cmu.beta2" -> 1.0,
      "expedia.model.cmu.beta5" -> 0.07999999821186066,
      "expedia.model.cmu.beta1" -> 1.0), Some(3),None)

  }

 private def createParamsCont6(): SimpleHyperParams = {
    SimpleHyperParams(Map(
      "expedia.model.cmu.beta6" -> 0.882,
      "expedia.model.cmu.beta3" -> 0.8,
      "expedia.model.cmumodel.beta1" -> 0.7885,
      "expedia.model.cmu.beta4" -> 0.21000000312924386,
      "expedia.model.cmu.beta8" -> 0.035,
      "expedia.model.cmu.beta7" -> 0.6,
      "expedia.model.cmu.beta2" -> 1.0,
      "expedia.model.cmu.beta5" -> 0.07999999821186066,
      "expedia.model.cmu.beta1" -> 1.0), Some(6),None)
  }

 private def createParamsCont4(): SimpleHyperParams = {
    SimpleHyperParams(Map(
      "expedia.model.cmu.beta6" -> 0.882,
      "expedia.model.cmu.beta3" -> 0.8,
      "expedia.model.cmumodel.beta1" -> 0.7885,
      "expedia.model.cmu.beta4" -> 0.21000000312924386,
      "expedia.model.cmu.beta8" -> 0.035,
      "expedia.model.cmu.beta7" -> 0.6,
      "expedia.model.cmu.beta2" -> 1.0,
      "expedia.model.cmu.beta5" -> 0.07999999821186066,
      "expedia.model.cmu.beta1" -> 1.0), Some(4),None)
  }

private  def createParamsCont2Country198(): SimpleHyperParams = {
    SimpleHyperParams(Map(
      "expedia.model.cmu.beta6" -> 0.882,
      "expedia.model.cmu.beta3" -> 0.8,
      "expedia.model.cmumodel.beta1" -> 0.7885,
      "expedia.model.cmu.beta4" -> 0.21000000312924386,
      "expedia.model.cmu.beta8" -> 0.035,
      "expedia.model.cmu.beta7" -> 0.6,
      "expedia.model.cmu.beta2" -> 1.0,
      "expedia.model.cmu.beta5" -> 0.07999999821186066,
      "expedia.model.cmu.beta1" -> 1.0),  None,Some(198))
  }

}