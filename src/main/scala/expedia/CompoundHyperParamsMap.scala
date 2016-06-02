package expedia

case class CompoundHyperParamsMap(hyperParamsMap: Map[String, CompoundHyperParams]) {

  def getModel(model: String): CompoundHyperParams = {
    hyperParamsMap.getOrElse(model, getDefaultCompoundHyperParams(model))
  }

  def addModel(model: String, hyperParams: CompoundHyperParams): CompoundHyperParamsMap = {
    CompoundHyperParamsMap(hyperParamsMap + (model -> hyperParams))
  }
  
  def deleteModelPArams(model:String):CompoundHyperParamsMap = {
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
      case "market" => {
        Map(
          "expedia.model.marketmodel.decayFactor" ->  -0.07,
          "expedia.model.marketmodel.beta1" -> 0.0350,
          "expedia.model.marketmodel.isBookingWeight" -> 1,
          "expedia.model.marketmodel.beta2" -> 1,
          "expedia.model.marketmodel.segmentSizeWeight" -> 0)
      }
    }
  }

}