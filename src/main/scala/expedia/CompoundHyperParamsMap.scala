package expedia

import scala.collection._

case class CompoundHyperParamsMap(hyperParamsMap: mutable.Map[String, CompoundHyperParams]) {

  def getModel(model: String): CompoundHyperParams = {
    hyperParamsMap.getOrElseUpdate(model, CompoundHyperParams(model, mutable.Map(), mutable.Map()))
  }

  def addModel(model: String, hyperParams: CompoundHyperParams): CompoundHyperParamsMap = {
    CompoundHyperParamsMap(hyperParamsMap + (model -> hyperParams))
  }

  def deleteModelParams(model: String): CompoundHyperParamsMap = {
    CompoundHyperParamsMap(hyperParamsMap - model)
  }

}