package expedia

import dk.gp.util.loadObject
import dk.gp.util.saveObject

object TestApp {
  
  def main(args:Array[String]) = {
    
       val hyperParamsMap = loadObject[CompoundHyperParamsMap]("target/hyperParamsMap_trained.kryo2")
  hyperParamsMap.getModel("marketdest").prioritizedHyperParams.foreach(params => println(params))
  
     //  val newHyperParamsMap = hyperParamsMap.addModelParams(None,Some(50))
     //  saveObject(newHyperParamsMap,"target/hyperParamsMap_trained.kryo2")
    
  }
}