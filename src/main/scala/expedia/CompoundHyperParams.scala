package expedia

import scala.collection.Seq
import scala.collection.mutable
import expedia.data.Click
import expedia.model.cmu.CmuModelParams

case class CompoundHyperParams(prioritizedHyperParams: Seq[SimpleHyperParams]) {

  def getParamValueForContAndCountry(param: String, continentId:Int,countryId: Int): Double = {
    val hyperParams = prioritizedHyperParams.find { params => params.containsClick(continentId, countryId) }
   if(hyperParams.isEmpty) {
     println("error, cont=%d, country=%d".format(continentId,countryId))
   }
    hyperParams.get.getParamValue(param)

  }

}
