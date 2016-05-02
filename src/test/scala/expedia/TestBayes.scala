package expedia

import dk.bayes.dsl.variable.Categorical
import dk.bayes.dsl._

object TestBayes {
  
   def main(args: Array[String]): Unit = {
    
     val c2 = Categorical(Array(0.97,0.03))
     val c1 = Categorical(c2,Array(0.98,0.02,0.3,0.7))
       val c11 = Categorical(c2,Array(0.98,0.02,0.02,0.98))
     c1.setValue(1)
     //c11.setValue(1)
     //c111.setValue(0)
     println(infer(c2).cpd)
      println(infer(c11).cpd)
   }
}