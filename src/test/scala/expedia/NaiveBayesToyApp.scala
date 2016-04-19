package expedia

import dk.bayes.dsl._
import dk.bayes.dsl.variable.Categorical

object NaiveBayesToyApp {
  
  def main(args: Array[String]): Unit = {
    
    
   val cluster = Categorical(Vector(4d/14, 3d/14,7d/14))
   val search =Categorical(cluster, Vector(
       1.0/1000, 999d/1000, 1.0/1000,1.0/1000, 
       1.0/3, 1.0/3, 1.0/3,0, 
       2.0/7, 2.0/7, 2.0/7,1.0/7
       ))
       
       search.setValue(1)
      println( infer(cluster).cpd)
  }
  
}