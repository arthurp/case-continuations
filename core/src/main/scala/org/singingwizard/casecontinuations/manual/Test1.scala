package org.singingwizard.casecontinuations.manual

/**
 * @author amp
 */
object Test1 {
  def main(args: Array[String]): Unit = {
    val x = args.size
    /*
    val f: () => Unit = continuation { () =>
      println(x)
    }
     */
    
    val f: () => Unit = new continuation__main_f(x)
    
    f()
  }
  
  class continuation__main_f(private val x: Int) extends (() => Unit) {
    def apply(): Unit = {
      println(x)
    }
  }
}
