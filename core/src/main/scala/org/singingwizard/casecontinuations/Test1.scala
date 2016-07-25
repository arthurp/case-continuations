package org.singingwizard.casecontinuations

@continuations
object Test1 {
  def main(args: Array[String]): Unit = {
    val k1 = Continuation(() => println(s"Hello world: $args"))
    val k2 = Continuation(() => println(s"Goodbye!"))
    k1()
    k2()
    //println(k.pickle())
  }
}
