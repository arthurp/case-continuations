package org.singingwizard.casecontinuations

object Test1 {
  def main(args: Array[String]): Unit = {
    val k = Continuation(() => println(s"Hello world: $args"))
    k()
    println(k.pickle())
  }
}