package org.singingwizard.casecontinuations

@continuations
object Test1Impl {
  def test(args: Array[String]): Unit = {
    val k = Continuation(() => println(s"Hello world: $args"))
    k()
    println(k.pickle())
  }
}


object Test1 {
  def main(args: Array[String]): Unit = {
    Test1Impl.test(args)
  }
}
