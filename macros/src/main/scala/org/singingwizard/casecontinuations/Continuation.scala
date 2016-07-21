package org.singingwizard.casecontinuations

import scala.meta._

class ContinuationImpl {
  inline def apply(f: () => Unit): () => Unit = meta {
    println(f)
    f
  }
}

object Continuation extends ContinuationImpl