package org.singingwizard.casecontinuations

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

object Continuation {
  def apply(): Any = macro ContinuationImpl.impl
}

class ContinuationImpl(val c: Context) {
  import c.universe._
  import c.internal._
  import decorators._

  import Flag._

  def impl(): c.Expr[Any] = {
    // If this is uncommented it crashes
    val param = valDef(enclosingOwner.newTermSymbol(TermName("x"), flags = DEFERRED).setInfo(typeOf[Int]))

    // If this is uncommented it works
    //val param = q"val x: Int"

    val cls = q"""
      class K($param) {
        def y = x
      } 
    """

    c.Expr[Any](q"""{
      $cls
      new K(1)
      }""")
  }
}
