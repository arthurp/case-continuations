package org.singingwizard.examples

import scala.meta._

class main extends scala.annotation.StaticAnnotation {
  inline def apply(defn: Any) = meta {
    val q"object $name { ..$stats }" = defn
    val main = q"""
      def main(args: Array[String]): Unit = {
        println("entering main") 
        ..$stats 
      }
    """
    q"object $name { $main }"
  }
}