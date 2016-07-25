package org.singingwizard.casecontinuations

import scala.meta._

class continuations extends scala.annotation.StaticAnnotation {
  inline def apply(defn: Any) = meta {
    // TODO: Extract all free variables and lift them

    def makeTemplate(cls: Defn.Class) = {
      val ctorRef = Ctor.Ref.Name(cls.name.value)
      val self = Term.Param(List(), Name.Anonymous(), None, None)
      Template(List(), List(q"$ctorRef(args)"), self, None)
    }

    def convertClosure(f: Tree): (Stat, Stat) = {
      val ContName = Type.fresh("GeneratedContinuation")

      val q"() => $body" = f
      val cls = q"""
        case class $ContName(args: Any) extends (() => Unit) {
          def apply() = $body
        }
      """
      val newCls = q"new ${makeTemplate(cls)}"
      (cls, newCls)
    }

    val q"object $name { ..$stats }" = defn
    val classes = scala.collection.mutable.Buffer[Stat]()
    val newStats = stats.map(_.transform {
      case q"Continuation($f)" =>
        val (cls, ns) = convertClosure(f)
        classes += cls
        ns
    }).map(_.asInstanceOf[Stat])
    q"object $name { ..${classes.toList}; ..$newStats }"
  }
  /*
  inline def apply(f: Any) = meta {
    println(f)
    q"null"
  }
   */
}

abstract class Continuation extends (() => Unit) {
  def apply(): Unit
  
  def pickle(): Any
}

/*
class ContinuationImpl(val c: Context) {
  import c.universe._
  import c.internal._
  import decorators._

  import Flag._

  def impl(f: c.Expr[() => Unit]): c.Expr[Continuation] = {
    println(f)

    val q"""() => $body""" = f.tree

    val Continuation = tq"_root_.org.singingwizard.casecontinuations.Continuation"

    val continuationName = c.freshName(TypeName("Continuation"))
    val continuationSym = enclosingOwner.newTypeSymbol(continuationName)

    val vars = FindFreeVars.findFreeVariabels(c)(f.tree).map(_._1)
    val newVars = vars.map(v => c.freshName(TermName(v.symbol.name.encodedName.toString)))

    // val defn1 = q"val test: Int"
    // val defn2 = valDef(enclosingOwner.newTermSymbol(TermName("test"), flags = PARAM).setInfo(typeOf[Int]))

    // println((defn1, defn2))
    // println((showRaw(defn1), showRaw(defn2)))

    // def printDefn(t: Tree) = {
    //   val ValDef(mods, name, tpt, rhs) = t
    //   println((mods, name, tpt, rhs))
    // }
    // printDefn(defn1)
    // printDefn(defn2)

    val (syms, params) = (vars zip newVars).map { a =>
      val (v, newName) = a
      val sym = continuationSym.newTermSymbol(newName, flags = PARAM).setInfo(v.tpe)
      val df = valDef(sym)
      //val df = q"val ${sym.name}: ${v.tpe}"
      val df2 = q"val ${newName}: ${v.tpe}"
      println((showRaw(df), showRaw(df2)))
      println(df.symbol)
      (sym, df)
    }.unzip

    val varSyms = vars.map(_.symbol)
    println(varSyms zip syms)
    println(body)

    val newBody = substituteSymbols(body, varSyms, syms)

    println(newBody)

    val cls =  q"""
      class ${continuationName}(..$params) extends $Continuation {
        def apply() = { $newBody }
        def pickle() = { ${f.tree.toString + " " + params} }
      } 
      """

    println(cls)

    c.Expr[Continuation](q"""{
      $cls
      new ${continuationName}(..$vars)
      }""")
  }
}
 */
