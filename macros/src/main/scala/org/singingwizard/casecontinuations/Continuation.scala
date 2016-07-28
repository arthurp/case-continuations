package org.singingwizard.casecontinuations

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

object Continuation {
  def apply(f: () => Unit): Continuation = macro ContinuationImpl.impl
}

abstract class Continuation extends (() => Unit) {
  def apply(): Unit
  
  def pickle(): Any
}

object ContinuationImpl {
}

// TODO: Make annotation macro on classes. This macro replaces every reference to Continuation.apply using the same macro below.

class ContinuationImpl(val c: Context) {
  import ContinuationImpl._
  import c.universe._
  import c.internal._
  import decorators._

  import Flag._

  // From scala-reflect
  object Utils {
    // Create a readable string describing a substitution.
    private def substituterString(fromStr: String, toStr: String, from: List[Any], to: List[Any]): String = {
      "subst[%s, %s](%s)".format(fromStr, toStr, (from, to).zipped map (_ + " -> " + _) mkString ", ")
    }

    // NOTE: calls shallowDuplicate on trees in `to` to avoid problems when symbols in `from`
    // occur multiple times in the `tree` passed to `transform`,
    // otherwise, the resulting Tree would be a graph, not a tree... this breaks all sorts of stuff,
    // notably concerning the mutable aspects of Trees (such as setting their .tpe)
    class TreeSubstituter(from: List[Symbol], to: List[Tree]) extends Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case Ident(_) =>
          def subst(from: List[Symbol], to: List[Tree]): Tree =
            if (from.isEmpty) tree
            else if (tree.symbol == from.head) to.head.duplicate // TODO: does it ever make sense *not* to perform a shallowDuplicate on `to.head`?
            else subst(from.tail, to.tail)
          subst(from, to)
        case _ =>
          super.transform(tree)
      }
      override def toString = substituterString("Symbol", "Tree", from, to)
    }
  }

  def impl(f: c.Expr[() => Unit]): c.Expr[Continuation] = {
    val q"""() => $body""" = f.tree

    val Continuation = tq"_root_.org.singingwizard.casecontinuations.Continuation"

    val continuationName = c.freshName(TypeName("Continuation"))

    val vars = FindFreeVars.findFreeVariabels(c)(f.tree).map(_._1)
    val newVars = vars.map(v => c.freshName(TermName(v.symbol.name.encodedName.toString)))

    val (syms, params) = (vars zip newVars).map { a =>
      val (v, newName) = a
      val df = q"val ${newName}: ${v.tpe}"
      val sym = q"$newName"
      (sym, df)
    }.unzip

    val varSyms = vars.map(_.symbol)

    // FIXME: untypecheck hack. This will fail if there are certain patterns in the body
    val newBody = new Utils.TreeSubstituter(varSyms, syms) transform c.untypecheck(body)

    val cls =  q"""
      class ${continuationName}(..$params) extends $Continuation {
        def apply() = { $newBody }
        def pickle() = { ${f.tree.toString + " " + params} }
      } 
      """

    c.Expr[Continuation](q"""{
      $cls
      new ${continuationName}(..$vars)
      }""")
  }
}
