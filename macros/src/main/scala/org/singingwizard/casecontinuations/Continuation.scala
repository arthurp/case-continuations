package org.singingwizard.casecontinuations

import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context
import scala.annotation.StaticAnnotation

object Continuation {
  def apply(f: () => Unit): Continuation = ??? // macro ContinuationImpl.impl
}

abstract class Continuation extends (() => Unit) {
  def apply(): Unit
  
  def pickle(): Any
}

class continuations extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro ContinuationImpl.onClass
}

object ContinuationImpl {
  case class ParentClassAttachment(sym: Any)
  def intermediateContinuation(clsName: String, args: Any*): Continuation = macro ContinuationImpl.onIntermediateContinuation

  case class OrigOwnerAttachment(sym: Any, origTree: Option[Any])

  // inspired by https://gist.github.com/retronym/10640845#file-macro2-scala
  // check out the gist for a detailed explanation of the technique
  class declSplicer(payload: String) extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro ContinuationImpl.declSplicerImpl
  }
}

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

  def onClass(annottees: c.Tree*): Tree = {
    val (annottee, expandees) = annottees.toList match {
      case (param: ValDef) :: (rest @ (_ :: _)) => (param, rest)
      case (param: TypeDef) :: (rest @ (_ :: _)) => (param, rest)
      case _ => (EmptyTree, annottees)
    }
    println((annottee, expandees))

    // TODO: Throw an error if the annotation is not on a class or object.
    // TODO: Support performing transformation on classes.

    assert(annottee == EmptyTree)
    assert(expandees.size == 1)
    assert(expandees.head.isDef)

    val orig = c.typecheck(expandees.head)

    //val q"object $name extends ..$parents { ..$body }" = orig

    /*
    val classes = scala.collection.mutable.HashMap[TypeName, ClassDef]()

    // Collect information about continuations
    val Continuation_apply = typeOf[Continuation.type].member(TermName("apply")).asMethod

    case class ContinuationReplacement(name: TypeName, vars: List[Tree])

    object ContinuationProcessor extends Traverser {
      override def traverse(tree: Tree): Unit = tree match {
        case Apply(callee, List(arg)) if callee.symbol == Continuation_apply =>
          println(tree)
          val (cls, vars) = onContinuation(arg, orig.symbol)
          classes += (cls.name -> cls)
          tree.updateAttachment(ContinuationReplacement(cls.name, vars))
        case _ => super.traverse(tree)
      }
    }

    ContinuationProcessor.traverse(orig)

    println("====== Closures:")
    println(classes.values.mkString("\n"))
    println("--------------")

    // Perform in place replacements
    val updated = typingTransform(orig)((tree, api) => tree match {
      //case m@ModuleDef(mods, name, Template(parents, self, body)) if m.symbol == orig.symbol => {
      //  val i1 = ModuleDef(mods, name, Template(parents, self, classes.values.toList ++ body))
      //  api.default(i1)
      //}
      case Apply(callee, List(arg)) if callee.symbol == Continuation_apply =>
        val ContinuationReplacement(closureName, vars) = tree.attachments.get[ContinuationReplacement].get
        //api.typecheck(q"new $closureName(..$vars)")
        api.default(tree)
      case _ =>
        api.default(tree)
    })

    val result = updated
     */

    val result = typingTransform(orig)((tree, api) => tree match {
      case _ =>
        api.default(tree)
    })


    /*
    // Build the new object
    val intermediate = q"""
      object $name extends ..$parents {
        ..${classes.map(declSplicer(true)).toList}
        ..${body.map(replaceContinuations).map(declSplicer(true, orig.symbol.asModule.moduleClass))}
      }
    """
    val result = intermediate
     */

    //MAYBE:
    // * Attach the same symbol to the object


    // TODO: Make sure symbol table is correct WRT new arrangement (The current tricks with declSplicer might be enough)

    //MAYBE:
    // This last part may not be needed since the it should be generated correctly if the symbol table is correct when the returned object is type checked.
    // * Rebuild the ClassInfoType for the object type (with all both classes and and all the original members)

    /*
    TODO: I think part of the problem is that the symbol table is incorrect when the new object is typechecked. This may be resulting in test being left out.
      It's not clear what order to set things up in. But if I do it all by hand I should be able to get it right.
     */


    println(s"result: $result")

    result
  }

  def onContinuation(f: c.Tree, parentCls: Symbol): (ClassDef, List[Tree]) = {
    val q"""() => $body""" = f

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

    val clsUntyped =  q"""
      class ${continuationName}(..$params) extends $Continuation {
        def apply() = { $newBody }
        def pickle() = ???
      } 
      """ // { ${f.tree.toString + " " + params} }
    //val cls = c.typecheck(clsUntyped).asInstanceOf[ClassDef]
    //println(s"gen'd class: ${cls.symbol} $parentCls")

    //val clsName = q"${continuationName.toString}"
    //clsName.updateAttachment(ParentClassAttachment(parentCls))
    //val call = q"_root_.org.singingwizard.casecontinuations.ContinuationImpl.intermediateContinuation($clsName, ..$vars)"
    //val call = q"new ${cls.symbol}(..$vars)"
    (clsUntyped, vars)
  }

  def intermediateContinuation(parentCls: Symbol, continuationName: TypeName, vars: List[Tree]): Tree = {
    val clsName = q"${continuationName.toString}"
    clsName.updateAttachment(ParentClassAttachment(parentCls))
    q"_root_.org.singingwizard.casecontinuations.ContinuationImpl.intermediateContinuation($clsName, ..$vars)"
  }


  def onIntermediateContinuation(clsName: c.Tree, args: c.Tree*): c.Tree = {
    val Literal(Constant(name: String)) = clsName
    println((name, args))
    val parentClsSym = clsName.attachments.get[ParentClassAttachment].map(_.sym).get.asInstanceOf[Symbol].asModule
    val continuationSym = parentClsSym.moduleClass.asClass.toType.member(TypeName(name))
    println((parentClsSym, continuationSym))
    val result = q"new ${continuationSym}(..$args)"
    println(result)
    result
  }


  def declSplicer(useOrig: Boolean, origOwner: Symbol = null)(tree: c.Tree): c.Tree = {
    val owner = if(origOwner == null) enclosingOwner else origOwner 
    val attach = OrigOwnerAttachment(owner, if(useOrig) Some(tree) else None)
    val payload = q"${owner.toString}"
    payload.updateAttachment(attach)
    val declSplicerAnnotation = q"new _root_.org.singingwizard.casecontinuations.ContinuationImpl.declSplicer($payload)"
    def extendMods(mods: Modifiers) = {
      val Modifiers(flags, priWithin, annotations) = mods
      Modifiers(flags, priWithin, declSplicerAnnotation :: annotations)
    }

    val result = tree match {
      case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
        DefDef(extendMods(mods), name, tparams, vparamss, tpt, rhs)
      case ClassDef(mods, name, tparams, impl) =>
        ClassDef(extendMods(mods), name, tparams, impl)
    }

    result
  }

  def declSplicerImpl(annottees: c.Tree*): c.Tree = {
    val q"new _root_.org.singingwizard.casecontinuations.ContinuationImpl.declSplicer($attachee).macroTransform($_)" = c.macroApplication
    val tree = annottees.head
    val attach = attachee.attachments.get[OrigOwnerAttachment].get
    val origOwner = attach.sym.asInstanceOf[Symbol]
    val origTree = attach.origTree.asInstanceOf[Option[c.Tree]]
    println((origOwner, tree, origTree, enclosingOwner))
    origTree match {
      case Some(t) => t.changeOwner(origOwner, enclosingOwner)
      case None => tree.changeOwner(origOwner, enclosingOwner)
    }
  }
}
