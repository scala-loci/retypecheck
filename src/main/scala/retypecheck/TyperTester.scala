package retypecheck

import scala.annotation.StaticAnnotation
import scala.reflect.macros.blackbox.Context
import scala.language.experimental.macros

object TyperTester {
  def createTypeTree[T]: String = macro createTypeTreeImpl[T]


  def retyper[T](code: T): T = macro retyperImpl[T]

  def retyperAll[T](code: T): T = macro retyperAllImpl[T]


  class retyper extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro retyperAnnoImpl
  }

  class retyperAll extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro retyperAnnoAllImpl
  }


  def retyperAnnoImpl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    val annottee :: companion = annottees map { _.tree }
    val result = annottee match {
      case ClassDef(_, _, _, _) => retyperTypecheck(c)(annottee)
      case ModuleDef(_, _, _) => retyperTypecheck(c)(annottee)
      case _ =>
        c.abort(
          c.enclosingPosition,
          "macro only applicable to class, trait or object")
    }

    if (companion.isEmpty)
      c.Expr[Any](result)
    else
      c.Expr[Any](q"$result; ${companion.head}")
  }

  def retyperAnnoAllImpl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    val annottee :: companion = annottees map { _.tree }
    val result = annottee match {
      case ClassDef(_, _, _, _) => retyperAllTypecheck(c)(annottee)
      case ModuleDef(_, _, _) => retyperAllTypecheck(c)(annottee)
      case _ =>
        c.abort(
          c.enclosingPosition,
          "macro only applicable to class, trait or object")
    }

    if (companion.isEmpty)
      c.Expr[Any](result)
    else
      c.Expr[Any](q"$result; ${companion.head}")
  }


  def createTypeTreeImpl[T: c.WeakTypeTag](c: Context): c.Expr[String] = {
    import c.universe._
    val t = c.retyper
    val tree = t createTypeTree (weakTypeOf[T], NoPosition)
    val fullyCreated = tree forAll {
      case _: TypeTree => false
      case _ => true
    }
    val string = if (fullyCreated) tree.toString else ""
    c.Expr[String](Literal(Constant(string.replaceAll("[\\r\\n\\s]+", " "))))
  }


  def retyperImpl[T](c: Context)(code: c.Expr[T]): c.Expr[T] =
    c.Expr[T](retyperTypecheck(c)(code.tree))

  def retyperAllImpl[T](c: Context)(code: c.Expr[T]): c.Expr[T] =
    c.Expr[T](retyperAllTypecheck(c)(code.tree))


  private def retyperTypecheck(c: Context)(tree: c.Tree): c.Tree = {
    val t = c.retyper
    if (!c.hasErrors)
      t untypecheck (t typecheck (t untypecheck (t typecheck (t untypecheck tree))))
    else
      tree
  }

  private def retyperAllTypecheck(c: Context)(tree: c.Tree): c.Tree = {
    val t = c.retyper
    if (!c.hasErrors)
      t untypecheckAll (t typecheck (t untypecheckAll (t typecheck (t untypecheckAll tree))))
    else
      tree
  }
}
