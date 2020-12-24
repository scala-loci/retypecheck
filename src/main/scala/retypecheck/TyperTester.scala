package retypecheck

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.blackbox

object TyperTester {
  def createTypeTree[T]: String = macro createTypeTreeImpl[T]


  def scalaWithoutBug11609(code: String): Unit = macro scalaWithoutBug11609Impl


  def retyper[T](code: T): T = macro retyperImpl[T]

  def retyperAll[T](code: T): T = macro retyperAllImpl[T]


  class retyper extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro retyperAnnoImpl
  }

  class retyperAll extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro retyperAnnoAllImpl
  }


  def retyperAnnoImpl(c: blackbox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    val annottee :: companion = annottees map { _.tree }: @unchecked
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

  def retyperAnnoAllImpl(c: blackbox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    val annottee :: companion = annottees map { _.tree }: @unchecked
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


  def createTypeTreeImpl[T: c.WeakTypeTag](c: blackbox.Context): c.Expr[String] = {
    import c.universe._

    object withAttrs extends Transformer {
      override def transform(tree: Tree) = tree match {
        case _: TypeTree => Ident(typeNames.ERROR)
        case _ => super.transform(tree)
      }
    }

    object withoutAttrs extends Transformer {
      override def transform(tree: Tree) = tree match {
        case _: TypeTree =>
          Ident(typeNames.ERROR)
        case _ =>
          if (tree.symbol != null && tree.symbol != NoSymbol)
            internal.setSymbol(tree, NoSymbol)
          super.transform(internal.setType(tree, null))
      }
    }

    val t = c.retyper
    val tree = t.createTypeTree(weakTypeOf[T], NoPosition)

    val stringWithAttrs = (withAttrs transform tree.duplicate).toString
      .replaceAll("[\\r\\n\\s]+", " ")
    val stringWithoutAttrs = (withoutAttrs transform tree.duplicate).toString
      .replaceAll("[\\r\\n\\s]+", " ")
    val string =
      if (stringWithAttrs != stringWithoutAttrs)
        s"different representations: <$stringWithAttrs> vs <$stringWithoutAttrs>"
      else
        stringWithAttrs

    c.Expr[String](Literal(Constant(string)))
  }


  def scalaWithoutBug11609Impl(c: blackbox.Context)(code: c.Expr[String]): c.Expr[Unit] = {
    import c.universe._

    val scalaBug11609 =
      c.classPath exists { url =>
        val file = url.getFile
        val index = file indexOf "/scala-library-"
        val start = index + 15
        val end = index + 19
        index != -1 && end <= file.length && (file.substring(start, end) == "2.13")
      }

    code.tree match {
      case Literal(Constant(code: String)) =>
        if (scalaBug11609)
          c.Expr[Unit](Literal(Constant(())))
        else
          c.Expr[Unit](c parse code)

      case _ =>
        c.abort(c.enclosingPosition, "string literal required")
    }
  }


  def retyperImpl[T](c: blackbox.Context)(code: c.Expr[T]): c.Expr[T] =
    c.Expr[T](retyperTypecheck(c)(code.tree))

  def retyperAllImpl[T](c: blackbox.Context)(code: c.Expr[T]): c.Expr[T] =
    c.Expr[T](retyperAllTypecheck(c)(code.tree))


  private def retyperTypecheck(c: blackbox.Context)(tree: c.Tree): c.Tree = {
    val t = c.retyper
    if (!c.hasErrors)
      t untypecheck (t typecheck (t untypecheck (t typecheck (t untypecheck tree))))
    else
      tree
  }

  private def retyperAllTypecheck(c: blackbox.Context)(tree: c.Tree): c.Tree = {
    val t = c.retyper
    if (!c.hasErrors)
      t untypecheckAll (t typecheck (t untypecheckAll (t typecheck (t untypecheckAll tree))))
    else
      tree
  }
}
