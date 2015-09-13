package retypecheck

import org.scalamacros.resetallattrs._
import scala.reflect.macros.blackbox.Context
import scala.reflect.macros.TypecheckException

object Typer {
  def apply[C <: Context](c: C): Typer[c.type] =
    new Typer[c.type](c)
}

class Typer[C <: Context](val c: C) {
  import c.universe._
  import Flag._

  /**
   * Re-type-checks the given tree, i.e., first un-type-checks it and then
   * type-checks it again using [[untypecheck]] and [[typecheck]], respectively.
   */
  def retypecheck(tree: Tree): Tree =
    typecheck(untypecheck(tree))

  /**
   * Re-type-checks the given tree resetting all symbols using the
   * `org.scalamacros.resetallattrs` library, i.e., first un-type-checks it and
   * then type-checks it again using [[untypecheckAll]] and [[typecheck]],
   * respectively.
   */
  def retypecheckAll(tree: Tree): Tree =
    typecheck(untypecheckAll(tree))

  /**
   * Type-checks the given tree. If type-checking fails, aborts the macro
   * expansion issuing the type-checking error.
   *
   * The type-checking process distorts certain ASTs (such as representations of
   * extractors, lazy values or case classes) in a way that they cannot be
   * type-checked again. The issue is described in
   * [[https://issues.scala-lang.org/browse/SI-5464 SI-5465]].
   *
   * This method tries to restore the AST to its original form, which can be
   * type-checked again.
   */
  def typecheck(tree: Tree): Tree = {
    try
      fixTypecheck(
        (syntheticParamListMarker transform
          (c typecheck
            (nonSyntheticParamListMarker transform tree))),
        abortWhenUnfixable = false)
    catch {
      case TypecheckException(pos, msg) =>
        c.abort(pos.asInstanceOf[Position], msg)
    }
  }

  /**
   * Un-type-checks the given tree.
   *
   * The type-checking process distorts certain ASTs (such as representations of
   * extractors, lazy values or case classes) in a way that they cannot be
   * type-checked again. The issue is described in
   * [[https://issues.scala-lang.org/browse/SI-5464 SI-5465]].
   *
   * This method tries to restore the AST to its original form, which can be
   * type-checked again, or abort the macro expansion if this is not possible.
   */
  def untypecheck(tree: Tree): Tree =
    c untypecheck
      (typeApplicationCleaner transform
        (syntheticParamListCleaner transform
          fixTypecheck(tree, abortWhenUnfixable = true)))

  /**
   * Un-type-checks the given tree resetting all symbols using the
   * `org.scalamacros.resetallattrs` library.
   *
   * The type-checking process distorts certain ASTs (such as representations of
   * extractors, lazy values or case classes) in a way that they cannot be
   * type-checked again. The issue is described in
   * [[https://issues.scala-lang.org/browse/SI-5464 SI-5465]].
   *
   * This method tries to restore the AST to its original form, which can be
   * type-checked again, or abort the macro expansion if this is not possible.
   */
  def untypecheckAll(tree: Tree): Tree =
    c resetAllAttrs
      (typeApplicationCleaner transform
        (syntheticParamListCleaner transform
          fixTypecheck(tree, abortWhenUnfixable = true)))

  /**
   * Cleans the flag set of the given modifiers.
   *
   * The type-checking process annotates definitions with various flags. Some
   * of them can also be inserted by user-code or even have a corresponding
   * Scala language construct, but others are only used by the type-checker.
   * Certain flags can interfere with type-checking and cause it to fail. Those
   * flags can be safely removed and will be re-inserted during type-checking
   * when needed.
   *
   * This method eliminates some problematic cases.
   */
  def cleanModifiers(mods: Modifiers): Modifiers = {
    val possibleFlags = Seq(ABSTRACT, ARTIFACT, BYNAMEPARAM, CASE, CASEACCESSOR,
      CONTRAVARIANT, COVARIANT, DEFAULTINIT, DEFAULTPARAM, DEFERRED, ENUM,
      FINAL, IMPLICIT, LAZY, LOCAL, MACRO, MUTABLE, OVERRIDE, PARAM,
      PARAMACCESSOR, PRESUPER, PRIVATE, PROTECTED, SEALED, SYNTHETIC)

    val flags = possibleFlags.fold(NoFlags) { (flags, flag) =>
      if (mods hasFlag flag) flags | flag else flags
    }
    
    Modifiers(flags, mods.privateWithin, mods.annotations)
  }

  /**
   * Creates an AST representing the given type.
   *
   * The type-checking process creates synthetic type trees and it is possible
   * to insert trees with type information, but it is not easily possible to
   * create an AST for a given type.
   *
   * This method attempts to create such an AST, which is persistent across
   * type-checking and un-type-checking.
   */
  def createTypeTree(tpe: Type): Tree = {
    def isClass(symbol: Symbol): Boolean =
      symbol.isClass && !symbol.isModule && !symbol.isPackage

    def expandSymbol(symbol: Symbol): Tree = {
      if (symbol.owner != NoSymbol)
        Select(expandSymbol(symbol.owner), symbol.name.toTermName)
      else
        Ident(termNames.ROOTPKG)
    }

    def expandType(tpe: Type): Tree = tpe match {
      case ThisType(pre) if isClass(pre) =>
        This(pre.asType.name)

      case ThisType(pre) =>
        expandSymbol(pre)

      case TypeRef(NoPrefix, sym, args) =>
        val ident = Ident(sym.name.toTypeName)
        if (!args.isEmpty)
          AppliedTypeTree(ident, args map expandType)
        else
          ident

      case TypeRef(pre, sym, args) =>
        val preTree = expandType(pre)

        val typeProjection = preTree match {
          case This(_) => false
          case _ => isClass(pre.typeSymbol)
        }

        val select =
          if (typeProjection) SelectFromTypeTree(preTree, sym.name.toTypeName)
          else Select(preTree, sym.name.toTypeName)

        if (!args.isEmpty)
          AppliedTypeTree(select, args map expandType)
        else
          select

      case SingleType(NoPrefix, sym) =>
        SingletonTypeTree(Ident(sym.name.toTermName))

      case SingleType(pre, sym) =>
        val preTree = expandType(pre) match {
          case SingletonTypeTree(pre) => pre
          case pre => pre
        }

        SingletonTypeTree(Select(preTree, sym.name.toTermName))

      case _ =>
        TypeTree(tpe)
    }

    expandType(tpe)
  }


  private case object NonSyntheticParamList

  private case object SyntheticParamList

  private object nonSyntheticParamListMarker extends Transformer {
    override def transform(tree: Tree) = tree match {
      case tree @ Apply(_, _) =>
        internal updateAttachment (tree, NonSyntheticParamList)
        super.transform(tree)

      case _ =>
        super.transform(tree)
    }
  }

  private object syntheticParamListMarker extends Transformer {
    override def transform(tree: Tree) = tree match {
      case tree @ Apply(_, _) =>
        val hasImplicitParamList =
          tree.symbol != null &&
          tree.symbol.isMethod &&
          (tree.symbol.asMethod.paramLists.lastOption flatMap {
            _.headOption map { _.isImplicit }
          } getOrElse false)

        val isNonSyntheticParamList =
          (internal attachments tree).get[NonSyntheticParamList.type].nonEmpty

        internal removeAttachment[NonSyntheticParamList.type] tree

        if (hasImplicitParamList && !isNonSyntheticParamList)
          internal updateAttachment (tree, SyntheticParamList)

        super.transform(tree)

      case _ =>
        super.transform(tree)
    }
  }

  private object syntheticParamListCleaner extends Transformer {
    override def transform(tree: Tree) = tree match {
      case tree @ Apply(fun, _) =>
        if ((internal attachments tree).get[SyntheticParamList.type].nonEmpty)
          super.transform(fun)
        else
          super.transform(tree)

      case _ =>
        super.transform(tree)
    }
  }


  object typeApplicationCleaner extends Transformer {
    object typeApplicationCleaner extends Transformer {
      override def transform(tree: Tree) = tree match {
        case tree: TypeTree =>
          if (tree.original != null)
            transform(tree.original)
          else
            internal setType (tree, null)
        case _ =>
          super.transform(tree)
      }
    }

    override def transform(tree: Tree) = tree match {
      case AppliedTypeTree(tpt, args) =>
        internal setPos (
          AppliedTypeTree(
            transform(tpt), args map typeApplicationCleaner.transform),
          tree.pos)
      case TypeApply(fun, args) =>
        internal setPos (
          TypeApply(
            transform(fun), args map typeApplicationCleaner.transform),
          tree.pos)
      case _ =>
        super.transform(tree)
    }
  }


  private def fixTypecheck(tree: Tree, abortWhenUnfixable: Boolean): Tree = {
    val rhss = (tree collect {
      case valDef @ ValDef(_, _, _, _) if valDef.symbol.isTerm =>
        val term = valDef.symbol.asTerm
        List(term.getter -> valDef, term.setter -> valDef)
    }).flatten.toMap - NoSymbol

    object typecheckFixer extends Transformer {
      override def transform(tree: Tree) = tree match {
        // fix extractors
        case UnApply(
            Apply(fun, List(Ident(TermName("<unapply-selector>")))), args) =>
          fun collect {
            case Select(fun, TermName("unapply" | "unapplySeq")) => fun
          } match {
            case fun :: _ =>
              val apply = Apply(fun, args)
              internal setType (apply, fun.tpe)
              internal setPos (apply, fun.pos)
              super.transform(apply)
            case _ =>
              super.transform(tree)
          }

        // fix vars, vals and lazy vals
        case ValDef(_, _, _, _)
            if tree.symbol.isTerm && {
              val term = tree.symbol.asTerm
              term.getter != NoSymbol &&
                (term.isLazy || (rhss contains term.getter))
            } =>
          EmptyTree

        // fix vars and vals
        case DefDef(_, _, _, _, _, _)
            if tree.symbol.isTerm && {
              val term = tree.symbol.asTerm
              term.isSetter && (rhss contains term)
            } =>
          EmptyTree
        case defDef @ DefDef(mods, name, _, _, tpt, _)
            if tree.symbol.isTerm && {
              val term = tree.symbol.asTerm
              !term.isLazy && term.isGetter && (rhss contains term)
            } =>
          val valDef = rhss(tree.symbol)
          val flags = cleanModifiers(mods).flags |
            (if (valDef.symbol.asTerm.isVar) MUTABLE else NoFlags)
          val privateWithin =
            if (defDef.symbol.asTerm.privateWithin != NoSymbol)
              defDef.symbol.asTerm.privateWithin.name
            else
              mods.privateWithin
          val newValDef = ValDef(
            Modifiers(
              flags, privateWithin, mods.annotations),
              name, super.transform(valDef.tpt), super.transform(valDef.rhs))
          internal setType (newValDef, valDef.tpe)
          internal setPos (newValDef, valDef.pos)

        // fix lazy vals
        case defDef @ DefDef(mods, name, _, _, tpt, rhs)
            if tree.symbol.isTerm && {
              val term = tree.symbol.asTerm
              term.isLazy && term.isGetter
            } =>
          val assignment = rhs collect {
            case Assign(_, rhs) => rhs
          } match {
            case rhs :: _ => rhs
            case _ => rhs
          }
          val flags = cleanModifiers(mods).flags
          val privateWithin =
            if (defDef.symbol.asTerm.privateWithin != NoSymbol)
              defDef.symbol.asTerm.privateWithin.name
            else
              mods.privateWithin
          val valDef = rhss get tree.symbol
          val typeTree = valDef map { _.tpt } getOrElse tpt
          val newValDef = ValDef(
            Modifiers(
              flags, privateWithin, mods.annotations),
              name, super.transform(typeTree), super.transform(assignment))
          valDef map { valDef =>
            internal setType (newValDef, valDef.tpe)
            internal setPos (newValDef, valDef.pos)
          } getOrElse newValDef

        // abort on case class
        case ClassDef(mods, _, _, _)
            if (mods hasFlag CASE) && abortWhenUnfixable =>
          c.abort(tree.pos, "case class not allowed inside macro application")
          EmptyTree

        case _ =>
          super.transform(tree)
      }
    }

    typecheckFixer transform tree
  }
}
