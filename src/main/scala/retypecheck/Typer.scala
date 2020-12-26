package retypecheck

import org.scalamacros.resetallattrs._
import scala.collection.mutable
import scala.reflect.internal.util
import scala.reflect.macros.blackbox
import scala.reflect.macros.TypecheckException

object ReTyper {
  def apply(c: blackbox.Context) = new ReTyper[c.type](c)
}

/**
 * heavy wizardry to fight the dark forces of Scala type-checking in macros
 */
class ReTyper[+C <: blackbox.Context](val c: C) {
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
   * [[https://issues.scala-lang.org/browse/SI-5464 SI-5464]].
   */
  def typecheck(tree: Tree): Tree =
    try {
      val typecheckedTree = if (tree.tpe != null) tree else c typecheck tree
      assignMissingTypes(
        maskUnusedImports(
          fixTypecheck(
            selfReferenceFixer transform typecheckedTree)))
    }
    catch {
      case TypecheckException(pos: Position @unchecked, msg) =>
        c.abort(pos, msg)
      case TypecheckException(_, msg) =>
        c.abort(c.enclosingPosition, msg)
    }

  /**
   * Un-type-checks the given tree.
   *
   * The type-checking process distorts certain ASTs (such as representations of
   * extractors, lazy values or case classes) in a way that they cannot be
   * type-checked again. The issue is described in
   * [[https://issues.scala-lang.org/browse/SI-5464 SI-5464]].
   *
   * This method tries to restore the AST to a form, which can be type-checked
   * again.
   */
  def untypecheck(tree: Tree): Tree =
    fixUntypecheck(
      c untypecheck
        maskUnusedImports(
          fixTypesAndSymbols(
            fixCaseClasses(
              fixTypecheck(tree)))))

  /**
   * Un-type-checks the given tree resetting all symbols using the
   * `org.scalamacros.resetallattrs` library.
   *
   * The type-checking process distorts certain ASTs (such as representations of
   * extractors, lazy values or case classes) in a way that they cannot be
   * type-checked again. The issue is described in
   * [[https://issues.scala-lang.org/browse/SI-5464 SI-5464]].
   *
   * This method tries to restore the AST to a form, which can be type-checked
   * again.
   */
  def untypecheckAll(tree: Tree): Tree =
    fixUntypecheck(
      c resetAllAttrs
        (selfReferenceFixer transform
          fixTypesAndSymbols(
            fixCaseClasses(
              fixTypecheck(tree)))))

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
      CONTRAVARIANT, COVARIANT, DEFAULTINIT, DEFAULTPARAM, DEFERRED, FINAL,
      IMPLICIT, LAZY, LOCAL, MACRO, MUTABLE, OVERRIDE, PARAM, PARAMACCESSOR,
      PRESUPER, PRIVATE, PROTECTED, SEALED, SYNTHETIC)

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
  def createTypeTree(tpe: Type, pos: Position): Tree =
    createTypeTree(tpe, pos, Set.empty)

  def createTypeTree(tpe: Type, pos: Position,
      owners: collection.Set[Symbol]): Tree = {
    val allOwners = ownerChain(c.internal.enclosingOwner).toSet union owners

    def expandPrefix(pre: Type, sym: Symbol) =
      if (pre != NoPrefix) {
        val preTree = expandType(pre)

        val preSymbolSingle = pre match {
          case SingleType(_, sym) => sym
          case _ => pre.typeSymbol
        }

        val prePathTree = preTree match {
          case SingletonTypeTree(Select(qualifier, name)) =>
            Some(Select(qualifier, name.toTermName))
          case SingletonTypeTree(Ident(name)) =>
            Some(Ident(name.toTermName))
          case Select(qualifier, name) =>
            Some(Select(qualifier, name.toTermName))
          case Ident(name) =>
            Some(Ident(name.toTermName))
          case _ =>
            None
        }

        val pathTree = prePathTree match {
          case Some(Select(tree, termNames.PACKAGE)) if tree.symbol.isPackage =>
            tree

          case Some(tree) =>
            val preSymbol = pre.typeSymbol
            if (preSymbol.isModuleClass) {
              val symbol = preSymbol.asClass.module
              val owner = symbol.owner
              val sym =
                if (symbol.name == termNames.PACKAGE && owner.isModuleClass)
                  owner.asClass.module
                else
                  symbol
              tree.withAttrs(sym, sym.typeSignature, pos)
            }
            else
              tree.withAttrs(preSymbolSingle, pre, pos)

          case _ =>
            preTree
        }

        val typeProjection = preTree match {
          case This(_) =>
            false
          case _ =>
            preSymbolSingle.isType &&
              !preSymbolSingle.isModule &&
              !preSymbolSingle.isModuleClass &&
              !preSymbolSingle.isPackage &&
              !preSymbolSingle.isPackageClass &&
              sym.isType
        }

        if (typeProjection)
          SelectFromTypeTree(preTree, sym.asType.name)
        else
          Select(pathTree, sym.name)
      }
      else
        Ident(sym.name)

    def expandType(tpe: Type): Tree = {
      val tree = tpe match {
        case ThisType(pre)
            if pre.isType && !pre.isModule && !pre.isPackage &&
               pre.name.toString != "<refinement>" =>
          This(pre.asType.name).withAttrs(tpe.typeSymbol, tpe, pos)

        case ThisType(pre)
            if isAccessible(pre, allOwners) &&
               pre.name.toString != "<refinement>" =>
          expandSymbol(pre, pos)

        case TypeRef(pre, sym, args) if sym.name.toString != "<refinement>" =>
          val module = if (sym.isModuleClass) sym.asClass.module else sym
          val prefix = expandPrefix(pre, module)
          val symbol = if (isNonRepresentableType(tpe)) NoSymbol else module
          if (args.nonEmpty)
            AppliedTypeTree(
                prefix.withAttrs(
                  symbol, internal.typeRef(pre, sym, List.empty), pos),
                args map expandType).withAttrs(
              symbol, tpe, pos)
          else if (sym.isModuleClass)
            SingletonTypeTree(prefix.withAttrs(symbol, tpe, pos))
          else
            prefix.withAttrs(symbol, tpe, pos)

        case SingleType(pre, sym) if sym.name.toString != "<refinement>" =>
          val module = if (sym.isModuleClass) sym.asClass.module else sym
          expandPrefix(pre, module) match {
            case Select(prefix, termNames.PACKAGE) if pre.typeSymbol.isPackage =>
              prefix
            case prefix =>
              SingletonTypeTree(prefix.withAttrs(module, tpe, pos))
          }

        case TypeBounds(lo, hi) =>
          TypeBoundsTree(
            if (lo =:= definitions.NothingTpe) EmptyTree else expandType(lo),
            if (hi =:= definitions.AnyTpe) EmptyTree else expandType(hi))

        case ExistentialType(quantified, underlying) =>
          val whereClauses = quantified map { quantified =>
            quantified.typeSignature match {
              case TypeBounds(_, _) =>
                val mods = Modifiers(
                  DEFERRED | (if (quantified.isSynthetic) SYNTHETIC else NoFlags))

                Some(TypeDef(
                  mods,
                  quantified.name.toTypeName,
                  List.empty,
                  expandType(quantified.typeSignature)))

              case _ =>
                None
            }
          }

          if (whereClauses exists { _.isEmpty })
            TypeTree(tpe)
          else
            ExistentialTypeTree(expandType(underlying), whereClauses.flatten)

        case ClassInfoType(parents, decls, typeSymbol) =>
          val publicDecls = internal.newScopeWith(decls.toSeq filter { symbol =>
            symbol.isPublic && !symbol.isConstructor
          }: _*)
          expandType(internal.refinedType(parents, publicDecls, typeSymbol))

        case RefinedType(parents, scope) =>
          def refiningType(sym: TypeSymbol): TypeDef = {
            val anyTree = Select(
              Select(
                Ident(termNames.ROOTPKG),
                TermName("scala")),
              TypeName("Any"))

            val (tree, deferred) = expandType(sym.typeSignature) match {
              case `anyTree` if sym.isClass =>
                TypeBoundsTree(EmptyTree, EmptyTree) -> true
              case tree if sym.isClass =>
                TypeBoundsTree(EmptyTree, tree) -> true
              case tree @ TypeBoundsTree(_, _) =>
                tree -> true
              case tree =>
                tree -> false
            }

            TypeDef(
              if (deferred) Modifiers(DEFERRED) else Modifiers(),
              sym.name,
              sym.typeParams map { param => refiningType(param.asType) },
              tree)
          }

          def refiningVal(sym: TermSymbol, flags: FlagSet): ValDef =
            ValDef(
              Modifiers(flags),
              sym.name,
              expandType(sym.typeSignature.finalResultType),
              EmptyTree)

          def refiningDef(sym: MethodSymbol, flags: FlagSet): DefDef =
            DefDef(
              Modifiers(flags),
              sym.name,
              sym.typeParams map { param => refiningType(param.asType) },
              sym.paramLists map {
                _ map { param => refiningVal(param.asTerm, PARAM) }
              },
              expandType(sym.typeSignature.finalResultType),
              EmptyTree)

          val body = scope map { symbol =>
            if (symbol.isMethod) {
              val method = symbol.asMethod
              if (method.isStable)
                Some(refiningVal(method, DEFERRED))
              else
                Some(refiningDef(method, DEFERRED))
            }
            else if (symbol.isType)
              Some(refiningType(symbol.asType))
            else
              None
          }

          if (body exists { _.isEmpty })
            TypeTree(tpe)
          else if (body.isEmpty && parents.size == 1)
            expandType(parents.head)
          else
            CompoundTypeTree(
              Template(parents map expandType, noSelfType, body.toList.flatten))

        case _ =>
          TypeTree(tpe)
      }

      if (tree.tpe == null)
        internal.setType(tree, tpe)
      internal.setPos(tree, pos)
    }

    expandType(tpe)
  }


  private def assignMissingTypes(tree: Tree): tree.type = {
    def typeDef(tree: TypeDef, symbol: TypeSymbol): Unit = {
      if (tree.tparams.size == symbol.typeParams.size)
        (tree.tparams zip symbol.typeParams) foreach { case (tree, symbol) =>
          if (tree.name == symbol.name)
            typeDef(tree, symbol.asType)
        }

      if (tree.rhs.tpe == null)
        internal.setType(tree.rhs, symbol.typeSignature)
    }

    def valDef(tree: ValDef, symbol: TermSymbol): Unit = {
      if (tree.tpt.tpe == null)
        internal.setType(tree.tpt, symbol.typeSignature.finalResultType)
    }

    def defDef(tree: DefDef, symbol: MethodSymbol): Unit = {
      val method = symbol.asMethod

      if (tree.tparams.size == method.typeParams.size)
        (tree.tparams zip method.typeParams) foreach { case (tree, symbol) =>
          if (tree.name == symbol.name)
            typeDef(tree, symbol.asType)
        }

      if (tree.vparamss.size == method.paramLists.size)
        (tree.vparamss zip method.paramLists) foreach { case (trees, symbols) =>
          (trees zip symbols) foreach { case (tree, symbol) =>
            if (tree.name == symbol.name)
              valDef(tree, symbol.asTerm)
          }
        }

      if (tree.tpt.tpe == null)
        internal.setType(tree.tpt, symbol.typeSignature.finalResultType)
    }

    def members(trees: Iterable[Tree], symbols: Iterable[Symbol]): Unit =
      if (trees.size == symbols.size)
        (trees zip symbols) foreach {
          case (tree: TypeDef, symbol)
              if symbol.isType && tree.name == symbol.name =>
            typeDef(tree, symbol.asType)
          case (tree: ValDef, symbol)
              if symbol.isTerm && tree.name == symbol.name =>
            valDef(tree, symbol.asTerm)
          case (tree: DefDef, symbol)
              if symbol.isMethod && tree.name == symbol.name =>
            defDef(tree, symbol.asMethod)
          case _ =>
        }

    object traverser extends Traverser {
      override def traverse(tree: Tree): Unit = {
        (tree, tree.tpe) match {
          case (tree: TypeTree, tpe) if tree.original != null =>
            if (tree.original.tpe == null)
              internal.setType(tree.original, tpe)
            traverse(tree.original)

          case (CompoundTypeTree(Template(parents, _, body)),
                RefinedType(parentTypes, decls)) =>
            members(body, decls)
            if (parents.size == parentTypes.size)
              (parents zip parentTypes) foreach { case (tree, tpe) =>
                if (tree.tpe == null)
                  internal.setType(tree, tpe)
              }

          case (ExistentialTypeTree(tpt, whereClauses),
                ExistentialType(quantified, underlying)) =>
            members(whereClauses, quantified)
            if (tpt.tpe == null)
              internal.setType(tpt, underlying)

          case (TypeBoundsTree(lo, hi),
                TypeBounds(loType, hiType)) =>
            if (lo.tpe == null)
              internal.setType(lo, loType)
            if (hi.tpe == null)
              internal.setType(hi, hiType)

          case (AppliedTypeTree(tpt, args),
                TypeRef(pre, sym, typeArgs)) =>
            if (args.size == typeArgs.size)
              (args zip typeArgs) foreach { case (tree, tpe) =>
                if (tree.tpe == null)
                  internal.setType(tree, tpe)
              }
            if (tpt.tpe == null)
              internal.setType(tpt, internal.typeRef(pre, sym, List.empty))

          case (Select(qualifier, _),
                TypeRef(pre, _, _)) if qualifier.tpe == null =>
            internal.setType(qualifier, pre)

          case _ =>
        }

        super.traverse(tree)
      }
    }

    traverser traverse tree

    tree
  }


  private def expandSymbol(symbol: Symbol, pos: Position): Tree = {
    val tree =
      if (symbol == c.mirror.RootClass)
        Ident(termNames.ROOTPKG)
      else if (!symbol.owner.isModule &&
               !symbol.owner.isModuleClass &&
               !symbol.owner.isPackage &&
               !symbol.owner.isPackageClass)
        Ident(symbol.name.toTermName)
      else
        Select(expandSymbol(symbol.owner, pos), symbol.name.toTermName)

    val sym = if (symbol.isModuleClass) symbol.asClass.module else symbol
    tree.withAttrs(sym, sym.typeSignature, pos)
  }

  private def ownerChain(symbol: Symbol): List[Symbol] =
    if (symbol.owner == NoSymbol)
      Nil
    else
      symbol :: ownerChain(symbol.owner)

  private def isAccessible(symbol: Symbol, owners: collection.Set[Symbol]) =
    ownerChain(symbol).toSet diff owners forall { symbol =>
      !symbol.isPrivate && !symbol.isProtected
    }

  private def isNonRepresentableType(tpe: Type) = tpe match {
    case TypeRef(NoPrefix, name, _) => name.toString endsWith ".type"
    case _ => false
  }


  private def fixTypesAndSymbols(tree: Tree): Tree = {
    val extractNamedArg =
      try {
        val namedArgClass = Class.forName(s"scala.reflect.internal.Trees$$NamedArg")
        val namedArg = c.universe.getClass.getMethod("NamedArg").invoke(c.universe)
        val unapply = namedArg.getClass.getMethod("unapply", namedArgClass)

        Some({ tree: Tree =>
          if (namedArgClass isInstance tree)
            try unapply.invoke(namedArg, tree) match {
              case Some((lhs: Tree, rhs: Tree)) => Some((lhs, rhs))
              case _ => None
            }
            catch { case _: ReflectiveOperationException => None }
          else
            None
        })
      }
      catch { case _: ReflectiveOperationException => None }

    object NamedArg {
      def unapply(tree: Tree): Option[(Tree, Tree)] =
        extractNamedArg flatMap { _(tree) }
    }

    val nowarn =
      try Some(c.mirror.staticClass("_root_.scala.annotation.nowarn").toType)
      catch { case _: ScalaReflectionException => None }

    val scala2134plus = c.classPath exists { url =>
      val file = url.getFile
      val index = file indexOf "/scala-library-"
      val start = index + 15
      val end = index + 22
      val version =
        if (index != -1 && end <= file.length)
          file.substring(start, end)
        else
          ""
      (version startsWith "2.13.") &&
      version != "2.13.0." && version != "2.13.0-" &&
      version != "2.13.1." && version != "2.13.1-" &&
      version != "2.13.2." && version != "2.13.2-" &&
      version != "2.13.3." && version != "2.13.3-"
    }

    val definedTypeSymbols = {
      var symbols = Set.empty[TypeSymbol]

      var currentSymbols = (tree collect {
        case tree @ TypeDef(_, _, _, _) if tree.symbol.isType =>
          tree.symbol.asType
        case tree @ ClassDef(_, _, _, _) if tree.symbol.isClass =>
          tree.symbol.asType
        case tree @ ModuleDef(_, _, _) if tree.symbol.isModule =>
          tree.symbol.asModule.moduleClass.asType
      }).toSet

      var foundAdditionals = true
      while (foundAdditionals) {
        symbols ++= currentSymbols
        currentSymbols = currentSymbols flatMap {
          _.toType.members collect {
            case symbol if symbol.isType => symbol.asType
          }
        }
        foundAdditionals = (currentSymbols -- symbols).nonEmpty
      }

      symbols
    }

    val owners = ownerChain(c.internal.enclosingOwner)

    val classNestingSet = mutable.Set(owners: _*)

    val classNestingList =
      mutable.ListBuffer(owners.reverse map { _.name.toTypeName }: _*)

    val shadowedNames = mutable.ListBuffer.empty[Set[Name]]

    def prependRootPackage(tree: Tree): Tree = tree match {
      case Ident(name) if tree.symbol.owner == c.mirror.RootClass =>
        Select(Ident(termNames.ROOTPKG), name)
      case Select(qualifier, name) =>
        Select(prependRootPackage(qualifier), name)
      case _ =>
        tree
    }

    def isTypeUnderExpansion(tpe: Type) = {
      val tpes = mutable.ListBuffer(tpe)
      val seen = mutable.Set.empty[Type]
      var underExpansion = false

      while (!underExpansion && tpes.nonEmpty)
        tpes.remove(0) exists { tpe =>
          if (!(seen contains tpe)) {
            if (!(definedTypeSymbols exists { tpe contains _ })) {
              seen += tpe
              val dealias = tpe.dealias
              val widen = tpe.widen
              if (tpe ne dealias)
                tpes += dealias
              if (tpe ne widen)
                tpes += widen
            }
            else
              underExpansion = true
          }

          underExpansion
        }

      underExpansion
    }

    def hasNonRepresentableType(trees: List[Tree]) = trees exists { tree =>
      val isWildcardType = tree match {
        case tree: TypeTree
            if tree.tpe != null &&
               tree.original == null &&
               tree.symbol.isType &&
               tree.symbol.asType.isExistential =>
          val str = tree.tpe.toString
          str == "_" || (str startsWith "_$")
        case _ =>
          false
      }

      isWildcardType ||
      tree.tpe != null && (tree.tpe exists isNonRepresentableType)
    }

    def removeAnnotationsToBeRemoved(tpe: Type): Type = tpe map {
      case AnnotatedType(annotations, underlying) =>
        val annots = annotations filterNot { annotation =>
          isAnnotationToBeRemoved(annotation.tree)
        }

        if (annots.nonEmpty)
          internal.annotatedType(annots, underlying)
        else
          underlying

      case tpe =>
        tpe
    }

    def isAnnotationToBeRemoved(tree: Tree): Boolean = tree match {
      case Apply(
          Select(New(tpt), termNames.CONSTRUCTOR),
          List(arg)) =>
        val condition = arg match {
          case Literal(Constant(value: String)) => value
          case NamedArg(_, Literal(Constant(value: String))) => value
          case _ => ""
        }

        def hasCondition = Seq(
          "early initializers",
          "initializers are deprecated",
          "trait parameters",
          "avoiding var",
          "val in traits") exists { condition contains _ }

        def isNowarn = nowarn exists { nowarn =>
          val isNowarn = tree.tpe match {
            case null if scala2134plus =>
              val name = tpt.toString
              name == "nowarn" || (name endsWith ".nowarn")
            case tpe if tpe != null && tpe <:< nowarn =>
              true
            case AnnotatedType(annotation :: _, _) =>
              annotation.tree.tpe != null && annotation.tree.tpe <:< nowarn
            case _ =>
              false
          }

          if (tree.tpe != null)
            internal.setType(tree, removeAnnotationsToBeRemoved(tree.tpe))

          isNowarn
        }

        hasCondition && isNowarn

      case _ =>
        false
    }

    object typesAndSymbolsFixer extends Transformer {
      def templateNames(impl: Template) = {
        (impl.parents flatMap { parent =>
          if (parent.symbol != null && parent.symbol.isType)
            parent.symbol.asType.toType.members collect {
              case symbol if symbol.isTerm => symbol.name
            }
          else
            Iterable.empty
        }) ++
        (impl.body collect {
          case defTree: DefTree if defTree.isTerm => defTree.name
        }) ++
        (if (impl.self.name != termNames.EMPTY) Some(impl.self.name) else None)
      }.toSet

      override def transformModifiers(mods: Modifiers) = {
        val annotations = mods.annotations filterNot isAnnotationToBeRemoved
        if (annotations.size != mods.annotations.size)
          Modifiers(mods.flags, mods.privateWithin, annotations)
        else
          mods
      }

      override def transform(tree: Tree) = tree match {
        case tree: TypeTree =>
          if (tree.original != null)
            transform(prependRootPackage(tree.original))
          else if (tree.tpe != null && isTypeUnderExpansion(tree.tpe)) {
            val typeTree = createTypeTree(tree.tpe, tree.pos, classNestingSet)

            val hasNonRepresentableType = typeTree exists {
              case _: TypeTree => true
              case _ => false
            }

            if (hasNonRepresentableType)
              TypeTree()
            else
              transform(typeTree)
          }
          else if (tree.tpe != null)
            TypeTree(removeAnnotationsToBeRemoved(tree.tpe))
          else
            TypeTree()

        case Annotated(annot, arg) if isAnnotationToBeRemoved(annot) =>
          if (arg.tpe != null)
            transform(internal.setType(
              arg,
              removeAnnotationsToBeRemoved(arg.tpe)))
          else
            transform(arg)

        case ClassDef(_, tpname, _, impl) =>
          classNestingList.prepend(tpname)
          if (tree.symbol.isClass)
            classNestingSet += tree.symbol

          shadowedNames.prepend(templateNames(impl))

          val classDef = super.transform(tree)

          shadowedNames.remove(0)

          if (tree.symbol.isClass)
            classNestingSet -= tree.symbol
          classNestingList.remove(0)

          classDef

        case ModuleDef(_, tname, impl) =>
          classNestingList.prepend(tname.toTypeName)
          if (tree.symbol.isModule)
            classNestingSet += tree.symbol.asModule.moduleClass

          shadowedNames.prepend(templateNames(impl))

          val moduleDef = super.transform(tree)

          shadowedNames.remove(0)

          if (tree.symbol.isModule)
            classNestingSet -= tree.symbol.asModule.moduleClass
          classNestingList.remove(0)

          moduleDef

        case DefDef(mods, termNames.CONSTRUCTOR, tparams, vparamss, _, rhs) =>
          shadowedNames.prepend((vparamss.flatten map { _.name }).toSet)

          val defDef = DefDef(
            transformModifiers(mods), termNames.CONSTRUCTOR,
            transformTypeDefs(tparams), transformValDefss(vparamss),
            TypeTree(), transform(rhs))

          shadowedNames.remove(0)

          defDef.withAttrs(
            tree.symbol,
            tree.tpe,
            if (tree.symbol.pos != NoPosition) tree.symbol.pos else tree.pos)

        case DefDef(mods, name, tparams, vparamss, tpt, EmptyTree) =>
          val typeSignature = tree.symbol.typeSignature
          val emptyTypeTree = tpt match {
            case tree: TypeTree => tree.tpe == null && tree.original == null
            case EmptyTree => true
            case _ => false
          }

          shadowedNames.prepend((vparamss.flatten map { _.name }).toSet + name)

          val defDef =
            if (emptyTypeTree && typeSignature != null && typeSignature != NoType)
              DefDef(
                transformModifiers(mods), name,
                transformTypeDefs(tparams), transformValDefss(vparamss),
                TypeTree(tree.symbol.typeSignature.finalResultType),
                EmptyTree).withAttrs(
                  tree.symbol, tree.tpe, tree.pos)
            else
              super.transform(tree)

          shadowedNames.remove(0)
          defDef

        case ValDef(mods, name, tpt, EmptyTree) =>
          val typeSignature = tree.symbol.typeSignature
          val emptyTypeTree = tpt match {
            case tree: TypeTree => tree.tpe == null && tree.original == null
            case EmptyTree => true
            case _ => false
          }

          shadowedNames.prepend(Set(name))

          val result =
            if (emptyTypeTree && typeSignature != null && typeSignature != NoType)
              ValDef(
                transformModifiers(mods), name,
                TypeTree(tree.symbol.typeSignature.finalResultType),
                EmptyTree).withAttrs(
                  tree.symbol, tree.tpe, tree.pos)
            else
              super.transform(tree)

          shadowedNames.remove(0)
          result

        case _: ValDef | _: DefDef | _: CaseDef | _: Function | _: Block =>
          val names = tree match {
            case ValDef(_, name, _, _) =>
              List(name)
            case DefDef(_, name, _, vparamss, _, _) =>
              name :: (vparamss.flatten map { _.name })
            case CaseDef(pat, _, _) =>
              pat collect { case Bind(name, _) => name }
            case Function(vparams, _) =>
              vparams map { _.name }
            case Block(stats, _) =>
              stats collect {
                case defTree: DefTree if defTree.isTerm => defTree.name
              }
            case _ =>
              List.empty
          }

          shadowedNames.prepend(names.toSet)

          val result = super.transform(tree)

          shadowedNames.remove(0)
          result

        case TypeApply(fun, targs) =>
          if (hasNonRepresentableType(targs))
            transform(fun)
          else
            super.transform(tree)

        case Apply(
              typeApply @ TypeApply(
                select @ Select(qual, TermName(name)),
                List(targ)),
              List())
            if name == "$" + "isInstanceOf" =>
          super.transform(
            TypeApply(
                Select(qual, TermName("isInstanceOf")).withAttrs(
                  NoSymbol, select.tpe, select.pos),
                List(targ)).withAttrs(
              NoSymbol, typeApply.tpe, typeApply.pos))

        case Apply(fun, args)
            if tree.symbol != null && tree.symbol.isMethod && {
              val paramLists = tree.symbol.asMethod.paramLists
              paramLists.size == 1 &&
                paramLists.head.nonEmpty &&
                paramLists.head.head.isImplicit
            } =>
          val hasNonRepresentableTypeArg = args exists {
            case Apply(TypeApply(_, targs), _) =>
              hasNonRepresentableType(targs)
            case TypeApply(_, targs) =>
              hasNonRepresentableType(targs)
            case _ =>
              false
          }

          if (hasNonRepresentableTypeArg)
            transform(fun)
          else
            super.transform(tree)

        case Select(This(_), termNames.CONSTRUCTOR) =>
          Ident(termNames.CONSTRUCTOR)

        case Select(qualifier @ This(qual), name) =>
          val fixedEnclosingSelfReference =
            if (!qualifier.symbol.isModuleClass &&
                !(shadowedNames exists { _ contains name }))
              classNestingList collectFirst {
                case outerName if outerName == qualifier.symbol.name =>
                  Ident(name)
              }
            else
              None

          fixedEnclosingSelfReference getOrElse {
            if (qual != typeNames.EMPTY &&
                qualifier.symbol.isModuleClass &&
                isAccessible(tree.symbol, classNestingSet) &&
                !isTypeUnderExpansion(qualifier.symbol.asType.toType) &&
                !(classNestingSet contains qualifier.symbol))
              Select(expandSymbol(qualifier.symbol, qualifier.pos), name)
            else
              super.transform(tree)
          }

        case Select(_, _) | Ident(_) | This(_)
            if tree.tpe != null && isTypeUnderExpansion(tree.tpe) =>
          internal.setSymbol(tree, NoSymbol)
          super.transform(tree)

        case _ =>
          super.transform(tree)
      }
    }

    typesAndSymbolsFixer transform tree
  }


  private def fixCaseClasses(tree: Tree): Tree = {
    case object CaseClassMarker

    val symbols = mutable.Set.empty[Symbol]

    val scala213 = c.classPath exists { url =>
      val file = url.getFile
      val index = file indexOf "/scala-library-"
      val start = index + 15
      val end = index + 19
      index != -1 && end <= file.length && file.substring(start, end) == "2.13"
    }

    val syntheticMethodNames =
      if (scala213)
        Set("apply", "unapply")
      else
        Set("apply", "canEqual", "copy", "equals",
            "hashCode", "productArity", "productElement", "productIterator",
            "productPrefix", "readResolve", "toString", "unapply")

    def isSyntheticMethodName(name: TermName) =
      (syntheticMethodNames contains name.toString) ||
      ((name.toString startsWith "copy$") && !(name.toString endsWith "$macro"))

    object caseClassFixer extends Transformer {
      def resetCaseImplBody(body: List[Tree]) =
        body filterNot {
          case DefDef(mods, name, _, _, _, _) =>
            (mods hasFlag SYNTHETIC) && isSyntheticMethodName(name)
          case _ => false
        }

      def resetCaseImplDef(implDef: ImplDef) = implDef match {
        case ModuleDef(mods, name, Template(parents, self, body)) =>
          val moduleDef = ModuleDef(mods, name,
            Template(parents, self, resetCaseImplBody(body)))

          internal.updateAttachment(moduleDef, CaseClassMarker)
          moduleDef.withAttrs(implDef.symbol, implDef.tpe, implDef.pos)

        case ClassDef(mods, tpname, tparams, Template(parents, self, body)) =>
          val classDef = ClassDef(mods, tpname, tparams,
            Template(parents, self, resetCaseImplBody(body)))

          internal.updateAttachment(classDef, CaseClassMarker)
          classDef.withAttrs(implDef.symbol, implDef.tpe, implDef.pos)

        case _ =>
          implDef
      }

      def fixCaseClasses(trees: List[Tree]) = {
        val names = (trees collect {
          case ClassDef(mods, tpname, _, _) if mods hasFlag CASE =>
            tpname.toTermName
        }).toSet

        val companions = (trees collect {
          case ModuleDef(_, name, _) if names contains name =>
            name.toTypeName
        }).toSet

        val havingCompanions = (trees collect {
          case classDef @ ClassDef(_, tpname, _, _) if companions contains tpname =>
            tpname -> classDef
        }).toMap

        val reorderedTrees =
          trees flatMap {
            case tree @ ClassDef(_, tpname, _, _) =>
              havingCompanions get tpname map { _ =>
                Seq.empty
              } getOrElse Seq(tree)
            case tree @ ModuleDef(_, name, _) =>
              havingCompanions get name.toTypeName map { classDef =>
                Seq(tree, classDef)
              } getOrElse Seq(tree)
            case tree =>
              Seq(tree)
          }

        symbols ++= (reorderedTrees collect {
          case tree @ ClassDef(mods, _, _, _)
              if tree.symbol != NoSymbol &&
                 (mods hasFlag CASE) =>
            Seq(tree.symbol)
          case tree @ ModuleDef(mods, name, _)
              if tree.symbol != NoSymbol &&
                 ((mods hasFlag CASE) || (names contains name)) =>
            Seq(tree.symbol, tree.symbol.asModule.moduleClass)
        }).flatten

        reorderedTrees map {
          case tree @ ModuleDef(mods, name, _)
              if (mods hasFlag CASE) || (names contains name) =>
            resetCaseImplDef(tree)
          case tree @ ClassDef(mods, _, _, _)
              if mods hasFlag CASE =>
            resetCaseImplDef(tree)
          case tree =>
            tree
        }
      }

      override def transform(tree: Tree) = tree match {
        case Template(parents, self, body) =>
          super.transform(Template(parents, self, fixCaseClasses(body)))
        case Block(stats, expr) =>
          val fixedExpr :: fixedStats = fixCaseClasses(expr :: stats): @unchecked
          super.transform(Block(fixedStats, fixedExpr))
        case _ =>
          super.transform(tree)
      }
    }

    object caseClassReferenceFixer extends Transformer {
      val owners = mutable.Set.empty[Symbol]

      def symbolsContains(symbol: Symbol): Boolean =
        symbol != null && symbol != NoSymbol &&
        ((symbols contains symbol) || symbolsContains(symbol.owner))

      override def transform(tree: Tree) = {
        val owner = tree match {
          case ClassDef(_, _, _, _) if tree.symbol.isClass =>
            Some(tree.symbol)
          case ModuleDef(_, _, _) if tree.symbol.isModule =>
            Some(tree.symbol.asModule.moduleClass)
          case _ =>
            None
        }

        owner foreach { owners += _ }

        val result = tree match {
          case _ if internal.attachments(tree).contains[CaseClassMarker.type] =>
            internal.removeAttachment[CaseClassMarker.type](tree)
            tree
          case tree: TypeTree if symbolsContains(tree.symbol) =>
            createTypeTree(tree.tpe, tree.pos, owners)
          case _ if symbolsContains(tree.symbol) =>
            super.transform(internal.setSymbol(tree, NoSymbol))
          case _ =>
            super.transform(tree)
        }

        owner foreach { owners -= _ }

        result
      }
    }

    caseClassReferenceFixer transform (caseClassFixer transform tree)
  }


  private def selfReferenceFixer = new Transformer {
    type ImplEnv = (Symbol, TypeName, Set[Name], Option[ValDef])
    type BlockEnv = Set[Name]

    val ownerStack: List[Either[ImplEnv, BlockEnv]] =
      (ownerChain(c.internal.enclosingOwner)
        filter { symbol =>
          (symbol.isClass || symbol.isModuleClass) &&
          !symbol.isPackage &&
          !symbol.isPackageClass
        }
        map { symbol =>
          val typeSymbol = symbol.asType
          val names = (typeSymbol.toType.members map { _.name }).toSet[Name]
          Left((symbol, typeSymbol.name, names, None))
        })

    val stack = mutable.ListBuffer(ownerStack: _*)

    override def transform(tree: Tree) = tree match {
      case tree: TypeTree =>
        if (tree.original != null)
          internal.setOriginal(tree, transform(tree.original))
        tree

      case implDef: ImplDef =>
        val symbol =
          if (implDef.symbol.isModule)
            implDef.symbol.asModule.moduleClass
          else
            implDef.symbol

        val declNames =
          (implDef.impl.parents flatMap { parent =>
            if (parent.symbol != null && parent.symbol.isType)
              parent.symbol.asType.toType.members map { _.name }
            else
              Iterable.empty
          }) ++
          (implDef.impl.body collect {
            case defTree: DefTree => defTree.name
          })

        val memberNames =
          if (symbol.isType)
            (symbol.asType.toType.members map { _.name }).toSet[Name]
          else
            Set.empty

        val names = declNames.toSet ++ memberNames

        val self =
          if (implDef.impl.self.name != termNames.EMPTY &&
              implDef.impl.self.name != termNames.WILDCARD)
            Some(implDef.impl.self)
          else
            None

        stack.prepend(Left((symbol, implDef.name.toTypeName, names, self)))

        val result = super.transform(implDef)

        stack.remove(0)
        result

      case _: ValDef | _: DefDef | _: CaseDef | _: Function | _: Block =>
        val names = tree match {
          case ValDef(_, name, _, _) =>
            List(name)
          case DefDef(_, name, _, vparamss, _, _) =>
            name :: (vparamss.flatten map { _.name })
          case CaseDef(pat, _, _) =>
            pat collect { case Bind(name, _) => name }
          case Function(vparams, _) =>
            vparams map { _.name }
          case Block(stats, _) =>
            stats collect { case defTree: DefTree => defTree.name }
          case _ =>
            List.empty
        }

        stack.prepend(Right(names.toSet))

        val result = super.transform(tree)

        stack.remove(0)
        result

      case Select(thisTree @ This(thisName), selectedName) =>
        trait Modification
        case class Self(self: ValDef) extends Modification
        case object Unqualified extends Modification
        case object Stable extends Modification
        case object Unmodified extends Modification

        val (modification, _, _) =
          stack.foldLeft[(Modification, Set[Name], Boolean)](
              (Stable, Set.empty, true)) {
            case ((modification, shadowed, innerImpl),
                  Left((symbol, name, names, self))) =>
              val updatedShadowed = shadowed ++ names

              val updatedModification =
                if (symbol != NoSymbol &&
                    symbol == thisTree.symbol &&
                    modification == Unmodified) {
                  if ((updatedShadowed contains selectedName) ||
                      tree.symbol == NoSymbol)
                    (self
                      filterNot { updatedShadowed contains _.name }
                      map Self
                      getOrElse Unmodified)
                  else
                    Unqualified
                }
                else if ((name == thisName ||
                         (innerImpl && thisName == typeNames.EMPTY)) &&
                          modification == Stable) {
                  if ((updatedShadowed contains selectedName) ||
                      tree.symbol == NoSymbol)
                    Unmodified
                  else
                    Unqualified
                }
                else
                  modification

              (self
                map { self =>
                  (updatedModification, updatedShadowed + self.name, false)
                }
                getOrElse {
                  (updatedModification, updatedShadowed, false)
                })

            case ((modification, shadowed, innerImpl), Right(names)) =>
              (modification, shadowed ++ names, innerImpl)
          }

        val result = modification match {
          case Self(self) =>
            val qualifier = Ident(self.name).withAttrs(
              self.symbol,
              thisTree.tpe,
              if (thisTree.pos != NoPosition) thisTree.pos else tree.pos)
            Some(Select(qualifier, selectedName))
          case Unqualified =>
            Some(Ident(selectedName))
          case Stable if thisTree.symbol != NoSymbol =>
            val qualifier = expandSymbol(thisTree.symbol, thisTree.pos)
            Some(Select(qualifier, selectedName))
          case _ =>
            None
        }

        result map {
          _.withAttrs(
            tree.symbol,
            tree.tpe,
            if (tree.pos != NoPosition) tree.pos else thisTree.pos)
        } getOrElse tree

      case _ =>
        super.transform(tree)
    }
  }

  private implicit class TreeOps(tree: Tree) {
    def withAttrs(symbol: Symbol, tpe: Type, pos: Position) =
      internal.setPos(
        internal.setType(
          internal.setSymbol(tree, symbol), tpe), pos)
    def equalsTypedStructure(other: Tree) =
      (tree equalsStructure other) &&
      ((tree collect { case tree: TypeTree => tree }).zipAll
       (other collect { case tree: TypeTree => tree }, null, null)
       forall { case (tree, other) =>
         tree != null && other != null &&
          (tree.tpe != null && tree.tpe =:= other.tpe || other.tpe == null)
       })
  }

  private implicit class TermOps(term: TermSymbol) {
    def getterOrNoSymbol =
      try term.getter
      catch { case _: reflect.internal.Symbols#CyclicReference => NoSymbol }
    def setterOrNoSymbol =
      try term.setter
      catch { case _: reflect.internal.Symbols#CyclicReference => NoSymbol }
  }

  private def fixTypecheck(tree: Tree): Tree = {
    val definedSymbols = (tree collect {
      case tree: DefTree if tree.symbol != null && tree.symbol != NoSymbol =>
        tree.symbol
    }).toSet

    val rhss = (tree collect {
      case valDef @ ValDef(_, _, _, _) if valDef.symbol.isTerm =>
        val term = valDef.symbol.asTerm
        val getter = term.getterOrNoSymbol
        val setter = term.setterOrNoSymbol

        val getterDef =
          if (getter != NoSymbol && getter != term)
            List(getter -> valDef)
          else
            List.empty

        val setterDef =
          if (setter != NoSymbol && setter != term)
            List(setter -> valDef)
          else
            List.empty

        getterDef ++ setterDef
    }).flatten.toMap

    def defaultArgDef(defDef: DefDef): Boolean = {
      val nameString = defDef.name.toString
      val symbol = defDef.symbol.owner.owner

      val isDefaultArg =
        (nameString contains "$default$") &&
        ((nameString endsWith "$macro") ||
         (defDef.mods hasFlag (SYNTHETIC | DEFAULTPARAM)))

      val isConstructorInsideExpression =
        (nameString startsWith termNames.CONSTRUCTOR.encodedName.toString) &&
        !symbol.isClass && !symbol.isModule && !symbol.isModuleClass

      symbol != NoSymbol && isDefaultArg && !isConstructorInsideExpression
    }

    val definedDefaultArgs = tree collect {
      case defDef @ DefDef(_, _, _, _, _, _) if defaultArgDef(defDef) =>
        defDef.symbol
    }

    val accessedDefaultArgs = (tree collect {
      case select @ Select(_, _) if definedDefaultArgs contains select.symbol =>
        select.symbol
    }).toSet

    def processDefaultArgs(stats: List[Tree]) = {
      val macroDefaultArgs = mutable.ListBuffer.empty[DefDef]
      val macroDefaultArgsPending = mutable.Map.empty[String, Option[DefDef]]

      val processedStats = stats map {
        case defDef @ DefDef(_, name, tparams, vparamss, tpt, rhs)
            if defaultArgDef(defDef) =>
          val nameString = name.toString
          val macroDefDef = DefDef(
            Modifiers(SYNTHETIC),
            TermName(s"$nameString$$macro"),
            tparams, vparamss, tpt, rhs)

          if (nameString endsWith "$macro") {
            if (accessedDefaultArgs contains defDef.symbol)
              macroDefaultArgsPending.getOrElseUpdate(
                nameString.substring(0, nameString.length - 6),
                None) foreach { macroDefaultArgs += _ }
            EmptyTree
          }
          else {
            if ((accessedDefaultArgs contains defDef.symbol) ||
                (macroDefaultArgsPending contains nameString))
              macroDefaultArgs += macroDefDef
            else
              macroDefaultArgsPending += nameString -> Some(macroDefDef)
            defDef
          }

        case stat =>
          stat
      }

      processedStats ++ macroDefaultArgs
    }

    def applyMetaProperties(from: Tree, to: Tree) = {
      if (from.symbol != null)
        internal.setSymbol(to, from.symbol)
      internal.setType(internal.setPos(to, from.pos), from.tpe)
    }

    object typecheckFixer extends Transformer {
      def fixModifiers(mods: Modifiers, symbol: Symbol,
          annotations: List[Annotation] = List.empty): Modifiers = {
        val flags = cleanModifiers(mods).flags

        val allAnnotations =
          (mods.annotations ++
           (symbol.annotations map { _.tree }) ++
           (annotations map { _.tree }))
            .foldLeft(List.empty[Tree]) { (all, tree) =>
              val transformedTree = transform(tree)
              if (all exists { _ equalsTypedStructure transformedTree })
                all
              else
                transformedTree :: all
            }.reverse

        Modifiers(flags, mods.privateWithin, allAnnotations)
      }

      override def transform(tree: Tree) = tree match {
        case tree: TypeTree =>
          if (tree.original != null)
            internal.setOriginal(tree, transform(tree.original))
          tree

        // workaround for default arguments
        case Template(parents, self, body) =>
          super.transform(
            applyMetaProperties(
              tree, Template(parents, self, processDefaultArgs(body))))

        case tree @ Block(stats, expr) =>
          val defaultArgStats = stats map {
            case tree @ ValDef(mods, name, tpt, rhs)
                if (mods hasFlag ARTIFACT) &&
                   !(name.toString startsWith "default$") =>
              val defaultName = TermName(s"default$$$name")
              val valDef = ValDef(mods, defaultName, tpt, rhs).withAttrs(
                tree.symbol, tree.tpe, tree.pos)
              Some(valDef -> (name -> tree.symbol -> defaultName))
            case _ =>
              None
          }

          val renamedDefaultArgs =
            if (!(defaultArgStats contains None)) {
              val names = (defaultArgStats collect {
                case Some((_, mapping)) => mapping
              }).toMap

              object transformer extends Transformer {
                override def transform(tree: Tree) = tree match {
                  case tree @ Ident(name: TermName) =>
                    val defaultName = names.getOrElse(name -> tree.symbol, name)
                    Ident(defaultName).withAttrs(
                      tree.symbol, tree.tpe, tree.pos)
                  case tree =>
                    super.transform(tree)
                }
              }

              Block(
                defaultArgStats collect { case Some((stat, _)) =>
                  transformer transform stat
                },
                transformer transform expr)
            }
            else
              tree

          val block = processDefaultArgs(
            renamedDefaultArgs.expr :: renamedDefaultArgs.stats)

          super.transform(
            applyMetaProperties(tree, Block(block.tail, block.head)))

        case Select(qualifier, name)
            if accessedDefaultArgs contains tree.symbol =>
          val macroName =
            if (name.toString endsWith "$macro") name
            else TermName(s"${name.toString}$$macro")
          super.transform(Select(qualifier, macroName))

        case Ident(name) if tree.symbol.isTerm && name == tree.symbol.name =>
          val expandedTree = expandSymbol(tree.symbol, tree.pos)
          if (expandedTree exists { definedSymbols contains _.symbol })
            tree
          else
            expandedTree

        // fix renamed imports
        case Select(qual, _) if tree.symbol != NoSymbol =>
          super.transform(
            applyMetaProperties(tree, Select(qual, tree.symbol.name)))

        // fix extractors
        case UnApply(
            Apply(fun, List(Ident(TermName("<unapply-selector>")))), args) =>
          fun collect {
            case Select(fun, TermName("unapply" | "unapplySeq")) => fun
          } match {
            case Seq(fun) =>
              transform(applyMetaProperties(fun, Apply(fun, args)))
            case _ =>
              super.transform(tree)
          }

        // fix vars, vals and lazy vals
        case ValDef(_, _, TypeTree(), rhs)
            if tree.symbol.isTerm && {
              val term = tree.symbol.asTerm
              (term.isLazy && term.isImplementationArtifact && rhs.isEmpty) ||
              (term.isPrivateThis &&
                (rhss contains term.asTerm.getterOrNoSymbol))
            } =>
          EmptyTree
        case DefDef(_, _, _, _, _, _)
            if tree.symbol.isTerm && tree.symbol.asTerm.isSetter =>
          EmptyTree

        // fix vars and vals
        case defDef @ DefDef(mods, name, _, _, _, _)
            if tree.symbol.isTerm && {
              val term = tree.symbol.asTerm
              !term.isLazy && term.isGetter
            } =>
          val term = tree.symbol.asTerm
          val valDef = rhss.getOrElse(tree.symbol, defDef)
          val valAnnotations = (rhss get tree.symbol).toList flatMap {
            _.symbol.annotations }
          val newMods = Modifiers(
            mods.flags
              | (if (valDef.mods hasFlag PRESUPER) PRESUPER else NoFlags)
              | (if (!valDef.symbol.asTerm.isStable) MUTABLE else NoFlags)
              | (if (term.isPrivate) PRIVATE else NoFlags)
              | (if (term.isProtected) PROTECTED else NoFlags)
              | (if (term.isPrivateThis ||
                     term.isProtectedThis) LOCAL else NoFlags),
            if (mods.privateWithin != typeNames.EMPTY)
              mods.privateWithin
            else if (defDef.symbol.privateWithin != NoSymbol)
              defDef.symbol.privateWithin.name
            else
              typeNames.EMPTY,
            mods.annotations)
          val newValDef = ValDef(
            fixModifiers(newMods, defDef.symbol, valAnnotations),
            name, transform(valDef.tpt), transform(valDef.rhs))
          newValDef.withAttrs(defDef.symbol, valDef.tpe, valDef.pos)

        // fix lazy vals
        case valOrDefDef: ValOrDefDef
            if tree.symbol.isTerm && {
              val term = tree.symbol.asTerm
              term.isLazy && term.isGetter
            } =>
          val mods = valOrDefDef.mods
          val (lazyValAnnotations, assignment) = valOrDefDef.rhs collect {
            case Assign(lhs, rhs) => lhs.symbol.annotations -> rhs
          } match {
            case (annotations, rhs) :: _ => annotations -> rhs
            case _ => List.empty -> valOrDefDef.rhs
          }
          val valDef = rhss get tree.symbol
          val typeTree = valDef map { _.tpt } getOrElse valOrDefDef.tpt
          val valAnnotations = lazyValAnnotations ++ 
            (valDef.toList flatMap { _.symbol.annotations })
          val newMods = Modifiers(
            mods.flags,
            if (mods.privateWithin != typeNames.EMPTY)
              mods.privateWithin
            else if (valOrDefDef.symbol.privateWithin != NoSymbol)
              valOrDefDef.symbol.privateWithin.name
            else
              typeNames.EMPTY,
            mods.annotations)
          val newValDef = ValDef(
            fixModifiers(newMods, valOrDefDef.symbol, valAnnotations),
            valOrDefDef.name, transform(typeTree), transform(assignment))
          newValDef.withAttrs(
            valOrDefDef.symbol,
            valDef map { _.tpe } getOrElse valOrDefDef.tpe,
            valDef map { _.pos } getOrElse valOrDefDef.pos
          )

        // fix vals
        case valDef @ ValDef(mods, name, tpt, rhs)
            if tree.symbol.isTerm =>
          val newValDef = ValDef(
            fixModifiers(mods, valDef.symbol), name,
            transform(tpt),
            transform(rhs))
          newValDef.withAttrs(valDef.symbol, valDef.tpe, valDef.pos)

        // fix defs
        case defDef @ DefDef(mods, name, tparams, vparamss, tpt, rhs)
            if tree.symbol.isTerm =>
          val newDefDef = DefDef(
            fixModifiers(mods, defDef.symbol), name,
            transformTypeDefs(tparams),
            transformValDefss(vparamss),
            transform(tpt),
            transform(rhs))
          internal.setType(newDefDef, defDef.tpe)
          internal.setPos(newDefDef, defDef.pos)
          if (tree.symbol.name != TermName("$init$"))
            internal.setSymbol(newDefDef, defDef.symbol)
          else
            newDefDef

        // fix classes
        case classDef @ ClassDef(mods, name, tparams, impl)
            if tree.symbol.isClass =>
          impl.body exists {
            case tree if tree.symbol != null && tree.symbol.isConstructor =>
              internal.setPos(tree, classDef.pos)
              true
            case _ =>
              false
          }

          val newClassDef = ClassDef(
            fixModifiers(mods, classDef.symbol), name,
            transformTypeDefs(tparams),
            transformTemplate(impl))
          newClassDef.withAttrs(classDef.symbol, classDef.tpe, classDef.pos)

        // fix objects
        case moduleDef @ ModuleDef(mods, name, impl)
            if tree.symbol.isModule =>
          val newModuleDef = ModuleDef(
            fixModifiers(mods, moduleDef.symbol), name,
            transformTemplate(impl))
          newModuleDef.withAttrs(moduleDef.symbol, moduleDef.tpe, moduleDef.pos)

        // fix type definitions
        case typeDef @ TypeDef(mods, name, tparams, rhs)
            if tree.symbol.isType =>
          val newTypeDef = TypeDef(
            fixModifiers(mods, typeDef.symbol), name,
            transformTypeDefs(tparams),
            transform(rhs))
          newTypeDef.withAttrs(typeDef.symbol, typeDef.tpe, typeDef.pos)

        case _ =>
          super.transform(tree)
      }
    }

    tree match {
      case _: ImplDef if tree.pos == NoPosition =>
        internal.setPos(tree, c.enclosingPosition)
      case _ =>
    }

    typecheckFixer transform tree
  }

  private class UndefinedOffsetPosition(
    override val source: util.SourceFile,
    override val point: Int)
      extends util.Position {
    override val start = point
    override val end = point
  }

  private class UndefinedPosition extends util.Position {
    override def source = util.NoSourceFile
    override def point = fail("start")
    override def start = fail("point")
    override def end = fail("end")
  }

  private def maskUnusedImports(tree: Tree): Tree = {
    def maskPosition(tree: Tree) = {
      val pos =
        if (tree.pos != NoPosition)
          new UndefinedOffsetPosition(tree.pos.source, tree.pos.point)
        else
          new UndefinedPosition

      pos match {
        case pos: Position => internal.setPos(tree, pos)
        case _ => tree
      }
    }

    def isPositionMasked(tree: Tree) = tree.pos match {
      case (_: UndefinedOffsetPosition) | (_: UndefinedPosition) => true
      case _ => false
    }

    object importMasker extends Traverser {
      type Imports = List[List[(Tree, Set[Symbol])]]

      var imports: Imports = List(List.empty)
      var hasImports = false

      def maskImports(symbol: Symbol) = {
        def maskImports(imports: Imports): Imports =
          imports match {
            case Nil =>
              Nil

            case Nil :: scopes =>
              Nil :: maskImports(scopes)

            case ((info @ (tree, symbols)) :: scope) :: scopes =>
              if (symbols contains symbol) {
                hasImports = hasImports || (scope :: scopes exists { _.nonEmpty })
                maskPosition(tree)
                scope :: scopes
              }
              else {
                hasImports = true
                val imports = maskImports(scope :: scopes)
                (info :: imports.head) :: imports.tail
              }
          }

        if (hasImports) {
          hasImports = false
          imports = maskImports(imports)
        }
      }

      override def traverse(tree: Tree) = tree match {
        case tree: TypeTree if tree.original != null =>
          traverse(tree.original)

        case Import(expr, selectors)
            if expr.symbol != NoSymbol && !isPositionMasked(tree) =>
          if (selectors forall { _.namePos != -1 }) {
            val (importedNames, hiddenNames, isWildcard) =
              selectors.foldLeft[(Set[String], Set[String], Boolean)](
                  (Set.empty, Set.empty, false)) {
                case ((importedNames, hiddenNames, isWildcard), selector) =>
                  if (selector.name != termNames.WILDCARD) {
                    if (selector.rename != termNames.WILDCARD)
                      (importedNames + selector.name.toString,
                       hiddenNames,
                       isWildcard)
                    else
                      (importedNames,
                       hiddenNames + selector.name.toString,
                       isWildcard)
                  }
                  else
                    (importedNames, hiddenNames, true)
              }

            val members =
              expr.symbol.info.members filter { symbol =>
                symbol.isPublic && !symbol.isConstructor
              }

            val imported =
              if (!isWildcard)
                members filter { importedNames contains _.name.toString }
              else
                members

            val unhidden =
              imported filterNot { hiddenNames contains _.name.toString }

            hasImports = true
            imports = (tree -> unhidden.toSet :: imports.head) :: imports.tail
          }
          else
            maskPosition(tree)

        case Select(_, _) if hasImports =>
          def traversePath(tree: Tree, found: Boolean): Unit = tree match {
            case Select(qualifier, name) =>
              if (!found && tree.pos == qualifier.pos) {
                if (tree.symbol != NoSymbol)
                  maskImports(tree.symbol)

                traversePath(
                  qualifier,
                  found = name.toString != "apply" && name.toString != "update")
              }
              else
                traversePath(qualifier, found)

            case _ =>
              traverse(tree)
          }

          traversePath(tree, found = false)

        case Block(_, _) | Template(_, _, _) =>
          imports ::= List.empty
          super.traverse(tree)
          imports = imports.tail

        case _ =>
          super.traverse(tree)
      }
    }

    importMasker traverse tree

    tree
  }

  private def fixUntypecheck(tree: Tree): Tree = {
    def replaceSuperCall(call: Tree, replacement: Tree): Option[Tree] =
      call match {
        case Apply(fun, args) =>
          replaceSuperCall(fun, replacement) map { Apply(_, args) }
        case TypeApply(fun, args) =>
          replaceSuperCall(fun, replacement) map { TypeApply(_, args) }
        case Select(Super(_, _), termNames.CONSTRUCTOR) =>
          Some(replacement)
        case _ =>
          None
      }

    object untypecheckFixer extends Transformer {
      override def transform(tree: Tree) = tree match {
        case tree: TypeTree =>
          if (tree.original != null)
            internal.setOriginal(tree, transform(tree.original))
          tree

        case Apply(fun, _) if fun.symbol != null && fun.symbol.isModule =>
          internal.setSymbol(fun, NoSymbol)
          super.transform(tree)

        case Typed(expr, tpt) =>
          tpt match {
            case Function(List(), EmptyTree) =>
              super.transform(tree)

            case Annotated(_, arg)
                if expr != null && arg != null &&
                   (expr equalsTypedStructure arg) =>
              super.transform(tpt)

            case tpt: TypeTree
                if (tpt.original match {
                  case Annotated(_, _) => true
                  case _ => false
                }) =>
              super.transform(tpt.original)

            case tpt if !tpt.isType =>
              super.transform(expr)

            case _ =>
              super.transform(tree)
          }

        case Ident(_) =>
          if (tree.symbol.isTerm && tree.symbol.asTerm.isLazy)
            internal.setSymbol(tree, NoSymbol)
          else
            tree

        case DefDef(mods, name, _, _, _, _)
            if (mods hasFlag (SYNTHETIC | DEFAULTPARAM)) &&
               (name.toString contains "$default$") &&
               !(name.toString endsWith "$macro") =>
          EmptyTree

        case ClassDef(mods, tpname, tparams, Template(parents, self, body))
            if (tpname.toString startsWith "$anon") && parents.nonEmpty =>
          val fixedPreSuperBody = body map {
            case tree @ ValDef(mods, name, tpt, rhs) if mods hasFlag PRESUPER =>
              tpt match {
                case _: TypeTree =>
                  tree
                case tpt =>
                  ValDef(mods, name, internal.setOriginal(TypeTree(), tpt), rhs)
              }
            case tree =>
              tree
          }

          if (body != fixedPreSuperBody) {
            val (fixedBody, fixedParentOptions) = (fixedPreSuperBody map {
              case tree @ DefDef(
                  mods, termNames.CONSTRUCTOR, tparams, vparamss, tpt, rhs) =>
                rhs match {
                  case Block(stats, expr) =>
                    val (fixedStats, fixedParentOptions) = (stats map { stat =>
                      val fixedParent = replaceSuperCall(stat, parents.head)
                      if (fixedParent.nonEmpty)
                        pendingSuperCall -> fixedParent
                      else
                        stat -> fixedParent
                    }).unzip

                    val fixedParent =
                      fixedParentOptions collect { case Some(parent) => parent }

                    if (fixedParent.size == 1)
                      DefDef(
                        mods, termNames.CONSTRUCTOR, tparams, vparamss, tpt,
                        Block(fixedStats, expr)) -> fixedParent.headOption
                    else
                      tree -> None

                  case tree =>
                    tree -> None
                }

              case tree =>
                tree -> None
            }).unzip

            val fixedParent =
              fixedParentOptions collect { case Some(parent) => parent }

            if (fixedParent.size == 1)
              super.transform(
                ClassDef(mods, tpname, tparams,
                  Template(fixedParent.head :: parents.tail, self, fixedBody)))
            else
              super.transform(tree)
          }
          else
            super.transform(tree)

        case ModuleDef(_, _, Template(_, _, _)) =>
          super.transform(tree) match {
            case ModuleDef(mods, _, Template(parents, `noSelfType`,
                  List() |
                  List(DefDef(_, termNames.CONSTRUCTOR,
                    List(), List(List()), _, _))))
                if (mods hasFlag SYNTHETIC) &&
                   (parents.isEmpty ||
                     (parents.size == 1 &&
                      parents.head.tpe != null &&
                      parents.head.tpe =:= definitions.AnyRefTpe)) =>
              EmptyTree

            case tree =>
              tree
          }

        case _ =>
          super.transform(tree)
      }
    }

    untypecheckFixer transform tree
  }
}
