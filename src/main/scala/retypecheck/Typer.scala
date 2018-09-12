package retypecheck

import org.scalamacros.resetallattrs._
import scala.collection.mutable
import scala.reflect.macros.blackbox.Context
import scala.reflect.macros.TypecheckException

object ReTyper {
  def apply(c: Context) = new ReTyper[c.type](c)
}

/**
 * heavy wizardry to fight the dark forces of Scala type-checking in macros
 */
class ReTyper[+C <: Context](val c: C) {
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
    try fixTypecheck(selfReferenceFixer transform (c typecheck tree))
    catch {
      case TypecheckException(pos, msg) =>
        c.abort(pos.asInstanceOf[Position], msg)
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
        fixTypesAndSymbols(
          fixCaseClasses(
            fixTypecheck(tree))))

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
              tree withAttrs (sym, sym.typeSignature, pos)
            }
            else
              tree withAttrs (pre.typeSymbol, pre, pos)

          case _ =>
            preTree
        }

        val typeProjection = preTree match {
          case This(_) =>
            false
          case _ =>
            val preSymbol = pre.typeSymbol
            preSymbol.isType &&
              !preSymbol.isModule &&
              !preSymbol.isModuleClass &&
              !preSymbol.isPackage &&
              !preSymbol.isPackageClass &&
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
        case ThisType(pre) if pre.isType && !pre.isModule && !pre.isPackage =>
          This(pre.asType.name) withAttrs (tpe.typeSymbol, tpe, pos)

        case ThisType(pre) if isAccessible(pre, allOwners) =>
          expandSymbol(pre, pos)

        case TypeRef(pre, sym, args) =>
          val prefix = expandPrefix(pre, sym)
          if (!args.isEmpty)
            AppliedTypeTree(
                prefix withAttrs (
                  sym, internal typeRef (pre, sym, List.empty), pos),
                args map expandType) withAttrs (
              sym, tpe, pos)
          else
            prefix withAttrs (sym, tpe, pos)

        case SingleType(pre, sym) =>
          expandPrefix(pre, sym) match {
            case Select(prefix, termNames.PACKAGE) if pre.typeSymbol.isPackage =>
              prefix
            case prefix =>
              SingletonTypeTree(prefix withAttrs (sym, tpe, pos))
          }

        case TypeBounds(lo, hi) =>
          TypeBoundsTree(
            if (lo =:= definitions.NothingTpe) EmptyTree else expandType(lo),
            if (hi =:= definitions.AnyTpe) EmptyTree else expandType(hi))

        case ExistentialType(quantified, underlying) =>
          object singletonTypeNameFixer extends Transformer {
            override def transform(tree: Tree) = tree match {
              case Ident(TypeName(name)) if name endsWith ".type" =>
                Ident(TermName(name substring (0, name.size - 5)))
              case _ =>
                super.transform(tree)
            }
          }

          val whereClauses = quantified map { quantified =>
            quantified.typeSignature match {
              case TypeBounds(lo, hi) =>
                val name = quantified.name.toString
                val mods = Modifiers(
                  DEFERRED | (if (quantified.isSynthetic) SYNTHETIC else NoFlags))

                if (!(name endsWith ".type"))
                  Some(TypeDef(
                    mods,
                    TypeName(name),
                    List.empty,
                    expandType(quantified.typeSignature)))
                else if (lo =:= definitions.NothingTpe)
                  Some(ValDef(
                    mods,
                    TermName(name substring (0, name.size - 5)),
                    expandType(hi),
                    EmptyTree))
                else
                  None

              case _ =>
                None
            }
          }

          if (whereClauses exists { _.isEmpty })
            TypeTree(tpe)
          else
            ExistentialTypeTree(
              singletonTypeNameFixer transform expandType(underlying),
              whereClauses.flatten)

        case ClassInfoType(parents, decls, typeSymbol) =>
          val publicDecls = internal newScopeWith (decls.toSeq filter { symbol =>
            symbol.isPublic && !symbol.isConstructor
          }: _*)
          expandType(internal refinedType (parents, publicDecls, typeSymbol))

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
        internal setType (tree, tpe)
      internal setPos (tree, pos)
    }

    expandType(tpe)
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
    tree withAttrs (sym, sym.typeSignature, pos)
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


  private def fixTypesAndSymbols(tree: Tree): Tree = {
    val definedTypeSymbols = (tree collect {
      case tree @ TypeDef(_, _, _, _) if tree.symbol.isType =>
        tree.symbol
      case tree @ ClassDef(_, _, _, _) if tree.symbol.isClass =>
        tree.symbol
      case tree @ ModuleDef(_, _, _) if tree.symbol.isModule =>
        tree.symbol.asModule.moduleClass
    }).toSet

    val owners = ownerChain(c.internal.enclosingOwner)

    val classNestingSet = mutable.Set(owners: _*)

    val classNestingList =
      mutable.ListBuffer((owners.reverse map { _.name.toTypeName }): _*)

    def prependRootPackage(tree: Tree): Tree = tree match {
      case Ident(name) if tree.symbol.owner == c.mirror.RootClass =>
        Select(Ident(termNames.ROOTPKG), name)
      case Select(qualifier, name) =>
        Select(prependRootPackage(qualifier), name)
      case _ =>
        tree
    }

    def isTypeUnderExpansion(tpe: Type) = tpe exists {
      definedTypeSymbols contains _.typeSymbol
    }

    def hasNonRepresentableType(trees: List[Tree]) = trees exists { tree =>
      tree.tpe != null && (tree.tpe exists {
        case TypeRef(NoPrefix, name, List()) => name.toString endsWith ".type"
        case _ => false
      })
    }

    object typesAndSymbolsFixer extends Transformer {
      override def transform(tree: Tree) = tree match {
        case tree: TypeTree =>
          if (tree.original != null)
            transform(prependRootPackage(tree.original))
          else if (tree.tpe != null && isTypeUnderExpansion(tree.tpe))
            createTypeTree(tree.tpe, tree.pos, classNestingSet)
          else
            TypeTree(tree.tpe)

        case ClassDef(_, tpname, _, _) =>
          classNestingList prepend tpname
          if (tree.symbol.isClass)
            classNestingSet += tree.symbol

          val classDef = super.transform(tree)

          if (tree.symbol.isClass)
            classNestingSet -= tree.symbol
          classNestingList remove 0

          classDef

        case ModuleDef(_, tname, _) =>
          classNestingList prepend tname.toTypeName
          if (tree.symbol.isModule)
            classNestingSet += tree.symbol.asModule.moduleClass

          val moduleDef = super.transform(tree)

          if (tree.symbol.isModule)
            classNestingSet -= tree.symbol.asModule.moduleClass
          classNestingList remove 0

          moduleDef

        case DefDef(mods, termNames.CONSTRUCTOR, tparams, vparamss, tpt, rhs) =>
          val defDef = DefDef(
            transformModifiers(mods), termNames.CONSTRUCTOR,
            transformTypeDefs(tparams), transformValDefss(vparamss),
            TypeTree(), transform(rhs))

          defDef withAttrs (tree.symbol, tree.tpe, tree.pos)

        case ValDef(mods, name, tpt, rhs) =>
          val typeTree = tpt match {
            case tree: TypeTree
              if !(mods hasFlag SYNTHETIC) &&
                 !(mods hasFlag LAZY) &&
                 !rhs.isEmpty &&
                 tree.original == null =>
              TypeTree()
            case tree =>
              transform(tree)
          }

          val valDef = ValDef(
            transformModifiers(mods), name, typeTree,
            transform(rhs))

          valDef withAttrs (tree.symbol, tree.tpe, tree.pos)

        case TypeApply(fun, targs) =>
          if (hasNonRepresentableType(targs))
            transform(fun)
          else
            super.transform(tree)

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
            if (!qualifier.symbol.isModuleClass)
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
            if (tree.tpe != null && isTypeUnderExpansion(tree.tpe)) =>
          internal setSymbol (tree, NoSymbol)
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

    val syntheticMethodNames = Set("apply", "canEqual", "copy", "equals",
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

          internal updateAttachment (moduleDef, CaseClassMarker)
          moduleDef withAttrs (implDef.symbol, implDef.tpe, implDef.pos)

        case ClassDef(mods, tpname, tparams, Template(parents, self, body)) =>
          val classDef = ClassDef(mods, tpname, tparams,
            Template(parents, self, resetCaseImplBody(body)))

          internal updateAttachment (classDef, CaseClassMarker)
          classDef withAttrs (implDef.symbol, implDef.tpe, implDef.pos)
      }

      def fixCaseClasses(trees: List[Tree]) = {
        val names = (trees collect {
          case ClassDef(mods, tpname, _, _) if mods hasFlag CASE =>
            tpname.toTermName
        }).toSet

        symbols ++= (trees collect {
          case tree @ ClassDef(mods, tpname, _, _)
              if tree.symbol != NoSymbol &&
                 (mods hasFlag CASE) =>
            Seq(tree.symbol)
          case tree @ ModuleDef(mods, name, _)
              if tree.symbol != NoSymbol &&
                 ((mods hasFlag CASE) || (names contains name)) =>
            Seq(tree.symbol, tree.symbol.asModule.moduleClass)
        }).flatten

        trees map {
          case tree @ ModuleDef(mods, name, _) if names contains name =>
            if (mods hasFlag SYNTHETIC)
              EmptyTree
            else
              resetCaseImplDef(tree)
          case tree @ ModuleDef(mods, _, _) if mods hasFlag CASE =>
            resetCaseImplDef(tree)
          case tree @ ClassDef(mods, _, _, _) if mods hasFlag CASE =>
            resetCaseImplDef(tree)
          case tree =>
            tree
        }
      }

      override def transform(tree: Tree) = tree match {
        case Template(parents, self, body) =>
          super.transform(Template(parents, self, fixCaseClasses(body)))
        case Block(stats, expr) =>
          val fixedExpr :: fixedStats = fixCaseClasses(expr :: stats)
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
          case _
              if (internal attachments tree).contains[CaseClassMarker.type] =>
            internal removeAttachment[CaseClassMarker.type] tree
            tree
          case tree: TypeTree if symbolsContains(tree.symbol) =>
            createTypeTree(tree.tpe, tree.pos, owners)
          case _
              if symbolsContains(tree.symbol) =>
            super.transform(internal setSymbol (tree, NoSymbol))
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
          val members = typeSymbol.toType.members
          val names = (members map { _.name }).toSet[Name]
          Left((symbol, typeSymbol.name, names, None))
        })

    val stack = mutable.ListBuffer(ownerStack: _*)

    override def transform(tree: Tree) = tree match {
      case tree: TypeTree =>
        if (tree.original != null)
          internal setOriginal (tree, transform(tree.original))
        tree

      case implDef: ImplDef =>
        val symbol =
          if (implDef.symbol.isModule)
            implDef.symbol.asModule.moduleClass
          else
            implDef.symbol

        val names =
          (implDef.impl.parents flatMap { parent =>
            if (parent.symbol != null && parent.symbol.isType)
              parent.symbol.asType.toType.members map { _.name }
            else
              Iterable.empty
          }) ++
          (implDef.impl.body collect {
            case defTree: DefTree => defTree.name
          })

        val self =
          if (implDef.impl.self.name != termNames.EMPTY)
            Some(implDef.impl.self)
          else
            None

        stack prepend Left((symbol, implDef.name.toTypeName, names.toSet, self))

        val tree = super.transform(implDef)
        stack remove 0
        tree

      case block: Block =>
        val names = block.stats collect {
          case defTree: DefTree => defTree.name
        }

        stack prepend Right(names.toSet)

        val tree = super.transform(block)
        stack remove 0
        tree

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
            val qualifier = Ident(self.name) withAttrs (
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
          _ withAttrs (
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
      internal setPos (
        internal setType (
          internal setSymbol (tree, symbol), tpe), pos)
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
        List(term.getterOrNoSymbol -> valDef, term.setterOrNoSymbol -> valDef)
    }).flatten.toMap - NoSymbol

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
        case defDef @ DefDef(mods, name, tparams, vparamss, tpt, rhs)
            if defaultArgDef(defDef) =>
          val nameString = name.toString
          val macroDefDef = DefDef(
            Modifiers(SYNTHETIC),
            TermName(s"$nameString$$macro"),
            tparams, vparamss, tpt, rhs)

          if (nameString endsWith "$macro") {
            if (accessedDefaultArgs contains defDef.symbol)
              (macroDefaultArgsPending
                getOrElseUpdate (
                  nameString substring (0, nameString.size - 6), None)
                foreach { macroDefaultArgs += _ })
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
        internal setSymbol (to, from.symbol)
      internal setType (internal setPos (to, from.pos), from.tpe)
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
              if (all exists { _ equalsStructure transformedTree })
                all
              else
                transformedTree :: all
            }.reverse

        Modifiers(flags, mods.privateWithin, allAnnotations)
      }

      override def transform(tree: Tree) = tree match {
        case tree: TypeTree =>
          if (tree.original != null)
            internal setOriginal (tree, transform(tree.original))
          tree

        // workaround for default arguments
        case Template(parents, self, body) =>
          super.transform(
            applyMetaProperties(
              tree, Template(parents, self, processDefaultArgs(body))))

        case Block(stats, expr) =>
          val block = processDefaultArgs(expr :: stats)
          super.transform(
            applyMetaProperties(tree, Block(block.tail, block.head)))

        case Select(qualifier, name)
            if (accessedDefaultArgs contains tree.symbol) =>
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
        case defDef @ DefDef(mods, name, _, _, tpt, _)
            if tree.symbol.isTerm && {
              val term = tree.symbol.asTerm
              !term.isLazy && term.isGetter
            } =>
          val valDef = rhss get tree.symbol getOrElse defDef
          val valAnnotations = (rhss get tree.symbol).toList flatMap {
            _.symbol.annotations }
          val newMods = Modifiers(
            mods.flags
              | (if (!valDef.symbol.asTerm.isStable) MUTABLE else NoFlags)
              | (if (valDef.mods hasFlag PRESUPER) PRESUPER else NoFlags),
            mods.privateWithin,
            mods.annotations)
          val newValDef = ValDef(
            fixModifiers(newMods, defDef.symbol, valAnnotations),
            name, transform(valDef.tpt), transform(valDef.rhs))
          newValDef withAttrs (defDef.symbol, valDef.tpe, valDef.pos)

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
          val newValDef = ValDef(
            fixModifiers(mods, valOrDefDef.symbol, valAnnotations),
            valOrDefDef.name, transform(typeTree), transform(assignment))
          newValDef withAttrs (
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
          newValDef withAttrs (valDef.symbol, valDef.tpe, valDef.pos)

        // fix defs
        case defDef @ DefDef(mods, name, tparams, vparamss, tpt, rhs)
            if tree.symbol.isTerm =>
          val newDefDef = DefDef(
            fixModifiers(mods, defDef.symbol), name,
            transformTypeDefs(tparams),
            transformValDefss(vparamss),
            transform(tpt),
            transform(rhs))
          internal setType (newDefDef, defDef.tpe)
          internal setPos (newDefDef, defDef.pos)
          if (tree.symbol.name != TermName("$init$"))
            internal setSymbol (newDefDef, defDef.symbol)
          else
            newDefDef

        // fix classes
        case classDef @ ClassDef(mods, name, tparams, impl)
            if tree.symbol.isClass =>
          val newClassDef = ClassDef(
            fixModifiers(mods, classDef.symbol), name,
            transformTypeDefs(tparams),
            transformTemplate(impl))
          newClassDef withAttrs (classDef.symbol, classDef.tpe, classDef.pos)

        // fix objects
        case moduleDef @ ModuleDef(mods, name, impl)
            if tree.symbol.isModule =>
          val newModuleDef = ModuleDef(
            fixModifiers(mods, moduleDef.symbol), name,
            transformTemplate(impl))
          newModuleDef withAttrs (moduleDef.symbol, moduleDef.tpe, moduleDef.pos)

        // fix type definitions
        case typeDef @ TypeDef(mods, name, tparams, rhs)
            if tree.symbol.isType =>
          val newTypeDef = TypeDef(
            fixModifiers(mods, typeDef.symbol), name,
            transformTypeDefs(tparams),
            transform(rhs))
          newTypeDef withAttrs (typeDef.symbol, typeDef.tpe, typeDef.pos)

        case _ =>
          super.transform(tree)
      }
    }

    typecheckFixer transform tree
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
            internal setOriginal (tree, transform(tree.original))
          tree

        case Apply(fun, args) if fun.symbol != null && fun.symbol.isModule =>
          internal setSymbol (fun, NoSymbol)
          super.transform(tree)

        case Typed(expr, tpt) =>
          tpt match {
            case Function(List(), EmptyTree) =>
              super.transform(tree)

            case Annotated(annot, arg)
                if expr != null && arg != null && (expr equalsStructure arg) =>
              super.transform(tpt)

            case tpt: TypeTree
                if (tpt.original match {
                  case Annotated(_, _) => true
                  case _ => false
                }) =>
              super.transform(tpt.original)

            case tpt if !tpt.isType =>
              super.transform(expr)

            case tpt =>
              super.transform(tree)
          }

        case Ident(name) =>
          if (tree.symbol.isTerm && tree.symbol.asTerm.isLazy)
            internal setSymbol (tree, NoSymbol)
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
                case tpt: TypeTree =>
                  tree
                case tpt =>
                  ValDef(mods, name, internal setOriginal (TypeTree(), tpt), rhs)
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

        case ModuleDef(mods, name, Template(parents, self, body)) =>
          super.transform(tree) match {
            case ModuleDef(mods, _, Template(parents, noSelfType,
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
