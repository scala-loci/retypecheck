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
    try fixTypecheck(c typecheck tree)
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

    def isClass(symbol: Symbol): Boolean =
      symbol.isClass && !symbol.isModule && !symbol.isPackage

    def expandType(tpe: Type): Tree = tpe.dealias match {
      case ThisType(pre) if isClass(pre) =>
        This(pre.asType.name)

      case ThisType(pre) if isAccessible(pre, allOwners) =>
        expandSymbol(pre, pos)

      case TypeRef(NoPrefix, sym, List()) if sym.isModuleClass =>
        SingletonTypeTree(Ident(sym.name.toTypeName))

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
          if (typeProjection)
            preTree match {
              case SingletonTypeTree(ref) =>
                Select(ref, sym.name.toTypeName)
              case _ =>
                SelectFromTypeTree(preTree, sym.name.toTypeName)
            }
          else if (isClass(sym))
            Select(preTree, sym.name.toTypeName)
          else
            Select(preTree, sym.name.toTermName)

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

      case RefinedType(parents, scope) =>
        def refiningType(sym: TypeSymbol): TypeDef =
          TypeDef(
            Modifiers(),
            sym.name,
            sym.typeParams map { param => refiningType(param.asType) },
            expandType(sym.typeSignature.finalResultType))

        def refiningVal(sym: TermSymbol): ValDef =
          ValDef(
            Modifiers(DEFERRED),
            sym.name,
            expandType(sym.typeSignature.finalResultType),
            EmptyTree)

        def refiningDef(sym: MethodSymbol): DefDef =
          DefDef(
            Modifiers(DEFERRED),
            sym.name,
            sym.typeParams map { param => refiningType(param.asType) },
            sym.paramLists map { _ map { param => refiningVal(param.asTerm) } },
            expandType(sym.typeSignature.finalResultType),
            EmptyTree)

        val body = scope map { symbol =>
          if (symbol.isMethod) {
            val method = symbol.asMethod
            if (method.isStable)
              Some(refiningVal(method))
            else
              Some(refiningDef(method))
          }
          else if (symbol.isType)
            Some(refiningType(symbol.asType))
          else
            None
        }

        if (body exists { _.isEmpty })
          TypeTree(tpe)
        else
          CompoundTypeTree(
            Template(parents map expandType, noSelfType, body.toList.flatten))

      case _ =>
        TypeTree(tpe)
    }

    expandType(tpe)
  }


  private def expandSymbol(symbol: Symbol, pos: Position): Tree = {
    val tree =
      if (symbol == c.mirror.RootClass)
        Ident(termNames.ROOTPKG)
      else if (!symbol.owner.isClass &&
               !symbol.owner.isModule &&
               !symbol.owner.isModuleClass &&
               !symbol.owner.isPackage &&
               !symbol.owner.isPackageClass)
        Ident(symbol.name.toTermName)
      else
        Select(expandSymbol(symbol.owner, pos), symbol.name.toTermName)
    internal setPos (tree, pos)
  }

  private def ownerChain(symbol: Symbol): List[Symbol] =
    if (symbol.owner == NoSymbol)
      Nil
    else
      symbol :: ownerChain(symbol.owner)

  private def isAccessible(symbol: Symbol, currentOwners: Set[Symbol]) =
    ownerChain(symbol).toSet diff currentOwners forall { symbol =>
      !symbol.isPrivate && !symbol.isProtected
    }


  private def fixTypesAndSymbols(tree: Tree): Tree = {
    val definedTypeSymbols = tree collect {
      case tree @ TypeDef(_, _, _, _) if tree.symbol.isType =>
        tree.symbol
      case tree @ ClassDef(_, _, _, _) if tree.symbol.isClass =>
        tree.symbol
      case tree @ ModuleDef(_, _, _) if tree.symbol.isModule =>
        tree.symbol.asModule.moduleClass
    }

    val classNesting =
      mutable.ListBuffer(ownerChain(c.internal.enclosingOwner).reverse: _*)

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

    object typesAndSymbolsFixer extends Transformer {
      override def transform(tree: Tree) = tree match {
        case tree: TypeTree =>
          if (tree.original != null)
            transform(prependRootPackage(tree.original))
          else if (tree.tpe != null && isTypeUnderExpansion(tree.tpe))
            createTypeTree(tree.tpe, tree.pos, classNesting.toSet)
          else
            tree

        case ClassDef(_, _, _, _) if tree.symbol.isClass =>
          classNesting prepend tree.symbol
          val classDef = super.transform(tree)
          classNesting remove 0
          classDef

        case ModuleDef(_, _, _) if tree.symbol.isModule =>
          classNesting prepend tree.symbol.asModule.moduleClass
          val moduleDef = super.transform(tree)
          classNesting remove 0
          moduleDef

        case DefDef(mods, termNames.CONSTRUCTOR, tparams, vparamss, tpt, rhs) =>
          val defDef = DefDef(
            transformModifiers(mods), termNames.CONSTRUCTOR,
            transformTypeDefs(tparams), transformValDefss(vparamss),
            tpt, transform(rhs))
          internal setSymbol (defDef, tree.symbol)
          internal setType (defDef, tree.tpe)
          internal setPos (defDef, tree.pos)

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
          internal setSymbol (valDef, tree.symbol)
          internal setType (valDef, tree.tpe)
          internal setPos (valDef, tree.pos)

        case TypeApply(fun, targs) =>
          val hasNonRepresentableType = targs exists { arg =>
            arg.tpe != null && (arg.tpe exists {
              case TypeRef(NoPrefix, name, List()) =>
                name.toString endsWith ".type"
              case _ =>
                false
            })
          }

          if (hasNonRepresentableType)
            transform(fun)
          else
            super.transform(tree)

        case Select(This(_), termNames.CONSTRUCTOR) =>
          Ident(termNames.CONSTRUCTOR)

        case Select(qualifier @ This(qual), name) =>
          val owners = classNesting.toSet

          val fixedEnclosingSelfReference =
            if (!qualifier.symbol.isModuleClass)
              classNesting collectFirst {
                case symbol if symbol.name == qualifier.symbol.name =>
                  Ident(name)
              }
            else
              None

          fixedEnclosingSelfReference getOrElse {
            if (qual != typeNames.EMPTY &&
                qualifier.symbol.isModuleClass &&
                isAccessible(tree.symbol, owners) &&
                !isTypeUnderExpansion(qualifier.symbol.asType.toType) &&
                !(owners contains qualifier.symbol))
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
          internal setSymbol (moduleDef, implDef.symbol)
          internal setType (moduleDef, implDef.tpe)
          internal setPos (moduleDef, implDef.pos)

        case ClassDef(mods, tpname, tparams, Template(parents, self, body)) =>
          val classDef = ClassDef(mods, tpname, tparams,
            Template(parents, self, resetCaseImplBody(body)))

          internal updateAttachment (classDef, CaseClassMarker)
          internal setSymbol (classDef, implDef.symbol)
          internal setType (classDef, implDef.tpe)
          internal setPos (classDef, implDef.pos)
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

        owner foreach { owners += _ }

        result
      }
    }

    caseClassReferenceFixer transform (caseClassFixer transform tree)
  }


  private def selfReferenceFixer = new Transformer {
    val stack = mutable.ListBuffer.empty[(TypeName, Set[Name])]

    override def transform(tree: Tree) = tree match {
      case implDef: ImplDef =>
        stack prepend implDef.name.toTypeName -> (
          (implDef.impl.parents flatMap { parent =>
            if (parent.symbol.isType)
              parent.symbol.asType.toType.members map { _.name }
            else
              Iterable.empty
          }) ++
          (implDef.impl.body collect {
            case defTree: DefTree => defTree.name
          })
        ).toSet

        val tree = super.transform(implDef)
        stack remove 0
        tree

      case Select(thisTree @ This(thisName), selectedName) =>
        val lookupResult =
          if (thisName.toString startsWith "$anon")
            Some(false)
          else
            stack collectFirst {
              case (name, names) if name == thisName =>
                names contains selectedName
            }

        lookupResult match {
          case Some(false) =>
            val ident = Ident(selectedName)
            if (tree.pos != NoPosition)
              internal setPos (ident, tree.pos)
            else
              internal setPos (ident, thisTree.pos)
            internal setType (ident, tree.tpe)

          case Some(true) =>
            tree

          case None =>
            if (thisTree.symbol.isModuleClass &&
                !thisTree.symbol.isPackage &&
                !thisTree.symbol.isPackageClass)
              Select(Ident(thisName.toTermName), selectedName)
            else
              tree
        }

      case _ =>
        super.transform(tree)
    }
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

    val accessedDefaultArgs = tree collect {
      case select @ Select(_, _) if definedDefaultArgs contains select.symbol =>
        select.symbol
    }

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

        val privateWithin =
          if (symbol.privateWithin != NoSymbol)
            symbol.privateWithin.name
          else
            mods.privateWithin

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

        Modifiers(flags, privateWithin, allAnnotations)
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

        case Ident(_) if tree.symbol.isTerm =>
          internal setSymbol (
            internal setType (expandSymbol(tree.symbol, tree.pos), tree.tpe),
            tree.symbol)

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
              !term.isLazy && term.isGetter && (rhss contains term)
            } =>
          val valDef = rhss(tree.symbol)
          val valAnnotations = rhss(tree.symbol).symbol.annotations
          val newMods = Modifiers(
            mods.flags | (if (valDef.symbol.asTerm.isVar) MUTABLE else NoFlags),
            mods.privateWithin,
            mods.annotations)
          val newValDef = ValDef(
            fixModifiers(newMods, defDef.symbol, valAnnotations),
            name, transform(valDef.tpt), transform(valDef.rhs))
          internal setSymbol (newValDef, defDef.symbol)
          internal setType (newValDef, valDef.tpe)
          internal setPos (newValDef, valDef.pos)

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
          internal setSymbol (newValDef, valOrDefDef.symbol)
          valDef foreach { valDef =>
            internal setType (newValDef, valDef.tpe)
            internal setPos (newValDef, valDef.pos)
          }
          newValDef

        // fix vals
        case valDef @ ValDef(mods, name, tpt, rhs)
            if tree.symbol.isTerm =>
          val newValDef = ValDef(
            fixModifiers(mods, valDef.symbol), name,
            transform(tpt),
            transform(rhs))
          internal setSymbol (newValDef, valDef.symbol)
          internal setType (newValDef, valDef.tpe)
          internal setPos (newValDef, valDef.pos)

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
          internal setSymbol (newClassDef, classDef.symbol)
          internal setType (newClassDef, classDef.tpe)
          internal setPos (newClassDef, classDef.pos)

        // fix objects
        case moduleDef @ ModuleDef(mods, name, impl)
            if tree.symbol.isModule =>
          val newModuleDef = ModuleDef(
            fixModifiers(mods, moduleDef.symbol), name,
            transformTemplate(impl))
          internal setSymbol (newModuleDef, moduleDef.symbol)
          internal setType (newModuleDef, moduleDef.tpe)
          internal setPos (newModuleDef, moduleDef.pos)

        // fix type definitions
        case typeDef @ TypeDef(mods, name, tparams, rhs)
            if tree.symbol.isType =>
          val newTypeDef = TypeDef(
            fixModifiers(mods, typeDef.symbol), name,
            transformTypeDefs(tparams),
            transform(rhs))
          internal setSymbol (newTypeDef, typeDef.symbol)
          internal setType (newTypeDef, typeDef.tpe)
          internal setPos (newTypeDef, typeDef.pos)

        case _ =>
          super.transform(tree)
      }
    }

    typecheckFixer transform tree
  }

  private def fixUntypecheck(tree: Tree): Tree = {
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
