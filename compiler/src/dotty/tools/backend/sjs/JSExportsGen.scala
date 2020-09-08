package dotty.tools.backend.sjs

import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.core._

import Contexts._
import Decorators._
import Denotations._
import Flags._
import Names._
import Periods._
import Phases._
import StdNames._
import Symbols._
import SymDenotations._
import Types._
import TypeErasure.ErasedValueType

import dotty.tools.dotc.transform.Erasure
import dotty.tools.dotc.util.SourcePosition
import dotty.tools.dotc.util.Spans.Span
import dotty.tools.dotc.report

import org.scalajs.ir
import org.scalajs.ir.{ClassKind, Position, Names => jsNames, Trees => js, Types => jstpe}
import org.scalajs.ir.Names.{ClassName, MethodName, SimpleMethodName}
import org.scalajs.ir.OriginalName
import org.scalajs.ir.OriginalName.NoOriginalName
import org.scalajs.ir.Trees.OptimizerHints

import dotty.tools.dotc.transform.sjs.JSInteropUtils._

import JSEncoding._

final class JSExportsGen(jsCodeGen: JSCodeGen)(using Context) {
  import jsCodeGen._
  import positionConversions._

  def genJSClassDispatchers(classSym: Symbol, dispatchMethodsNames: List[JSName]): List[js.MemberDef] = {
    dispatchMethodsNames.map(genJSClassDispatcher(classSym, _))
  }

  private def genJSClassDispatcher(classSym: Symbol, name: JSName): js.MemberDef = {
    val alts = classSym.info.membersBasedOnFlags(required = Method, excluded = Bridge)
      .map(_.symbol)
      .filter { sym =>
        /* scala-js#3939: Object is not a "real" superclass of JS types.
         * as such, its methods do not participate in overload resolution.
         * An exception is toString, which is handled specially in genExportMethod.
         */
        sym.owner != defn.ObjectClass && sym.jsName == name
      }
      .toList

    assert(!alts.isEmpty, s"Ended up with no alternatives for ${classSym.fullName}::$name.")

    val (propSyms, methodSyms) = alts.partition(_.isJSProperty)
    val isProp = propSyms.nonEmpty

    if (isProp && methodSyms.nonEmpty) {
      val firstAlt = alts.head
      report.error(
          i"Conflicting properties and methods for ${classSym.fullName}::$name.",
          firstAlt.srcPos)
      implicit val pos = firstAlt.span
      js.JSPropertyDef(js.MemberFlags.empty, genExpr(name)(firstAlt.sourcePos), None, None)
    } else {
      genMemberExportOrDispatcher(name, isProp, alts, static = false)
    }
  }

  private def genMemberExportOrDispatcher(jsName: JSName, isProp: Boolean,
      alts: List[Symbol], static: Boolean): js.MemberDef = {
    withNewLocalNameScope {
      if (isProp)
        genExportProperty(alts, jsName, static)
      else
        genExportMethod(alts.map(new Exported(_)), jsName, static)
    }
  }

  def genJSConstructorDispatch(alts: List[Symbol]): (Option[List[js.ParamDef]], js.JSMethodDef) = {
    val exporteds = alts.map(new Exported(_))

    val isLiftedJSCtor = exporteds.head.isLiftedJSConstructor
    assert(exporteds.tail.forall(_.isLiftedJSConstructor == isLiftedJSCtor),
        s"Alternative constructors $alts do not agree on whether they are lifted JS constructors or not")
    val captureParams = if (!isLiftedJSCtor) {
      None
    } else {
      Some(for {
        exported <- exporteds
        param <- exported.captureParamsFront ::: exported.captureParamsBack
      } yield {
        genParamDef(param.sym)
      })
    }

    val ctorDef = genExportMethod(exporteds, JSName.Literal("constructor"), static = false)

    (captureParams, ctorDef)
  }

  private def genExportProperty(alts: List[Symbol], jsName: JSName, static: Boolean): js.JSPropertyDef = {
    ???
  }

  private def genExportMethod(alts0: List[Exported], jsName: JSName, static: Boolean): js.JSMethodDef = {
    ???
  }

  private final class ParamSpec(val sym: Symbol, val info: Type,
      val isRepeated: Boolean, val hasDefault: Boolean) {
    override def toString(): String =
      s"ParamSpec(${sym.name}, $info, $isRepeated, $hasDefault)"
  }

  private object ParamSpec extends (Symbol => ParamSpec) {
    def apply(sym: Symbol): ParamSpec = {
      val hasDefault = sym.is(HasDefault)
      val repeated = sym.info.isRepeatedParam
      val info = if (repeated) sym.info.elemType else sym.info
      new ParamSpec(sym, info, repeated, hasDefault)
    }
  }

  private final class Exported(val sym: Symbol) {
    private val isAnonJSClassConstructor =
      //sym.isClassConstructor && sym.owner.isAnonymousClass && isJSType(sym.owner)
      false

    val isLiftedJSConstructor =
      //sym.isClassConstructor && isNestedJSClass(sym.owner)
      false

    val (params, captureParamsFront, captureParamsBack) = {
      val allParamsElimRepeated =
        atPhase(elimRepeatedPhase)(sym.paramSymss.flatten.map(ParamSpec))
      val allParamsElimErasedValueType =
        atPhase(elimErasedValueTypePhase)(sym.paramSymss.flatten.map(ParamSpec))
      val allParamsNow = sym.paramSymss.flatten.map(ParamSpec)

      def mergeElimRepeatedAndElimErasedValueType(paramsElimRepeated: List[ParamSpec],
          paramsElimErasedValueType: List[ParamSpec]): List[ParamSpec] = {
        for {
          (paramElimRepeated, paramElimErasedValueType) <- paramsElimRepeated.zip(paramsElimErasedValueType)
        } yield {
          if (paramElimRepeated.isRepeated) paramElimRepeated
          else paramElimErasedValueType
        }
      }

      if (!isLiftedJSConstructor && !isAnonJSClassConstructor) {
        /* Easy case: all params are formal params, and we only need to
         * travel back before uncurry to handle repeated params, or before
         * posterasure for other params.
         */
        assert(allParamsElimRepeated.size == allParamsElimErasedValueType.size,
            s"Found ${allParamsElimRepeated.size} params entering uncurry but " +
            s"${allParamsElimErasedValueType.size} params entering posterasure for " +
            s"non-lifted symbol ${sym.fullName}")
        val formalParams =
          mergeElimRepeatedAndElimErasedValueType(allParamsElimRepeated, allParamsElimErasedValueType)
        (formalParams.toIndexedSeq, Nil, Nil)
      } else {
        // This never happens for now
        ??? : (IndexedSeq[ParamSpec], List[ParamSpec], List[ParamSpec])
      }
    }

    val hasRepeatedParam = params.nonEmpty && params.last.isRepeated

    def pos: Position = sym.span

    def exportArgTypeAt(paramIndex: Int): Type = {
      if (paramIndex < params.length) {
        params(paramIndex).info
      } else {
        assert(hasRepeatedParam, i"$sym does not have varargs nor enough params for $paramIndex")
        params.last.info
      }
    }

    //def genBody(formalArgsRegistry: FormalArgsRegistry, static: Boolean): js.Tree =
    //  genApplyForSym(formalArgsRegistry, this, static)

    def typeInfo: String = sym.info.toString
  }
}
