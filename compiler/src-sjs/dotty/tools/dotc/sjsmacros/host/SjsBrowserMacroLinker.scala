package dotty.tools.dotc.sjsmacros.host

import dotty.tools.io.JSByteArrays
import dotty.tools.sjs.JSInterop.{dynamicArray, isDefined, stringArray}

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.annotation.JSGlobal

object SjsBrowserMacroLinker:
  @js.native
  @JSGlobal("globalThis")
  private object GlobalThis extends js.Object

  private val CompilerIRFingerprintGlobalName = SjsMacroBrowserGlobals.CompilerIRFingerprint
  private val MacroArtifactsGlobalName = SjsMacroBrowserGlobals.MacroArtifacts
  private val ScalaJSLinkerGlobalName = SjsMacroBrowserGlobals.ScalaJSLinker

  def relink(request: js.Dynamic): js.Any =
    val requestedPackageNames = stringArray(request.selectDynamic("packageNames")).distinct
    val entryPointsIR = entryPointIRArray(request.selectDynamic("entryPointsIR"))
    val artifacts = configuredArtifacts()
    val artifactPackages = SjsMacroArtifacts.packageNames(artifacts)
    val hasPackageAgnosticArtifact = artifacts.exists(_.macroPackages.isEmpty)
    val unknownPackages =
      if hasPackageAgnosticArtifact then Nil
      else requestedPackageNames.filterNot(artifactPackages.contains)
    if unknownPackages.nonEmpty then
      throw js.JavaScriptException(
        s"missing browser Scala.js macro artifacts for package(s): ${unknownPackages.sorted.mkString(", ")}"
      )

    val macroImplementationIR =
      SjsMacroIRClosure.collect(entryPointsIR.map(_.file), SjsMacroArtifacts.implementationIR(artifacts))
    val compilerIRFingerprintValue = readGlobal(CompilerIRFingerprintGlobalName)
    if !isDefined(compilerIRFingerprintValue) || compilerIRFingerprintValue.toString.isEmpty then
      throw js.JavaScriptException(
        s"missing browser compiler IR fingerprint: set globalThis.$CompilerIRFingerprintGlobalName before macro relinking"
      )
    val linkerConfig = Seq(
      "moduleKind=ESModule",
      "moduleInitializers=none",
    ) ++ SjsMacroArtifacts.descriptors(artifacts).map("macroArtifact=" + _)
    val cacheKey = SjsMacroRelinkCacheKey.compute(
      compilerIRFingerprint = compilerIRFingerprintValue.toString,
      entryPointsIR = entryPointsIR.map(_.file),
      macroImplementationIR = macroImplementationIR,
      linkerConfig = linkerConfig,
    )

    val scalaJSLinker = readGlobal(ScalaJSLinkerGlobalName)
    if !isDefined(scalaJSLinker) then
      throw js.JavaScriptException(
        s"missing browser Scala.js linker: set globalThis.$ScalaJSLinkerGlobalName.link(request)"
      )

    val link = scalaJSLinker.selectDynamic("link")
    if js.typeOf(link) != "function" then
      throw js.JavaScriptException(s"browser Scala.js linker must define link(request)")

    link.asInstanceOf[js.Function1[js.Dynamic, js.Any]](
      js.Dynamic.literal(
        protocolVersion = 1,
        ids = request.selectDynamic("ids"),
        packageNames = requestedPackageNames.toJSArray,
        entryPointsIR = entryPointsIR.map(toJSEntryPointIR).toJSArray,
        macroImplementationIR = macroImplementationIR.map(toJSIRFile).toJSArray,
        macroArtifacts = SjsMacroArtifacts.descriptors(artifacts).toJSArray,
        linkerConfig = linkerConfig.toJSArray,
        cacheKey = cacheKey,
      )
    )

  private def configuredArtifacts(): Seq[SjsMacroArtifacts.Artifact] =
    val value = readGlobal(MacroArtifactsGlobalName)
    if !isDefined(value) then Nil
    else
      dynamicArray(value).map { artifact =>
        val directIR = artifact.selectDynamic("implementationIR")
        if isDefined(directIR) then
          SjsMacroArtifacts.Artifact(
            id = readOptionalString(artifact, "id").getOrElse("browser-macro-artifact"),
            macroPackages = readOptionalStringArray(artifact, "macroPackages").getOrElse(Nil),
            implementationIR = irFileArray(directIR, "implementationIR"),
          )
        else
          val root = readOptionalString(artifact, "root").getOrElse {
            throw js.JavaScriptException("browser macro artifact must define `root` or `implementationIR`")
          }
          val id = readOptionalString(artifact, "id")
          val macroPackages = readOptionalStringArray(artifact, "macroPackages")
          (id, macroPackages) match
            case (Some(id), Some(macroPackages)) =>
              SjsMacroArtifactFS.read(id, macroPackages, root)
            case _ =>
              throw js.JavaScriptException(
                "browser macro artifact with `root` must also define `id` and `macroPackages`"
              )
      }

  private def entryPointIRArray(value: js.Any): Seq[SjsMacroArtifacts.EntryPointIR] =
    dynamicArray(value).map { entry =>
      SjsMacroArtifacts.EntryPointIR(
        packageName = entry.selectDynamic("packageName").toString,
        path = entry.selectDynamic("path").toString,
        bytes = JSByteArrays.toByteArray(entry.selectDynamic("bytes").asInstanceOf[js.Any]),
      )
    }

  private def irFileArray(value: js.Any, label: String): Seq[SjsMacroArtifacts.IRFile] =
    dynamicArray(value).map { file =>
      val path = file.selectDynamic("path")
      val bytes = file.selectDynamic("bytes")
      if !isDefined(path) || !isDefined(bytes) then
        throw js.JavaScriptException(s"$label entries must define `path` and `bytes`")
      SjsMacroArtifacts.IRFile(path.toString, JSByteArrays.toByteArray(bytes.asInstanceOf[js.Any]))
    }

  private def toJSIRFile(file: SjsMacroArtifacts.IRFile): js.Dynamic =
    js.Dynamic.literal(
      path = file.path,
      bytes = JSByteArrays.toUint8Array(file.bytes),
    )

  private def toJSEntryPointIR(entry: SjsMacroArtifacts.EntryPointIR): js.Dynamic =
    js.Dynamic.literal(
      packageName = entry.packageName,
      path = entry.path,
      bytes = JSByteArrays.toUint8Array(entry.bytes),
    )

  private def readOptionalString(obj: js.Dynamic, field: String): Option[String] =
    val value = obj.selectDynamic(field)
    Option.when(isDefined(value))(value.toString)

  private def readOptionalStringArray(obj: js.Dynamic, field: String): Option[Seq[String]] =
    val value = obj.selectDynamic(field)
    Option.when(isDefined(value))(stringArray(value.asInstanceOf[js.Any]))

  private def readGlobal(name: String): js.Dynamic =
    GlobalThis.asInstanceOf[js.Dynamic].selectDynamic(name)
