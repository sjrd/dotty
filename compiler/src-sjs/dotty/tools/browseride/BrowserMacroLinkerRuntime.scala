package dotty.tools.browseride

import scala.collection.mutable
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.typedarray.Uint8Array
import scala.util.control.NonFatal

import dotty.tools.dotc.sjsmacros.host.SjsMacroBrowserGlobals
import dotty.tools.sjs.JSInterop.{isDefined, stringOr}

object BrowserMacroLinkerRuntime:
  private val CompilerJSZipImport = """import * as importedjszip from "jszip";"""
  private val CompilerLoaderImport = """import { load as __load } from "./__loader.js";"""
  // Stable, cheap fingerprint for browser macro compiler IR cache keys.
  private val Fnv64OffsetBasis = -3750763034362895579L
  private val Fnv64Prime = 1099511628211L

  trait Config extends js.Object:
    val compilerIRBytes: js.Function0[js.Promise[Uint8Array]]
    val compilerIRFiles: js.Function0[js.Promise[js.Array[BrowserLinkerBridge.IRInput]]]
    val linkCompilerModule: js.Function1[js.Array[BrowserLinkerBridge.IRInput], js.Promise[js.Dynamic]]
    val recordTiming: js.Function2[String, Double, Unit]
    val jszipWrapperUrl: String

  def install(config: Config): js.Promise[Unit] =
    config.compilerIRBytes().`then`[Unit] { compilerIRBytes =>
      val compilerIRFingerprint = timed(config, "compiler IR fingerprint")(bytesFingerprint(compilerIRBytes))
      val global = BrowserJS.global
      global.updateDynamic(SjsMacroBrowserGlobals.CompilerIRFingerprint)(compilerIRFingerprint)

      val runtime = new Runtime(config)
      val link: js.Function1[js.Dynamic, js.Promise[js.Dynamic]] = request => runtime.link(request)
      global.updateDynamic(SjsMacroBrowserGlobals.ScalaJSLinker)(js.Dynamic.literal(link = link))
      ()
    }

  private final class Runtime(config: Config):
    private val linkedCompilerCache = mutable.Map.empty[String, js.Dynamic]

    def link(request: js.Dynamic): js.Promise[js.Dynamic] =
      val cacheKey = stringOr(request.selectDynamic("cacheKey"), "")
      linkedCompilerCache.get(cacheKey) match
        case Some(linkedCompiler) =>
          js.Promise.resolve(linkedCompiler)
        case None =>
          config.compilerIRFiles().`then`[js.Dynamic] { compilerIRFiles =>
            val allIRFiles =
              (compilerIRFiles.toSeq ++
                irInputArray(request.selectDynamic("entryPointsIR")) ++
                irInputArray(request.selectDynamic("macroImplementationIR"))).toJSArray
            timedPromise(config, "macro compiler link")(config.linkCompilerModule(allIRFiles)).`then`[js.Dynamic] { linkResult =>
              val linkedCompiler = timed(config, "macro compiler import")(materializeLinkedCompilerModule(linkResult))
              linkedCompilerCache(cacheKey) = linkedCompiler
              linkedCompiler
            }
          }

    private def materializeLinkedCompilerModule(linkResult: js.Dynamic): js.Dynamic =
      val linkedFiles = linkResultFiles(linkResult)
      if linkedFiles.isEmpty then
        js.Dynamic.literal(
          moduleUrl = BrowserJS.createModuleBlobURL(patchLinkedCompilerCode(stringOr(linkResult.selectDynamic("code"), "")))
        )
      else
        val byPath = linkedFiles.map(file => file.selectDynamic("path").toString -> file).toMap
        val paths = byPath.keys.toSeq
        val jsFileName = stringOr(linkResult.selectDynamic("jsFileName"), "main.js")
        val jsFile = byPath.get(jsFileName)
        val baseCode =
          patchLinkedCompilerCode(jsFile.map(file => BrowserJS.decodeUTF8(fileBytes(file))).getOrElse(stringOr(linkResult.selectDynamic("code"), "")))
        val codeWithLoader = paths.find(_.endsWith("__loader.js")).fold(baseCode) { loaderFileName =>
          val loaderUrl = BrowserJS.createModuleBlobURL(BrowserJS.decodeUTF8(fileBytes(byPath(loaderFileName))))
          baseCode.replace(CompilerLoaderImport, s"import { load as __load } from ${js.JSON.stringify(loaderUrl)};")
        }

        val codeWithWasm = paths.find(_.endsWith(".wasm")).fold(codeWithLoader) { wasmFileName =>
          val wasmUrl = BrowserJS.createBlobURL(fileBytes(byPath(wasmFileName)), "application/wasm")
          codeWithLoader.replace(js.JSON.stringify(s"./$wasmFileName"), js.JSON.stringify(wasmUrl))
        }

        js.Dynamic.literal(moduleUrl = BrowserJS.createModuleBlobURL(codeWithWasm))

    private def patchLinkedCompilerCode(code: String): String =
      code.replace(
        CompilerJSZipImport,
        s"import * as importedjszip from ${js.JSON.stringify(config.jszipWrapperUrl)};",
      )

  private def timed[A](config: Config, name: String)(operation: => A): A =
    val startedAt = js.Date.now()
    try operation
    finally config.recordTiming(name, js.Date.now() - startedAt)

  private def timedPromise[A](config: Config, name: String)(operation: => js.Promise[A]): js.Promise[A] =
    val startedAt = js.Date.now()
    def record(): Unit =
      config.recordTiming(name, js.Date.now() - startedAt)
    try
      operation.`then`[A](
        value =>
          record()
          value,
        error =>
          record()
          throw js.JavaScriptException(error.asInstanceOf[js.Any]),
      )
    catch
      case NonFatal(t) =>
        record()
        js.Promise.reject(t.asInstanceOf[js.Any])

  private def bytesFingerprint(bytes: Uint8Array): String =
    var hash = Fnv64OffsetBasis
    var i = 0
    while i < bytes.length do
      hash ^= bytes(i).toLong & 0xffL
      hash *= Fnv64Prime
      i += 1
    s"${bytes.byteLength}:${java.lang.Long.toHexString(hash)}"

  private def linkResultFiles(linkResult: js.Dynamic): Seq[js.Dynamic] =
    val value = linkResult.selectDynamic("files")
    if isDefined(value) then value.asInstanceOf[js.Array[js.Dynamic]].toSeq
    else Nil

  private def irInputArray(value: js.Any): Seq[BrowserLinkerBridge.IRInput] =
    if isDefined(value) then value.asInstanceOf[js.Array[BrowserLinkerBridge.IRInput]].toSeq
    else Nil

  private def fileBytes(file: js.Dynamic): Uint8Array =
    file.selectDynamic("bytes").asInstanceOf[Uint8Array]
