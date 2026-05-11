package dotty.tools.browseride

import scala.collection.mutable
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.annotation.JSExportTopLevel
import scala.scalajs.js.typedarray.Uint8Array
import scala.util.control.NonFatal

import dotty.tools.dotc.sjsmacros.host.SjsMacroBrowserGlobals
import dotty.tools.sjs.JSInterop.{isDefined, stringOr}

object BrowserMacroLinkerRuntime:
  private val MacroLinkCacheDB = "scala-browser-ide-macro-link-cache"
  private val MacroLinkCacheStore = "linked-compilers"
  private val CompilerJSZipImport = """import * as importedjszip from "jszip";"""
  private val CompilerLoaderImport = """import { load as __load } from "./__loader.js";"""

  private var dbPromise: Option[js.Promise[Option[js.Dynamic]]] = None

  trait Config extends js.Object:
    val compilerIRBytes: js.Function0[js.Promise[Uint8Array]]
    val compilerIRFiles: js.Function0[js.Promise[js.Array[BrowserLinkerBridge.IRInput]]]
    val linkCompilerModule: js.Function1[js.Array[BrowserLinkerBridge.IRInput], js.Promise[js.Dynamic]]
    val recordTiming: js.Function2[String, Double, Unit]
    val jszipWrapperUrl: String

  @JSExportTopLevel("installScala3BrowserMacroLinkerAsync")
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
          readPersistedLinkedCompiler(cacheKey).`then`[js.Dynamic] {
            case Some(persistedLinkResult) =>
              val linkedCompiler = timed(config, "macro compiler import")(materializeLinkedCompilerModule(persistedLinkResult))
              linkedCompilerCache(cacheKey) = linkedCompiler
              linkedCompiler
            case None =>
              config.compilerIRFiles().`then`[js.Dynamic] { compilerIRFiles =>
                val allIRFiles =
                  (compilerIRFiles.toSeq ++
                    irInputArray(request.selectDynamic("entryPointsIR")) ++
                    irInputArray(request.selectDynamic("macroImplementationIR"))).toJSArray
                timedPromise(config, "macro compiler link")(config.linkCompilerModule(allIRFiles)).`then`[js.Dynamic] { linkResult =>
                  val cacheableCompiler = cacheableLinkResult(linkResult)
                  val linkedCompiler = timed(config, "macro compiler import")(materializeLinkedCompilerModule(cacheableCompiler))
                  linkedCompilerCache(cacheKey) = linkedCompiler
                  writePersistedLinkedCompiler(cacheKey, cacheableCompiler)
                  linkedCompiler
                }
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

  private def cacheableLinkResult(linkResult: js.Dynamic): js.Dynamic =
    val normalizedFiles = linkResultFiles(linkResult).map { file =>
      js.Dynamic.literal(
        path = file.selectDynamic("path").toString,
        bytes = fileBytes(file),
      )
    }

    val result = js.Dynamic.literal(
      code = if normalizedFiles.isEmpty then stringOr(linkResult.selectDynamic("code"), "") else "",
      files = normalizedFiles.toJSArray,
    )
    val jsFileName = linkResult.selectDynamic("jsFileName")
    if isDefined(jsFileName) then
      result.updateDynamic("jsFileName")(jsFileName.toString)
    result

  private def readPersistedLinkedCompiler(cacheKey: String): js.Promise[Option[js.Dynamic]] =
    openMacroLinkCacheDB().`then`[Option[js.Dynamic]] {
      case Some(db) =>
        try
          val transaction = db.asInstanceOf[js.Dynamic].transaction(MacroLinkCacheStore, "readonly")
          idbRequest(transaction.objectStore(MacroLinkCacheStore).get(cacheKey)).`then`[Option[js.Dynamic]] { record =>
            if !isDefined(record) then None
            else
              val linkResult = record.asInstanceOf[js.Dynamic].selectDynamic("linkResult")
              Option.when(isDefined(linkResult))(linkResult.asInstanceOf[js.Dynamic])
          }
        catch
          case NonFatal(_) => None
      case None => None
    }

  private def writePersistedLinkedCompiler(cacheKey: String, linkResult: js.Dynamic): Unit =
    openMacroLinkCacheDB().`then`[Unit] { db =>
      try
        db.foreach { db =>
          val transaction = db.transaction(MacroLinkCacheStore, "readwrite")
          idbRequest(transaction.objectStore(MacroLinkCacheStore).put(js.Dynamic.literal(
            cacheKey = cacheKey,
            linkResult = linkResult,
          )))
          ()
        }
      catch case NonFatal(_) => ()
    }
    ()

  private def openMacroLinkCacheDB(): js.Promise[Option[js.Dynamic]] =
    val indexedDB = BrowserJS.global.selectDynamic("indexedDB")
    if !isDefined(indexedDB) then js.Promise.resolve(None)
    else
      dbPromise.getOrElse {
        val opened = new js.Promise[Option[js.Dynamic]]((resolve, _) =>
          val request = indexedDB.open(MacroLinkCacheDB)
          request.updateDynamic("onupgradeneeded") { (_: js.Any) =>
            request.selectDynamic("result").createObjectStore(
              MacroLinkCacheStore,
              js.Dynamic.literal(keyPath = "cacheKey"),
            )
          }
          request.updateDynamic("onsuccess") { (_: js.Any) =>
            resolve(Some(request.selectDynamic("result").asInstanceOf[js.Dynamic]))
          }
          request.updateDynamic("onerror") { (_: js.Any) =>
            resolve(None)
          }
        )
        dbPromise = Some(opened)
        opened
      }

  private def idbRequest(request: js.Dynamic): js.Promise[js.Any] =
    new js.Promise[js.Any]((resolve, reject) =>
      request.updateDynamic("onsuccess") { (_: js.Any) =>
        resolve(request.selectDynamic("result"))
      }
      request.updateDynamic("onerror") { (_: js.Any) =>
        val error = request.selectDynamic("error")
        reject(if isDefined(error) then error else "IndexedDB request failed")
      }
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
    var hash = -3750763034362895579L
    var i = 0
    while i < bytes.length do
      hash ^= bytes(i).toLong & 0xffL
      hash *= 1099511628211L
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
