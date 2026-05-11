package dotty.tools
package dotc

import dotty.tools.dotc.sjsmacros.{MacroRuntimeExports, MacroRuntimeRegistry, MissingMacroEntryPointException}
import dotty.tools.dotc.sjsmacros.host.SjsBrowserMacroLinker
import dotty.tools.dotc.core.MacroClassPathScanner
import dotty.tools.io.JSByteArrays
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSGlobal, JSExportTopLevel}
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.wasm.JSPI.allowOrphanJSAwait
import scala.collection.mutable
import scala.util.control.NonFatal

/** JS entry point of the `dotc` batch compiler. */
object MainJS extends JSDriver:
  @js.native
  @JSGlobal("globalThis")
  private object GlobalThis extends js.Object

  private val MissingMacroEntryPointExitCode = 125
  private val DefaultMaxMacroLinkRestarts = 8
  private val importedModuleNamespaces = mutable.LinkedHashMap.empty[String, js.Dynamic]
  private var lastMissingMacroEntryPointsOrNull: MissingMacroEntryPointException | Null = null

  @JSExportTopLevel("runScala3CompilerSJSAsync")
  def runAsync(args: js.Array[String]): js.Promise[Int] =
    js.async {
      runImpl(args.toArray)
    }

  private def runWithMacroLinkingAsync(args: js.Array[String], relink: js.Function1[js.Dynamic, js.Any]): js.Promise[Int] =
    runWithMacroLinkingAsyncWithLimit(args, relink, DefaultMaxMacroLinkRestarts)

  @JSExportTopLevel("runScala3CompilerSJSWithBrowserMacroLinkingAsync")
  def runWithBrowserMacroLinkingAsync(args: js.Array[String]): js.Promise[Int] =
    val relink: js.Function1[js.Dynamic, js.Any] =
      (request: js.Dynamic) => SjsBrowserMacroLinker.relink(request)
    runWithMacroLinkingAsync(args, relink)

  @JSExportTopLevel("runScala3CompilerSJSWithMacroLinkingAsyncWithLimit")
  def runWithMacroLinkingAsyncWithLimit(args: js.Array[String], relink: js.Function1[js.Dynamic, js.Any], maxRestarts: Int): js.Promise[Int] =
    js.async {
      try runWithMacroLinkingImpl(args.toArray, relink, maxRestarts)
      finally clearPublicModuleUrls()
    }

  override def main(args: Array[String]): Unit =
    val jsArgs = args.toJSArray
    if js.typeOf(js.Dynamic.global.process) != "undefined" && js.typeOf(js.Dynamic.global.process.exit) == "function" then
      runAsync(jsArgs).`then`[Unit](
        (code: Int) =>
          js.Dynamic.global.process.exit(code)
          (),
        (err: scala.Any) =>
          js.Dynamic.global.console.error(err.asInstanceOf[js.Any])
          js.Dynamic.global.process.exit(1)
          ()
      )
      ()
    else
      runAsync(jsArgs).`then`[Unit](
        (code: Int) =>
          if code != 0 then
            throw js.JavaScriptException(s"compiler exited with code $code")
          (),
        (err: scala.Any) =>
          throw js.JavaScriptException(err.asInstanceOf[js.Any])
      )
      ()

  private def runImpl(args: Array[String]): Int =
    try
      lastMissingMacroEntryPointsOrNull = null
      MacroRuntimeRegistry.clearAll()
      MacroRuntimeRegistry.setDefaultResolver(resolveLinkedMacroEntryPoint)
      mainResult(args)
    catch
      case ex: MissingMacroEntryPointException =>
        lastMissingMacroEntryPointsOrNull = ex
        MissingMacroEntryPointExitCode
      case jse: js.JavaScriptException =>
        logJavaScriptException(jse)
        throw jse
      case NonFatal(t) =>
        logThrowable(t)
        throw t

  private def emitMacroEntryPointsIRImpl(args: Array[String], packageNames: Seq[String]): Seq[MacroClassPathScanner.SerializedMacroEntryPointsIR] =
    val rootCtx = initCtx
    setup(args, rootCtx, sourcesRequired = false) match
      case Some((_, emitCtx)) =>
        val compiler = newCompiler(using emitCtx)
        val run = compiler.newRun(using emitCtx)
        packageNames.toList.map(_.trim).filter(_.nonEmpty).distinct.flatMap { packageName =>
          MacroClassPathScanner.serializedMacroEntryPointsIRInPackage(packageName)(using run.runContext)
        }
      case None =>
        if rootCtx.reporter.hasErrors then
          throw js.JavaScriptException("failed to configure Scala.js macro entry point IR emission")
        else Nil

  private def toJSMacroEntryPointsIR(entry: MacroClassPathScanner.SerializedMacroEntryPointsIR): js.Dynamic =
    js.Dynamic.literal(
      packageName = entry.packageName,
      path = entry.path,
      bytes = JSByteArrays.toUint8Array(entry.bytes),
    )

  private def runWithMacroLinkingImpl(args: Array[String], relink: js.Function1[js.Dynamic, js.Any], remainingRestarts: Int): Int =
    val code = runImpl(args)
    if code != MissingMacroEntryPointExitCode then code
    else if remainingRestarts <= 0 then
      val missing = lastMissingMacroEntryPointsOrNull
      val ids = if missing == null then "unknown macro entry points" else missing.ids.mkString(", ")
      throw js.JavaScriptException(s"exceeded maximum Scala.js macro relink restarts while resolving $ids")
    else
      val missing = lastMissingMacroEntryPointsOrNull
      if missing == null then
        throw js.JavaScriptException("missing macro entry point interrupt did not record any entry points")

      val emittedEntryPointsIR =
        emitMacroEntryPointsIRImpl(args, missing.packageNames)
      val entryPointsIR =
        if emittedEntryPointsIR.nonEmpty then emittedEntryPointsIR
        else missing.entryPointsIR
      val request = js.Dynamic.literal(
        protocolVersion = 1,
        ids = missing.ids.toJSArray,
        packageNames = missing.packageNames.toJSArray,
        entryPointsIR = entryPointsIR.map(toJSMacroEntryPointsIR).toJSArray,
      )
      val linkedCompiler = linkedCompilerFromRelinkResult(awaitMaybePromise(relink(request)))
      restartWithLinkedCompiler(linkedCompiler, args, relink, remainingRestarts - 1)

  private def registerMacroEntryPoints(entries: js.Array[js.Array[js.Any]]): Unit =
    entries.foreach { entry =>
      val id = entry(0).asInstanceOf[String]
      val f = entry(1).asInstanceOf[js.Function1[js.Array[js.Any], js.Any]]
      MacroRuntimeRegistry.register(id, args => f(args.map(_.asInstanceOf[js.Any]).toJSArray))
    }

  private def awaitMaybePromise(value: js.Any): js.Any =
    if value == null || js.isUndefined(value) then
      throw js.JavaScriptException("Scala.js macro relinker did not return a compiler module")

    val dynamicValue = value.asInstanceOf[js.Dynamic]
    val thenValue = dynamicValue.selectDynamic("then")
    if js.typeOf(thenValue) == "function" then
      js.await(value.asInstanceOf[js.Promise[js.Any]])
    else value

  private def linkedCompilerFromRelinkResult(value: js.Any): js.Dynamic =
    if value == null || js.isUndefined(value) then
      throw js.JavaScriptException("Scala.js macro relinker did not return a compiler module")
    else if js.typeOf(value) == "string" then
      importPublicModule(value.toString)
    else
      val module = value.asInstanceOf[js.Dynamic]
      if hasCompilerEntryPoint(module) then module
      else
        val moduleUrl = module.selectDynamic("moduleUrl")
        if js.typeOf(moduleUrl) == "string" then importPublicModule(moduleUrl.toString)
        else module

  private def importPublicModule(url: String): js.Dynamic =
    rememberPublicModuleUrl(url)
    importModule(url)

  private def hasCompilerEntryPoint(module: js.Dynamic): Boolean =
    js.typeOf(module.selectDynamic("runScala3CompilerSJSWithMacroLinkingAsyncWithLimit")) == "function" ||
      js.typeOf(module.selectDynamic("runScala3CompilerSJSAsync")) == "function"

  private def restartWithLinkedCompiler(
      linkedCompiler: js.Dynamic,
      args: Array[String],
      relink: js.Function1[js.Dynamic, js.Any],
      remainingRestarts: Int,
  ): Int =
    val runWithLimit = linkedCompiler.selectDynamic("runScala3CompilerSJSWithMacroLinkingAsyncWithLimit")
    if js.typeOf(runWithLimit) == "function" then
      val run = runWithLimit.asInstanceOf[js.Function3[js.Array[String], js.Function1[js.Dynamic, js.Any], Int, js.Promise[Int]]]
      js.await(run(args.toJSArray, relink, remainingRestarts))
    else
      val runPlain = linkedCompiler.selectDynamic("runScala3CompilerSJSAsync")
      if js.typeOf(runPlain) != "function" then
        throw js.JavaScriptException("Scala.js macro relinker returned a module without a compiler entry point")
      val run = runPlain.asInstanceOf[js.Function1[js.Array[String], js.Promise[Int]]]
      js.await(run(args.toJSArray))

  private def logThrowable(t: Throwable): Unit =
    js.Dynamic.global.console.error(
      "scala3-compiler-sjs throwable",
      t.getClass.getName.asInstanceOf[js.Any],
      Option(t.getMessage).orNull.asInstanceOf[js.Any],
      t.toString.asInstanceOf[js.Any]
    )

  private def logJavaScriptException(jse: js.JavaScriptException): Unit =
    val raw = jse.exception
    val rawDyn = raw.asInstanceOf[js.Dynamic]
    val ctorName =
      try
        val ctor = rawDyn.selectDynamic("constructor")
        if js.isUndefined(ctor) || ctor == null then null
        else
          val name = ctor.selectDynamic("name")
          if js.isUndefined(name) || name == null then null else name.toString
      catch
        case NonFatal(_) => null
    val rawStack =
      try
        val stack = rawDyn.selectDynamic("stack")
        if js.isUndefined(stack) || stack == null then null else stack.toString
      catch
        case NonFatal(_) => null

    js.Dynamic.global.console.error(
      "scala3-compiler-sjs js exception",
      ctorName.asInstanceOf[js.Any],
      raw.asInstanceOf[js.Any],
      rawStack.asInstanceOf[js.Any]
    )

  private def resolveLinkedMacroEntryPoint(id: String): Boolean =
    val packageName = MacroRuntimeExports.packageNameFromMacroId(id)

    try
      val exportName = MacroRuntimeExports.entryPointsExportName(packageName)
      currentModules().exists { module =>
        val exported = module.selectDynamic(exportName)
        if js.typeOf(exported) == "function" then
          val exportedFunction =
            exported.asInstanceOf[js.Function0[js.Array[js.Array[js.Any]]]]
          val entries = exportedFunction()
          registerMacroEntryPoints(entries)
          MacroRuntimeRegistry.hasRegistered(id)
        else false
      }
    catch
      case NonFatal(_) => false

  private def currentModules(): List[js.Dynamic] =
    currentModuleUrls().flatMap(importModuleIfAvailable)

  private def importModule(url: String): js.Dynamic =
    importedModuleNamespaces.getOrElseUpdate(url, js.await(js.`import`[js.Dynamic](url)))

  private def importModuleIfAvailable(url: String): Option[js.Dynamic] =
    try Some(importModule(url))
    catch
      case NonFatal(_) => None

  private def currentModuleUrls(): List[String] =
    val metaUrl = js.`import`.meta.selectDynamic("url").toString
    val publicModuleUrl =
      if metaUrl.endsWith("/__loader.js") then Some(metaUrl.stripSuffix("__loader.js") + "main.js")
      else None
    (rememberedPublicModuleUrls().reverse ::: metaUrl :: publicModuleUrl.toList).distinct

  private def rememberedPublicModuleUrls(): List[String] =
    val urls = GlobalThis.asInstanceOf[js.Dynamic].selectDynamic("__scala3CompilerSJSPublicModuleUrls")
    if urls == null || js.isUndefined(urls) then Nil
    else urls.asInstanceOf[js.Array[String]].toSeq.toList

  private def rememberPublicModuleUrl(url: String): Unit =
    val urls = publicModuleUrls()
    var i = 0
    while i < urls.length do
      if urls(i) == url then return
      i += 1
    urls.push(url)
    ()

  private def publicModuleUrls(): js.Array[String] =
    val globalThis = GlobalThis.asInstanceOf[js.Dynamic]
    val urls = globalThis.selectDynamic("__scala3CompilerSJSPublicModuleUrls")
    if urls == null || js.isUndefined(urls) then
      val created = new js.Array[String]()
      globalThis.updateDynamic("__scala3CompilerSJSPublicModuleUrls")(created)
      created
    else urls.asInstanceOf[js.Array[String]]

  private def clearPublicModuleUrls(): Unit =
    val globalThis = GlobalThis.asInstanceOf[js.Dynamic]
    globalThis.updateDynamic("__scala3CompilerSJSPublicModuleUrls")(new js.Array[String]())
