package dotty.tools
package dotc

import dotty.tools.dotc.sjsmacros.{MacroRuntimeExports, MacroRuntimeRegistry, MissingMacroEntryPointException}
import dotty.tools.dotc.sjsmacros.host.SjsBrowserMacroLinker
import dotty.tools.dotc.sbt.interfaces.ProgressCallback
import dotty.tools.dotc.core.MacroClassPathScanner
import dotty.tools.dotc.core.Contexts.*
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

  // Internal interrupt code used to relink macro entry points, not a process exit status.
  private val MissingMacroEntryPointExitCode = 125
  // A runaway macro relink loop usually means a macro artifact is missing or stale.
  private val DefaultMaxMacroLinkRestarts = 8
  private val MaxBrowserCompilerSessions = 4
  private val MacroSessionExport = "runScala3SjsMacroSessionAsync"
  private val PublicModuleUrlsGlobalName = "__scala3CompilerSJSPublicModuleUrls"
  private val importedModuleNamespaces = mutable.LinkedHashMap.empty[String, js.Dynamic]
  private val browserCompilerSessions = mutable.LinkedHashMap.empty[String, BrowserCompilerSession]
  private var lastMissingMacroEntryPointsOrNull: MissingMacroEntryPointException | Null = null
  private val browserMacroRelink: js.Function1[js.Dynamic, js.Any] =
    (request: js.Dynamic) => SjsBrowserMacroLinker.relink(request)

  @JSExportTopLevel("runScala3CompilerSJSAsync")
  def runAsync(args: js.Array[String]): js.Promise[Int] =
    js.async {
      runImpl(args.toArray, null)
    }

  def runBrowserSessionWithPhaseTimingAsync(
      setupArgs: js.Array[String],
      sourcePaths: js.Array[String],
      recordPhaseTiming: js.Function2[String, Double, Unit],
  ): js.Promise[Int] =
    js.async {
      runBrowserSessionImpl(setupArgs.toArray, sourcePaths.toArray, new PhaseTimingProgressCallback(recordPhaseTiming))
    }

  def runBrowserSessionWithRetainedMacroCompilerAndPhaseTimingAsync(
      setupArgs: js.Array[String],
      sourcePaths: js.Array[String],
      recordPhaseTiming: js.Function2[String, Double, Unit],
  ): js.Promise[Int] =
    retainedMacroCompilerModule() match
      case Some(module) =>
        val run = module.selectDynamic(MacroSessionExport).asInstanceOf[
          js.Function5[
            js.Array[String],
            js.Array[String],
            js.Function1[js.Dynamic, js.Any],
            Int,
            js.Function2[String, Double, Unit],
            js.Promise[Int],
          ]
        ]
        run(setupArgs, sourcePaths, browserMacroRelink, DefaultMaxMacroLinkRestarts, recordPhaseTiming)
      case None =>
        runBrowserSessionWithMacroLinkingAsync(
          setupArgs,
          sourcePaths,
          browserMacroRelink,
          DefaultMaxMacroLinkRestarts,
          recordPhaseTiming,
        )

  @JSExportTopLevel("runScala3SjsMacroSessionAsync")
  def runMacroSessionAsync(
      setupArgs: js.Array[String],
      sourcePaths: js.Array[String],
      relink: js.Function1[js.Dynamic, js.Any],
      maxRestarts: Int,
      recordPhaseTiming: js.Function2[String, Double, Unit],
  ): js.Promise[Int] =
    runBrowserSessionWithMacroLinkingAsync(setupArgs, sourcePaths, relink, maxRestarts, recordPhaseTiming)

  def clearRetainedMacroModules(): Unit =
    importedModuleNamespaces.clear()
    MacroRuntimeRegistry.clearAll()
    clearPublicModuleUrls()

  private def runBrowserSessionWithMacroLinkingAsync(
      setupArgs: js.Array[String],
      sourcePaths: js.Array[String],
      relink: js.Function1[js.Dynamic, js.Any],
      maxRestarts: Int,
      recordPhaseTiming: js.Function2[String, Double, Unit] | Null,
  ): js.Promise[Int] =
    js.async {
      runBrowserSessionWithMacroLinkingImpl(
        setupArgs.toArray,
        sourcePaths.toArray,
        relink,
        maxRestarts,
        recordPhaseTiming,
        restartIndex = 1,
      )
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

  private def runImpl(args: Array[String], progressCallback: ProgressCallback | Null): Int =
    withMissingMacroEntryPointHandling(progressCallback):
      mainResultWithProgress(args, progressCallback)

  private def runBrowserSessionImpl(
      setupArgs: Array[String],
      sourcePaths: Array[String],
      progressCallback: ProgressCallback | Null,
  ): Int =
    withMissingMacroEntryPointHandling(progressCallback):
      browserSessionResultWithProgress(setupArgs, sourcePaths, progressCallback)

  private def withMissingMacroEntryPointHandling(progressCallback: ProgressCallback | Null)(operation: => Int): Int =
    try
      lastMissingMacroEntryPointsOrNull = null
      MacroRuntimeRegistry.clearAll()
      MacroRuntimeRegistry.setDefaultResolver(resolveLinkedMacroEntryPoint)
      operation
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
    finally
      progressCallback match
        case callback: PhaseTimingProgressCallback => callback.finish()
        case _ => ()

  private def mainResultWithProgress(args: Array[String], progressCallback: ProgressCallback | Null): Int =
    if progressCallback == null then mainResult(args)
    else
      progressCallback match
        case callback: PhaseTimingProgressCallback =>
          callback.withCompilerRun:
            mainResultWithProgressCallback(args, callback)
        case _ =>
          mainResultWithProgressCallback(args, progressCallback)

  private def mainResultWithProgressCallback(args: Array[String], progressCallback: ProgressCallback): Int =
    val _ = NonFatal
    val compileCtx = initCtx.fresh
    compileCtx.setProgressCallback(progressCallback)
    if process(args, compileCtx).hasErrors then 1 else 0

  private def browserSessionResultWithProgress(
      setupArgs: Array[String],
      sourcePaths: Array[String],
      progressCallback: ProgressCallback | Null,
  ): Int =
    def compile(): Int =
      browserCompilerSession(setupArgs).compile(sourcePaths, progressCallback)

    progressCallback match
      case null => compile()
      case callback: PhaseTimingProgressCallback =>
        callback.withCompilerRun(compile())
      case _ =>
        compile()

  private final class BrowserCompilerSession(setupArgs: Array[String]):
    private val rootCtx = initCtx.fresh
    rootCtx.base.retainPlatformBetweenRuns = true

    private val setupCtx: Context =
      setup(setupArgs, rootCtx, sourcesRequired = false) match
        case Some((_, ctx)) => ctx
        case None =>
          throw js.JavaScriptException("failed to configure browser compiler session")

    private val compiler = newCompiler(using setupCtx)

    def compile(sourcePaths: Array[String], progressCallback: ProgressCallback | Null): Int =
      val compileCtx = freshRunContext(progressCallback)
      val files = sourcePaths.toList.map(compileCtx.getFile)
      val reporter = doCompile(compiler, files)(using compileCtx)
      if reporter.hasErrors then 1 else 0

    def emitMacroEntryPointsIR(packageNames: Seq[String]): Seq[MacroClassPathScanner.SerializedMacroEntryPointsIR] =
      val emitCtx = freshRunContext(null)
      val run = compiler.newRun(using emitCtx)
      packageNames.toList.map(_.trim).filter(_.nonEmpty).distinct.flatMap { packageName =>
        MacroClassPathScanner.serializedMacroEntryPointsIRInPackage(packageName)(using run.runContext)
      }

    private def freshRunContext(progressCallback: ProgressCallback | Null): Context =
      val fresh = setupCtx.fresh.setReporter(new reporting.ConsoleReporter())
      if progressCallback != null then fresh.setProgressCallback(progressCallback)
      fresh

  private def browserCompilerSession(setupArgs: Array[String]): BrowserCompilerSession =
    val key = compilerSessionKey(setupArgs)
    browserCompilerSessions.getOrElseUpdate(key, {
      if browserCompilerSessions.size >= MaxBrowserCompilerSessions then
        browserCompilerSessions.remove(browserCompilerSessions.head._1)
      new BrowserCompilerSession(setupArgs.clone())
    })

  private def compilerSessionKey(args: Array[String]): String =
    args.map(arg => s"${arg.length}:$arg").mkString

  private final class PhaseTimingProgressCallback(
      recordPhaseTiming: js.Function2[String, Double, Unit],
      compilerRunName: String = "compiler run",
      compilerSetupName: String = "compiler setup",
  ) extends ProgressCallback:
    private var activePhase: String | Null = null
    private var activeStartedAt = 0.0
    private var runStartedAt = Double.NaN
    private var runSetupRecorded = true

    def withCompilerRun[A](operation: => A): A =
      runStartedAt = js.Date.now()
      runSetupRecorded = false
      try operation
      finally
        finishActivePhase()
        if !runSetupRecorded then
          recordDuration(name = compilerSetupName, startedAt = runStartedAt)
        recordDuration(name = compilerRunName, startedAt = runStartedAt)
        runStartedAt = Double.NaN
        runSetupRecorded = true

    override def informUnitStarting(phase: String, unit: CompilationUnit): Unit =
      enterPhase(phase)

    override def progress(current: Int, total: Int, currPhase: String, nextPhase: String): Boolean =
      enterPhase(currPhase)
      true

    def finish(): Unit =
      finishActivePhase()

    private def enterPhase(phase: String): Unit =
      if phase != null && phase.nonEmpty && activePhase != phase then
        if !runSetupRecorded then
          recordDuration(name = compilerSetupName, startedAt = runStartedAt)
          runSetupRecorded = true
        finishActivePhase()
        activePhase = phase
        activeStartedAt = js.Date.now()

    private def finishActivePhase(): Unit =
      activePhase match
        case null => ()
        case phase: String =>
          recordDuration(name = s"phase: $phase", startedAt = activeStartedAt)
          activePhase = null

    private def recordDuration(name: String, startedAt: Double): Unit =
      MainJS.recordDuration(recordPhaseTiming, name, startedAt)

  private def toJSMacroEntryPointsIR(entry: MacroClassPathScanner.SerializedMacroEntryPointsIR): js.Dynamic =
    js.Dynamic.literal(
      packageName = entry.packageName,
      path = entry.path,
      bytes = JSByteArrays.toUint8Array(entry.bytes),
    )

  private def runBrowserSessionWithMacroLinkingImpl(
      setupArgs: Array[String],
      sourcePaths: Array[String],
      relink: js.Function1[js.Dynamic, js.Any],
      remainingRestarts: Int,
      recordPhaseTiming: js.Function2[String, Double, Unit] | Null,
      restartIndex: Int,
  ): Int =
    val code = runBrowserSessionImpl(setupArgs, sourcePaths, phaseTimingProgressCallback(
      recordPhaseTiming,
      compilerRunName = s"compiler run $restartIndex",
      compilerSetupName = s"compiler setup $restartIndex",
    ))
    if code != MissingMacroEntryPointExitCode then code
    else if remainingRestarts <= 0 then
      val missing = lastMissingMacroEntryPointsOrNull
      val ids = if missing == null then "unknown macro entry points" else missing.ids.mkString(", ")
      throw js.JavaScriptException(s"exceeded maximum Scala.js macro relink restarts while resolving $ids")
    else
      val missing = lastMissingMacroEntryPointsOrNull
      if missing == null then
        throw js.JavaScriptException("missing macro entry point interrupt did not record any entry points")

      val emittedEntryPointsIR = recordOptionalTiming(recordPhaseTiming, "macro entrypoint IR"):
        browserCompilerSession(setupArgs).emitMacroEntryPointsIR(missing.packageNames)
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
      recordOptionalTiming(recordPhaseTiming, s"macro compiler restart $restartIndex"):
        restartWithLinkedBrowserSessionCompiler(
          linkedCompiler,
          setupArgs,
          sourcePaths,
          relink,
          remainingRestarts - 1,
          recordPhaseTiming,
          restartIndex,
        )

  private def phaseTimingProgressCallback(
      recordPhaseTiming: js.Function2[String, Double, Unit] | Null,
      compilerRunName: String = "compiler run",
      compilerSetupName: String = "compiler setup",
  ): ProgressCallback | Null =
    if recordPhaseTiming == null then null
    else new PhaseTimingProgressCallback(
      recordPhaseTiming.asInstanceOf[js.Function2[String, Double, Unit]],
      compilerRunName,
      compilerSetupName,
    )

  private def recordOptionalTiming[A](recordTiming: js.Function2[String, Double, Unit] | Null, name: String)(operation: => A): A =
    if recordTiming == null then operation
    else
      val startedAt = js.Date.now()
      try operation
      finally recordDuration(recordTiming.asInstanceOf[js.Function2[String, Double, Unit]], name, startedAt)

  private def recordDuration(recordTiming: js.Function2[String, Double, Unit], name: String, startedAt: Double): Unit =
    val durationMs = math.max(0.0, js.Date.now() - startedAt)
    if durationMs.isFinite then recordTiming(name, durationMs)

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
    js.typeOf(module.selectDynamic(MacroSessionExport)) == "function"

  private def restartWithLinkedBrowserSessionCompiler(
      linkedCompiler: js.Dynamic,
      setupArgs: Array[String],
      sourcePaths: Array[String],
      relink: js.Function1[js.Dynamic, js.Any],
      remainingRestarts: Int,
      recordPhaseTiming: js.Function2[String, Double, Unit] | Null,
      restartIndex: Int,
  ): Int =
    val linkedRecordPhaseTiming = linkedCompilerTimingRecorder(recordPhaseTiming, restartIndex + 1)
    val runValue = linkedCompiler.selectDynamic(MacroSessionExport)
    if js.typeOf(runValue) != "function" then
      throw js.JavaScriptException(s"Scala.js macro relinker returned a module without $MacroSessionExport")
    val record =
      if linkedRecordPhaseTiming != null then linkedRecordPhaseTiming.asInstanceOf[js.Function2[String, Double, Unit]]
      else ((_: String, _: Double) => ()).asInstanceOf[js.Function2[String, Double, Unit]]
    val run = runValue.asInstanceOf[
      js.Function5[
        js.Array[String],
        js.Array[String],
        js.Function1[js.Dynamic, js.Any],
        Int,
        js.Function2[String, Double, Unit],
        js.Promise[Int],
      ]
    ]
    js.await(run(setupArgs.toJSArray, sourcePaths.toJSArray, relink, remainingRestarts, record))

  private def linkedCompilerTimingRecorder(
      recordPhaseTiming: js.Function2[String, Double, Unit] | Null,
      compilerRunIndex: Int,
  ): js.Function2[String, Double, Unit] | Null =
    if recordPhaseTiming == null then null
    else
      val record = recordPhaseTiming.asInstanceOf[js.Function2[String, Double, Unit]]
      (name: String, durationMs: Double) =>
        val adjustedName =
          if name == "compiler run" then s"compiler run $compilerRunIndex"
          else if name == "compiler setup" then s"compiler setup $compilerRunIndex"
          else name
        record(adjustedName, durationMs)

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

  private def retainedMacroCompilerModule(): Option[js.Dynamic] =
    rememberedPublicModuleUrls().reverseIterator
      .flatMap(url => importModuleIfAvailable(url))
      .find(hasCompilerEntryPoint)

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
    val urls = GlobalThis.asInstanceOf[js.Dynamic].selectDynamic(PublicModuleUrlsGlobalName)
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
    val urls = globalThis.selectDynamic(PublicModuleUrlsGlobalName)
    if urls == null || js.isUndefined(urls) then
      val created = new js.Array[String]()
      globalThis.updateDynamic(PublicModuleUrlsGlobalName)(created)
      created
    else urls.asInstanceOf[js.Array[String]]

  private def clearPublicModuleUrls(): Unit =
    val globalThis = GlobalThis.asInstanceOf[js.Dynamic]
    globalThis.updateDynamic(PublicModuleUrlsGlobalName)(new js.Array[String]())
