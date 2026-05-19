package dotty.tools.browseride

import scala.collection.mutable
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.annotation.{JSGlobal, JSExportTopLevel, JSImport}
import scala.scalajs.js.typedarray.{ArrayBuffer, Uint8Array}
import scala.scalajs.js.wasm.JSPI.allowOrphanJSAwait
import scala.util.control.NonFatal

import dotty.tools.dotc.MainJS
import dotty.tools.dotc.sjsmacros.host.SjsMacroBrowserGlobals
import dotty.tools.io.JSByteArrays
import dotty.tools.sjs.JSInterop.{dynamicArray, isDefined, stringOr}

object BrowserIDEWorker:
  private val SourceRoot = "/workspace/src"
  private val RunnerSourcePath = s"$SourceRoot/BrowserIDERunner.scala"
  private val OutputDir = "/workspace/out"
  private val RunnerExportName = "runBrowserIDEProgram"
  private val UserMacroArtifactId = "browser-editor-macros"
  private val ImportedMacroRoot = "/imported-macro-artifacts"

  @js.native
  @JSImport("jszip", JSImport.Default)
  private object JSZip extends js.Object:
    def loadAsync(data: Uint8Array): js.Promise[ZipHandle] = js.native

  @js.native
  @JSGlobal("fetch")
  private def fetch(url: String): js.Promise[FetchResponse] = js.native

  @js.native
  private trait FetchResponse extends js.Object:
    val ok: Boolean = js.native
    val status: Int = js.native
    val statusText: String = js.native
    def arrayBuffer(): js.Promise[ArrayBuffer] = js.native

  @js.native
  private trait ZipHandle extends js.Object:
    val files: js.Dictionary[ZipEntry] = js.native

  @js.native
  private trait ZipEntry extends js.Object:
    val name: String = js.native
    val dir: Boolean = js.native
    def async(kind: String): js.Promise[Uint8Array] = js.native

  trait Config extends js.Object:
    val manifest: js.Dynamic
    val fs: js.Dynamic
    val jszipWrapperUrl: String

  private final case class Runtime(
      fs: js.Dynamic,
      manifest: js.Dynamic,
      macroClasspath: mutable.ArrayBuffer[String] = mutable.ArrayBuffer.empty,
      importedMacroArtifacts: mutable.ArrayBuffer[js.Dynamic] = mutable.ArrayBuffer.empty,
      var dynamicMacroArtifacts: Seq[js.Dynamic] = Nil,
      var retainedMacroRuntimeKey: String = "",
      var browserMacroRuntimeReady: Boolean = false,
      var activeTimings: Option[mutable.Map[String, Double]] = None,
  )

  private enum RunStatus:
    case AwaitingInput, Completed

  private final case class SourceFile(path: String, sourcePath: String, content: String)
  private final case class Entrypoint(key: String, qualifiedName: String, kind: String)
  private final case class Selection(entrypoint: Option[Entrypoint] = None, error: Option[String] = None)
  private final case class CompilerRun(exitCode: Int, lines: Seq[String], emittedFiles: Seq[String])
  private final case class CompileResult(
      ok: Boolean,
      exitCode: Int,
      lines: Seq[String],
      emittedFiles: Seq[String],
      irFiles: Seq[BrowserLinkerBridge.IRInput],
      runtime: Runtime,
      hints: Seq[String],
      timings: mutable.Map[String, Double],
  )
  private final case class LinkedSession(moduleURL: String, runner: js.Function1[String, js.Dynamic], input: String = "")
  private enum LinkedSessionResult:
    case Failed(output: String)
    case Ready(session: LinkedSession)

  private final case class SessionResult(status: RunStatus, output: String)
  private final case class Captured[A](result: A, lines: Seq[String])

  @JSExportTopLevel("startScala3BrowserIDEWorker")
  def start(config: Config): Unit =
    Worker(config).install()

  private final class Worker(config: Config):
    private val global = BrowserJS.global
    private var runtimePromise: Option[js.Promise[Runtime]] = None
    private var executionRuntimePromise: Option[js.Promise[Seq[BrowserLinkerBridge.IRInput]]] = None
    private var browserMacroRuntimePromise: Option[js.Promise[Unit]] = None
    private var activeSession: Option[LinkedSession] = None
    private var importedMacroArtifactCounter = 0

    def install(): Unit =
      val listener: js.Function1[js.Dynamic, Unit] = event =>
        val data = event.selectDynamic("data")
        stringOr(data.selectDynamic("type"), "") match
          case "init" =>
            onComplete(ensureRuntime())(
              _ => post("ready"),
              error => post("runtime-error", "error" -> formatErrorForUser(error)),
            )
          case "run" =>
            post("status", "text" -> "Compiling and running...")
            val files = data.selectDynamic("files")
            startRun(if js.Array.isArray(files) then files.asInstanceOf[js.Array[js.Dynamic]] else js.Array())
          case "stdin" =>
            continueRun(stringOr(data.selectDynamic("line"), ""))
          case "import-macro-artifact" =>
            onComplete(importMacroArtifact(data.selectDynamic("files")))(
              output => post("macro-import-result", "ok" -> true, "output" -> output),
              error => post("macro-import-result", "ok" -> false, "output" -> formatErrorForUser(error)),
            )
          case _ => ()
      global.addEventListener("message", listener)
      initializeCompiler()

    private def initializeCompiler(): Unit =
      onComplete(ensureRuntime())(
        _ => post("ready"),
        error => post("runtime-error", "error" -> formatErrorForUser(error)),
      )

    private def ensureRuntime(): js.Promise[Runtime] =
      runtimePromise.getOrElse {
        val initialized = initializeRuntime()
        runtimePromise = Some(initialized)
        initialized
      }

    private def initializeRuntime(): js.Promise[Runtime] =
      js.async {
        post("status", "text" -> "Loading compile-time classpath...", "output" -> "Loading compile-time classpath...")
        for entry <- config.manifest.selectDynamic("classpath").asInstanceOf[js.Array[js.Dynamic]] do
          config.fs.writeBinary(entry.selectDynamic("path"), js.await(fetchBytes(entry.selectDynamic("url").toString)))
        Runtime(config.fs, config.manifest)
      }

    private def ensureExecutionRuntime(runtime: Runtime): js.Promise[Seq[BrowserLinkerBridge.IRInput]] =
      executionRuntimePromise.getOrElse {
        val initialized = loadZippedIRFiles(runtime.manifest.selectDynamic("runtimeIR").toString, "/runtime/")
        executionRuntimePromise = Some(initialized)
        initialized
      }

    private def ensureBrowserMacroRuntime(runtime: Runtime): js.Promise[Unit] =
      browserMacroRuntimePromise.getOrElse {
        val initialized = initializeBrowserMacroRuntime(runtime)
        browserMacroRuntimePromise = Some(initialized)
        initialized
      }

    private def initializeBrowserMacroRuntime(runtime: Runtime): js.Promise[Unit] =
      js.async {
        post("status", "text" -> "Preparing macro linker...")
        var compilerIRBytesPromise: Option[js.Promise[Uint8Array]] = None
        var compilerIRFilesPromise: Option[js.Promise[js.Array[BrowserLinkerBridge.IRInput]]] = None

        val compilerIRBytes: js.Function0[js.Promise[Uint8Array]] = () =>
          compilerIRBytesPromise.getOrElse {
            val loaded = fetchBytes(runtime.manifest.selectDynamic("compilerIR").toString)
            compilerIRBytesPromise = Some(loaded)
            loaded
          }

        val compilerIRFiles: js.Function0[js.Promise[js.Array[BrowserLinkerBridge.IRInput]]] = () =>
          compilerIRFilesPromise.getOrElse {
            val loaded =
              compilerIRBytes().`then`[js.Array[BrowserLinkerBridge.IRInput]] { bytes =>
                readZippedIRFilesAsync(bytes, "/compiler-ir/").`then`[js.Array[BrowserLinkerBridge.IRInput]](_.toJSArray)
              }
            compilerIRFilesPromise = Some(loaded)
            loaded
          }

        val linkCompilerModule: js.Function1[js.Array[BrowserLinkerBridge.IRInput], js.Promise[js.Dynamic]] =
          files => BrowserLinkerBridge.linkCompilerModuleAsync(files).asInstanceOf[js.Promise[js.Dynamic]]

        js.await(BrowserMacroLinkerRuntime.install(js.Dynamic.literal(
          compilerIRBytes = compilerIRBytes,
          compilerIRFiles = compilerIRFiles,
          linkCompilerModule = linkCompilerModule,
          jszipWrapperUrl = config.jszipWrapperUrl,
          recordTiming = ((name: String, durationMs: Double) => recordTiming(runtime.activeTimings, name, durationMs)),
        ).asInstanceOf[BrowserMacroLinkerRuntime.Config]))
        runtime.browserMacroRuntimeReady = true
        refreshBrowserMacroArtifacts(runtime)
      }

    private def refreshBrowserMacroArtifacts(runtime: Runtime): Unit =
      if runtime.browserMacroRuntimeReady then
        global.updateDynamic(SjsMacroBrowserGlobals.MacroArtifacts)(
          (runtime.importedMacroArtifacts.toSeq ++ runtime.dynamicMacroArtifacts).toJSArray
        )

    private def importMacroArtifact(value: js.Any): js.Promise[String] =
      ensureRuntime().`then`[String] { runtime =>
        val files = dynamicArray(value).map { file =>
          js.Dynamic.literal(
            name = safeAssetName(stringOr(file.selectDynamic("name"), "")),
            bytes = JSByteArrays.uint8ArrayOrEmpty(file.selectDynamic("bytes")),
          )
        }
        val jars = files.filter(_.selectDynamic("name").toString.toLowerCase.endsWith(".jar"))
        val irZips = files.filter { file =>
          val name = file.selectDynamic("name").toString.toLowerCase
          name.endsWith(".zip") && !name.endsWith(".jar")
        }
        if irZips.isEmpty then
          throw js.JavaScriptException("Select a Scala.js macro implementation IR .zip file.")

        importedMacroArtifactCounter += 1
        val baseId = safeAssetName(irZips.head.selectDynamic("name").toString.replaceAll("(?i)(?:-sjsir)?\\.zip$", ""))
        val id = s"$baseId-$importedMacroArtifactCounter"
        val classpathRoot = s"$ImportedMacroRoot/$id/classpath"
        readAllZippedIRFiles(irZips.toSeq).`then`[String] { implementationIR =>
          if implementationIR.isEmpty then
            throw js.JavaScriptException("The selected macro implementation zip does not contain any .sjsir files.")

          runtime.fs.mkdir(classpathRoot)
          val classpathEntries = jars.map { file =>
            val path = s"$classpathRoot/${file.selectDynamic("name")}"
            runtime.fs.writeBinary(path, JSByteArrays.uint8ArrayOrEmpty(file.selectDynamic("bytes")))
            path
          }

          runtime.macroClasspath ++= classpathEntries
          runtime.importedMacroArtifacts += js.Dynamic.literal(
            id = id,
            implementationIR = implementationIR.toJSArray,
          )
          MainJS.clearRetainedMacroModules()
          runtime.retainedMacroRuntimeKey = ""
          refreshBrowserMacroArtifacts(runtime)

          Seq(
            s"Imported macro library: $baseId",
            s"Classpath jars: ${classpathEntries.length}",
            s"Implementation IR files: ${implementationIR.length}",
            "Packages: any missing macro package",
          ).mkString("\n")
        }
      }

    private def runCompiler(
        runtime: Runtime,
        sourcePaths: Seq[String],
        macroArtifacts: Seq[js.Dynamic],
        timings: mutable.Map[String, Double],
    ): js.Promise[CompilerRun] =
      js.async {
        val compileStartedAt = nowMs()
        val previousTimings = runtime.activeTimings
        runtime.activeTimings = Some(timings)
        runtime.dynamicMacroArtifacts = macroArtifacts
        refreshBrowserMacroArtifacts(runtime)

        try
          val setupArgs = Seq(
            "-classpath",
            classpathArgument(runtime),
            "-d",
            OutputDir,
          ).toJSArray
          val hasMacroArtifacts = runtime.importedMacroArtifacts.nonEmpty || runtime.dynamicMacroArtifacts.nonEmpty
          if hasMacroArtifacts then
            js.await(measureTiming(timings, "macro assets")(ensureBrowserMacroRuntime(runtime)))
          val recordPhaseTiming: js.Function2[String, Double, Unit] =
            (name: String, durationMs: Double) => recordTiming(Some(timings), name, durationMs)
          val compile =
            if hasMacroArtifacts then
              MainJS.runBrowserSessionWithRetainedMacroCompilerAndPhaseTimingAsync(setupArgs, sourcePaths.toJSArray, recordPhaseTiming)
            else
              MainJS.runBrowserSessionWithPhaseTimingAsync(setupArgs, sourcePaths.toJSArray, recordPhaseTiming)
          val captured = js.await(captureConsole(compile))
          CompilerRun(
            exitCode = captured.result,
            lines = captured.lines,
            emittedFiles = filesAt(runtime.fs, OutputDir),
          )
        finally
          runtime.activeTimings = previousTimings
          recordTiming(Some(timings), "compile", nowMs() - compileStartedAt)
      }

    private def compileToIR(sourceOrFiles: js.Any): js.Promise[CompileResult] =
      js.async {
        val timings = mutable.LinkedHashMap.empty[String, Double]
        val runtime = js.await(ensureRuntime())
        val sourceFiles = normalizeSourceFiles(sourceOrFiles)
        val sourcePaths = sourceFiles.map(_.sourcePath)
        val macroArtifacts = dynamicMacroArtifacts(sourceFiles)

        runtime.fs.removeTree("/workspace")
        runtime.fs.mkdir("/workspace")
        runtime.fs.mkdir(OutputDir)
        writeSourceFiles(runtime.fs, sourceFiles)

        retainMacroModulesForCurrentSources(runtime, sourceFiles)

        def timedRunCompiler(paths: Seq[String]): CompilerRun =
          js.await(runCompiler(runtime, paths, macroArtifacts, timings))

        selectSourceEntrypoint(sourceFiles) match
          case Some(Selection(Some(entrypoint), _)) =>
            ensureParentDirectories(runtime.fs, RunnerSourcePath)
            runtime.fs.writeText(RunnerSourcePath, createRunnerSource(entrypoint))
            val compile = timedRunCompiler(sourcePaths :+ RunnerSourcePath)
            compileResult(compile, runtime, timings)
          case _ =>
            val discoveryCompile = timedRunCompiler(sourcePaths)
            if discoveryCompile.exitCode != 0 then
              compileResult(discoveryCompile, runtime, timings)
            else
              selectEntrypoint(discoveryCompile.emittedFiles, sourceFiles) match
                case Selection(Some(entrypoint), _) =>
                  runtime.fs.removeTree(OutputDir)
                  runtime.fs.mkdir(OutputDir)
                  ensureParentDirectories(runtime.fs, RunnerSourcePath)
                  runtime.fs.writeText(RunnerSourcePath, createRunnerSource(entrypoint))
                  val finalCompile = timedRunCompiler(sourcePaths :+ RunnerSourcePath)
                  compileResult(finalCompile, runtime, timings)
                case Selection(_, Some(error)) =>
                  CompileResult(false, 0, Seq(error), discoveryCompile.emittedFiles, Nil, runtime, Nil, timings)
                case _ =>
                  CompileResult(false, 0, Seq("No runnable entry point was found."), Nil, Nil, runtime, Nil, timings)
      }

    private def createLinkedSession(sourceOrFiles: js.Any): js.Promise[LinkedSessionResult] =
      js.async {
        val startedAt = nowMs()
        val compileResult = js.await(compileToIR(sourceOrFiles))
        if !compileResult.ok then
          LinkedSessionResult.Failed(summarizeCompileFailure(compileResult))
        else
          post("status", "text" -> "Linking...")
          val runtimeIR = js.await(measureTiming(compileResult.timings, "runtime IR")(ensureExecutionRuntime(compileResult.runtime)))
          val linkResult = js.await(measureTiming(compileResult.timings, "program link")(
            BrowserLinkerBridge.linkModuleAsync((runtimeIR ++ compileResult.irFiles).toJSArray)
          )).asInstanceOf[js.Dynamic]
          val linkedRunner = js.await(measureTiming(compileResult.timings, "program import")(loadLinkedRunner(linkResult.selectDynamic("code").toString)))
          post("compile-duration",
            "durationMs" -> math.max(0.0, nowMs() - startedAt),
            "phases" -> compileResult.timings.collect {
              case (name, durationMs) if durationMs >= 0.5 =>
                js.Dynamic.literal(name = name, durationMs = durationMs)
            }.toJSArray,
          )
          LinkedSessionResult.Ready(LinkedSession(
            linkedRunner.selectDynamic("moduleURL").toString,
            linkedRunner.selectDynamic("runner").asInstanceOf[js.Function1[String, js.Dynamic]],
          ))
      }

    private def startRun(sourceOrFiles: js.Any): Unit =
      onComplete(js.async {
        disposeActiveSession()
        val sessionResult = js.await(createLinkedSession(sourceOrFiles))
        sessionResult match
          case LinkedSessionResult.Failed(output) =>
            post("run-result", "ok" -> false, "output" -> output)
          case LinkedSessionResult.Ready(session) =>
            activeSession = Some(session)
            post("status", "text" -> "Running...")
            postSessionResult(executeSession(session))
      })(
        _ => (),
        error =>
          disposeActiveSession()
          post("run-result", "ok" -> false, "output" -> formatErrorForUser(error)),
      )

    private def continueRun(line: String): Unit =
      activeSession match
        case None =>
          post("run-result", "ok" -> false, "output" -> "No running program is waiting for input.")
        case Some(session) =>
          val nextSession = session.copy(input = session.input + s"$line\n")
          activeSession = Some(nextSession)
          onComplete(js.async {
            post("status", "text" -> "Running...")
            postSessionResult(executeSession(nextSession))
          })(
            _ => (),
            error =>
              disposeActiveSession()
              post("run-result", "ok" -> false, "output" -> formatErrorForUser(error)),
          )

    private def executeSession(session: LinkedSession): SessionResult =
      val result = session.runner(session.input)
      val runnerOutput = stringOr(result.selectDynamic("output"), "")
      SessionResult(
        status =
          if stringOr(result.selectDynamic("status"), "") == "awaiting-input" then RunStatus.AwaitingInput
          else RunStatus.Completed,
        output = runnerOutput,
      )

    private def postSessionResult(sessionResult: SessionResult): Unit =
      sessionResult.status match
        case RunStatus.AwaitingInput =>
          post("awaiting-input", "output" -> sessionResult.output)
        case RunStatus.Completed =>
          disposeActiveSession()
          post("run-result", "ok" -> true, "output" -> sessionResult.output)

    private def loadLinkedRunner(jsCode: String): js.Promise[js.Dynamic] =
      js.async {
        val moduleURL = BrowserJS.createModuleBlobURL(jsCode)
        try
          val module = js.await(js.`import`[js.Dynamic](moduleURL))
          val runner = module.selectDynamic(RunnerExportName)
          if js.typeOf(runner) != "function" then
            throw js.JavaScriptException(s"Linked program did not export $RunnerExportName().")
          js.Dynamic.literal(moduleURL = moduleURL, runner = runner)
        catch
          case NonFatal(t) =>
            BrowserJS.revokeObjectURL(moduleURL)
            throw t
      }

    private def disposeActiveSession(): Unit =
      activeSession.foreach(session => BrowserJS.revokeObjectURL(session.moduleURL))
      activeSession = None

    private def compileResult(compile: CompilerRun, runtime: Runtime, timings: mutable.Map[String, Double]): CompileResult =
      CompileResult(
        ok = compile.exitCode == 0,
        exitCode = compile.exitCode,
        lines = compile.lines,
        emittedFiles = compile.emittedFiles,
        irFiles = if compile.exitCode == 0 then collectOutputIRFiles(runtime.fs, OutputDir) else Nil,
        runtime = runtime,
        hints = compileHints(compile.lines),
        timings = timings,
      )

    private def summarizeCompileFailure(result: CompileResult): String =
      joinOutput(Seq(
        joinOutput(result.lines).ensuring(_ => true) match
          case "" => s"Compiler exited with code ${result.exitCode}."
          case output => output,
        if result.hints.nonEmpty then "\n" + result.hints.mkString("\n") else "",
      ))

    private def fetchBytes(url: String): js.Promise[Uint8Array] =
      fetch(url).`then`[Uint8Array] { response =>
        if !response.ok then throw js.JavaScriptException(s"Failed to fetch $url: ${response.status} ${response.statusText}")
        response.arrayBuffer().`then`[Uint8Array](buffer => new Uint8Array(buffer))
      }

    private def loadZippedIRFiles(url: String, pathPrefix: String): js.Promise[Seq[BrowserLinkerBridge.IRInput]] =
      fetchBytes(url).`then`[Seq[BrowserLinkerBridge.IRInput]] { bytes =>
        readZippedIRFilesAsync(bytes, pathPrefix)
      }

    private def readZippedIRFilesAsync(zipBytes: Uint8Array, pathPrefix: String = ""): js.Promise[Seq[BrowserLinkerBridge.IRInput]] =
      JSZip.loadAsync(zipBytes).`then`[Seq[BrowserLinkerBridge.IRInput]] { zip =>
        zip.files.values
          .filter(entry => !entry.dir && entry.name.endsWith(".sjsir"))
          .toSeq
          .sortBy(_.name)
          .foldLeft(js.Promise.resolve(Seq.empty[BrowserLinkerBridge.IRInput])) { (collected, entry) =>
            collected.`then`[Seq[BrowserLinkerBridge.IRInput]] { previous =>
              entry.async("uint8array").`then`[Seq[BrowserLinkerBridge.IRInput]] { bytes =>
                previous :+ BrowserLinkerBridge.irInput(s"$pathPrefix${entry.name}", bytes)
              }
            }
          }
      }

    private def readAllZippedIRFiles(files: Seq[js.Dynamic]): js.Promise[Seq[BrowserLinkerBridge.IRInput]] =
      files.foldLeft(js.Promise.resolve(Seq.empty[BrowserLinkerBridge.IRInput])) { (collected, file) =>
        collected.`then`[Seq[BrowserLinkerBridge.IRInput]] { previous =>
          readZippedIRFilesAsync(JSByteArrays.uint8ArrayOrEmpty(file.selectDynamic("bytes"))).`then`[Seq[BrowserLinkerBridge.IRInput]] { next =>
            previous ++ next
          }
        }
      }

    private def captureConsole[A](run: js.Promise[A]): js.Promise[Captured[A]] =
      js.async {
        val lines = mutable.ArrayBuffer.empty[String]
        val console = global.selectDynamic("console")
        val originalLog = console.selectDynamic("log")
        val originalWarn = console.selectDynamic("warn")
        val originalError = console.selectDynamic("error")
        val capture: js.Function1[js.Any, Unit] = value =>
          lines += formatValue(value)
          ()
        try
          console.updateDynamic("log")(capture)
          console.updateDynamic("warn")(capture)
          console.updateDynamic("error")(capture)
          Captured(js.await(run), lines.toSeq)
        finally
          console.updateDynamic("log")(originalLog)
          console.updateDynamic("warn")(originalWarn)
          console.updateDynamic("error")(originalError)
      }

    private def classpathArgument(runtime: Runtime): String =
      runtime.manifest.selectDynamic("classpath").asInstanceOf[js.Array[js.Dynamic]]
        .map(_.selectDynamic("path").toString)
        .concat(runtime.macroClasspath.toJSArray)
        .mkString(":")

    private def collectOutputIRFiles(fs: js.Dynamic, path: String): Seq[BrowserLinkerBridge.IRInput] =
      filesAt(fs, path)
        .filter(_.endsWith(".sjsir"))
        .sorted
        .map(file => BrowserLinkerBridge.irInput(file, fs.readBinary(file).asInstanceOf[Uint8Array]))

    private def normalizeSourceFiles(value: js.Any): Seq[SourceFile] =
      val rawFiles =
        if js.Array.isArray(value) then value.asInstanceOf[js.Array[js.Dynamic]].toSeq
        else Seq(js.Dynamic.literal(path = "Main.scala", content = stringOr(value, "")))
      val seen = mutable.Set.empty[String]
      rawFiles.zipWithIndex.map { case (file, index) =>
        val path = normalizeSourcePath(stringOr(file.selectDynamic("path"), s"Main${index + 1}.scala"), index)
        if seen(path) then throw js.JavaScriptException(s"Two files have the same path: $path")
        seen += path
        SourceFile(path, s"$SourceRoot/$path", stringOr(file.selectDynamic("content"), ""))
      }

    private def normalizeSourcePath(path: String, index: Int): String =
      val parts = path.replace('\\', '/').split("/").filter(part => part.nonEmpty && part != ".")
      if parts.contains("..") then throw js.JavaScriptException(s"File paths cannot contain ..: $path")
      val normalized = if parts.isEmpty then s"Main${index + 1}.scala" else parts.mkString("/")
      if !normalized.endsWith(".scala") then throw js.JavaScriptException(s"File paths must end with .scala: $normalized")
      normalized

    private def writeSourceFiles(fs: js.Dynamic, sourceFiles: Seq[SourceFile]): Unit =
      fs.mkdir(SourceRoot)
      for file <- sourceFiles do
        ensureParentDirectories(fs, file.sourcePath)
        fs.writeText(file.sourcePath, file.content)

    private def ensureParentDirectories(fs: js.Dynamic, path: String): Unit =
      path.split("/").dropRight(1).filter(_.nonEmpty).foldLeft("") { (parent, part) =>
        val next = s"$parent/$part"
        fs.mkdir(next)
        next
      }
      ()

    private def sourceEntrypointCandidates(sourceFiles: Seq[SourceFile]): Seq[Entrypoint] =
      val mainDefPattern = raw"@main\s+(?:[\w\s\[\]:=><.,?]+\s+)?def\s+([A-Za-z_][\w]*)".r
      val objectAppPattern = raw"\bobject\s+([A-Za-z_][\w]*)\s+extends\s+(?:[A-Za-z_][\w]*\.)?App\b".r
      val objectMainPattern = raw"\bobject\s+([A-Za-z_][\w]*)[\s\S]*?\bdef\s+main\s*\(".r
      val candidates = sourceFiles.flatMap { file =>
        val packageName = inferPackageName(file.content)
        def entrypoint(m: scala.util.matching.Regex.Match, kind: String): Entrypoint =
          val qualifiedName = qualifyName(packageName, m.group(1).nn)
          Entrypoint(qualifiedName.replace('.', '/'), qualifiedName, kind)

        mainDefPattern.findAllMatchIn(file.content).map(entrypoint(_, "topLevelMain")).toSeq ++
          objectAppPattern.findAllMatchIn(file.content).map(entrypoint(_, "objectMain")).toSeq ++
          objectMainPattern.findAllMatchIn(file.content).map(entrypoint(_, "objectMain")).toSeq
      }
      deduplicateCandidates(candidates)

    private def inferPackageName(content: String): String =
      val packageParts = mutable.ArrayBuffer.empty[String]
      val packagePattern = raw"^\s*package\s+([A-Za-z_][\w]*(?:\s*\.\s*[A-Za-z_][\w]*)*)\s*:?".r
      val stopPattern = raw"^\s*(import|object|class|trait|enum|def|val|var|@main)\b".r
      var stopped = false
      for line <- content.split("\\r?\\n") if !stopped do
        packagePattern.findFirstMatchIn(line) match
          case Some(m) => packageParts += m.group(1).nn.replaceAll("\\s+", "")
          case None =>
            if stopPattern.findFirstIn(line).nonEmpty then stopped = true
      packageParts.mkString(".")

    private def selectSourceEntrypoint(sourceFiles: Seq[SourceFile]): Option[Selection] =
      val candidates = sourceEntrypointCandidates(sourceFiles)
      if candidates.isEmpty then None else Some(selectEntrypointCandidate(candidates))

    private def selectEntrypoint(emittedFiles: Seq[String], sourceFiles: Seq[SourceFile]): Selection =
      selectSourceEntrypoint(sourceFiles).getOrElse(selectEntrypointCandidate(createEntryCandidates(emittedFiles)))

    private def createEntryCandidates(emittedFiles: Seq[String]): Seq[Entrypoint] =
      val irFiles = emittedFiles.filter(_.endsWith(".sjsir")).map(outputRelativePath)
      val emittedSet = irFiles.toSet
      irFiles.map(_.stripSuffix(".sjsir")).filterNot(_.contains("$")).distinct.map { key =>
        Entrypoint(key, key.replace('/', '.'), if emittedSet(s"$key$$.sjsir") then "objectMain" else "topLevelMain")
      }.sortBy(candidate => (!isMain(candidate.qualifiedName), candidate.qualifiedName))

    private def selectEntrypointCandidate(candidates: Seq[Entrypoint]): Selection =
      if candidates.isEmpty then
        Selection(error = Some("No runnable entry point was found. Define `object Main` with `def main(args: Array[String]): Unit`, or add a single top-level `@main`."))
      else candidates.find(candidate => isMain(candidate.qualifiedName)).orElse(if candidates.length == 1 then candidates.headOption else None) match
        case Some(entrypoint) => Selection(entrypoint = Some(entrypoint))
        case None =>
          Selection(error = Some(Seq(
            "Multiple runnable entry points were found.",
            "",
            "Define `object Main`, or keep a single top-level `@main`.",
            "",
            "Detected entry points:",
          ).concat(candidates.map(candidate => s"- ${candidate.qualifiedName}")).mkString("\n")))

    private def retainMacroModulesForCurrentSources(runtime: Runtime, sourceFiles: Seq[SourceFile]): Unit =
      val key = sourceMacroRuntimeKey(sourceFiles)
      if runtime.retainedMacroRuntimeKey != key then
        MainJS.clearRetainedMacroModules()
        runtime.retainedMacroRuntimeKey = key

    private def dynamicMacroArtifacts(sourceFiles: Seq[SourceFile]): Seq[js.Dynamic] =
      val packages = macroPackages(macroSourceFiles(sourceFiles))
      if packages.isEmpty then Nil
      else Seq(js.Dynamic.literal(id = UserMacroArtifactId, macroPackages = packages.toJSArray, root = OutputDir))

    private def macroSourceFiles(sourceFiles: Seq[SourceFile]): Seq[SourceFile] =
      sourceFiles.filter(sourceFile => mayDefineQuotedMacro(sourceFile.content))

    private def macroPackages(sourceFiles: Seq[SourceFile]): Seq[String] =
      sourceFiles.map(sourceFile => inferPackageName(sourceFile.content)).distinct.sorted

    private def sourceMacroRuntimeKey(sourceFiles: Seq[SourceFile]): String =
      val macroFiles = macroSourceFiles(sourceFiles)
      if macroFiles.isEmpty then ""
      else sourceFilesKey(macroFiles)

    private def sourceFilesKey(sourceFiles: Seq[SourceFile]): String =
      def part(value: String): String = s"${value.length}:$value"
      sourceFiles.sortBy(_.path).map(file => part(file.path) + part(file.content)).mkString

    private def createRunnerSource(entrypoint: Entrypoint): String =
      val invocation = if entrypoint.kind == "objectMain" then s"${entrypoint.qualifiedName}.main(Array.empty)" else s"${entrypoint.qualifiedName}()"
      s"""import java.io.{ByteArrayOutputStream, PrintStream, Reader}
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

object BrowserIDERunner:
  private final class AwaitingInput extends RuntimeException(null, null, false, false)
  private val awaitingInput = new AwaitingInput

  private final class TerminalReader(input: String) extends Reader:
    private val data =
      if input == null then ""
      else input
    private var index = 0

    override def read(buffer: Array[Char], offset: Int, length: Int): Int =
      if length == 0 then 0
      else if index >= data.length then throw awaitingInput
      else
        val count = math.min(length, data.length - index)
        var i = 0
        while i < count do
          buffer(offset + i) = data.charAt(index + i)
          i += 1
        val chunk = data.substring(index, index + count)
        index += count
        scala.Console.print(chunk)
        count

    override def close(): Unit = ()

  @JSExportTopLevel("$RunnerExportName")
  def run(input: String): js.Object =
    val output = new ByteArrayOutputStream()
    val terminal = new PrintStream(output, true)
    def result(status: String): js.Object =
      terminal.flush()
      js.Dynamic.literal(status = status, output = output.toString("UTF-8"))
    try
      scala.Console.withOut(terminal) {
        scala.Console.withErr(terminal) {
          scala.Console.withIn(new TerminalReader(input)) {
            $invocation
          }
        }
      }
      result("completed")
    catch
      case _: AwaitingInput =>
        result("awaiting-input")
"""

    private def compileHints(lines: Seq[String]): Seq[String] =
      if joinOutput(lines).contains("Not found: readLine") then
        Seq("Hint: in Scala 3, use `import scala.io.StdIn.readLine` or call `scala.io.StdIn.readLine()`.")
      else Nil

    private def formatValue(value: js.Any): String =
      stripAnsi(if js.typeOf(value) == "string" then value.toString else js.JSON.stringify(value).toString)

    private def formatErrorForUser(value: js.Any): String =
      try stripAnsi(value.asInstanceOf[js.Dynamic].selectDynamic("message").toString)
      catch case NonFatal(_) => formatValue(value)

    private def post(messageType: String, fields: (String, js.Any)*): Unit =
      val message = js.Dynamic.literal()
      message.updateDynamic("type")(messageType)
      for (name, value) <- fields do message.updateDynamic(name)(value)
      global.postMessage(message)

    private def onComplete[A](promise: js.Promise[A])(success: A => Unit, failure: js.Any => Unit): Unit =
      promise.`then`[Unit](
        value => success(value),
        error => failure(error.asInstanceOf[js.Any]),
      )
      ()

    private def measureTiming[A](timings: mutable.Map[String, Double], name: String)(operation: => js.Promise[A]): js.Promise[A] =
      js.async {
        val startedAt = nowMs()
        try js.await(operation)
        finally recordTiming(Some(timings), name, nowMs() - startedAt)
      }

    private def recordTiming(timings: Option[mutable.Map[String, Double]], name: String, durationMs: Double): Unit =
      if durationMs.isFinite then timings.foreach { timings =>
        timings(name) = timings.getOrElse(name, 0.0) + math.max(0.0, durationMs)
      }

    private def nowMs(): Double =
      val performance = global.selectDynamic("performance")
      if isDefined(performance) && js.typeOf(performance.selectDynamic("now")) == "function" then
        performance.now().asInstanceOf[Double]
      else js.Date.now()

  private def safeAssetName(name: String): String =
    val safe = name.replace('\\', '/').split("/").lastOption.getOrElse("")
      .replaceAll("[^A-Za-z0-9._-]", "-")
      .replaceAll("^-+|-+$", "")
    if safe.nonEmpty then safe else "artifact"

  private def filesAt(fs: js.Dynamic, path: String): Seq[String] =
    fs.listFiles(path).asInstanceOf[js.Array[String]].toSeq

  private def outputRelativePath(file: String): String =
    val prefix = s"$OutputDir/"
    if file.startsWith(prefix) then file.stripPrefix(prefix) else file

  private def mayDefineQuotedMacro(content: String): Boolean =
    "\\$\\s*\\{".r.findFirstIn(content).nonEmpty || content.contains("scala.quoted")

  private def qualifyName(packageName: String, name: String): String =
    if packageName.nonEmpty then s"$packageName.$name" else name

  private def deduplicateCandidates(candidates: Seq[Entrypoint]): Seq[Entrypoint] =
    candidates.groupBy(candidate => s"${candidate.qualifiedName}:${candidate.kind}").values.map(_.head).toSeq

  private def isMain(name: String): Boolean =
    name == "Main" || name.endsWith(".Main")

  private def joinOutput(lines: Seq[String]): String =
    lines.filter(line => line != null && line.trim.nonEmpty).mkString("\n")

  private def stripAnsi(text: String): String =
    text.replaceAll("\\u001b\\[[0-9;]*m", "")
