import java.io.{File, FileOutputStream, IOException}
import java.net.{InetSocketAddress, URLDecoder}
import java.nio.charset.StandardCharsets
import java.nio.file.{FileSystems, Files, Path}
import java.util.concurrent.{CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.AtomicReference
import java.util.zip.{ZipEntry, ZipFile, ZipOutputStream}

import com.sun.net.httpserver.{HttpExchange, HttpServer}
import org.scalajs.linker._
import org.scalajs.linker.interface.{ModuleInitializer, StandardConfig}
import org.scalajs.logging.{Level => ScalaJSLogLevel, ScalaConsoleLogger}
import sbt.*

import scala.collection.mutable
import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration.Duration
import scala.jdk.CollectionConverters._
import scala.sys.process.{Process => SysProcess, ProcessLogger}
import scala.util.control.NonFatal

object SjsCompilerBrowserIDE {
  private val CompilerJSImport = """import * as importedjszip from "jszip";"""
  private val PatchedJSImport = """import * as importedjszip from "../vendor/jszip-wrapper.js";"""
  private val CompilerWasmStreamingLoad =
    """return await WebAssembly.instantiateStreaming(fetch(resolvedURL), importsObj, options);"""
  private val CompilerWasmRobustLoad =
    """const response = await fetch(resolvedURL, { cache: "no-cache" });
      |    if (!response.ok) {
      |      throw new Error(`Failed to fetch ${resolvedURL}: ${response.status} ${response.statusText}`);
      |    }
      |    return await WebAssembly.instantiate(await response.arrayBuffer(), importsObj, options);""".stripMargin

  def bundleCompilerLibs(
      targetDir: File,
      scalaLibClasses: File,
      scalaLibSjsClasses: File,
      scalaJSLibJar: File,
      scalaJSJavaLibJar: File,
      log: Logger,
  ): File = {
    val libDir = targetDir / "node-libs"
    val scalaLibDir = libDir / "scala-lib"
    val scalaJSLibDir = libDir / "scalajs-lib"
    val sjsirDir = libDir / "sjsir"

    if (!scalaLibDir.exists()) {
      log.info(s"Copying scala-library classes from $scalaLibClasses to $scalaLibDir")
      IO.copyDirectory(scalaLibClasses, scalaLibDir)
    }

    if (!scalaJSLibDir.exists()) {
      log.info(s"Extracting scalajs-library classes from $scalaJSLibJar to $scalaJSLibDir")
      IO.createDirectory(scalaJSLibDir)
      IO.unzip(scalaJSLibJar, scalaJSLibDir, "*.class")
    }

    if (!sjsirDir.exists()) {
      log.info(s"Extracting linker inputs to $sjsirDir")
      IO.createDirectory(sjsirDir)
      IO.unzip(scalaJSLibJar, sjsirDir, "*.sjsir")
      IO.unzip(scalaJSJavaLibJar, sjsirDir, "*.sjsir")
      val scalaLibrarySjsIRFiles = (scalaLibSjsClasses ** "*.sjsir").get
      scalaLibrarySjsIRFiles.foreach { f =>
        val rel = scalaLibSjsClasses.toPath.relativize(f.toPath)
        val targetFile = sjsirDir.toPath.resolve(rel).toFile
        IO.createDirectory(targetFile.getParentFile)
        if (!targetFile.exists())
          IO.copyFile(f, targetFile)
      }
    }

    libDir
  }

  def prepareBrowserIDE(
      browserIdeDir: File,
      compilerOutputDir: File,
      compilerIRZip: File,
      scalaLibJar: File,
      scalaJSLibJar: File,
      rtJar: File,
      runtimeIRZip: File,
      exampleMacroJar: File,
      exampleMacroIRZip: File,
      jszipDist: File,
      log: Logger,
  ): File = {
    val assetsDir = browserIdeDir / "assets"
    val compilerDir = assetsDir / "compiler"
    val classpathDir = assetsDir / "classpath"
    val runtimeDir = assetsDir / "runtime"
    val examplesDir = assetsDir / "examples"
    val vendorDir = assetsDir / "vendor"

    IO.delete(assetsDir)
    Seq(compilerDir, classpathDir, runtimeDir, examplesDir, vendorDir).foreach(IO.createDirectory)

    val compilerMain = compilerOutputDir / "main.js"
    val compilerWasm = compilerOutputDir / "main.wasm"
    val compilerLoader = compilerOutputDir / "__loader.js"
    if (!compilerMain.exists() || !compilerWasm.exists() || !compilerLoader.exists())
      sys.error(s"Missing scala3-compiler-sjs fullLink output in $compilerOutputDir. Run fullLinkJS first.")

    val compilerMainContents = IO.read(compilerMain)
    val patchedMain = compilerMainContents.replace(CompilerJSImport, PatchedJSImport)
    if (patchedMain == compilerMainContents)
      sys.error(s"Could not rewrite JSZip import in ${compilerMain.getAbsolutePath}")
    val compilerLoaderContents = IO.read(compilerLoader)
    val patchedLoader = compilerLoaderContents.replace(CompilerWasmStreamingLoad, CompilerWasmRobustLoad)
    if (patchedLoader == compilerLoaderContents)
      sys.error(s"Could not rewrite WebAssembly loader in ${compilerLoader.getAbsolutePath}")

    IO.write(compilerDir / "main.js", patchedMain)
    IO.write(compilerDir / "__loader.js", patchedLoader)
    Seq(
      compilerWasm -> (compilerDir / "main.wasm"),
      compilerIRZip -> (compilerDir / "compiler-sjsir.zip"),
      rtJar -> (classpathDir / "rt.jar"),
      scalaLibJar -> (classpathDir / "scala-lib.jar"),
      scalaJSLibJar -> (classpathDir / "scalajs-lib.jar"),
      runtimeIRZip -> (runtimeDir / "runtime-sjsir.zip"),
      exampleMacroJar -> (examplesDir / "external-macro.jar"),
      exampleMacroIRZip -> (examplesDir / "external-macro-sjsir.zip"),
      jszipDist -> (vendorDir / "jszip.global.js"),
    ).foreach { case (src, dest) => IO.copyFile(src, dest) }

    IO.write(
      vendorDir / "jszip-wrapper.js",
      """import "./jszip.global.js";
        |export default globalThis.JSZip;
        |""".stripMargin,
    )
    IO.write(
      assetsDir / "manifest.json",
      """{
        |  "compilerModule": "./assets/compiler/main.js",
        |  "compilerIR": "./assets/compiler/compiler-sjsir.zip",
        |  "runtimeIR": "./assets/runtime/runtime-sjsir.zip",
        |  "classpath": [
        |    { "path": "/lib/rt.jar", "url": "./assets/classpath/rt.jar" },
        |    { "path": "/lib/scala-lib.jar", "url": "./assets/classpath/scala-lib.jar" },
        |    { "path": "/lib/scalajs-lib.jar", "url": "./assets/classpath/scalajs-lib.jar" }
        |  ]
        |}
        |""".stripMargin,
    )

    log.info(s"Prepared browser IDE assets in $browserIdeDir")
    browserIdeDir
  }

  def zipIRClasspath(classpathEntries: Seq[File], targetZip: File): File = {
    val files = mutable.LinkedHashMap.empty[String, Array[Byte]]
    def add(path: String, bytes: Array[Byte]): Unit = {
      val normalized = path.replace('\\', '/').stripPrefix("/")
      if (normalized.endsWith(".sjsir") && !files.contains(normalized))
        files(normalized) = bytes
    }

    classpathEntries.foreach {
      case entry if entry.isDirectory =>
        (entry ** "*.sjsir").get.foreach { file =>
          IO.relativize(entry, file).foreach(relativePath => add(relativePath, IO.readBytes(file)))
        }
      case entry if entry.isFile && (entry.getName.endsWith(".jar") || entry.getName.endsWith(".zip")) =>
        val zipFile = new ZipFile(entry)
        try zipFile.entries().asScala.foreach { zipEntry =>
          if (!zipEntry.isDirectory && zipEntry.getName.endsWith(".sjsir")) {
            val in = zipFile.getInputStream(zipEntry)
            try add(zipEntry.getName, in.readAllBytes())
            finally in.close()
          }
        }
        finally zipFile.close()
      case _ =>
    }

    writeZip(targetZip, files.toSeq)
  }

  def zipDirectory(sourceDir: File, targetZip: File): File =
    writeZip(
      targetZip,
      (sourceDir ** "*").get.filter(_.isFile).flatMap { file =>
        IO.relativize(sourceDir, file).map(_ -> IO.readBytes(file))
      },
    )

  private def writeZip(targetZip: File, files: Seq[(String, Array[Byte])]): File = {
    IO.createDirectory(targetZip.getParentFile)
    val zipStream = new ZipOutputStream(new FileOutputStream(targetZip))
    try files.sortBy(_._1).foreach { case (path, bytes) =>
      zipStream.putNextEntry(new ZipEntry(path))
      zipStream.write(bytes)
      zipStream.closeEntry()
    }
    finally zipStream.close()
    targetZip
  }

  def extractRTJar(targetRTJar: File): Unit = {
    val fs = FileSystems.getFileSystem(java.net.URI.create("jrt:/"))
    IO.createDirectory(targetRTJar.getParentFile)

    val zipStream = new ZipOutputStream(new FileOutputStream(targetRTJar))
    try {
      val javaBasePath = fs.getPath("modules", "java.base")
      Files.walk(javaBasePath).forEach { p =>
        if (Files.isRegularFile(p)) {
          try {
            val data = Files.readAllBytes(p)
            val outPath = javaBasePath.relativize(p).iterator().asScala.mkString("/")
            val ze = new ZipEntry(outPath)
            zipStream.putNextEntry(ze)
            zipStream.write(data)
            zipStream.closeEntry()
          } catch {
            case NonFatal(t) =>
              throw new IOException(s"Exception while extracting $p", t)
          }
        }
      }
    } finally {
      zipStream.close()
    }
  }
}

object SjsCompilerValidationTest {
  private val BrowserValidationTimeoutSeconds = 300L
  private val BrowserValidationTimeoutMillis = BrowserValidationTimeoutSeconds * 1000L
  private val ValidationLogExcerptChars = 8000
  private val BrowserValidationHtml =
    """<!doctype html>
      |<meta charset="utf-8">
      |<title>scala3-compiler-sjs browser validation</title>
      |<body>running</body>
      |<script type="module" src="./__sjs_browser_validation.js"></script>
      |""".stripMargin
  private val BrowserValidationScript =
    """const VALIDATION_TIMEOUT_MS = __VALIDATION_TIMEOUT_MS__;
      |const messages = [];
      |const waiters = [];
      |const log = [];
      |
      |function describe(message) {
      |  try {
      |    return JSON.stringify(message);
      |  } catch (_) {
      |    return String(message);
      |  }
      |}
      |
      |function dispatch(message) {
      |  log.push(describe(message));
      |  const index = waiters.findIndex((waiter) => waiter.types.includes(message.type));
      |  if (index >= 0) {
      |    const waiter = waiters.splice(index, 1)[0];
      |    clearTimeout(waiter.timeoutId);
      |    waiter.resolve(message);
      |  } else {
      |    messages.push(message);
      |  }
      |}
      |
      |function waitFor(types, label, timeoutMs = VALIDATION_TIMEOUT_MS) {
      |  const accepted = Array.isArray(types) ? types : [types];
      |  const queuedIndex = messages.findIndex((message) => accepted.includes(message.type));
      |  if (queuedIndex >= 0) {
      |    const [message] = messages.splice(queuedIndex, 1);
      |    return Promise.resolve(message);
      |  }
      |
      |  return new Promise((resolve, reject) => {
      |    const waiter = { types: accepted, resolve, reject, timeoutId: 0 };
      |    waiter.timeoutId = setTimeout(() => {
      |      const index = waiters.indexOf(waiter);
      |      if (index >= 0) waiters.splice(index, 1);
      |      reject(new Error(`${label} timed out. Recent worker messages:\n${log.slice(-20).join("\n")}`));
      |    }, timeoutMs);
      |    waiters.push(waiter);
      |  });
      |}
      |
      |async function report(result) {
      |  document.body.textContent = result.ok ? "ok" : result.error;
      |  await fetch("./__sjs_validation_result", {
      |    method: "POST",
      |    headers: { "content-type": "application/json" },
      |    body: JSON.stringify({ ...result, log: log.slice(-50) }),
      |  });
      |}
      |
      |async function runCase(worker, name, files, expectedOutput) {
      |  worker.postMessage({ type: "run", files });
      |  const result = await waitFor(["run-result", "runtime-error"], `${name} result`);
      |  if (result.type === "runtime-error") {
      |    throw new Error(`${name} failed before producing a run result: ${result.error}`);
      |  }
      |  if (!result.ok) {
      |    throw new Error(`${name} failed:\n${result.output}`);
      |  }
      |  if (result.output !== expectedOutput) {
      |    throw new Error(`${name} printed ${JSON.stringify(result.output)}, expected ${JSON.stringify(expectedOutput)}`);
      |  }
      |
      |  const compileDuration = await waitFor("compile-duration", `${name} compile duration`);
      |  const compilerLog = (compileDuration.compilerLog ?? []).join("\n");
      |  if (compilerLog.includes("Scala.js macro entry point")) {
      |    throw new Error(`${name} leaked the internal macro relink diagnostic:\n${compilerLog}`);
      |  }
      |}
      |
      |const helloFiles = [
      |  {
      |    path: "Main.scala",
      |    content: `@main def hello() = println("Hello, World!")\n`,
      |  },
      |];
      |
      |const macroFiles = [
      |  {
      |    path: "mymacros/MyMacros.scala",
      |    content: `package mymacros
      |
      |import scala.quoted.*
      |
      |object MyMacros:
      |  inline def shout(inline x: String): String = \${ shoutImpl('x) }
      |
      |  def shoutImpl(x: Expr[String])(using Quotes): Expr[String] =
      |    Expr(x.valueOrAbort.toUpperCase)
      |`,
      |  },
      |  {
      |    path: "Main.scala",
      |    content: `import mymacros.MyMacros
      |
      |@main def macroDemo() =
      |  println(MyMacros.shout("hello from the browser"))
      |`,
      |  },
      |];
      |
      |try {
      |  const worker = new Worker(new URL(`./worker.js?validation=${Date.now()}`, import.meta.url), { type: "module" });
      |  worker.addEventListener("message", (event) => dispatch(event.data));
      |  worker.addEventListener("error", (event) => {
      |    event.preventDefault();
      |    dispatch({ type: "runtime-error", error: event.message || "worker error" });
      |  });
      |
      |  const ready = await waitFor(["ready", "runtime-error"], "compiler worker readiness");
      |  if (ready.type === "runtime-error") {
      |    throw new Error(ready.error);
      |  }
      |
      |  await runCase(worker, "browser hello world", helloFiles, "Hello, World!\n");
      |  await runCase(worker, "browser hello world again", helloFiles, "Hello, World!\n");
      |  await runCase(worker, "browser macro", macroFiles, "HELLO FROM THE BROWSER\n");
      |  await runCase(worker, "browser macro again", macroFiles, "HELLO FROM THE BROWSER\n");
      |  worker.terminate();
      |  await report({ ok: true });
      |} catch (error) {
      |  await report({ ok: false, error: error instanceof Error ? `${error.message}\n${error.stack ?? ""}` : String(error) });
      |}
      |""".stripMargin.replace("__VALIDATION_TIMEOUT_MS__", BrowserValidationTimeoutMillis.toString)

  def runBrowserIDEValidation(browserIdeDir: File, targetDir: File, log: Logger): Unit = {
    if (!browserIdeDir.exists())
      sys.error(s"Missing browser IDE directory: ${browserIdeDir.getAbsolutePath}")

    val chrome = findChrome().getOrElse {
      sys.error("Could not find Chrome/Chromium for scala3-compiler-sjs browser validation. Set CHROME_BIN to a Chrome executable.")
    }
    val result = new AtomicReference[String]()
    val reported = new CountDownLatch(1)
    val server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0)

    server.createContext("/__sjs_validation_result", exchange => {
      try {
        if (exchange.getRequestMethod != "POST")
          sendText(exchange, 405, "method not allowed")
        else {
          result.set(readBody(exchange))
          sendText(exchange, 200, "ok")
          reported.countDown()
        }
      } catch {
        case NonFatal(t) =>
          result.set(s"""{"ok":false,"error":"${t.getMessage}"}""")
          try sendText(exchange, 500, "error")
          catch { case NonFatal(_) => () }
          reported.countDown()
      }
    })
    server.createContext("/__sjs_browser_validation.html", exchange => sendText(exchange, 200, BrowserValidationHtml, "text/html; charset=utf-8"))
    server.createContext("/__sjs_browser_validation.js", exchange => sendText(exchange, 200, BrowserValidationScript, "text/javascript; charset=utf-8"))
    server.createContext("/", exchange => serveBrowserIDEFile(browserIdeDir, exchange))

    IO.delete(targetDir)
    IO.createDirectory(targetDir)
    val chromeOutput = new StringBuilder
    var process: java.lang.Process = null

    server.start()
    try {
      val validationUrl = s"http://127.0.0.1:${server.getAddress.getPort}/__sjs_browser_validation.html"
      val userDataDir = targetDir / "chrome-profile"
      val command = Seq(
        chrome,
        "--headless=new",
        "--disable-gpu",
        "--disable-dev-shm-usage",
        "--no-sandbox",
        s"--user-data-dir=${userDataDir.getAbsolutePath}",
        "--enable-features=WebAssemblyJSPI",
        validationUrl,
      )

      log.info(s"Running scala3-compiler-sjs browser validation in ${new File(chrome).getName}")
      val builder = new ProcessBuilder(command: _*)
      builder.directory(browserIdeDir)
      builder.redirectErrorStream(true)
      process = builder.start()
      val outputReader = new Thread(new Runnable {
        override def run(): Unit = {
          val source = scala.io.Source.fromInputStream(process.getInputStream)
          try source.getLines().foreach { line =>
            chromeOutput.synchronized {
              chromeOutput.append(line).append('\n')
            }
          }
          finally source.close()
        }
      }, "scala3-compiler-sjs-browser-validation-chrome-output")
      outputReader.setDaemon(true)
      outputReader.start()

      val deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(BrowserValidationTimeoutSeconds)
      var done = false
      while (!done && System.nanoTime() < deadline) {
        done = reported.await(1, TimeUnit.SECONDS)
        if (!done && process != null && !process.isAlive)
          sys.error(s"Chrome exited before reporting the browser validation result.\n${truncate(chromeOutput.toString, ValidationLogExcerptChars)}")
      }
      if (!done)
        sys.error(s"Timed out after ${BrowserValidationTimeoutSeconds}s waiting for the browser validation result.\n${truncate(chromeOutput.toString, ValidationLogExcerptChars)}")

      val body = Option(result.get()).getOrElse("")
      if (!body.contains(""""ok":true"""))
        sys.error(s"scala3-compiler-sjs browser validation failed:\n$body\n${truncate(chromeOutput.toString, ValidationLogExcerptChars)}")

      log.info("scala3-compiler-sjs browser validation test passed")
    } finally {
      if (process != null && process.isAlive)
        process.destroyForcibly()
      server.stop(0)
    }
  }

  private def findChrome(): Option[String] =
    sys.env.get("CHROME_BIN").filter(_.nonEmpty).orElse {
      Seq("google-chrome", "google-chrome-stable", "chromium", "chromium-browser").flatMap(findCommand).headOption
    }

  private def findCommand(command: String): Option[String] = {
    val output = new StringBuilder
    val exit = scala.sys.process.Process(Seq("sh", "-lc", s"command -v $command")).!(scala.sys.process.ProcessLogger(
      line => output.append(line).append('\n'),
      _ => (),
    ))
    if (exit == 0) {
      val path = output.toString.trim
      if (path.nonEmpty) Some(path) else None
    } else None
  }

  private def serveBrowserIDEFile(browserIdeDir: File, exchange: HttpExchange): Unit = {
    try {
      if (exchange.getRequestMethod != "GET" && exchange.getRequestMethod != "HEAD")
        sendText(exchange, 405, "method not allowed")
      else {
        val rawPath = exchange.getRequestURI.getPath.stripPrefix("/")
        val decoded = URLDecoder.decode(rawPath, StandardCharsets.UTF_8.name())
        val relativePath = if (decoded.isEmpty) "index.html" else decoded
        val base = browserIdeDir.toPath.normalize()
        val file = base.resolve(relativePath).normalize()
        if (!file.startsWith(base) || !Files.isRegularFile(file))
          sendText(exchange, 404, "not found")
        else
          sendBytes(exchange, 200, contentType(file.getFileName.toString), Files.readAllBytes(file))
      }
    } catch {
      case NonFatal(t) => sendText(exchange, 500, t.getMessage)
    }
  }

  private def contentType(fileName: String): String = {
    val lower = fileName.toLowerCase
    if (lower.endsWith(".html")) "text/html; charset=utf-8"
    else if (lower.endsWith(".js") || lower.endsWith(".mjs")) "text/javascript; charset=utf-8"
    else if (lower.endsWith(".css")) "text/css; charset=utf-8"
    else if (lower.endsWith(".json")) "application/json; charset=utf-8"
    else if (lower.endsWith(".wasm")) "application/wasm"
    else if (lower.endsWith(".zip") || lower.endsWith(".jar")) "application/zip"
    else "application/octet-stream"
  }

  private def readBody(exchange: HttpExchange): String = {
    val in = exchange.getRequestBody
    try new String(in.readAllBytes(), StandardCharsets.UTF_8)
    finally in.close()
  }

  private def sendText(exchange: HttpExchange, status: Int, text: String, contentType: String = "text/plain; charset=utf-8"): Unit =
    sendBytes(exchange, status, contentType, text.getBytes(StandardCharsets.UTF_8))

  private def sendBytes(exchange: HttpExchange, status: Int, contentType: String, bytes: Array[Byte]): Unit = {
    exchange.getResponseHeaders.set("content-type", contentType)
    exchange.getResponseHeaders.set("cache-control", "no-store")
    exchange.sendResponseHeaders(status, bytes.length.toLong)
    val out = exchange.getResponseBody
    try out.write(bytes)
    finally out.close()
  }

  private def truncate(value: String, maxLength: Int): String =
    if (value.length <= maxLength) value else value.take(maxLength) + "\n..."
}

object SjsCompilerNodeUnitTest {
  private val BootstrapperSuffix = "$scalajs$junit$bootstrapper.sjsir"
  private val SuccessMarker = "scala.js sjsJUnitTests passed"

  def run(
      compilerMain: File,
      sources: Seq[File],
      compilerOptions: Seq[String],
      classpath: Seq[File],
      linkerClasspath: Seq[File],
      jsEnvScripts: Seq[File],
      nodeFlags: Seq[String],
      targetDir: File,
      log: Logger
  ): Unit = {
    val sourceDir = new File(targetDir, "src")
    val classesDir = new File(targetDir, "classes")
    val linkedDir = new File(targetDir, "linked")

    sources.foreach { source =>
      if (!source.isFile)
        sys.error(s"Missing sjsJUnitTests source: $source")
    }
    jsEnvScripts.foreach { script =>
      if (!script.isFile)
        sys.error(s"Missing sjsJUnitTests JS env input: $script")
    }

    IO.delete(sourceDir)
    IO.delete(classesDir)
    IO.delete(linkedDir)
    IO.createDirectory(targetDir)
    IO.createDirectory(sourceDir)
    IO.createDirectory(classesDir)
    IO.createDirectory(linkedDir)

    val runnerSource = new File(sourceDir, "Test.scala")
    val compilerLauncher = new File(targetDir, "run-compiler.mjs")
    IO.write(
      compilerLauncher,
      s"""import { pathToFileURL } from "node:url";
         |const compiler = await import(pathToFileURL(${jsString(compilerMain.getAbsolutePath)}).href);
         |const code = await compiler.runScala3CompilerSJSAsync(process.argv.slice(2));
         |process.exit(code);
         |""".stripMargin
    )

    val compilerClasspath = classpath.map(_.getAbsolutePath).mkString(File.pathSeparator)
    runCompiler(
      compilerLauncher,
      compilerClasspath,
      classesDir,
      compilerOptions,
      sources,
      nodeFlags,
      "scala3-compiler-sjs sjsJUnitTests compilation",
      log
    )

    val testClasses = discoverTestClasses(classesDir)
    if (testClasses.isEmpty)
      sys.error(s"Did not find any generated Scala.js JUnit bootstrappers in $classesDir")
    log.info(s"Discovered ${testClasses.size} sjsJUnitTests test classes from generated JUnit bootstrappers")

    IO.write(runnerSource, junitRunner(testClasses))
    runCompiler(
      compilerLauncher,
      (classesDir +: classpath).map(_.getAbsolutePath).mkString(File.pathSeparator),
      classesDir,
      compilerOptions,
      Seq(runnerSource),
      nodeFlags,
      "scala3-compiler-sjs sjsJUnitTests runner compilation",
      log
    )

    val linked = link(classesDir +: linkerClasspath, linkedDir.toPath)
    val output = runProcess(
      linkedCommand(jsEnvScripts, linked, targetDir),
      "scala3-compiler-sjs sjsJUnitTests execution",
      log
    )
    if (!output.contains(SuccessMarker))
      sys.error(s"scala3-compiler-sjs sjsJUnitTests did not print the expected output:\n$output")

    log.info("scala3-compiler-sjs sjsJUnitTests passed")
  }

  private def link(classpath: Seq[File], linkedDir: Path): Path = {
    implicit val ec: ExecutionContext = ExecutionContext.global

    val logger = new ScalaConsoleLogger(ScalaJSLogLevel.Warn)
    val config = StandardConfig()
      .withCheckIR(true)
      .withSemantics(build.TestSuiteLinkerOptions.semantics _)
      .withSourceMap(false)
      .withBatchMode(true)
    val linker = StandardImpl.linker(config)
    val cache = StandardImpl.irFileCache().newCache
    val moduleInitializers =
      build.TestSuiteLinkerOptions.moduleInitializers :+
        ModuleInitializer.mainMethodWithArgs("Test", "main", Nil)

    val result =
      PathIRContainer
        .fromClasspath(classpath.map(_.toPath))
        .map(_._1)
        .flatMap(cache.cached)
        .flatMap(linker.link(_, moduleInitializers, PathOutputDirectory(linkedDir), logger))

    val report = Await.result(result, Duration.Inf)
    if (report.publicModules.size != 1)
      sys.error(s"Expected one public Scala.js module, got ${report.publicModules.size}.")
    linkedDir.resolve(report.publicModules.head.jsFileName)
  }

  private def runCompiler(
      compilerLauncher: File,
      classpath: String,
      classesDir: File,
      compilerOptions: Seq[String],
      sources: Seq[File],
      nodeFlags: Seq[String],
      label: String,
      log: Logger
  ): Unit = {
    runProcess(
      Seq("node") ++ nodeFlags ++ Seq(
        compilerLauncher.getAbsolutePath,
        "-classpath",
        classpath,
        "-d",
        classesDir.getAbsolutePath
      ) ++ compilerOptions ++ sources.map(_.getAbsolutePath),
      label,
      log
    )
  }

  private def discoverTestClasses(classesDir: File): Seq[String] = {
    val stream = Files.walk(classesDir.toPath)
    try {
      stream.iterator.asScala
        .filter(path => path.getFileName.toString.endsWith(BootstrapperSuffix))
        .map { path =>
          val relative = classesDir.toPath.relativize(path).toString.replace(File.separatorChar, '.')
          relative.stripSuffix(BootstrapperSuffix)
        }
        .toSeq
        .distinct
        .sorted
    } finally {
      stream.close()
    }
  }

  private def junitRunner(testClasses: Seq[String]): String = {
    val tests = testClasses.map(c => "    " + scalaString(c)).mkString("Array(\n", ",\n", "\n  )")
    s"""import scala.scalajs.js
       |import sbt.testing._
       |
       |object Test {
       |  private val TestClassNames: Array[String] =
       |    $tests
       |
       |  private object JUnitFingerprint extends AnnotatedFingerprint {
       |    def annotationName(): String = "org.junit.Test"
       |    def isModule(): Boolean = false
       |  }
       |
       |  private final class CountingEventHandler extends EventHandler {
       |    var succeeded: Int = 0
       |    var failed: Int = 0
       |    var skipped: Int = 0
       |
       |    def handle(event: Event): Unit = {
       |      event.status() match {
       |        case Status.Success => succeeded += 1
       |        case Status.Skipped | Status.Ignored | Status.Pending => skipped += 1
       |        case _ => failed += 1
       |      }
       |    }
       |  }
       |
       |  private object ConsoleLogger extends Logger {
       |    def ansiCodesSupported(): Boolean = false
       |    def error(message: String): Unit = println("[error] " + message)
       |    def warn(message: String): Unit = println("[warn] " + message)
       |    def info(message: String): Unit = println(message)
       |    def debug(message: String): Unit = ()
       |    def trace(t: Throwable): Unit = {
       |      println(t.toString)
       |      t.getStackTrace.foreach(frame => println("    at " + frame.toString))
       |    }
       |  }
       |
       |  def main(args: Array[String]): Unit = {
       |    val handler = new CountingEventHandler
       |    val runner = new org.scalajs.junit.JUnitFramework()
       |      .runner(Array("-s", "-n"), Array.empty[String], null)
       |
       |    def finish(): Unit = {
       |      val total = handler.succeeded + handler.failed + handler.skipped
       |      println(
       |        "scala.js sjsJUnitTests completed: " +
       |        handler.succeeded + " succeeded, " +
       |        handler.failed + " failed, " +
       |        handler.skipped + " skipped, " +
       |        total + " total"
       |      )
       |      runner.done()
       |      if (handler.failed == 0) {
       |        println("$SuccessMarker")
       |      } else {
       |        js.Dynamic.global.process.exit(1)
       |      }
       |    }
       |
       |    def runAt(index: Int): Unit = {
       |      if (index == TestClassNames.length) {
       |        finish()
       |      } else {
       |        val taskDef = new TaskDef(
       |          TestClassNames(index),
       |          JUnitFingerprint,
       |          false,
       |          Array[Selector](new SuiteSelector)
       |        )
       |        val tasks = runner.tasks(Array(taskDef))
       |        if (tasks.length != 1) {
       |          throw new AssertionError("Expected one JUnit task for " + TestClassNames(index))
       |        }
       |        tasks(0).execute(handler, Array(ConsoleLogger), { (next: Array[Task]) =>
       |          if (next.nonEmpty)
       |            throw new AssertionError("Unexpected nested JUnit tasks for " + TestClassNames(index))
       |          runAt(index + 1)
       |        })
       |      }
       |    }
       |
       |    runAt(0)
       |  }
       |}
       |""".stripMargin
  }

  private def linkedCommand(jsEnvScripts: Seq[File], linked: Path, targetDir: File): Seq[String] = {
    val launcher = new File(targetDir, "run-linked.cjs")
    val linkedFile = linked.toAbsolutePath.toString
    val prelude = Seq(
      """const fs = require("node:fs");""",
      """const vm = require("node:vm");""",
      "globalThis.require = require;",
    )
    val linkedRun =
      s"""vm.runInThisContext(fs.readFileSync(${jsString(linkedFile)}, "utf8"), { filename: ${jsString(linkedFile)} });"""
    IO.write(
      launcher,
      (prelude ++ jsEnvScripts.map(script => s"require(${jsString(script.getAbsolutePath)});") :+ linkedRun)
        .mkString("", "\n", "\n")
    )
    Seq("node", launcher.getAbsolutePath)
  }

  private def runProcess(command: Seq[String], label: String, log: Logger): String = {
    val output = new StringBuilder
    log.info(s"Running $label")
    log.debug(command.mkString(" ", " ", ""))
    val exit = SysProcess(command).!(ProcessLogger(
      line => {
        output.append(line).append('\n')
        log.info(line)
      },
      line => {
        output.append(line).append('\n')
        log.warn(line)
      }
    ))
    val text = output.toString
    if (exit != 0)
      sys.error(s"$label failed with exit code $exit:\n$text")
    text
  }

  private def scalaString(value: String): String =
    "\"" + value.iterator.flatMap {
      case '\\' => "\\\\"
      case '"' => "\\\""
      case '\n' => "\\n"
      case '\r' => "\\r"
      case '\t' => "\\t"
      case c if c < ' ' || c > '~' => "\\u%04x".format(c.toInt)
      case c => c.toString
    } + "\""

  private def jsString(value: String): String =
    "\"" + value.iterator.flatMap {
      case '\\' => "\\\\"
      case '"' => "\\\""
      case '\n' => "\\n"
      case '\r' => "\\r"
      case '\t' => "\\t"
      case c => c.toString
    } + "\""
}
