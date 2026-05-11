import java.io.{FileOutputStream, IOException}
import java.net.InetSocketAddress
import java.nio.charset.StandardCharsets
import java.nio.file.{FileSystems, Files}
import java.security.MessageDigest
import java.util.Base64
import java.util.zip.{ZipEntry, ZipFile, ZipOutputStream}
import com.sun.net.httpserver.{HttpExchange, HttpServer}

import sbt.*
import org.scalajs.linker.PathIRContainer
import org.scalajs.linker.PathOutputDirectory
import org.scalajs.linker.StandardImpl
import org.scalajs.linker.interface.ModuleInitializer
import org.scalajs.linker.interface.ModuleKind
import org.scalajs.linker.interface.ESVersion
import org.scalajs.linker.interface.StandardConfig
import org.scalajs.logging.Level
import org.scalajs.logging.ScalaConsoleLogger

import scala.concurrent.Await
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.Duration
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

object SjsCompilerHelloWorld {
  private val BrowserIDECompilerJSImport = """import * as importedjszip from "jszip";"""
  private val BrowserIDEPatchedJSImport = """import * as importedjszip from "../vendor/jszip-wrapper.js";"""
  private val BrowserIDECompilerLoaderJSImport = """import { load as __load } from "./__loader.js";"""
  private def browserIDECompilerLoaderJSImport(assetVersion: String): String =
    s"""import { load as __load } from "./__loader.js?v=$assetVersion";"""
  private val BrowserIDECompilerWasmLoad = """__load("./main.wasm","""
  private def browserIDECompilerWasmLoad(assetVersion: String): String =
    s"""__load("./main.wasm?v=$assetVersion","""
  private val BrowserIDECompilerWasmStreamingLoad =
    """return await WebAssembly.instantiateStreaming(fetch(resolvedURL), importsObj, options);"""
  private val BrowserIDECompilerWasmRobustLoad =
    """const status = globalThis.__scala3CompilerSJSStatus;
      |    if (typeof status === "function") status("Fetching compiler WebAssembly...");
      |    const response = await fetch(resolvedURL, { cache: "no-cache" });
      |    if (!response.ok) {
      |      throw new Error(`Failed to fetch ${resolvedURL}: ${response.status} ${response.statusText}`);
      |    }
      |    const body = await response.arrayBuffer();
      |    if (typeof status === "function") {
      |      status(`Instantiating compiler WebAssembly (${Math.round(body.byteLength / 1024 / 1024)} MB)...`);
      |    }
      |    return await WebAssembly.instantiate(body, importsObj, options);""".stripMargin
  private val MacroFixtureId = "hello-world-macro-fixture"
  private val MacroFixturePackages = Seq("smokemacros", "othermacros")

  private final case class SmokeIRFile(path: String, bytes: Array[Byte])

  private final case class SmokeMacroArtifact(
      id: String,
      macroPackages: Seq[String],
      root: File,
      implementationIR: Seq[SmokeIRFile],
  )

  private def jsonString(value: String): String =
    "\"" + value.flatMap {
      case '\\' => "\\\\"
      case '"' => "\\\""
      case '\n' => "\\n"
      case '\r' => "\\r"
      case '\t' => "\\t"
      case c if c < ' ' => "\\u%04x".format(c.toInt)
      case c => c.toString
    } + "\""

  private def readIRFiles(root: File): Seq[SmokeIRFile] =
    (root ** "*.sjsir").get.flatMap { file =>
      IO.relativize(root, file).map { relativePath =>
        SmokeIRFile(relativePath, Files.readAllBytes(file.toPath))
      }
    }.sortBy(_.path)

  private def smokeMacroArtifact(id: String, macroPackages: Seq[String], root: File): SmokeMacroArtifact =
    SmokeMacroArtifact(
      id = id,
      macroPackages = macroPackages.distinct.sorted,
      root = root,
      implementationIR = readIRFiles(root),
    )

  private def smokeRelinkCacheKey(
      entryPointsIR: Seq[SmokeIRFile],
      macroImplementationIR: Seq[SmokeIRFile],
      linkerConfig: Seq[String],
  ): String = {
    val digest = MessageDigest.getInstance("SHA-256")
    updateDigestString(digest, "scala3-sjs-macro-smoke-relink-v1")
    updateDigestIRFiles(digest, "macro-entrypoints-ir", entryPointsIR)
    updateDigestIRFiles(digest, "macro-implementation-ir", macroImplementationIR)
    linkerConfig.sorted.foreach(updateDigestString(digest, _))
    digest.digest().map(b => "%02x".format(b & 0xff)).mkString
  }

  private def updateDigestIRFiles(digest: MessageDigest, section: String, files: Seq[SmokeIRFile]): Unit = {
    updateDigestString(digest, section)
    files.sortBy(_.path).foreach { file =>
      updateDigestString(digest, file.path)
      digest.update(file.bytes)
    }
  }

  private def updateDigestString(digest: MessageDigest, value: String): Unit =
    digest.update(value.getBytes(StandardCharsets.UTF_8))

  private def browserIDEAssetVersion(files: File*): String = {
    val digest = MessageDigest.getInstance("SHA-256")
    files.foreach { file =>
      updateDigestString(digest, file.getName)
      digest.update(Files.readAllBytes(file.toPath))
    }
    digest.digest().take(8).map(b => "%02x".format(b & 0xff)).mkString
  }

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
      jszipDist: File,
      log: Logger,
  ): File = {
    val assetsDir = browserIdeDir / "assets"
    val compilerDir = assetsDir / "compiler"
    val vendorDir = assetsDir / "vendor"
    val classpathDir = assetsDir / "classpath"
    val runtimeDir = assetsDir / "runtime"

    IO.delete(assetsDir)
    Seq(compilerDir, vendorDir, classpathDir, runtimeDir).foreach(IO.createDirectory)

    val compilerMain = compilerOutputDir / "main.js"
    val compilerWasm = compilerOutputDir / "main.wasm"
    val compilerLoader = compilerOutputDir / "__loader.js"
    if (!compilerMain.exists() || !compilerWasm.exists() || !compilerLoader.exists())
      sys.error(s"Missing scala3-compiler-sjs fastLink output in $compilerOutputDir. Run fastLinkJS first.")

    val assetVersion = browserIDEAssetVersion(compilerMain, compilerWasm, compilerLoader)
    val compilerMainContents = IO.read(compilerMain)
    val compilerMainWithJSZip = compilerMainContents.replace(BrowserIDECompilerJSImport, BrowserIDEPatchedJSImport)
    if (compilerMainWithJSZip == compilerMainContents)
      sys.error(s"Could not rewrite JSZip import in ${compilerMain.getAbsolutePath}")
    val compilerMainWithVersionedLoader = compilerMainWithJSZip.replace(
      BrowserIDECompilerLoaderJSImport,
      browserIDECompilerLoaderJSImport(assetVersion),
    )
    if (compilerMainWithVersionedLoader == compilerMainWithJSZip)
      sys.error(s"Could not version WebAssembly loader import in ${compilerMain.getAbsolutePath}")
    val patchedCompilerMain = compilerMainWithVersionedLoader.replace(
      BrowserIDECompilerWasmLoad,
      browserIDECompilerWasmLoad(assetVersion),
    )
    if (patchedCompilerMain == compilerMainWithVersionedLoader)
      sys.error(s"Could not version WebAssembly file URL in ${compilerMain.getAbsolutePath}")

    val compilerLoaderContents = IO.read(compilerLoader)
    val patchedCompilerLoader = compilerLoaderContents.replace(
      BrowserIDECompilerWasmStreamingLoad,
      BrowserIDECompilerWasmRobustLoad,
    )
    if (patchedCompilerLoader == compilerLoaderContents)
      sys.error(s"Could not rewrite WebAssembly loader in ${compilerLoader.getAbsolutePath}")

    IO.write(compilerDir / "main.js", patchedCompilerMain)
    IO.write(compilerDir / "__loader.js", patchedCompilerLoader)
    Seq(
      compilerWasm -> (compilerDir / "main.wasm"),
      jszipDist -> (vendorDir / "jszip.global.js"),
      rtJar -> (classpathDir / "rt.jar"),
      scalaLibJar -> (classpathDir / "scala-lib.jar"),
      scalaJSLibJar -> (classpathDir / "scalajs-lib.jar"),
      compilerIRZip -> (compilerDir / "compiler-sjsir.zip"),
      runtimeIRZip -> (runtimeDir / "runtime-sjsir.zip"),
    ).foreach { case (src, dest) => IO.copyFile(src, dest) }

    IO.write(
      vendorDir / "jszip-wrapper.js",
      """import "./jszip.global.js";
        |
        |const JSZip = globalThis.JSZip;
        |
        |if (!JSZip) {
        |  throw new Error("JSZip failed to initialize for the browser IDE.");
        |}
        |
        |export default JSZip;
        |""".stripMargin
    )

    IO.write(
      assetsDir / "manifest.json",
      s"""{
        |  "assetVersion": "$assetVersion",
        |  "compilerModule": "./assets/compiler/main.js",
        |  "compilerIR": "./assets/compiler/compiler-sjsir.zip",
        |  "runtimeIR": "./assets/runtime/runtime-sjsir.zip",
        |  "classpath": [
        |    { "path": "/lib/rt.jar", "url": "./assets/classpath/rt.jar" },
        |    { "path": "/lib/scala-lib.jar", "url": "./assets/classpath/scala-lib.jar" },
        |    { "path": "/lib/scalajs-lib.jar", "url": "./assets/classpath/scalajs-lib.jar" }
        |  ]
        |}
        |""".stripMargin
    )

    log.info(s"Prepared browser IDE assets in $browserIdeDir")
    browserIdeDir
  }

  def zipIRClasspath(classpathEntries: Seq[File], targetZip: File): File = {
    IO.createDirectory(targetZip.getParentFile)

    val files = mutable.LinkedHashMap.empty[String, Array[Byte]]

    def add(path: String, bytes: Array[Byte]): Unit = {
      val normalized = path.replace('\\', '/').stripPrefix("/")
      if (normalized.endsWith(".sjsir") && !files.contains(normalized))
        files(normalized) = bytes
    }

    classpathEntries.foreach { entry =>
      if (entry.isDirectory) {
        (entry ** "*.sjsir").get.foreach { file =>
          IO.relativize(entry, file).foreach { relativePath =>
            add(relativePath, IO.readBytes(file))
          }
        }
      } else if (entry.isFile && (entry.getName.endsWith(".jar") || entry.getName.endsWith(".zip"))) {
        val zipFile = new ZipFile(entry)
        try {
          zipFile.entries().asScala.foreach { zipEntry =>
            if (!zipEntry.isDirectory && zipEntry.getName.endsWith(".sjsir")) {
              val in = zipFile.getInputStream(zipEntry)
              try add(zipEntry.getName, in.readAllBytes())
              finally in.close()
            }
          }
        } finally {
          zipFile.close()
        }
      }
    }

    val zipStream = new ZipOutputStream(new FileOutputStream(targetZip))
    try {
      files.toSeq.sortBy(_._1).foreach { case (path, bytes) =>
        zipStream.putNextEntry(new ZipEntry(path))
        zipStream.write(bytes)
        zipStream.closeEntry()
      }
    } finally {
      zipStream.close()
    }

    targetZip
  }

  def zipDirectory(sourceDir: File, targetZip: File): File = {
    IO.createDirectory(targetZip.getParentFile)

    val files = (sourceDir ** "*").get.filter(_.isFile)
    val zipStream = new ZipOutputStream(new FileOutputStream(targetZip))
    try {
      files.foreach { file =>
        val relative = sourceDir.toPath.relativize(file.toPath).iterator().asScala.mkString("/")
        zipStream.putNextEntry(new ZipEntry(relative))
        zipStream.write(IO.readBytes(file))
        zipStream.closeEntry()
      }
    } finally {
      zipStream.close()
    }

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

  def runTest(
      baseDir: File,
      targetDir: File,
      outputDir: File,
      libsDir: File,
      nodeFlags: Seq[String],
      compilerClasspathEntries: Seq[File],
      compilerSJSClasspathEntries: Seq[File],
      jvmSeedCompilerClasspathEntries: Seq[File],
      smokeDirName: String,
      successMessage: String,
      log: Logger,
  ): Unit = {
    IO.write(targetDir / "package.json", """{"type":"module"}""" + "\n")

    val jszipModule = baseDir / "compiler" / "node_modules" / "jszip"
    if (!jszipModule.exists())
      sys.error("Missing Node dependency `jszip`. Run `cd compiler && npm install` before executing scala3-compiler-sjs hello world tests.")

    val smokeRoot = targetDir / smokeDirName
    val macroSeedOut = smokeRoot / "macro-classpath"
    val macroEntryPointsOut = smokeRoot / "macro-entrypoints"
    val macroImplementationIROut = smokeRoot / "macro-implementation-ir"
    val compileOut = smokeRoot / "classes"
    val linkOut = smokeRoot / "linked"
    val helloSource = baseDir / "compiler" / "test-resources" / "sjs-compiler" / "HelloWorld.scala"
    val macroSource = baseDir / "compiler" / "test-resources" / "sjs-compiler" / "HelloWorldMacro.scala"
    val macroHelperSource = baseDir / "compiler" / "test-resources" / "sjs-compiler" / "HelloWorldMacroHelper.scala"
    val otherMacroSource = baseDir / "compiler" / "test-resources" / "sjs-compiler" / "HelloWorldOtherMacro.scala"
    val compilerMain = outputDir / "main.js"
    val compilerRunner = smokeRoot / "run-compiler.mjs"
    val macroScanPackages = MacroFixturePackages
    val expectedMacroIds = Seq(
      "smokemacros.MacroLibrary$#smokemacros.MacroLibrary$.macro1Signature(List(java.lang.String),java.lang.String)",
      "smokemacros.MacroLibrary$#smokemacros.MacroLibrary$.macro2Signature(List(java.lang.String),java.lang.String)",
      "othermacros.OtherMacroLibrary$#othermacros.OtherMacroLibrary$.macro3Signature(List(java.lang.String),java.lang.String)",
    )
    val macroCompilerLinkOut = smokeRoot / "macro-compiler-linked"
    val macroUseSource = smokeRoot / "MacroUse.scala"
    val macroUseOut = smokeRoot / "macro-use-classes"
    val macroUseLinkOut = smokeRoot / "macro-use-linked"
    val macroUseCompileRunner = smokeRoot / "run-macro-use-compile.mjs"

    def mkClasspath(entries: Seq[File]): String =
      entries.map(_.getAbsolutePath).mkString(java.io.File.pathSeparator)

    def runLogged(command: Seq[String], failureMessage: String): String = {
      val output = new StringBuilder
      val exit = scala.sys.process.Process(command, baseDir).!(scala.sys.process.ProcessLogger(
        out => output.append(out).append('\n'),
        err => output.append(err).append('\n'),
      ))
      if (exit != 0)
        sys.error(s"$failureMessage:\n$output")
      output.toString
    }

    def seedMacroClasspath(): Unit = {
      IO.createDirectory(macroSeedOut)
      runLogged(
        Seq(
          "java",
          "-cp",
          mkClasspath(jvmSeedCompilerClasspathEntries),
          "dotty.tools.dotc.Main",
          "-classpath",
          (libsDir / "scala-lib").getAbsolutePath,
          "-d",
          macroSeedOut.getAbsolutePath,
          "-usejavacp",
          macroSource.getAbsolutePath,
          macroHelperSource.getAbsolutePath,
          otherMacroSource.getAbsolutePath,
        ),
        "Failed to seed macro classpath",
      )
    }

    def addMacroImplementationIRToClasspath(): Unit = {
      runLogged(
        Seq("node") ++ nodeFlags ++ Seq(
          compilerRunner.getAbsolutePath,
          "-classpath", mkClasspath(compilerClasspathEntries :+ macroSeedOut),
          "-d", macroSeedOut.getAbsolutePath,
          macroSource.getAbsolutePath,
          macroHelperSource.getAbsolutePath,
          otherMacroSource.getAbsolutePath,
        ),
        "scala3-compiler-sjs failed to add macro implementation IR to the macro classpath fixture",
      )
    }

    def writeIRFiles(rootFile: File, files: Seq[SmokeIRFile]): Unit = {
      val root = rootFile.toPath.toAbsolutePath.normalize()
      IO.delete(rootFile)
      IO.createDirectory(rootFile)

      for (file <- files) {
        val relativePath = file.path
        if (relativePath.startsWith("/") || relativePath.contains('\\'))
          sys.error(s"Unsafe IR path: $relativePath")

        val target = root.resolve(relativePath).normalize()
        if (!target.startsWith(root))
          sys.error(s"Unsafe IR path: $relativePath")

        val parent = target.getParent
        if (parent != null)
          Files.createDirectories(parent)
        Files.write(target, file.bytes)
      }
    }

    def writeMacroUseSource(target: File): Unit =
      IO.write(
        target,
        """import smokemacros.MacroLibrary
          |import othermacros.OtherMacroLibrary
          |
          |object Test:
          |  def main(args: Array[String]): Unit =
          |    println(MacroLibrary.macro1("hello macro"))
          |    println(MacroLibrary.macro2("batch macro"))
          |    println(OtherMacroLibrary.macro3("package macro"))
          |""".stripMargin
      )

    def writeResolvingMacroCompileRunner(
        target: File,
        compilerJS: File,
        compileArgs: Seq[String],
        relinkURL: String,
    ): Unit = {
      def toJSLiteral(str: String): String =
        "\"" + str.flatMap {
          case '\\' => "\\\\"
          case '"' => "\\\""
          case '\n' => "\\n"
          case '\r' => "\\r"
          case '\t' => "\\t"
          case c => c.toString
        } + "\""

      val jsCompileArgs = compileArgs.map(toJSLiteral).mkString("[", ", ", "]")
      val expectedMacroIdsJS = expectedMacroIds.map(toJSLiteral).mkString("[", ", ", "]")
      val expectedMacroPackagesJS = macroScanPackages.map(toJSLiteral).mkString("[", ", ", "]")

      IO.write(
        target,
        s"""import { Buffer } from 'node:buffer'
           |import { request as httpRequest } from 'node:http'
           |
           |const compiler = await import('${compilerJS.toURI}')
           |const relinkURL = ${toJSLiteral(relinkURL)}
           |
           |function encodeEntry(entry) {
           |  return [
           |    Buffer.from(entry.packageName, 'utf8').toString('base64'),
           |    Buffer.from(entry.path, 'utf8').toString('base64'),
           |    Buffer.from(entry.bytes).toString('base64')
           |  ].join('\\t')
           |}
           |
           |function requestLinkedCompiler(entries) {
           |  return new Promise((resolve, reject) => {
           |    const req = httpRequest(relinkURL, { method: 'POST' }, res => {
           |      let data = ''
           |      res.setEncoding('utf8')
           |      res.on('data', chunk => data += chunk)
           |      res.on('end', () => {
           |        if (res.statusCode !== 200) {
           |          reject(new Error(`macro relink service failed: $${res.statusCode} $${data}`))
           |        } else {
           |          resolve(JSON.parse(data))
           |        }
           |      })
           |    })
           |    req.on('error', reject)
           |    req.write(entries.map(encodeEntry).join('\\n'))
           |    req.end()
           |  })
           |}
           |
           |const expectedMacroIds = $expectedMacroIdsJS
           |const expectedMacroIdList = [...expectedMacroIds].sort().join('|')
           |const expectedMacroPackages = $expectedMacroPackagesJS
           |const expectedMacroPackageList = [...expectedMacroPackages].sort().join('|')
           |
           |const relinkRequests = []
           |globalThis.__scala3CompilerSJSMacroLinker = {
           |  async linkCompilerWithMacros(request) {
           |    if (request.protocolVersion !== 1) {
           |      throw new Error(`unexpected macro relink protocol version: $${request.protocolVersion}`)
           |    }
           |    const requestIds = [...request.ids].sort()
           |    const requestPackages = [...request.packageNames].sort()
           |    relinkRequests.push(requestIds.join('|'))
           |    if (requestIds.join('|') !== expectedMacroIdList) {
           |      throw new Error(`unexpected macro relink request: $${requestIds.join('|')}`)
           |    }
           |    if (requestPackages.join('|') !== expectedMacroPackageList) {
           |      throw new Error(`unexpected macro relink package: $${requestPackages.join('|')}`)
           |    }
           |
           |    const emitted = [...request.entryPointsIR]
           |    if (emitted.length !== expectedMacroPackages.length) {
           |      throw new Error(`unexpected emitted macro entrypoint count: $${emitted.length}`)
           |    }
           |
           |    const linked = await requestLinkedCompiler(emitted)
           |    const cached = await requestLinkedCompiler(emitted)
           |    if (!cached.cacheHit || cached.cacheKey !== linked.cacheKey || cached.moduleUrl !== linked.moduleUrl) {
           |      throw new Error(`unexpected macro relink cache response: $${JSON.stringify(cached)}`)
           |    }
           |    return linked.moduleUrl
           |  }
           |}
           |
           |const code = await compiler.runScala3CompilerSJSWithBrowserMacroLinkingAsync($jsCompileArgs)
           |if (code !== 0) process.exit(code)
           |
           |if (relinkRequests.length !== 1 || relinkRequests[0] !== expectedMacroIdList) {
           |  console.error(`unexpected relink requests: $${relinkRequests.join('|')}`)
           |  process.exit(1)
           |}
           |""".stripMargin
      )
    }

    def withMacroRelinkServer(macroArtifacts: Seq[SmokeMacroArtifact])(body: String => Unit): Unit = {
      val availableMacroImplementationIR = macroArtifacts.flatMap(_.implementationIR).sortBy(_.path)
      val availableMacroPackages = macroArtifacts.iterator.flatMap(_.macroPackages).toSet
      val macroArtifactDescriptors =
        macroArtifacts.map(artifact => s"${artifact.id}:${artifact.macroPackages.mkString(",")}").sorted

      def send(exchange: HttpExchange, status: Int, response: String): Unit = {
        val bytes = response.getBytes(StandardCharsets.UTF_8)
        exchange.getResponseHeaders.add("content-type", "application/json; charset=utf-8")
        exchange.sendResponseHeaders(status, bytes.length.toLong)
        val out = exchange.getResponseBody
        try out.write(bytes)
        finally out.close()
      }

      final case class PostedMacroEntryPoint(packageName: String, file: SmokeIRFile)

      def readPostedMacroEntryPoints(exchange: HttpExchange): Seq[PostedMacroEntryPoint] = {
        val body =
          try new String(exchange.getRequestBody.readAllBytes(), StandardCharsets.UTF_8)
          finally exchange.getRequestBody.close()

        body.linesIterator.filter(_.nonEmpty).map { line =>
          val parts = line.split("\t", -1)
          if (parts.length != 3)
            sys.error("Malformed macro entry point upload")

          val packageName = new String(Base64.getDecoder.decode(parts(0)), StandardCharsets.UTF_8)
          val relativePath = new String(Base64.getDecoder.decode(parts(1)), StandardCharsets.UTF_8)
          if (relativePath.startsWith("/") || relativePath.contains('\\'))
            sys.error(s"Unsafe macro entry point path: $relativePath")

          PostedMacroEntryPoint(packageName, SmokeIRFile(relativePath, Base64.getDecoder.decode(parts(2))))
        }.toSeq
      }

      val server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0)
      val linkedCompilerCache = mutable.LinkedHashMap.empty[String, File]
      server.createContext("/link", (exchange: HttpExchange) => {
        try {
          if (exchange.getRequestMethod != "POST") {
            send(exchange, 405, """{"error":"method not allowed"}""")
          } else {
            val postedEntryPoints = readPostedMacroEntryPoints(exchange)
            val requestedPackages = postedEntryPoints.map(_.packageName).toSet
            val unknownPackages = requestedPackages -- availableMacroPackages
            if (unknownPackages.nonEmpty)
              sys.error(s"Relink request contains packages missing from macro artifacts: ${unknownPackages.toList.sorted.mkString(", ")}")

            val entryPointsIR = postedEntryPoints.map(_.file)
            val macroImplementationIR = availableMacroImplementationIR
            val cacheKey = smokeRelinkCacheKey(
              entryPointsIR = entryPointsIR,
              macroImplementationIR = macroImplementationIR,
              linkerConfig = Seq(
                "moduleKind=ESModule",
                "moduleInitializers=none",
                "useWebAssembly=true",
                "esVersion=ES2018",
              ) ++ macroArtifactDescriptors.map("macroArtifact=" + _),
            )
            val shortCacheKey = cacheKey.take(16)
            val cacheHit = linkedCompilerCache.contains(cacheKey)
            val macroImplementationOut = macroImplementationIROut / cacheKey

            writeIRFiles(macroEntryPointsOut, entryPointsIR)
            writeIRFiles(macroImplementationOut, macroImplementationIR)

            val linkedJS =
              if (cacheHit) {
                log.info(s"Macro relink cache hit $shortCacheKey")
                linkedCompilerCache(cacheKey)
              } else {
                log.info(s"Macro relink cache miss $shortCacheKey implementation IR: ${macroImplementationIR.map(_.path).sorted.mkString(", ")}")
                val linked = linkScalaJSForTest(
                  compilerSJSClasspathEntries ++ Seq(macroImplementationOut, macroEntryPointsOut),
                  macroCompilerLinkOut / cacheKey,
                  Nil,
                  ModuleKind.ESModule,
                  log,
                  useWebAssembly = true,
                )
                linkedCompilerCache(cacheKey) = linked
                linked
              }
            send(exchange, 200, s"""{"moduleUrl":${jsonString(linkedJS.toURI.toASCIIString)},"cacheKey":${jsonString(cacheKey)},"cacheHit":$cacheHit}""")
          }
        } catch {
          case NonFatal(t) =>
            send(exchange, 500, s"""{"error":${jsonString(t.toString)}}""")
        } finally {
          exchange.close()
        }
      })

      server.start()
      try body(s"http://127.0.0.1:${server.getAddress.getPort}/link")
      finally server.stop(0)
    }

    IO.delete(smokeRoot)
    IO.createDirectory(compileOut)
    writeRunner(compilerRunner, compilerMain)
    seedMacroClasspath()
    addMacroImplementationIRToClasspath()
    val macroArtifacts = Seq(
      smokeMacroArtifact(
        id = MacroFixtureId,
        macroPackages = macroScanPackages,
        root = macroSeedOut,
      )
    )

    val compileLog = runLogged(
      Seq("node") ++ nodeFlags ++ Seq(
        compilerRunner.getAbsolutePath,
        "-classpath", mkClasspath(compilerClasspathEntries),
        "-d", compileOut.getAbsolutePath,
        helloSource.getAbsolutePath,
      ),
      "scala3-compiler-sjs failed to compile HelloWorld.scala",
    )

    compileLog.linesIterator
      .filter(_.startsWith("[classpath-macros]"))
      .foreach(line => log.info(line))

    writeMacroUseSource(macroUseSource)
    IO.createDirectory(macroUseOut)
    val macroUseCompileArgs = Seq(
      "-classpath", mkClasspath(compilerClasspathEntries ++ macroArtifacts.map(_.root)),
      "-d", macroUseOut.getAbsolutePath,
      macroUseSource.getAbsolutePath,
    )

    withMacroRelinkServer(macroArtifacts) { relinkURL =>
      writeResolvingMacroCompileRunner(
        macroUseCompileRunner,
        compilerMain,
        macroUseCompileArgs,
        relinkURL,
      )
      runLogged(
        Seq("node") ++ nodeFlags ++ Seq(macroUseCompileRunner.getAbsolutePath),
        "Macro use compilation with on-demand entry point resolution failed",
      )
    }

    val macroUseJS = linkScalaJSForTest(
      Seq(macroUseOut, libsDir / "sjsir"),
      macroUseLinkOut,
      Seq(ModuleInitializer.mainMethodWithArgs("Test", "main", Nil)),
      ModuleKind.ESModule,
      log,
    )

    val macroUseOutput = runLogged(
      Seq("node", macroUseJS.getAbsolutePath),
      "Linked macro-use output failed",
    )
    if (macroUseOutput != "hello macro\nbatch macro\npackage macro\n")
      sys.error(s"Expected macro-expanded program to print `hello macro`, `batch macro`, and `package macro`, got:\n$macroUseOutput")

    val linkedJS = linkScalaJSForTest(
      Seq(compileOut, libsDir / "sjsir"),
      linkOut,
      Seq(ModuleInitializer.mainMethodWithArgs("Test", "main", Nil)),
      ModuleKind.ESModule,
      log,
    )

    val output = runLogged(
      Seq("node", linkedJS.getAbsolutePath),
      "Linked HelloWorld.js failed",
    )
    if (output != "hello world\n")
      sys.error(s"Expected HelloWorld.js to print `hello world`, got:\n$output")

    log.info(successMessage)
  }

  private def writeRunner(compilerRunner: File, compilerMain: File): Unit =
    IO.write(
      compilerRunner,
      s"""const { runScala3CompilerSJSAsync } = await import('${compilerMain.toURI}')
         |
         |const code = await runScala3CompilerSJSAsync(process.argv.slice(2))
         |if (code !== 0) process.exit(code)
         |""".stripMargin
    )

  private def linkScalaJSForTest(
      classpathEntries: Seq[File],
      outputDir: File,
      moduleInitializers: Seq[ModuleInitializer],
      moduleKind: ModuleKind,
      log: Logger,
      useWebAssembly: Boolean = false,
  ): File = {
    implicit val ec: ExecutionContext = ExecutionContext.global

    IO.delete(outputDir)
    IO.createDirectory(outputDir)

    val logger = new ScalaConsoleLogger(Level.Warn)
    val baseLinkerConfig = StandardConfig()
      .withCheckIR(true)
      .withSourceMap(false)
      .withBatchMode(true)
      .withModuleKind(moduleKind)
    val linkerConfig =
      if (useWebAssembly)
        baseLinkerConfig
          .withExperimentalUseWebAssembly(true)
          .withESFeatures(_.withESVersion(ESVersion.ES2018))
      else baseLinkerConfig
    val linker = StandardImpl.linker(linkerConfig)
    val cache = StandardImpl.irFileCache().newCache

    val result = PathIRContainer
      .fromClasspath(classpathEntries.map(_.toPath))
      .map(_._1)
      .flatMap(cache.cached)
      .flatMap(linker.link(
        _,
        moduleInitializers,
        PathOutputDirectory(outputDir.toPath),
        logger,
      ))

    val report = Await.result(result, Duration.Inf)
    if (report.publicModules.size != 1)
      sys.error(s"Expected exactly one public Scala.js module, got: $report")

    outputDir.toPath.resolve(report.publicModules.head.jsFileName).toFile
  }
}
