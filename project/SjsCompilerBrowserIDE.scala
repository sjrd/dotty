import java.io.{FileOutputStream, IOException}
import java.nio.file.{FileSystems, Files}
import java.util.zip.{ZipEntry, ZipFile, ZipOutputStream}

import sbt.*

import scala.collection.mutable
import scala.jdk.CollectionConverters.*
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
      jszipDist: File,
      log: Logger,
  ): File = {
    val assetsDir = browserIdeDir / "assets"
    val compilerDir = assetsDir / "compiler"
    val classpathDir = assetsDir / "classpath"
    val runtimeDir = assetsDir / "runtime"
    val vendorDir = assetsDir / "vendor"

    IO.delete(assetsDir)
    Seq(compilerDir, classpathDir, runtimeDir, vendorDir).foreach(IO.createDirectory)

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
