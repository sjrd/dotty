package dotty.tools.browseride

import java.nio.charset.StandardCharsets

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.typedarray.Uint8Array
import scala.util.control.NonFatal

import dotty.tools.io.JSByteArrays
import org.scalajs.ir.{SHA1, Version}
import org.scalajs.linker.MemOutputDirectory
import org.scalajs.linker.StandardImpl
import org.scalajs.linker.interface.{ESVersion, IRFile, ModuleInitializer, ModuleKind, Report, StandardConfig}
import org.scalajs.linker.interface.unstable.IRContainerImpl
import org.scalajs.linker.standard.MemIRFileImpl
import org.scalajs.logging.NullLogger

object BrowserLinkerBridge:
  private given ExecutionContext = JSExecutionContext.queue
  private val baseConfig = StandardConfig()
    .withCheckIR(false)
    .withSemantics(_.optimized)
    .withMinify(false)
    .withSourceMap(false)
    .withModuleKind(ModuleKind.ESModule)
  private val compilerModuleConfig = baseConfig
    .withExperimentalUseWebAssembly(true)
    .withESFeatures(_.withESVersion(ESVersion.ES2018))
  private val moduleLinker = new LinkerSession(baseConfig)
  private val compilerModuleLinker = new LinkerSession(compilerModuleConfig)

  trait IRInput extends js.Object:
    val path: String
    val bytes: Uint8Array
    val versionHash: js.Array[Int]

  def irInput(path: String, bytes: Uint8Array): IRInput =
    val versionHash = contentHash(bytes).map(byte => byte.toInt & 0xff).toJSArray
    js.Dynamic.literal(
      path = path,
      bytes = bytes,
      versionHash = versionHash,
    ).asInstanceOf[IRInput]

  def linkAsync(irFiles: js.Array[IRInput], mainClassName: String): js.Promise[js.Object] =
    moduleLinker.link(
      irFiles,
      Seq(ModuleInitializer.mainMethodWithArgs(mainClassName, "main", Nil)),
    )

  def linkModuleAsync(irFiles: js.Array[IRInput]): js.Promise[js.Object] =
    moduleLinker.link(irFiles, Nil)

  def linkCompilerModuleAsync(irFiles: js.Array[IRInput]): js.Promise[js.Object] =
    compilerModuleLinker.link(irFiles, Nil)

  private final class LinkerSession(config: StandardConfig):
    private val irFileCache = StandardImpl.irFileCache()
    private var cache = irFileCache.newCache
    private val linker = StandardImpl.clearableLinker(config)

    def link(irFiles: js.Array[IRInput], moduleInitializers: Seq[ModuleInitializer]): js.Promise[js.Object] =
      val outputDir = MemOutputDirectory()
      val containers = irFiles.toSeq.map(input => new InputIRContainer(input))
      cache.cached(containers)
        .flatMap(linker.link(_, moduleInitializers, outputDir, NullLogger))
        .map(report => linkResult(report, outputDir))
        .recoverWith {
          case NonFatal(t) =>
            reset()
            Future.failed(t)
        }
        .toJSPromise

    private def reset(): Unit =
      cache.free()
      cache = irFileCache.newCache

  private final class InputIRContainer(input: IRInput) extends IRContainerImpl(input.path, inputVersion(input)):
    def sjsirFiles(implicit ec: ExecutionContext): Future[List[IRFile]] =
      Future.successful(List(memIRFile(input)))

  private def memIRFile(input: IRInput): MemIRFileImpl =
    new MemIRFileImpl(input.path, inputVersion(input), JSByteArrays.toByteArray(input.bytes))

  private def inputVersion(input: IRInput): Version =
    val value = input.asInstanceOf[js.Dynamic].selectDynamic("versionHash")
    if js.isUndefined(value) || value == null then contentVersion(input.bytes)
    else Version.fromBytes(hashBytes(value.asInstanceOf[js.Array[Int]]))

  private def contentVersion(bytes: Uint8Array): Version =
    Version.fromBytes(contentHash(bytes))

  private def contentHash(bytes: Uint8Array): Array[Byte] =
    // The persistent linker trusts IR versions, so changed bytes must change the version.
    val digest = new SHA1.DigestBuilder
    var i = 0
    while i < bytes.length do
      digest.update(bytes(i).toByte)
      i += 1
    digest.finalizeDigest()

  private def hashBytes(hash: js.Array[Int]): Array[Byte] =
    val bytes = new Array[Byte](hash.length)
    var i = 0
    while i < hash.length do
      bytes(i) = hash(i).toByte
      i += 1
    bytes

  private def linkResult(report: Report, outputDir: MemOutputDirectory): js.Object =
    val publicModule = report.publicModules.headOption.getOrElse {
      throw new IllegalStateException("Scala.js linker produced no public module.")
    }

    val jsCodeBytes = outputDir.content(publicModule.jsFileName).getOrElse {
      throw new IllegalStateException(s"Linked output `${publicModule.jsFileName}` was not captured.")
    }

    js.Dynamic.literal(
      jsFileName = publicModule.jsFileName,
      code = new String(jsCodeBytes, StandardCharsets.UTF_8),
      files = outputFiles(outputDir).toJSArray,
    )

  private def outputFiles(outputDir: MemOutputDirectory): Seq[js.Dynamic] =
    outputDir.fileNames().sorted.map { name =>
      val bytes = outputDir.content(name).getOrElse {
        throw new IllegalStateException(s"Linked output `$name` was not captured.")
      }
      js.Dynamic.literal(
        path = name,
        bytes = JSByteArrays.toUint8Array(bytes),
      )
    }
