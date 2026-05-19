package dotty.tools.browseride

import java.nio.charset.StandardCharsets

import scala.concurrent.ExecutionContext
import scala.scalajs.concurrent.JSExecutionContext
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.typedarray.Uint8Array

import dotty.tools.io.JSByteArrays
import org.scalajs.ir.Version
import org.scalajs.linker.MemOutputDirectory
import org.scalajs.linker.StandardImpl
import org.scalajs.linker.interface.{ESVersion, ModuleInitializer, ModuleKind, StandardConfig}
import org.scalajs.linker.standard.MemIRFileImpl
import org.scalajs.logging.NullLogger

object BrowserLinkerBridge:
  private given ExecutionContext = JSExecutionContext.queue
  private val baseConfig = StandardConfig()
    .withCheckIR(false)
    .withSemantics(_.optimized)
    .withMinify(false)
    .withBatchMode(true)
    .withSourceMap(false)
    .withModuleKind(ModuleKind.ESModule)
  private val compilerModuleConfig = baseConfig
    .withExperimentalUseWebAssembly(true)
    .withESFeatures(_.withESVersion(ESVersion.ES2018))

  trait IRInput extends js.Object:
    val path: String
    val bytes: Uint8Array

  def irInput(path: String, bytes: Uint8Array): IRInput =
    js.Dynamic.literal(path = path, bytes = bytes).asInstanceOf[IRInput]

  def linkAsync(irFiles: js.Array[IRInput], mainClassName: String): js.Promise[js.Object] =
    link(irFiles, Seq(ModuleInitializer.mainMethodWithArgs(mainClassName, "main", Nil)))

  def linkModuleAsync(irFiles: js.Array[IRInput]): js.Promise[js.Object] =
    link(irFiles, Nil)

  def linkCompilerModuleAsync(irFiles: js.Array[IRInput]): js.Promise[js.Object] =
    link(irFiles, Nil, compilerModuleConfig)

  private def link(
      irFiles: js.Array[IRInput],
      moduleInitializers: Seq[ModuleInitializer],
      config: StandardConfig = baseConfig,
  ): js.Promise[js.Object] =
    val linker = StandardImpl.linker(config)
    val outputDir = MemOutputDirectory()
    val inputIRFiles =
      irFiles.toSeq.zipWithIndex.map { case (irFile, index) =>
        new MemIRFileImpl(irFile.path, Version.fromInt(index), JSByteArrays.toByteArray(irFile.bytes))
      }

    linker
      .link(inputIRFiles, moduleInitializers, outputDir, NullLogger)
      .map { report =>
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
      }
      .toJSPromise

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
