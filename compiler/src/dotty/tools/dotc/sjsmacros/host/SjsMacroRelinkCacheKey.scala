package dotty.tools.dotc.sjsmacros.host

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

object SjsMacroRelinkCacheKey:
  import SjsMacroArtifacts.IRFile
  private val Fnv64OffsetBasis = 0xcbf29ce484222325L
  private val Fnv64Prime = 0x100000001b3L

  def compute(
      compilerIRFingerprint: String,
      entryPointsIR: Seq[IRFile],
      macroImplementationIR: Seq[IRFile],
      linkerConfig: Seq[String],
  ): String =
    val digest = new Fingerprint
    digest.updateString("scala3-sjs-macro-relink-cache-v2")
    digest.updateString("compiler-ir-fingerprint")
    digest.updateString(compilerIRFingerprint)
    updateIRFiles(digest, "macro-entrypoints-ir", entryPointsIR)
    updateIRFiles(digest, "macro-implementation-ir", macroImplementationIR)
    updateStrings(digest, "linker-config", linkerConfig.sorted)
    digest.result()

  private def updateIRFiles(digest: Fingerprint, section: String, files: Seq[IRFile]): Unit =
    digest.updateString(section)
    files.sortBy(_.path).foreach { file =>
      digest.updateString(file.path)
      digest.updateBytes(file.bytes)
    }

  private def updateStrings(digest: Fingerprint, section: String, values: Seq[String]): Unit =
    digest.updateString(section)
    values.foreach(digest.updateString)

  /** A small deterministic browser-friendly fingerprint.
   *
   *  This is a cache key, not a security boundary. Hosts that need collision
   *  resistance can wrap the same inputs with Web Crypto or another SHA-256
   *  implementation outside the compiler runtime.
   */
  private final class Fingerprint:
    private var hash = Fnv64OffsetBasis

    def updateString(value: String): Unit =
      updateBytes(value.getBytes(StandardCharsets.UTF_8))

    def updateBytes(bytes: Array[Byte]): Unit =
      val length = ByteBuffer.allocate(8).putLong(bytes.length.toLong).array()
      updateRawBytes(length)
      updateRawBytes(bytes)

    def result(): String =
      java.lang.Long.toUnsignedString(hash, 16)

    private def updateRawBytes(bytes: Array[Byte]): Unit =
      var i = 0
      while i < bytes.length do
        hash ^= bytes(i).toLong & 0xffL
        hash *= Fnv64Prime
        i += 1
