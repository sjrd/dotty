package dotty.tools.dotc.sjsmacros.host

import dotty.tools.io.{Directory, Path, Streamable}

/** Reads browser-hosted Scala.js macro artifacts from the compiler HostFS.
 *
 *  The expected layout is:
 *
 *      artifact-root/
 *        sjsir/
 *          ... .sjsir
 *
 *  The artifact id and macro packages are supplied by the browser host.
 */
object SjsMacroArtifactFS:
  def read(id: String, macroPackages: Seq[String], rootPath: String): SjsMacroArtifacts.Artifact =
    val root = Directory(rootPath).normalize
    val implementationRoot =
      val sjsir = (root / "sjsir").toDirectory
      if sjsir.exists && sjsir.isDirectory then sjsir else root

    SjsMacroArtifacts.Artifact(
      id = id,
      macroPackages = macroPackages.distinct.sorted,
      implementationIR = readIRFiles(implementationRoot),
    )

  private def readIRFiles(root: Directory): Seq[SjsMacroArtifacts.IRFile] =
    if !root.exists then Nil
    else
      root.deepFiles
        .filter(_.name.endsWith(".sjsir"))
        .map { file =>
          val relativePath = normalizeRelativePath(root.relativize(file))
          SjsMacroArtifacts.IRFile(relativePath, Streamable.bytes(file.inputStream()))
        }
        .toSeq
        .sortBy(_.path)

  private def normalizeRelativePath(path: Path): String =
    path.path.replace('\\', '/').stripPrefix("/")
