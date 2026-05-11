package dotty.tools.io

import scala.language.unsafeNulls
import scala.scalajs.js
import scala.scalajs.js.annotation.*
import scala.scalajs.js.typedarray.Uint8Array
import scala.scalajs.js.wasm.JSPI.allowOrphanJSAwait

import java.io.{ByteArrayInputStream, IOException, InputStream, OutputStream, FilterInputStream}
import java.net.URL
import java.util.jar.Manifest

import scala.collection.mutable
import scala.jdk.CollectionConverters.*

@js.native
@JSImport("jszip", JSImport.Default)
private object JSImportJSZip extends js.Object:
  def loadAsync(data: Uint8Array): js.Promise[JSZipArchiveHandle] = js.native

@js.native
private trait JSZipArchiveHandle extends js.Object:
  val files: js.Dictionary[JSZipEntryHandle] = js.native
  def file(path: String): JSZipEntryHandle | Null = js.native

@js.native
private trait JSZipEntryHandle extends js.Object:
  val name: String = js.native
  val dir: Boolean = js.native
  val date: js.Date = js.native
  def async(kind: String): js.Promise[Uint8Array] = js.native

object ZipArchive:
  private[io] val closeZipFile: Boolean = sys.props.get("scala.classpath.closeZip").exists(_.toBoolean)

  def fromFile(file: File): FileZipArchive = fromPath(file.jpath)
  def fromPath(jpath: JPath): FileZipArchive = new FileZipArchive(jpath, release = None)

  def fromManifestURL(url: URL): AbstractFile = new ManifestResources(url)

  private def dirName(path: String) = splitPath(path, front = true)
  private def baseName(path: String) = splitPath(path, front = false)
  private def splitPath(path0: String, front: Boolean): String =
    val isDir = path0.charAt(path0.length - 1) == '/'
    val path = if isDir then path0.substring(0, path0.length - 1) else path0
    val idx = path.lastIndexOf('/')

    if idx < 0 then
      if front then "/" else path
    else if front then path.substring(0, idx + 1)
    else path.substring(idx + 1)
end ZipArchive

import ZipArchive.*

abstract class ZipArchive(override val jpath: JPath, release: Option[String]) extends AbstractFile with Equals:
  self =>

  override def underlyingSource: Option[ZipArchive] = Some(this)
  def isDirectory: Boolean = true
  def lookupName(name: String, directory: Boolean): AbstractFile = unsupported()
  def lookupNameUnchecked(name: String, directory: Boolean): AbstractFile = unsupported()
  def output: OutputStream = unsupported()
  def container: AbstractFile = unsupported()
  def absolute: AbstractFile = unsupported()

  sealed abstract class Entry(path: String, val parent: Entry) extends VirtualFile(baseName(path), path):
    def getArchive: AnyRef = null
    override def underlyingSource: Option[ZipArchive] = Some(self)
    override def container: Entry = parent
    override def toString: String = self.path + "(" + path + ")"

  class DirEntry(path: String, parent: Entry) extends Entry(path, parent):
    val entries: mutable.HashMap[String, Entry] = mutable.HashMap()

    override def isDirectory: Boolean = true
    override def iterator: Iterator[Entry] = entries.valuesIterator
    override def lookupName(name: String, directory: Boolean): Entry =
      if directory then entries.get(name + "/").orNull
      else entries.get(name).orNull

  private def ensureDir(dirs: mutable.Map[String, DirEntry], path: String): DirEntry =
    dirs.get(path) match
      case Some(v) => v
      case None =>
        val parent = ensureDir(dirs, dirName(path))
        val dir = new DirEntry(path, parent)
        parent.entries(baseName(path)) = dir
        dirs(path) = dir
        dir

  protected def getDir(dirs: mutable.Map[String, DirEntry], path: String, isDir: Boolean): DirEntry =
    if isDir then ensureDir(dirs, path)
    else ensureDir(dirs, dirName(path))

  def close(): Unit
end ZipArchive

final class FileZipArchive(jpath: JPath, release: Option[String]) extends ZipArchive(jpath, release):
  private lazy val archiveLastModified: Long = HostFS.lastModified(jpath.toString)

  private lazy val archiveBytes: Array[Byte] =
    HostFS.readBytes(jpath.toString).getOrElse {
      new PlainFile(new Path(jpath)).toByteArray
    }

  private lazy val archiveHandle: JSZipArchiveHandle =
    js.await(JSImportJSZip.loadAsync(JSByteArrays.toUint8Array(archiveBytes)))

  private def entryLastModified(entry: JSZipEntryHandle): Long =
    val dyn = entry.asInstanceOf[js.Dynamic]
    val rawDate = dyn.selectDynamic("date")
    if js.isUndefined(rawDate) || rawDate == null then archiveLastModified
    else
      try
        val t = rawDate.asInstanceOf[js.Date].getTime().toLong
        if t > 0L then t else archiveLastModified
      catch
        case _: Throwable => archiveLastModified

  private case class VersionedEntry(logicalName: String, zipEntry: JSZipEntryHandle, lastModified: Long)

  private lazy val selectedEntries: List[VersionedEntry] =
    val chosen = mutable.HashMap.empty[String, (Int, JSZipEntryHandle)]
    val maybeRelease = release.flatMap(_.toIntOption)
    val prefix = "META-INF/versions/"

    def parseVersioned(path: String): Option[(Int, String)] =
      if !path.startsWith(prefix) then None
      else
        val rest = path.substring(prefix.length)
        val slash = rest.indexOf('/')
        if slash < 0 then None
        else
          rest.substring(0, slash).toIntOption.map { version =>
            val logicalName = rest.substring(slash + 1)
            (version, logicalName)
          }

    for key <- js.Object.keys(archiveHandle.files.asInstanceOf[js.Object]) do
      val entry = archiveHandle.files(key)
      if !entry.dir then
        parseVersioned(entry.name) match
          case Some((version, logicalName)) =>
            if maybeRelease.exists(version <= _) then
              chosen.get(logicalName) match
                case Some((currentVersion, _)) if currentVersion >= version => ()
                case _ => chosen(logicalName) = ((version, entry))
          case None =>
            chosen.get(entry.name) match
              case Some((currentVersion, _)) if currentVersion > 0 => ()
              case _ => chosen(entry.name) = ((0, entry))

    chosen.iterator.map { case (logicalName, (_, entry)) =>
      VersionedEntry(logicalName, entry, entryLastModified(entry))
    }.toList

  private class LazyEntry(name: String, zipPath: String, time: Long, parent: DirEntry) extends Entry(name, parent):
    override def lastModified: Long = time

    override def input: InputStream =
      val entry = archiveHandle.file(zipPath)
      if entry == null then
        throw new IOException(s"Missing zip entry: $zipPath in ${FileZipArchive.this.path}")
      new ByteArrayInputStream(JSByteArrays.toByteArray(js.await(entry.async("uint8array"))))

    override def sizeOption: Option[Int] =
      val entry = archiveHandle.file(zipPath)
      if entry == null then None
      else Some(JSByteArrays.toByteArray(js.await(entry.async("uint8array"))).length)

  lazy val (root, allDirs): (DirEntry, collection.Map[String, DirEntry]) =
    val root = new DirEntry("/", null)
    val dirs = mutable.HashMap[String, DirEntry]("/" -> root)

    for entry <- selectedEntries do
      val dir = getDir(dirs, entry.logicalName, isDir = false)
      val fileEntry = new LazyEntry(entry.logicalName, entry.zipEntry.name, entry.lastModified, dir)
      dir.entries(fileEntry.name) = fileEntry

    (root, dirs)

  def iterator: Iterator[Entry] = root.iterator

  def name: String = jpath.getFileName().toString
  def path: String = jpath.toString
  def input: InputStream = new ByteArrayInputStream(archiveBytes)
  def lastModified: Long = archiveLastModified

  override def sizeOption: Option[Int] = Some(archiveBytes.length)
  override def canEqual(other: Any): Boolean = other.isInstanceOf[FileZipArchive]
  override def hashCode(): Int = jpath.hashCode
  override def equals(that: Any): Boolean = that match
    case x: FileZipArchive => jpath.toAbsolutePath == x.jpath.toAbsolutePath
    case _ => false

  override def close(): Unit = ()
end FileZipArchive

final class ManifestResources(val url: URL) extends ZipArchive(null, None):
  def iterator: Iterator[AbstractFile] =
    val root = new DirEntry("/", null)
    val dirs = mutable.HashMap[String, DirEntry]("/" -> root)
    val stream = input
    val manifest = new Manifest(stream)
    val iter = manifest.getEntries().keySet().iterator().asScala.filter(_.endsWith(".class"))

    closeables ::= stream

    for entryName <- iter do
      val dir = getDir(dirs, entryName, isDir = false)
      class FileEntry extends Entry(entryName, dir):
        override def lastModified: Long = ManifestResources.this.lastModified
        override def input: InputStream = resourceInputStream(this.path)
        override def sizeOption: Option[Int] = None
      val fileEntry = new FileEntry()
      dir.entries(fileEntry.name) = fileEntry

    try root.iterator
    finally dirs.clear()

  def name: String = path
  def path: String =
    val s = url.getPath
    val n = s.lastIndexOf('!')
    s.substring(0, n)

  def input: InputStream = url.openStream()
  def lastModified: Long =
    try url.openConnection().getLastModified()
    catch case _: IOException => 0L

  override def canEqual(other: Any): Boolean = other.isInstanceOf[ManifestResources]
  override def hashCode(): Int = url.hashCode
  override def equals(that: Any): Boolean = that match
    case x: ManifestResources => url == x.url
    case _ => false

  private def resourceInputStream(path: String): InputStream =
    new FilterInputStream(null):
      override def read(): Int =
        if in == null then in = Thread.currentThread().getContextClassLoader().getResourceAsStream(path)
        if in == null then throw new RuntimeException(path + " not found")
        super.read()

      override def close(): Unit =
        super.close()
        in = null

  private var closeables: List[java.io.Closeable] = Nil
  override def close(): Unit =
    closeables.foreach(_.close())
    closeables = Nil
end ManifestResources
