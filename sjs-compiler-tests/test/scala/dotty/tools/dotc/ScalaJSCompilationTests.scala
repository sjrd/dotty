package dotty
package tools
package dotc

import java.io.{BufferedReader, BufferedWriter, File, InputStreamReader, OutputStreamWriter}
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.util.Base64
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicInteger

import org.junit.{ Test, BeforeClass, AfterClass }
import org.junit.experimental.categories.Category

import scala.collection.mutable
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import reporting.TestReporter
import vulpix._
import org.junit.Ignore

@Category(Array(classOf[ScalaJSCompilationTests]))
class ScalaJSCompilationTests {
  import ParallelTesting._
  import TestConfiguration._
  import ScalaJSCompilationTests._
  import CompilationTest.aggregateTests

  // Negative tests ------------------------------------------------------------

  @Test def negScalaJS: Unit = {
    implicit val testGroup: TestGroup = TestGroup("negScalaJS")
    aggregateTests(
      compileFilesInDir("tests/neg-scalajs", scalaJSOptions),
    ).checkExpectedErrors()
  }

  @Test def runScalaJS: Unit = {
    implicit val testGroup: TestGroup = TestGroup("runScalaJS")
    aggregateTests(
      compileFilesInDir("tests/run", scalaJSOptions),
    ).checkRuns()
  }
}

object ScalaJSCompilationTests extends ParallelTesting {
  implicit val summaryReport: SummaryReporting = new SummaryReport

  // Test suite configuration --------------------------------------------------
  def maxDuration = 60.seconds
  def numberOfWorkers = 5
  def safeMode = Properties.testsSafeMode
  def isInteractive = SummaryReport.isInteractive
  def testFilter = Properties.testsFilter
  def updateCheckFiles: Boolean = Properties.testsUpdateCheckfile
  def failedTests = TestReporter.lastRunFailedTests

  @AfterClass def tearDown(): Unit =
    SjsCompilerBridge.shutdown()
    cleanup()
    summaryReport.echoSummary()

  // Run tests -----------------------------------------------------------------

  override protected def shouldSkipTestSource(testSource: TestSource): Boolean =
    testSource.allToolArgs.get(ToolName.ScalaJS).exists(_.contains("--skip"))
    || super.shouldSkipTestSource(testSource)

  override protected def testPlatform: TestPlatform = TestPlatform.ScalaJS

  override protected def processScalaSources(
      args: Array[String],
      sourcePaths: Array[String],
      targetDir: File,
      reporter: TestReporter,
      driver: Driver,
  ): Option[ScalaCompilerFailure] =
    if SjsCompilerBridge.isEnabled then
      SjsCompilerBridge.process(args ++ sourcePaths).map { output =>
        ScalaCompilerFailure(output, "scala3-compiler-sjs", Some(SjsCompilerBridge.checkOutputLines(output)))
      }
    else super.processScalaSources(args, sourcePaths, targetDir, reporter, driver)

  override def runMain(classPath: String, toolArgs: ToolArgs)(implicit summaryReport: SummaryReporting): Status =
    import scala.concurrent.ExecutionContext.Implicits.global

    val scalaJSOptions = toolArgs.getOrElse(ToolName.ScalaJS, Nil)

    try
      val useCompliantSemantics = scalaJSOptions.contains("--compliant-semantics")
      val sjsCode = ScalaJSLink.link(classPath, useCompliantSemantics)
      JSRun.runJSCode(sjsCode)
    catch
      case t: Exception =>
        val writer = new java.io.StringWriter()
        t.printStackTrace(new java.io.PrintWriter(writer))
        Failure(writer.toString())
  end runMain
}

private object SjsCompilerBridge:
  private val CompilerMainProperty = "dotty.tests.sjsCompilerMain"
  private val ExtraClasspathProperty = "dotty.tests.sjsCompilerExtraClasspath"
  private val NodeFlagsProperty = "dotty.tests.sjsCompilerNodeFlags"
  private val CompilerName = "scala3-compiler-sjs"
  private val Response = """\{"id":(\d+),"code":(-?\d+),"output":"([^"]*)"\}""".r

  private val servers = mutable.LinkedHashSet.empty[CompilerServer]
  private val threadServer = ThreadLocal[CompilerServer]()

  def isEnabled: Boolean =
    sys.props.contains(CompilerMainProperty)

  def process(args: Array[String]): Option[String] =
    sys.props.get(CompilerMainProperty).flatMap { compilerMain =>
      val (code, output) = server(compilerMain).run(withExtraClasspath(args))
      Option.when(code != 0)(output)
    }

  def checkOutputLines(output: String): List[String] =
    output.linesIterator.filterNot { line =>
      line.startsWith("checking ") || line.matches("\\d+ (?:warning|error)s? found")
    }.toList

  def shutdown(): Unit = synchronized {
    servers.foreach(_.close())
    servers.clear()
    threadServer.remove()
  }

  private def server(compilerMain: String): CompilerServer = synchronized {
    threadServer.get() match
      case server: CompilerServer if server.compilerMain == compilerMain => server
      case server: CompilerServer =>
        server.close()
        servers -= server
        val next = new CompilerServer(compilerMain, nodeFlags)
        servers += next
        threadServer.set(next)
        next
      case null =>
        val next = new CompilerServer(compilerMain, nodeFlags)
        servers += next
        threadServer.set(next)
        next
  }

  private def nodeFlags: Seq[String] =
    sys.props.get(NodeFlagsProperty).toSeq
      .flatMap(_.split(',').toSeq)
      .filter(_.nonEmpty)

  private def withExtraClasspath(args: Array[String]): Array[String] =
    sys.props.get(ExtraClasspathProperty).filter(_.nonEmpty) match
      case Some(extraClasspath) =>
        val index = args.indexOf("-classpath")
        if index >= 0 && index + 1 < args.length then
          args.updated(index + 1, args(index + 1) + File.pathSeparator + extraClasspath)
        else
          Array("-classpath", extraClasspath) ++ args
      case None =>
        args

  private final class CompilerServer(val compilerMain: String, nodeFlags: Seq[String]):
    private val nextId = AtomicInteger()
    private val launcher = Files.createTempFile("sjs-compiler-test-server", ".mjs")
    Files.writeString(launcher, serverScript, StandardCharsets.UTF_8)
    launcher.toFile.deleteOnExit()

    private val process =
      ProcessBuilder((Seq("node") ++ nodeFlags ++ Seq(launcher.toString, compilerMain)).asJava)
        .redirectErrorStream(true)
        .start()
    private val stdout =
      BufferedReader(InputStreamReader(process.getInputStream, StandardCharsets.UTF_8))
    private val stdin =
      BufferedWriter(OutputStreamWriter(process.getOutputStream, StandardCharsets.UTF_8))

    private val ready = stdout.readLine()
    if ready != """{"ready":true}""" then
      throw IllegalStateException(s"$CompilerName server did not start. stdout: $ready")

    def run(args: Array[String]): (Int, String) = synchronized {
      val id = nextId.incrementAndGet()
      stdin.write(request(id, args))
      stdin.newLine()
      stdin.flush()

      val line = stdout.readLine()
      if line == null then
        throw IllegalStateException(s"$CompilerName server stopped unexpectedly.")

      line match
        case Response(responseId, code, encodedOutput) if responseId.nn.toInt == id =>
          val output = String(Base64.getUrlDecoder.decode(encodedOutput.nn), StandardCharsets.UTF_8)
          code.nn.toInt -> output
        case _ =>
          throw IllegalStateException(s"Unexpected $CompilerName server response: $line")
    }

    def close(): Unit =
      try
        stdin.write("""{"type":"shutdown"}""")
        stdin.newLine()
        stdin.flush()
      catch case _: Exception => ()

      if !process.waitFor(5, TimeUnit.SECONDS) then
        process.destroyForcibly()
      Files.deleteIfExists(launcher)

  private def request(id: Int, args: Array[String]): String =
    args.iterator.map(jsonString).mkString(s"""{"id":$id,"args":[""", ",", "]}")

  private def jsonString(value: String): String =
    value.iterator.map {
      case '\\' => "\\\\"
      case '"' => "\\\""
      case '\n' => "\\n"
      case '\r' => "\\r"
      case '\t' => "\\t"
      case c if c < ' ' => f"\\u${c.toInt}%04x"
      case c => c.toString
    }.mkString("\"", "", "\"")

  private val serverScript =
    """import { pathToFileURL } from "node:url";
      |import readline from "node:readline";
      |
      |const compilerMain = process.argv[2];
      |const compiler = await import(pathToFileURL(compilerMain).href);
      |const originalStdoutWrite = process.stdout.write.bind(process.stdout);
      |const originalStderrWrite = process.stderr.write.bind(process.stderr);
      |
      |function send(value) {
      |  originalStdoutWrite(JSON.stringify(value) + "\n");
      |}
      |
      |function formatError(error) {
      |  return error && error.stack ? String(error.stack) : String(error);
      |}
      |
      |function makeCapture() {
      |  let output = "";
      |  const write = (chunk, encoding, callback) => {
      |    output += Buffer.isBuffer(chunk)
      |      ? chunk.toString(typeof encoding === "string" ? encoding : "utf8")
      |      : String(chunk);
      |    if (typeof encoding === "function") encoding();
      |    if (typeof callback === "function") callback();
      |    return true;
      |  };
      |  return { write, output: () => output };
      |}
      |
      |send({ ready: true });
      |
      |const lines = readline.createInterface({ input: process.stdin, crlfDelay: Infinity });
      |for await (const line of lines) {
      |  const request = JSON.parse(line);
      |  if (request.type === "shutdown") break;
      |
      |  const capture = makeCapture();
      |  process.stdout.write = capture.write;
      |  process.stderr.write = capture.write;
      |
      |  let code = 1;
      |  try {
      |    code = await compiler.runScala3CompilerSJSAsync(request.args);
      |  } catch (error) {
      |    code = 1;
      |    process.stderr.write(formatError(error) + "\n");
      |  } finally {
      |    process.stdout.write = originalStdoutWrite;
      |    process.stderr.write = originalStderrWrite;
      |  }
      |
      |  send({
      |    id: request.id,
      |    code,
      |    output: Buffer.from(capture.output(), "utf8").toString("base64url")
      |  });
      |}
      |""".stripMargin
end SjsCompilerBridge
