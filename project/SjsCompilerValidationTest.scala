import java.net.{InetSocketAddress, URLDecoder}
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.util.concurrent.{CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.AtomicReference

import com.sun.net.httpserver.{HttpExchange, HttpServer}

import sbt.*

import scala.util.control.NonFatal

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
      |  await runCase(worker, "browser macro", macroFiles, "HELLO FROM THE BROWSER\n");
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
