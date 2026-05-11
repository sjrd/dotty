import { createMemoryFileSystem } from "./memory-fs.js";

const COMPILER_LOAD_TIMEOUT_MS = 120000;

function formatError(error) {
  return error instanceof Error ? error.message : String(error ?? "");
}

function postStatus(text) {
  self.postMessage({ type: "status", text, output: text });
}

function postRuntimeError(error) {
  self.postMessage({ type: "runtime-error", error: formatError(error) });
}

globalThis.__scala3CompilerSJSStatus = postStatus;

self.addEventListener("error", (event) => {
  postRuntimeError(event.error ?? event.message);
});

self.addEventListener("unhandledrejection", (event) => {
  event.preventDefault();
  postRuntimeError(event.reason);
});

function withTimeout(promise, milliseconds, label) {
  let timeoutId;
  const timeout = new Promise((_, reject) => {
    timeoutId = setTimeout(() => {
      reject(new Error(`${label} timed out after ${Math.round(milliseconds / 1000)} seconds.`));
    }, milliseconds);
  });

  return Promise.race([promise, timeout]).finally(() => clearTimeout(timeoutId));
}

try {
  postStatus("Loading compiler manifest...");
  const manifestResponse = await fetch("./assets/manifest.json", { cache: "no-store" });
  if (!manifestResponse.ok) {
    throw new Error(`Failed to fetch compiler manifest: ${manifestResponse.status} ${manifestResponse.statusText}`);
  }
  const manifest = await manifestResponse.json();
  const fs = createMemoryFileSystem();
  globalThis.__scala3CompilerSJSHostFS = fs.hostFS;

  postStatus("Loading compiler bundle...");
  const compilerModuleURL = new URL(manifest.compilerModule, self.location.href);
  compilerModuleURL.searchParams.set("v", String(manifest.assetVersion ?? Date.now()));
  const compiler = await withTimeout(
    import(compilerModuleURL.href),
    COMPILER_LOAD_TIMEOUT_MS,
    "Loading the Scala.js compiler bundle"
  );
  postStatus("Initializing compiler runtime...");
  compiler.startScala3BrowserIDEWorker({
    manifest,
    fs,
    jszipWrapperUrl: new URL("./assets/vendor/jszip-wrapper.js", self.location.href).href,
  });
} catch (error) {
  postRuntimeError(error);
}
