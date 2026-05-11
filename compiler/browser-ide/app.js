const EXAMPLES = {
  hello: {
    files: [
      {
        path: "Main.scala",
        content: `@main def hello() = println("Hello, World!")\n`,
      },
    ],
  },
  multi: {
    files: [
      {
        path: "Main.scala",
        content: `@main def hello() =
  println(Messages.greeting("browser"))
`,
      },
      {
        path: "Messages.scala",
        content: `object Messages:
  def greeting(name: String): String =
    s"Hello, $name!"
`,
      },
    ],
  },
  authorMacro: {
    files: [
      {
        path: "mymacros/MyMacros.scala",
        content: `package mymacros

import scala.quoted.*

object MyMacros:
  inline def shout(inline x: String): String = \${ shoutImpl('x) }

  def shoutImpl(x: Expr[String])(using Quotes): Expr[String] =
    Expr(x.valueOrAbort.toUpperCase)
`,
      },
      {
        path: "Main.scala",
        content: `import mymacros.MyMacros

@main def macroDemo() =
  println(MyMacros.shout("hello from the browser"))
`,
      },
    ],
  },
};

const DEFAULT_EXAMPLE = "hello";

const sourceEl = document.querySelector("#source");
const outputEl = document.querySelector("#output");
const runButton = document.querySelector("#run-button");
const exampleSelectEl = document.querySelector("#example-select");
const fileTabsEl = document.querySelector("#file-tabs");
const filePathEl = document.querySelector("#file-path");
const addFileButton = document.querySelector("#add-file-button");
const deleteFileButton = document.querySelector("#delete-file-button");
const compileTimerEl = document.querySelector("#compile-timer");
const importMacroButton = document.querySelector("#import-macro-button");
const importMacroInput = document.querySelector("#import-macro-input");
const terminalForm = document.querySelector("#terminal-form");
const terminalInput = document.querySelector("#terminal-input");

const worker = new Worker(new URL(`./worker.js?boot=${Date.now()}`, import.meta.url), { type: "module" });

let ready = false;
let running = false;
let importingMacro = false;
let waitingForInput = false;
let runtimeBlocked = false;
let nextFileId = 0;
let files = [];
let currentFileId = null;

function newFile(path, content = "") {
  nextFileId += 1;
  return {
    id: `file-${nextFileId}`,
    path,
    content,
  };
}

function currentFile() {
  return files.find((file) => file.id === currentFileId) ?? files[0] ?? null;
}

function setCompileTimer(text) {
  compileTimerEl.textContent = text;
  compileTimerEl.hidden = text === "";
}

function formatDuration(milliseconds) {
  if (!Number.isFinite(milliseconds)) {
    return "";
  }
  if (milliseconds < 1000) {
    return `${Math.round(milliseconds)} ms`;
  }
  const seconds = milliseconds / 1000;
  return `${seconds.toFixed(seconds < 10 ? 2 : 1)} s`;
}

function formatTimingSummary(message) {
  const durationText = formatDuration(message.durationMs);
  if (!durationText) {
    return "";
  }

  const topPhases = [...(message.phases ?? [])]
    .filter((phase) => phase.name && phase.durationMs >= 1)
    .sort((left, right) => right.durationMs - left.durationMs)
    .slice(0, 3)
    .map((phase) => `${phase.name} ${formatDuration(phase.durationMs)}`);
  const details = topPhases.length > 0 ? ` · ${topPhases.join(" · ")}` : "";
  return `Ready in ${durationText}${details}`;
}

function normalizePathForDisplay(path) {
  const normalized = String(path ?? "")
    .replace(/\\/g, "/")
    .split("/")
    .filter((part) => part && part !== ".")
    .join("/");
  return normalized || "Main.scala";
}

function uniquePath(basePath) {
  const normalizedBase = normalizePathForDisplay(basePath);
  const used = new Set(files.map((file) => normalizePathForDisplay(file.path)));
  if (!used.has(normalizedBase)) {
    return normalizedBase;
  }

  const extensionIndex = normalizedBase.endsWith(".scala")
    ? normalizedBase.length - ".scala".length
    : normalizedBase.length;
  const prefix = normalizedBase.slice(0, extensionIndex);
  const suffix = normalizedBase.slice(extensionIndex);
  let index = 2;
  while (used.has(`${prefix}${index}${suffix}`)) {
    index += 1;
  }
  return `${prefix}${index}${suffix}`;
}

function syncCurrentFile() {
  const file = currentFile();
  if (!file) {
    return;
  }

  file.content = sourceEl.value;
  file.path = normalizePathForDisplay(filePathEl.value);
}

function renderOutput(text) {
  outputEl.textContent = text;
  outputEl.scrollTop = outputEl.scrollHeight;
}

function blockRuntime(message) {
  ready = false;
  running = false;
  waitingForInput = false;
  runtimeBlocked = true;
  setTerminalInputVisible(false);
  setCompileTimer("");
  updateButtonState();
  renderOutput(message);
}

function renderFileTabs() {
  fileTabsEl.replaceChildren();

  for (const file of files) {
    const tab = document.createElement("button");
    tab.type = "button";
    tab.className = "file-tab";
    tab.role = "tab";
    tab.ariaSelected = String(file.id === currentFileId);
    tab.textContent = file.path;
    tab.disabled = running;
    tab.addEventListener("click", () => selectFile(file.id));
    fileTabsEl.append(tab);
  }
}

function loadCurrentFile() {
  const file = currentFile();
  if (!file) {
    sourceEl.value = "";
    filePathEl.value = "";
    return;
  }

  sourceEl.value = file.content;
  filePathEl.value = file.path;
  renderFileTabs();
}

function selectFile(fileId) {
  if (running || fileId === currentFileId) {
    return;
  }

  syncCurrentFile();
  currentFileId = fileId;
  loadCurrentFile();
  sourceEl.focus();
}

function setExample(exampleKey) {
  const example = EXAMPLES[exampleKey] ?? EXAMPLES[DEFAULT_EXAMPLE];
  files = example.files.map((file) => newFile(file.path, file.content));
  currentFileId = files[0]?.id ?? null;
  loadCurrentFile();
}

function addFile() {
  if (running) {
    return;
  }

  syncCurrentFile();
  const path = uniquePath(`File${files.length + 1}.scala`);
  const file = newFile(path);
  files.push(file);
  currentFileId = file.id;
  loadCurrentFile();
  sourceEl.focus();
}

function deleteCurrentFile() {
  if (running || files.length <= 1) {
    return;
  }

  const index = files.findIndex((file) => file.id === currentFileId);
  if (index < 0) {
    return;
  }

  files.splice(index, 1);
  currentFileId = files[Math.min(index, files.length - 1)]?.id ?? null;
  loadCurrentFile();
}

function validateFiles() {
  const seen = new Set();
  for (const file of files) {
    const path = normalizePathForDisplay(file.path);
    if (!path.endsWith(".scala")) {
      return `File paths must end with .scala: ${path}`;
    }
    if (path.includes("..")) {
      return `File paths cannot contain ..: ${path}`;
    }
    if (seen.has(path)) {
      return `Two files have the same path: ${path}`;
    }
    seen.add(path);
  }
  return null;
}

function setTerminalInputVisible(visible) {
  terminalForm.hidden = !visible;
  terminalInput.disabled = !visible;

  if (visible) {
    terminalInput.focus();
  } else {
    terminalInput.value = "";
  }
}

function updateButtonState() {
  const idleState = ready ? "ready" : runtimeBlocked ? "blocked" : "loading";

  runButton.disabled = !ready || running;
  exampleSelectEl.disabled = running;
  filePathEl.disabled = running;
  addFileButton.disabled = running;
  deleteFileButton.disabled = running || files.length <= 1;
  importMacroButton.disabled = !ready || running || importingMacro;
  runButton.dataset.state = running ? "loading" : idleState;
  importMacroButton.dataset.state = importingMacro ? "loading" : idleState;
  importMacroButton.textContent = importingMacro ? "Importing..." : "Import Macro";
  runButton.textContent =
    running ? "Running..." : ready && waitingForInput ? "Restart" : ready ? "Run" : runtimeBlocked ? "Unavailable" : "Loading...";
  renderFileTabs();
}

function runSource() {
  if (!ready || running) {
    return;
  }

  syncCurrentFile();
  const validationError = validateFiles();
  if (validationError) {
    renderOutput(validationError);
    return;
  }

  running = true;
  waitingForInput = false;
  setTerminalInputVisible(false);
  setCompileTimer("Compiling...");
  updateButtonState();
  renderOutput("Compiling and running...");
  worker.postMessage({
    type: "run",
    files: files.map(({ path, content }) => ({ path, content })),
  });
}

function submitTerminalInput() {
  if (!waitingForInput || running) {
    return;
  }

  running = true;
  waitingForInput = false;
  const line = terminalInput.value;
  setTerminalInputVisible(false);
  updateButtonState();
  worker.postMessage({
    type: "stdin",
    line,
  });
}

async function readImportFiles(selectedFiles) {
  return Promise.all([...selectedFiles].map(async (file) => ({
    name: file.name,
    bytes: new Uint8Array(await file.arrayBuffer()),
  })));
}

async function importMacroFiles(selectedFiles) {
  if (!ready || running || importingMacro || selectedFiles.length === 0) {
    return;
  }

  importingMacro = true;
  updateButtonState();
  renderOutput("Importing macro library...");

  try {
    const importFiles = await readImportFiles(selectedFiles);
    worker.postMessage(
      {
        type: "import-macro-artifact",
        files: importFiles,
      },
      importFiles.map((file) => file.bytes.buffer),
    );
  } catch (error) {
    importingMacro = false;
    updateButtonState();
    renderOutput(error instanceof Error ? error.message : String(error));
  }
}

runButton.addEventListener("click", runSource);
addFileButton.addEventListener("click", addFile);
deleteFileButton.addEventListener("click", deleteCurrentFile);
importMacroButton.addEventListener("click", () => {
  if (!ready || running || importingMacro) {
    return;
  }

  importMacroInput.value = "";
  importMacroInput.click();
});

importMacroInput.addEventListener("change", () => {
  importMacroFiles(importMacroInput.files ?? []);
});

exampleSelectEl.addEventListener("change", () => {
  setExample(exampleSelectEl.value);
  sourceEl.focus();
});

filePathEl.addEventListener("input", () => {
  const file = currentFile();
  if (!file) {
    return;
  }
  file.path = normalizePathForDisplay(filePathEl.value);
  renderFileTabs();
});

sourceEl.addEventListener("input", () => {
  const file = currentFile();
  if (file) {
    file.content = sourceEl.value;
  }
});

sourceEl.addEventListener("keydown", (event) => {
  if ((event.metaKey || event.ctrlKey) && event.key === "Enter") {
    event.preventDefault();
    runSource();
  }
});

terminalForm.addEventListener("submit", (event) => {
  event.preventDefault();
  submitTerminalInput();
});

worker.addEventListener("message", (event) => {
  const message = event.data;

  switch (message.type) {
    case "status":
      if (!running && !waitingForInput && message.output) {
        renderOutput(message.output);
      }
      break;

    case "ready":
      ready = true;
      running = false;
      waitingForInput = false;
      runtimeBlocked = false;
      setTerminalInputVisible(false);
      updateButtonState();
      renderOutput("Run to execute.");
      break;

    case "awaiting-input":
      ready = true;
      running = false;
      waitingForInput = true;
      runtimeBlocked = false;
      updateButtonState();
      renderOutput(message.output);
      setTerminalInputVisible(true);
      break;

    case "run-result":
      ready = true;
      running = false;
      waitingForInput = false;
      runtimeBlocked = false;
      setTerminalInputVisible(false);
      if (compileTimerEl.textContent === "Compiling...") {
        setCompileTimer("");
      }
      updateButtonState();
      renderOutput(message.output);
      break;

    case "compile-duration":
      {
        setCompileTimer(formatTimingSummary(message));
      }
      break;

    case "macro-import-result":
      importingMacro = false;
      ready = !runtimeBlocked;
      updateButtonState();
      renderOutput(message.output);
      break;

    case "runtime-error":
      blockRuntime(message.error);
      break;

    default:
      break;
  }
});

worker.addEventListener("error", (event) => {
  event.preventDefault();
  blockRuntime(event.message || "The compiler worker failed before it could report an error.");
});

worker.addEventListener("messageerror", () => {
  blockRuntime("The compiler worker sent a message that the browser could not deserialize.");
});

setExample(DEFAULT_EXAMPLE);
exampleSelectEl.value = DEFAULT_EXAMPLE;
outputEl.textContent = "Loading compiler...";
updateButtonState();
setTerminalInputVisible(false);
