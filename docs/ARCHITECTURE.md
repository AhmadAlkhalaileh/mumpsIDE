# Ahmad IDE - Architecture Documentation

Deep dive into the technical architecture, design decisions, and implementation details.

---

## Table of Contents

1. [System Overview](#system-overview)
2. [Process Model](#process-model)
3. [Component Architecture](#component-architecture)
4. [Data Flow](#data-flow)
5. [MUMPS Integration](#mumps-integration)
6. [Debugging Architecture](#debugging-architecture)
7. [Security Model](#security-model)
8. [Performance Considerations](#performance-considerations)
9. [Design Decisions](#design-decisions)

---

## System Overview

Ahmad IDE is a desktop application built on Electron, designed for professional MUMPS development with remote runtime support.

### Core Principles

1. **Security First** - Sandboxed renderer, context isolation, minimal Node.js exposure
2. **Remote Native** - Built for Docker/SSH connections from day one
3. **Performance** - Optimized for low-end systems, animations disabled
4. **Developer Experience** - inspired UI for professional feel
5. **MUMPS Native** - Custom parser, linter, and debugger for M language

### High-Level Architecture

```
┌───────────────────────────────────────────────────────────────┐
│                     ELECTRON FRAMEWORK                        │
├───────────────────────────────────────────────────────────────┤
│                                                               │
│  ┌─────────────────────┐           ┌────────────────────┐   │
│  │   MAIN PROCESS      │           │  RENDERER PROCESS  │   │
│  │   (Node.js)         │◄─────────►│  (Chromium)        │   │
│  │                     │    IPC    │                    │   │
│  │  • App Lifecycle    │           │  • UI Logic        │   │
│  │  • IPC Handlers     │           │  • Monaco Editor   │   │
│  │  • File System      │           │  • xterm.js        │   │
│  │  • Child Processes  │           │  • Event Handlers  │   │
│  │  • Bridge Module    │           │  • State Mgmt      │   │
│  └─────────────────────┘           └────────────────────┘   │
│           │                                  │                │
│           │ exec/spawn                       │ DOM/CSS        │
│           ▼                                  ▼                │
│  ┌─────────────────────┐           ┌────────────────────┐   │
│  │  OS/SHELL           │           │  Browser APIs      │   │
│  │  • Docker CLI       │           │  • localStorage    │   │
│  │  • SSH (ssh2)       │           │  • IndexedDB       │   │
│  │  • Git CLI          │           │  • Canvas/WebGL    │   │
│  │  • Terminal (PTY)   │           │                    │   │
│  └─────────────────────┘           └────────────────────┘   │
└───────────────────────────────────────────────────────────────┘
                    │
                    │ TCP/SSH/Docker
                    ▼
        ┌──────────────────────────┐
        │  REMOTE MUMPS RUNTIME    │
        │  • YottaDB / GT.M        │
        │  • AHMDBG.m Debugger     │
        │  • Routine Files (.m)    │
        │  • Global Database       │
        └──────────────────────────┘
```

---

## Process Model

Electron uses a multi-process architecture similar to Chrome.

### Main Process

**File:** `main.js`
**Runtime:** Node.js
**Responsibilities:**
- Create browser windows
- Handle system events (quit, activate)
- Register IPC handlers
- Manage security settings
- Control hardware acceleration

**Key Code:**
```javascript
function createWindow() {
  const win = new BrowserWindow({
    width: 1400,
    height: 900,
    webPreferences: {
      preload: path.join(__dirname, 'preload.js'),
      contextIsolation: true,      // Security
      nodeIntegration: false,       // Security
      sandbox: true                 // Security
    }
  });
  win.loadFile('index.html');
}
```

---

### Renderer Process

**File:** `renderer.js`
**Runtime:** Chromium (V8 JavaScript engine)
**Restrictions:**
- No direct Node.js access
- No `require()` function
- Sandboxed environment
- Can only use APIs exposed via preload

**Responsibilities:**
- Render UI (HTML/CSS/JavaScript)
- Handle user interactions
- Manage Monaco editor
- Control xterm terminals
- Update UI state
- Call IPC methods

**Key Pattern:**
```javascript
// renderer.js
async function runCode() {
  const code = editor.getValue();
  const result = await window.ahmadIDE.execute(code);

  if (result.ok) {
    terminalWrite(result.stdout);
  } else {
    showError(result.error);
  }
}
```

---

### Preload Script

**File:** `preload.js`
**Purpose:** Security bridge between main and renderer
**Context:** Has access to both Node.js and DOM (before page loads)

**Security Pattern:**
```javascript
// preload.js
const { contextBridge, ipcRenderer } = require('electron');

// Expose ONLY safe, specific methods
contextBridge.exposeInMainWorld('ahmadIDE', {
  execute: (code) => ipcRenderer.invoke('exec:run', { code })
  // NOT exposed: ipcRenderer, require, process, etc.
});
```

**Why This Matters:**
- Prevents XSS attacks from executing arbitrary Node.js code
- Limits attack surface to specific IPC channels
- Ensures renderer can't access file system directly

---

## Component Architecture

### Main Process Components

```
main.js
├─ app lifecycle
├─ window management
├─ IPC handler registration (ipcHandle)
├─ error handlers (uncaughtException, unhandledRejection)
└─ terminal session management

bridge.js (3400+ lines)
├─ Connection Management
│  ├─ Docker connection (exec wrapper)
│  ├─ SSH connection (ssh2 library)
│  └─ Environment path building
├─ MUMPS Execution
│  ├─ Code execution (via Docker/SSH)
│  ├─ Linting (local parser)
│  └─ Routine I/O (read/write/zlink)
├─ Debugging
│  ├─ AHMDDG lifecycle (stdio)
│  ├─ Debug client (JSON protocol)
│  ├─ Breakpoint management
│  └─ Variable inspection
├─ Git Integration
│  ├─ Command execution
│  ├─ Config management
│  └─ Diff/log parsing
└─ Helper Functions
   ├─ buildYdbEnv (env variable export strings)
   ├─ wrapDockerCmd (sg docker wrapper)
   ├─ runHostCommand (exec wrapper)
   └─ file I/O helpers

utils/
├─ logger.js      - Structured JSON logging
└─ debug-log.js   - Debug output helper
```

---

### Renderer Process Components

```
renderer.js (Main controller)
├─ State Management
│  ├─ Global state object
│  ├─ Current connection config
│  ├─ Active debug session
│  └─ Terminal sessions map
└─ Module initialization

src/editor/ (Modular components)
├─ mumps/
│  ├─ Monaco MUMPS renderer
│  ├─ Language definition
│  └─ Syntax highlighting
├─ tabs/
│  ├─ Tab creation/closing
│  └─ Tab management
├─ terminal/
│  ├─ xterm.js integration
│  └─ PTY session management
├─ debug/
│  ├─ Debug bar controls
│  ├─ Variables panel
│  ├─ Breakpoints panel
│  └─ Call stack panel
├─ git/
│  ├─ Git status/changes
│  ├─ Commit workflow
│  └─ Diff viewer
├─ project/
│  ├─ Project tree view
│  └─ Filtering
├─ routines/
│  ├─ Routine I/O
│  └─ ZLINK operations
├─ diagnostics/
│  └─ Linting integration
├─ problems/
│  └─ Problems panel UI
├─ search/
│  └─ Find/Replace
└─ ui/
   ├─ Toast notifications
   ├─ Modal dialogs
   └─ Keyboard shortcuts

index.html
├─ Layout structure
├─ Tool window definitions
├─ Panel templates
└─ Modal overlays

styles.css (2000+ lines)
├─ CSS variables (theme)
├─ Layout (flexbox/grid)
├─ Component styles
└─ Animation overrides (disabled)
```

---

### MUMPS Language Support

```
assets/mumps/
├─ mumps-lexer.js
│  └─ Tokenizes MUMPS code into tokens
├─ mumps-parser.js
│  └─ Builds AST from tokens
├─ mumps-ast.js
│  └─ AST node type definitions
├─ mumps-validator.js
│  └─ Validates AST for semantic errors
└─ mumps-linter.js
   └─ Produces error/warning messages

Flow:
Code → Lexer → Tokens → Parser → AST → Validator → Errors
```

**Example Flow:**
```javascript
// Input code
const code = "SET X=42";

// Lexer
const tokens = lexer.tokenize(code);
// [
//   { type: 'KEYWORD', value: 'SET' },
//   { type: 'IDENTIFIER', value: 'X' },
//   { type: 'OPERATOR', value: '=' },
//   { type: 'NUMBER', value: '42' }
// ]

// Parser
const ast = parser.parse(tokens);
// {
//   type: 'Program',
//   lines: [
//     {
//       type: 'Line',
//       commands: [
//         { type: 'SET', var: 'X', value: { type: 'Number', value: 42 } }
//       ]
//     }
//   ]
// }

// Validator
const errors = validator.validate(ast);
// [] (no errors)
```

---

## Data Flow

### IPC Communication Flow

```
USER ACTION (e.g., clicks "Run" button)
    │
    ▼
[renderer.js] Click handler
    │
    │ const result = await window.ahmadIDE.execute(code);
    ▼
[preload.js] Exposed API
    │
    │ ipcRenderer.invoke('exec:run', { code })
    ▼
[Electron IPC] Secure channel
    │
    ▼
[main.js] IPC handler
    │
    │ ipcHandle('exec:run', async (event, payload) => {
    │   return bridge.execute(payload.code);
    │ })
    ▼
[bridge.js] Business logic
    │
    │ buildCommand, exec child process
    ▼
[Docker/SSH] Remote execution
    │
    │ mumps -run ROUTINE
    ▼
[YottaDB] MUMPS runtime
    │
    │ Executes code, produces output
    ▼
[bridge.js] Parse output
    │
    │ { ok: true, stdout: "...", stderr: "..." }
    ▼
[main.js] IPC response
    │
    ▼
[preload.js] Return to renderer
    │
    ▼
[renderer.js] Update UI
    │
    │ terminalWrite(result.stdout);
    ▼
USER SEES OUTPUT in terminal
```

---

### Debug Session Flow

```
1. USER: Clicks Debug button

2. RENDERER:
   - Collects breakpoints from editor decorations
   - Calls debugStart(code, breakpoints)

3. MAIN (bridge.js):
   - Checks if AHMDBG server is running
   - If not, starts AHMDBG server:
     a. Write AHMDBG.m to /tmp/ahmad_dbg/
     b. Compile: mumps AHMDBG.m
     c. Start server: mumps -run MAIN^AHMDBG
     d. Wait for port 9200 to open
   - Create MDebugClient instance
   - Connect to 127.0.0.1:9200 (Docker container IP)

4. MDEBUG CLIENT:
   - TCP connection established
   - Sends breakpoints: SETBPJSON;{"routine":"TEST","tag":"MAIN","offset":3}
   - Sends start command: INTO

5. AHMDBG (YottaDB):
   - Receives INTO command
   - Executes: ZSTEP INTO
   - Enters routine at first line
   - $ZSTEP action fires: calls STEPJSON^AHMDBG
   - STEPJSON sends stopped event (JSON via stdout):
     {"event":"stopped","routine":"TEST","line":5,"tag":"MAIN","offset":3}
   - Waits for next command (READ from stdin)

6. MDEBUG CLIENT:
   - Receives stopped event
   - Parses JSON
   - Converts position to file:line
   - Sends GETVARS command
   - AHMDBG responds with {"event":"vars","vars":{...}}

7. MAIN → RENDERER:
   - Returns debug state via IPC
   - { ok: true, stopped: true, file: "TEST.m", line: 5, vars: {...} }

8. RENDERER:
   - Shows debug bar
   - Highlights current line
   - Updates Variables panel
   - Updates Call Stack panel

9. USER: Clicks "Step Over" (F8)

10. RENDERER → MAIN → CLIENT:
    - debugStep(sessionId, 'over')
    - Client sends: OVER

11. AHMDBG:
    - Executes: ZSTEP OVER
    - Stops at next line
    - Sends stopped event

12. REPEAT steps 6-11 until:
    - User clicks Continue (CONTINUE command)
    - User clicks Stop (HALT command)
    - Routine ends (QUIT)
    - Error occurs
```

---

## MUMPS Integration

### Connection Types

#### Docker Connection

**Implementation:**
```javascript
// Execute command in Docker container
const cmd = wrapDockerCmd(
  `docker exec ${containerId} bash -c "${ydbEnv} && ${command}"`
);
exec(cmd, callback);
```

**sg docker Wrapper:**
```bash
# Without sg (requires user in docker group)
docker exec abc123 bash -c "command"

# With sg (for users NOT in docker group)
sg docker -c "docker exec abc123 bash -c 'command'"
```

**Environment Setup:**
```bash
export gtmgbldir="/var/worldvista/prod/globals/mumps.gld"
export gtmroutines="/var/worldvista/prod/localr /opt/fis-gtm/YDB136"
export ydb_gbldir="/var/worldvista/prod/globals/mumps.gld"
export ydb_routines="/var/worldvista/prod/localr /opt/fis-gtm/YDB136"
```

---

#### SSH Connection

**Implementation:**
```javascript
const SSHClient = require('ssh2').Client;
const conn = new SSHClient();

conn.on('ready', () => {
  conn.exec(command, (err, stream) => {
    stream.on('data', (data) => { stdout += data; });
    stream.on('close', () => { callback(null, stdout); });
  });
});

conn.connect({
  host: '10.0.0.5',
  port: 22,
  username: 'ahmad-cc',
  password: 'password'
});
```

**Session Management:**
```javascript
const sshSessions = new Map();

// Store session
sshSessions.set(sessionId, conn);

// Reuse session
const conn = sshSessions.get(sessionId);
if (conn) {
  conn.exec(command, callback);
}
```

---

### Routine Execution

**1. Write Code to Temp File:**
```javascript
const tempFile = `/tmp/ahmad_ide_${Date.now()}.m`;
const writeCmd = `echo ${shellQuote(code)} > ${tempFile}`;
await runHostCommand(writeCmd);
```

**2. Compile (if needed):**
```javascript
const compileCmd = `${ydbPath}/mumps ${tempFile}`;
await runHostCommand(compileCmd);
```

**3. Execute:**
```javascript
const execCmd = `${ydbPath}/mumps -run ${routine}`;
const result = await runHostCommand(execCmd);
```

**4. Cleanup:**
```javascript
await runHostCommand(`rm -f ${tempFile}`);
```

---

## Debugging Architecture

### AHMDDG.m Design

**Execution Model:**
```
IDE (renderer.js)
    ↓ IPC
Main (bridge.js)
    ↓ stdio (stdin/stdout)
    ↓ JSON protocol
AHMDDG.m (YottaDB)
    ↓ $ZSTEP
User's MUMPS code
```

**$ZSTEP Integration:**
```mumps
SET $ZSTEP="ZSHOW ""V"":^%AHMDDG($J,""VARS"") SET %STP=$$STEPJSON^AHMDDG() ZSTEP:%STP=""I"" INTO ZSTEP:%STP=""O"" OVER ZSTEP:%STP=""F"" OUTOF ZCONTINUE:%STP=""C"" HALT:%STP=""H"""
```

**Breakdown:**
1. `ZSHOW "V":^%AHMDDG($J,"VARS")` - Capture variables to global
2. `SET %STP=$$STEPJSON^AHMDDG()` - Call step handler (returns "I"/"O"/"F"/"C"/"H")
3. `ZSTEP:%STP="I" INTO` - If %STP is "I", do ZSTEP INTO
4. `ZSTEP:%STP="O" OVER` - If %STP is "O", do ZSTEP OVER
5. `ZSTEP:%STP="F" OUTOF` - If %STP is "F", do ZSTEP OUTOF
6. `ZCONTINUE:%STP="C"` - If %STP is "C", do ZCONTINUE
7. `HALT:%STP="H"` - If %STP is "H", do HALT

**STEPJSON Handler:**
```mumps
STEPJSON()
    NEW USR,POS,ROU,TAG,OFF,LIN
    SET USR=$SELECT($STACK>0:$STACK-1,1:$STACK)  ; Get user frame
    SET POS=$STACK(USR,"PLACE")                   ; Get position
    SET ROU=$PIECE(POS,"^",2)                     ; Extract routine
    ; Skip AHMDDG itself
    IF ROU="AHMDDG" QUIT "I"
    ; Parse position
    SET TAG=$PIECE($PIECE(POS,"^"),"+",1)
    SET OFF=+$PIECE($PIECE(POS,"^"),"+",2)
    ; Compute line number
    SET LIN=$$LINENUM(TAG,OFF,ROU)
    ; Send stopped event (JSON)
    USE $PRINCIPAL
    WRITE "{""event"":""stopped"",""routine"":""",ROU,""",""line"":",LIN,...
    ; Wait for command and return mode
    QUIT $$READCMDJSON()
```

---

### Breakpoint Implementation

**Setting Breakpoints:**
```mumps
; Client sends:
SETBPJSON;{"routine":"TEST","tag":"MAIN","offset":3}

; AHMDBG executes:
SET BP="MAIN+3^TEST"
ZBREAK @BP:"N"   ; Action "N" = no-op (don't drop to direct mode)
```

**YottaDB ZBREAK:**
- Sets a breakpoint at TAG+OFFSET^ROUTINE
- When hit, executes action (default: drop to direct mode)
- Action "N" does nothing, allowing $ZSTEP to continue

**Clearing Breakpoints:**
```mumps
; Clear specific
ZBREAK -+5^TEST

; Clear all in routine
ZBREAK -*^TEST

; Clear all
ZBREAK -*
```

---

### Variable Inspection

**Capture Variables:**
```mumps
; In $ZSTEP action (before calling STEPJSON):
ZSHOW "V":^%AHMDDG($J,"VARS")
```

**Send Variables:**
```mumps
SENDVARS
    USE $PRINCIPAL
    WRITE "{""event"":""vars"",""vars"":{"
    SET I=""
    FOR  SET I=$ORDER(^%AHMDDG($J,"VARS","V",I)) QUIT:I=""  DO
    . SET LINE=^%AHMDDG($J,"VARS","V",I)
    . ; Parse: VAR=value
    . SET VAR=$PIECE(LINE,"=",1)
    . SET VAL=$PIECE(LINE,"=",2,999)
    . ; Skip internal vars
    . IF VAR="SKIP" QUIT
    . ; Output JSON
    . WRITE """",VAR,""":""",VAL,""""
    WRITE "}}",!
```

---

### Eval/REPL Implementation

**Execute Arbitrary Code:**
```mumps
EVJSON(CMD)
    NEW CODE,FN,OLDIO
    SET CODE=$PIECE(CMD,";",2,999)  ; Extract code after "EVAL;"
    SET FN="/tmp/ahmad_dbg/eval_"_$J_".tmp"
    SET OLDIO=$IO
    ; Capture output to file
    OPEN FN:NEWVERSION
    USE FN
    XECUTE CODE                      ; Execute user code
    USE OLDIO
    CLOSE FN
    ; Read output
    OPEN FN:READONLY
    SET I=0
    FOR  READ LINE QUIT:$ZEOF  DO
    . SET OUT(I)=LINE,I=I+1
    CLOSE FN
    ; Send result
    WRITE "{""event"":""eval"",""ok"":1,""output"":""",$$JOINOUT(.OUT),"""}",!
```

**Why Use File I/O:**
- XECUTE executes code in current context
- Output (WRITE statements) must be captured
- USE device switches output destination
- File is safest way to capture without losing data

---

## Security Model

### Electron Security

**Context Isolation:**
```javascript
webPreferences: {
  contextIsolation: true  // Separate context for preload and renderer
}
```

- Renderer cannot access preload context
- Prevents malicious code from calling Node.js APIs
- Uses `contextBridge` to expose specific APIs

**Sandbox:**
```javascript
webPreferences: {
  sandbox: true  // Run renderer in sandboxed process
}
```

- Limits system calls
- Restricts file system access
- Prevents privilege escalation

**No Node Integration:**
```javascript
webPreferences: {
  nodeIntegration: false  // No require() in renderer
}
```

- Renderer cannot load Node.js modules
- Prevents XSS from executing arbitrary code

---

### IPC Security

**Safe Patterns:**
```javascript
// ✅ GOOD: Specific, validated handler
ipcHandle('exec:run', async (event, payload) => {
  const code = payload?.code || '';
  if (typeof code !== 'string') {
    return { ok: false, error: 'Invalid code' };
  }
  return bridge.execute(code);
});

// ❌ BAD: Exposing arbitrary function execution
ipcHandle('eval', async (event, payload) => {
  eval(payload.code);  // DANGEROUS!
});

// ❌ BAD: Exposing entire module
contextBridge.exposeInMainWorld('fs', require('fs'));
```

**Validation:**
- Always validate IPC payload types
- Sanitize file paths (check for ../ etc.)
- Limit buffer sizes (maxBuffer in exec)
- Timeout long operations

---

### Remote Execution Risks

**SSH Password Storage:**
- Passwords stored in renderer memory only
- Not persisted to disk by default
- Saved environments use localStorage (insecure!)

**⚠️ Security Warning:**
```javascript
// Current implementation stores passwords in plaintext
localStorage.setItem('ssh_env', JSON.stringify({
  host: '10.0.0.5',
  password: 'secret123'  // ⚠️ PLAINTEXT!
}));
```

**Recommended Improvements:**
1. Use OS keychain (keytar library)
2. Encrypt before localStorage
3. Support SSH key authentication
4. Prompt for password each session

**Docker Security:**
- Requires `sg docker` or docker group membership
- Docker socket gives root-equivalent access
- Container escape vulnerabilities

---

## Performance Considerations

### Why Animations Are Disabled

```css
*, *::before, *::after {
  transition: none !important;
  animation: none !important;
}
```

**Reasoning:**
- Electron/Chromium on weak GPUs struggles with CSS animations
- Repaints and reflows are expensive
- Disabled transitions improve responsiveness
- Trade-off: Less visual polish, more performance

---

### Hardware Acceleration Disabled

```javascript
app.disableHardwareAcceleration();
app.commandLine.appendSwitch('disable-gpu');
```

**Reasoning:**
- GPU issues on development system
- Software rendering more stable (though slower)
- Better compatibility with VMs and remote desktop
- Trade-off: Higher CPU usage, lower frame rate

---

### Monaco Editor Optimization

```javascript
// Disable expensive features
monaco.editor.create(container, {
  minimap: { enabled: true },  // Keep minimap, useful for navigation
  scrollBeyondLastLine: false,
  renderWhitespace: 'none',
  renderControlCharacters: false,
  fontLigatures: false,        // Disable ligatures (expensive)
  smoothScrolling: false       // Disable smooth scroll
});
```

---

### Terminal Performance

**PTY vs Spawn:**
- `node-pty` provides true PTY (better compatibility)
- `spawn` is lighter (fewer dependencies)
- Configurable via environment variable

**Buffer Management:**
```javascript
// Limit scrollback to prevent memory bloat
xterm.options.scrollback = 1000;

// Clear buffer periodically
terminal.clear();
```

---

### IPC Optimization

**Payload Summarization:**
```javascript
// Don't log entire code blocks
const summarizePayload = (payload) => {
  if (payload.code && payload.code.length > 180) {
    return { ...payload, code: `[len:${payload.code.length}]` };
  }
  return payload;
};
```

**Batching:**
```javascript
// Batch terminal output updates
let buffer = '';
socket.on('data', (chunk) => {
  buffer += chunk;
});
setInterval(() => {
  if (buffer) {
    win.webContents.send('terminal:data', { id, data: buffer });
    buffer = '';
  }
}, 16);  // ~60fps
```

---

## Design Decisions

### Why Electron?

**Pros:**
- Cross-platform (Linux, macOS, Windows)
- Rich UI with web technologies
- Mature ecosystem (Monaco, xterm.js)
- Easy IPC between processes
- Auto-updates support

**Cons:**
- Large bundle size (~200MB)
- High memory usage (~200MB base)
- Slow startup (~2-3 seconds)
- GPU issues on some systems

**Alternatives Considered:**
- Native (C++/Qt): More performant, harder to develop
- Web app: Can't access Docker/SSH directly
- VSCode extension: Limited by VSCode APIs

---

### Why Monaco Editor?

**Pros:**
- Same editor as VS Code
- Excellent syntax highlighting
- Rich API (decorations, markers, etc.)
- Multi-cursor, minimap, etc.

**Cons:**
- Large bundle (~10MB)
- Complex API
- Heavy on resources

**Alternatives:**
- CodeMirror: Lighter, less features
- Ace Editor: Older, less modern
- Custom textarea: Too basic

---

### Why Custom MUMPS Parser?

**Reasoning:**
- No existing MUMPS parsers in JavaScript
- Need control over AST structure for linting/debugging
- Educational value

**Challenges:**
- MUMPS syntax is complex (postconditionals, indirection, etc.)
- Parser is incomplete (doesn't handle all edge cases)
- Maintenance burden

**Future:**
- Consider using tree-sitter for better parsing
- Contribute to open-source MUMPS grammar

---

### Why JSON Debug Protocol?

**Alternatives:**
- TCP with custom binary protocol (complex)
- VSCode Debug Adapter Protocol (DAP) (overkill)
- Legacy TCP protocols (complex, harder to maintain)

**JSON Benefits:**
- Easy to parse in both JavaScript and MUMPS
- Human-readable for debugging
- Extensible (add new fields without breaking)
- Structured (type safety with TypeScript)

**Implementation:**
```mumps
; Easy to generate in MUMPS
WRITE "{""event"":""stopped"",""line"":",LIN,"}",!

// Easy to parse in JavaScript
const event = JSON.parse(line);
if (event.event === 'stopped') {
  updateUI(event.line);
}
```

---

### Why Docker AND SSH?

**Docker:**
- Isolated environments
- Easy setup/teardown
- Reproducible
- Developer-friendly

**SSH:**
- Production systems
- Legacy environments
- Remote debugging
- Real-world workflows

**Both:**
- Flexibility for different use cases
- Same codebase handles both (abstracted in bridge.js)

---

**End of Architecture Documentation**

For API details, see [API.md](API.md).
For user documentation, see [README.md](README.md).
