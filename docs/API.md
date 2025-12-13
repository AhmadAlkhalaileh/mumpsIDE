# Ahmad IDE - API Reference

Complete API documentation for all exposed methods and IPC channels.

---

## Table of Contents

1. [IPC API (Renderer → Main)](#ipc-api-renderer--main)
2. [Bridge API (Internal)](#bridge-api-internal)
3. [Debug Protocol](#debug-protocol)
4. [Event APIs (Main → Renderer)](#event-apis-main--renderer)
5. [Data Structures](#data-structures)

---

## IPC API (Renderer → Main)

All APIs exposed through `window.ahmadIDE` in the renderer process.

### Environment

#### `getEnv()`
Get system environment information.

**Returns:**
```typescript
{
  platform: string,        // 'linux' | 'darwin' | 'win32'
  versions: {
    node: string,
    chrome: string,
    electron: string
  },
  cwd: string             // Current working directory
}
```

**Example:**
```javascript
const env = await window.ahmadIDE.getEnv();
console.log(env.platform); // 'linux'
```

---

### Code Execution

#### `lint(code: string)`
Run linter on MUMPS code.

**Parameters:**
- `code` (string) - MUMPS code to lint

**Returns:**
```typescript
{
  ok: boolean,
  errors?: Array<{
    line: number,
    column: number,
    severity: 'error' | 'warning' | 'info',
    message: string,
    source: string
  }>,
  warnings?: Array<...>,
  error?: string          // Error message if ok=false
}
```

**Example:**
```javascript
const result = await window.ahmadIDE.lint(`
TEST    ; Test routine
        SET X=42
        WRITE X,!
        QUIT
`);

if (result.ok) {
  console.log('Errors:', result.errors);
}
```

---

#### `execute(code: string)`
Execute MUMPS code on remote system.

**Parameters:**
- `code` (string) - MUMPS code to execute

**Returns:**
```typescript
{
  ok: boolean,
  stdout?: string,        // Standard output
  stderr?: string,        // Standard error
  error?: string,         // Error message if ok=false
  exitCode?: number       // Process exit code
}
```

**Example:**
```javascript
const result = await window.ahmadIDE.execute(`
TEST    ; Test
        WRITE "Hello World!",!
        QUIT
`);

if (result.ok) {
  console.log(result.stdout); // "Hello World!\n"
}
```

---

### Connection Management

#### `setConnection(type: string, config: object)`
Configure Docker or SSH connection.

**Parameters:**
- `type` ('docker' | 'ssh') - Connection type
- `config` (object) - Connection configuration

**Docker Config:**
```typescript
{
  containerId: string,              // Docker container ID
  ydbPath?: string,                 // Default: '/opt/fis-gtm/YDB136'
  envKey?: string,                  // Default: 'hakeem'
  routinesPath?: string,            // Auto-generated from envKey
  gldPath?: string                  // Auto-generated from envKey
}
```

**SSH Config:**
```typescript
{
  host: string,                     // SSH host
  port: number,                     // SSH port (default 22)
  username: string,                 // SSH username
  password: string,                 // SSH password
  envKey?: string,                  // Environment key (auto-derived from username)
  ydbPath?: string,                 // Default: '/opt/fis-gtm/YDB136'
  routinesPath?: string,            // Auto-generated
  gldPath?: string                  // Auto-generated
}
```

**Returns:**
```typescript
{
  ok: boolean,
  error?: string
}
```

**Example:**
```javascript
// Docker
await window.ahmadIDE.setConnection('docker', {
  containerId: '8c21cf79fb67'
});

// SSH
await window.ahmadIDE.setConnection('ssh', {
  host: '10.0.0.5',
  port: 22,
  username: 'ahmad-cc',
  password: 'password123',
  envKey: 'cc'
});
```

---

### Debugging

#### `debugStart(code: string, breakpoints: array, startLine?: number)`
Start debug session.

**Parameters:**
- `code` (string) - MUMPS code to debug
- `breakpoints` (array) - Array of `{ file: string, line: number }`
- `startLine` (number, optional) - Line to start at

**Returns:**
```typescript
{
  ok: boolean,
  sessionId?: string,               // Debug session ID
  error?: string
}
```

**Example:**
```javascript
const result = await window.ahmadIDE.debugStart(
  `TEST\n SET X=42\n WRITE X,!\n QUIT`,
  [{ file: 'TEST.m', line: 2 }],
  null
);

if (result.ok) {
  console.log('Session ID:', result.sessionId);
}
```

---

#### `debugStep(sessionId: string, stepType: string)`
Step debugger.

**Parameters:**
- `sessionId` (string) - Debug session ID
- `stepType` ('into' | 'over' | 'out') - Step type

**Returns:**
```typescript
{
  ok: boolean,
  stopped?: boolean,                // True if paused at next line
  file?: string,                    // Current file
  line?: number,                    // Current line (1-based)
  routine?: string,                 // Current routine
  tag?: string,                     // Current tag
  offset?: number,                  // Offset from tag
  depth?: number,                   // Stack depth
  vars?: object,                    // Local variables
  error?: string
}
```

**Example:**
```javascript
const result = await window.ahmadIDE.debugStep(sessionId, 'into');

if (result.stopped) {
  console.log(`Stopped at ${result.file}:${result.line}`);
  console.log('Variables:', result.vars);
}
```

---

#### `debugContinue(sessionId: string)`
Continue execution until next breakpoint.

**Parameters:**
- `sessionId` (string) - Debug session ID

**Returns:**
```typescript
{
  ok: boolean,
  stopped?: boolean,
  file?: string,
  line?: number,
  vars?: object,
  error?: string
}
```

**Example:**
```javascript
await window.ahmadIDE.debugContinue(sessionId);
```

---

#### `debugStop(sessionId: string)`
Stop debug session.

**Parameters:**
- `sessionId` (string) - Debug session ID

**Returns:**
```typescript
{
  ok: boolean,
  error?: string
}
```

**Example:**
```javascript
await window.ahmadIDE.debugStop(sessionId);
```

---

#### `debugEval(sessionId: string, code: string)`
Evaluate MUMPS expression while debugging.

**Parameters:**
- `sessionId` (string) - Debug session ID
- `code` (string) - MUMPS code to evaluate

**Returns:**
```typescript
{
  ok: boolean,
  output?: string,                  // Captured output
  locals?: object,                  // Updated local variables
  error?: string
}
```

**Example:**
```javascript
const result = await window.ahmadIDE.debugEval(sessionId, 'WRITE X,!');

if (result.ok) {
  console.log('Output:', result.output); // "42\n"
  console.log('Locals:', result.locals);
}
```

---

### Docker

#### `listDocker()`
List running Docker containers.

**Returns:**
```typescript
{
  ok: boolean,
  containers?: Array<{
    id: string,                     // Container ID
    name: string,                   // Container name
    status: string                  // Status string
  }>,
  error?: string
}
```

**Example:**
```javascript
const result = await window.ahmadIDE.listDocker();

if (result.ok) {
  result.containers.forEach(c => {
    console.log(`${c.id}: ${c.name} (${c.status})`);
  });
}
```

---

### SSH

#### `sshConnect(config: object)`
Establish SSH connection.

**Parameters:**
- `config` (object) - SSH configuration (see setConnection)

**Returns:**
```typescript
{
  ok: boolean,
  sessionId?: string,               // SSH session ID
  error?: string
}
```

**Example:**
```javascript
const result = await window.ahmadIDE.sshConnect({
  host: '10.0.0.5',
  port: 22,
  username: 'ahmad',
  password: 'pass'
});

if (result.ok) {
  console.log('Session ID:', result.sessionId);
}
```

---

#### `sshExec(sessionId: string, command: string)`
Execute command over SSH.

**Parameters:**
- `sessionId` (string) - SSH session ID from sshConnect
- `command` (string) - Shell command to execute

**Returns:**
```typescript
{
  ok: boolean,
  stdout?: string,
  stderr?: string,
  error?: string
}
```

**Example:**
```javascript
const result = await window.ahmadIDE.sshExec(sessionId, 'ls -la');

if (result.ok) {
  console.log(result.stdout);
}
```

---

#### `sshDisconnect(sessionId: string)`
Close SSH connection.

**Parameters:**
- `sessionId` (string) - SSH session ID

**Returns:**
```typescript
{
  ok: boolean,
  error?: string
}
```

**Example:**
```javascript
await window.ahmadIDE.sshDisconnect(sessionId);
```

---

### Routines

#### `listRoutines(search?: string)`
List MUMPS routines.

**Parameters:**
- `search` (string, optional) - Filter pattern (glob)

**Returns:**
```typescript
{
  ok: boolean,
  routines?: string[],              // Array of routine names
  error?: string
}
```

**Example:**
```javascript
const result = await window.ahmadIDE.listRoutines('USER*');

if (result.ok) {
  console.log(result.routines); // ['USER', 'USERAUTH', 'USERINFO', ...]
}
```

---

#### `searchRoutines(term: string, options?: object)`
Search routine content.

**Parameters:**
- `term` (string) - Search term
- `options` (object, optional) - Search options

**Options:**
```typescript
{
  caseSensitive?: boolean,          // Default: false
  wholeWord?: boolean,              // Default: false
  regex?: boolean                   // Default: false
}
```

**Returns:**
```typescript
{
  ok: boolean,
  results?: Array<{
    file: string,                   // File path
    line: number,                   // Line number
    column: number,                 // Column number
    text: string                    // Matching line text
  }>,
  error?: string
}
```

**Example:**
```javascript
const result = await window.ahmadIDE.searchRoutines('SET X=', {
  caseSensitive: true
});

if (result.ok) {
  result.results.forEach(r => {
    console.log(`${r.file}:${r.line}: ${r.text}`);
  });
}
```

---

#### `readRoutine(name: string)`
Read routine content.

**Parameters:**
- `name` (string) - Routine name (without .m extension)

**Returns:**
```typescript
{
  ok: boolean,
  code?: string,                    // Routine source code
  error?: string
}
```

**Example:**
```javascript
const result = await window.ahmadIDE.readRoutine('TEST');

if (result.ok) {
  console.log(result.code);
}
```

---

#### `saveRoutine(name: string, code: string)`
Save routine to remote system.

**Parameters:**
- `name` (string) - Routine name
- `code` (string) - Routine source code

**Returns:**
```typescript
{
  ok: boolean,
  error?: string
}
```

**Example:**
```javascript
const result = await window.ahmadIDE.saveRoutine('TEST', `
TEST    ; Test routine
        SET X=42
        WRITE X,!
        QUIT
`);

if (result.ok) {
  console.log('Saved successfully');
}
```

---

#### `zlinkRoutine(name: string)`
Compile and link routine.

**Parameters:**
- `name` (string) - Routine name

**Returns:**
```typescript
{
  ok: boolean,
  output?: string,                  // Compilation output
  error?: string
}
```

**Example:**
```javascript
const result = await window.ahmadIDE.zlinkRoutine('TEST');

if (result.ok) {
  console.log('ZLINKed:', result.output);
}
```

---

### Terminal

#### `terminalCreate(options?: object)`
Create new terminal session.

**Parameters:**
- `options` (object, optional) - Terminal options

**Options:**
```typescript
{
  shell?: string,                   // Shell path (default: $SHELL)
  cwd?: string,                     // Working directory
  cols?: number,                    // Columns (default: 80)
  rows?: number                     // Rows (default: 24)
}
```

**Returns:**
```typescript
{
  ok: boolean,
  id?: string,                      // Terminal session ID
  error?: string
}
```

**Example:**
```javascript
const result = await window.ahmadIDE.terminalCreate({
  shell: '/bin/bash',
  cwd: '/home/ahmad',
  cols: 120,
  rows: 30
});

if (result.ok) {
  console.log('Terminal ID:', result.id);
}
```

---

#### `terminalWrite(id: string, data: string)`
Send input to terminal.

**Parameters:**
- `id` (string) - Terminal session ID
- `data` (string) - Input data (including control chars)

**Returns:**
```typescript
{
  ok: boolean,
  error?: string
}
```

**Example:**
```javascript
await window.ahmadIDE.terminalWrite(termId, 'ls -la\r');
```

---

#### `terminalResize(id: string, cols: number, rows: number)`
Resize terminal.

**Parameters:**
- `id` (string) - Terminal session ID
- `cols` (number) - New column count
- `rows` (number) - New row count

**Returns:**
```typescript
{
  ok: boolean,
  error?: string
}
```

**Example:**
```javascript
await window.ahmadIDE.terminalResize(termId, 120, 40);
```

---

#### `terminalClose(id: string)`
Close terminal session.

**Parameters:**
- `id` (string) - Terminal session ID

**Returns:**
```typescript
{
  ok: boolean,
  error?: string
}
```

**Example:**
```javascript
await window.ahmadIDE.terminalClose(termId);
```

---

#### `hostExec(command: string)`
Execute one-shot command on host.

**Parameters:**
- `command` (string) - Shell command

**Returns:**
```typescript
{
  ok: boolean,
  stdout?: string,
  stderr?: string,
  error?: string
}
```

**Example:**
```javascript
const result = await window.ahmadIDE.hostExec('pwd');
console.log(result.stdout); // "/home/ahmad"
```

---

### Git

#### `git(command: string)`
Execute git command.

**Parameters:**
- `command` (string) - Git command (without 'git' prefix)

**Returns:**
```typescript
{
  ok: boolean,
  stdout?: string,
  stderr?: string,
  error?: string
}
```

**Example:**
```javascript
const result = await window.ahmadIDE.git('status');

if (result.ok) {
  console.log(result.stdout);
}
```

---

#### `getGitConfig(projectPath: string)`
Get git configuration.

**Parameters:**
- `projectPath` (string) - Project directory path

**Returns:**
```typescript
{
  ok: boolean,
  config?: {
    name: string,                   // user.name
    email: string,                  // user.email
    remote: string                  // origin URL
  },
  error?: string
}
```

**Example:**
```javascript
const result = await window.ahmadIDE.getGitConfig('/home/ahmad/project');

if (result.ok) {
  console.log(result.config.name);
  console.log(result.config.email);
}
```

---

### Project

#### `createProject(config: object)`
Create new project structure.

**Parameters:**
- `config` (object) - Project configuration

**Config:**
```typescript
{
  name: string,                     // Project name
  path: string,                     // Parent directory
  fetchRoutines?: boolean           // Auto-fetch routines (default: true)
}
```

**Returns:**
```typescript
{
  ok: boolean,
  projectPath?: string,             // Full project path
  error?: string
}
```

**Example:**
```javascript
const result = await window.ahmadIDE.createProject({
  name: 'my-project',
  path: '/home/ahmad/projects',
  fetchRoutines: true
});

if (result.ok) {
  console.log('Created:', result.projectPath);
}
```

---

#### `openProject(path: string)`
Open existing project.

**Parameters:**
- `path` (string) - Project directory path

**Returns:**
```typescript
{
  ok: boolean,
  routines?: string[],              // Loaded routines
  error?: string
}
```

**Example:**
```javascript
const result = await window.ahmadIDE.openProject('/home/ahmad/projects/my-project');

if (result.ok) {
  console.log('Loaded routines:', result.routines);
}
```

---

### Dialogs

#### `openFolderDialog()`
Show folder picker dialog.

**Returns:**
```typescript
{
  ok: boolean,
  path?: string,                    // Selected folder path
  canceled?: boolean,               // True if user canceled
  error?: string
}
```

**Example:**
```javascript
const result = await window.ahmadIDE.openFolderDialog();

if (result.ok && !result.canceled) {
  console.log('Selected:', result.path);
}
```

---

#### `revealInExplorer(path: string)`
Show file/folder in system file manager.

**Parameters:**
- `path` (string) - File or folder path

**Returns:**
```typescript
{
  ok: boolean,
  error?: string
}
```

**Example:**
```javascript
await window.ahmadIDE.revealInExplorer('/home/ahmad/file.m');
```

---

### Application

#### `toggleDevTools()`
Toggle Chrome DevTools.

**Returns:**
```typescript
{
  ok: boolean
}
```

**Example:**
```javascript
await window.ahmadIDE.toggleDevTools();
```

---

#### `exitApp()`
Quit application.

**Returns:**
```typescript
{
  ok: boolean
}
```

**Example:**
```javascript
await window.ahmadIDE.exitApp();
```

---

## Event APIs (Main → Renderer)

Events sent from main process to renderer via IPC.

### Terminal Events

#### `onTerminalData(callback)`
Receive terminal output.

**Callback Signature:**
```typescript
(payload: { id: string, data: string }) => void
```

**Example:**
```javascript
window.ahmadIDE.onTerminalData(({ id, data }) => {
  console.log(`Terminal ${id} output:`, data);
  // Write to xterm instance
  terminals[id].write(data);
});
```

---

#### `onTerminalExit(callback)`
Terminal process exited.

**Callback Signature:**
```typescript
(payload: { id: string, code: number }) => void
```

**Example:**
```javascript
window.ahmadIDE.onTerminalExit(({ id, code }) => {
  console.log(`Terminal ${id} exited with code ${code}`);
  // Clean up terminal UI
});
```

---

## Debug Protocol

Communication between IDE and AHMDDG.m debugger.

### Commands (IDE → AHMDDG via stdin)

**Format:** Plain text commands, one per line

| Command | Description | Response Event |
|---------|-------------|----------------|
| `INTO` | Step into function | `stopped` |
| `OVER` | Step over line | `stopped` |
| `OUTOF` | Step out of function | `stopped` |
| `CONTINUE` | Continue execution | `stopped` (at breakpoint) |
| `HALT` | Stop debugging | `exit` |
| `GETVARS` | Request variables | `vars` |
| `SETBPJSON;{...}` | Set breakpoint (JSON) | `bp-set` |
| `CLEARBP;<routine>;<line>` | Clear breakpoint | `bp-cleared` |
| `EVAL;<code>` | Evaluate expression | `eval` |

---

### Events (AHMDDG → IDE via stdout)

**Format:** JSON objects, one per line

#### `ready`
Debugger ready, waiting for first command.

```json
{
  "event": "ready",
  "routine": "TEST",
  "tag": "MAIN"
}
```

---

#### `stopped`
Execution paused at line.

```json
{
  "event": "stopped",
  "routine": "TEST",
  "line": 5,
  "pos": "MAIN+3^TEST",
  "depth": 1,
  "tag": "MAIN",
  "offset": 3
}
```

**Fields:**
- `routine` - Current routine name
- `line` - 1-based line number
- `pos` - MUMPS position string (TAG+OFFSET^ROUTINE)
- `depth` - Stack depth
- `tag` - Current tag/label
- `offset` - Offset from tag

---

#### `vars`
Local variables.

```json
{
  "event": "vars",
  "vars": {
    "X": "42",
    "Y": "Hello",
    "Z": "123"
  }
}
```

---

#### `bp-set`
Breakpoint set successfully.

```json
{
  "event": "bp-set",
  "routine": "TEST",
  "tag": "MAIN",
  "offset": 3
}
```

---

#### `bp-cleared`
Breakpoint cleared.

```json
{
  "event": "bp-cleared",
  "routine": "TEST",
  "line": 5
}
```

---

#### `bp-cleared-routine`
All breakpoints cleared in routine.

```json
{
  "event": "bp-cleared-routine",
  "routine": "TEST"
}
```

---

#### `bp-cleared-all`
All breakpoints cleared.

```json
{
  "event": "bp-cleared-all"
}
```

---

#### `bp-error`
Breakpoint could not be set.

```json
{
  "event": "bp-error",
  "message": "Could not set breakpoint at MAIN+3^TEST"
}
```

---

#### `eval`
Expression evaluation result.

```json
{
  "event": "eval",
  "ok": 1,
  "output": "42\nHello\n",
  "locals": {
    "X": "42",
    "Y": "Hello"
  }
}
```

**Or on error:**
```json
{
  "event": "eval",
  "ok": 0,
  "error": "<UNDEF>X"
}
```

---

#### `error`
Error occurred.

```json
{
  "event": "error",
  "message": "<SYNTAX> error message"
}
```

---

#### `exit`
Debug session ended.

```json
{
  "event": "exit"
}
```

---

## Data Structures

### Breakpoint
```typescript
{
  file: string,              // File path (e.g., '/path/ROUTINE.m')
  line: number,              // 1-based line number
  routine?: string,          // Routine name (uppercase, no .m)
  tag?: string,              // Tag/label name
  offset?: number            // Offset from tag
}
```

---

### Debug Session State
```typescript
{
  sessionId: string,         // Unique session ID
  routine: string,           // Current routine
  file: string,              // Current file path
  line: number,              // Current line (1-based)
  stopped: boolean,          // True if paused
  breakpoints: Breakpoint[], // Active breakpoints
  vars: object,              // Local variables
  stack: StackFrame[]        // Call stack
}
```

---

### Stack Frame
```typescript
{
  routine: string,           // Routine name
  tag: string,               // Tag name
  offset: number,            // Offset from tag
  line: number,              // Line number (1-based)
  depth: number,             // Stack depth (0 = top)
  pos: string                // MUMPS position string
}
```

---

### Terminal Session
```typescript
{
  id: string,                // Session ID
  shell: string,             // Shell path
  cwd: string,               // Working directory
  cols: number,              // Terminal columns
  rows: number,              // Terminal rows
  process: object            // PTY or ChildProcess instance
}
```

---

### Git Status
```typescript
{
  branch: string,            // Current branch
  ahead: number,             // Commits ahead of remote
  behind: number,            // Commits behind remote
  staged: string[],          // Staged file paths
  unstaged: string[],        // Modified file paths
  untracked: string[],       // Untracked file paths
  conflicts: string[]        // Conflicted file paths
}
```

---

## Error Handling

All APIs return objects with `ok: boolean`. Check `ok` before accessing data.

**Success:**
```javascript
{
  ok: true,
  // ... result data
}
```

**Failure:**
```javascript
{
  ok: false,
  error: "Error message describing what went wrong"
}
```

**Best Practice:**
```javascript
const result = await window.ahmadIDE.someMethod();

if (result.ok) {
  // Success - use result.data
  console.log(result.data);
} else {
  // Error - show result.error
  console.error('Operation failed:', result.error);
  showErrorToast(result.error);
}
```

---

## Bridge API (Internal)

These are internal functions in `bridge.js`, not directly exposed to renderer.

### Core Functions

- `lint(code)` - Lint MUMPS code
- `execute(code)` - Execute MUMPS code
- `setConnection(type, config)` - Set connection config
- `debugStart(code, breakpoints, startLine)` - Start debugging
- `debugStep(sessionId, stepType)` - Step debugger
- `debugContinue(sessionId)` - Continue debugging
- `debugStop(sessionId)` - Stop debugging
- `debugEval(sessionId, code)` - Evaluate expression
- `sshConnect(config)` - Connect SSH
- `sshExec(sessionId, command)` - Execute SSH command
- `sshDisconnect(sessionId)` - Disconnect SSH
- `listRoutines(search)` - List routines
- `searchRoutines(term, options)` - Search routine content
- `readRoutine(name)` - Read routine file
- `saveRoutine(name, code)` - Save routine file
- `zlinkRoutine(name)` - Compile and link routine
- `hostExec(command)` - Execute shell command
- `git(command)` - Execute git command
- `getGitConfig(projectPath)` - Get git config
- `createProject(config)` - Create project structure
- `openProject(path)` - Open project

### Helper Functions

- `buildYdbEnv(config, options)` - Build YottaDB environment exports
- `wrapDockerCmd(cmd)` - Wrap command with `sg docker -c`
- `runHostCommand(cmd)` - Execute command on host
- `writeRemoteFile(path, content)` - Write file to remote system
- `readRemoteFile(path)` - Read file from remote system
- `startMdebugServer(host, port)` - Start AHMDBG server
- `ensureMdebugServer(host, port)` - Ensure server is running
- `convertMdebugPosition(posString)` - Convert MUMPS position to file:line

---

**End of API Documentation**
