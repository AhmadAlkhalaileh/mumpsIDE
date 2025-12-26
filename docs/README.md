# Mumps Studio - Complete Documentation

**Version:** 1.0.0
**Author:** Ahmad
**Last Updated:** December 12, 2024

---

## Table of Contents

1. [Overview](#overview)
2. [Features](#features)
3. [Architecture](#architecture)
4. [Installation & Setup](#installation--setup)
5. [User Interface Guide](#user-interface-guide)
6. [Core Features](#core-features)
7. [Technical Documentation](#technical-documentation)
8. [API Reference](#api-reference)
9. [Development Guide](#development-guide)
10. [Troubleshooting](#troubleshooting)

---

## Overview

Mumps Studio is a modern, inspired Integrated Development Environment built specifically for **MUMPS (M)** language development. It provides a complete development workflow including code editing, debugging, execution, version control, and remote connection management through Docker and SSH.

### Key Highlights

- **Desktop Application**: Built with Electron for cross-platform support (Linux, macOS, Windows)
- **MUMPS-First**: Native support for MUMPS/M language with custom parser, linter, and debugger
- **Modern UI**: style dark theme interface with professional developer experience
- **Remote Development**: Connect to YottaDB/GT.M instances via Docker or SSH
- **Integrated Debugger**: Advanced JSON-based debugger (AHMDBG) with breakpoints, stepping, and variable inspection
- **Git Integration**: Built-in version control with visual diff, commit, push/pull workflows
- **Terminal**: Integrated xterm.js terminal with multiple tab support

---

## Features

### Development Tools
✅ **Monaco Editor** - Microsoft's VS Code editor engine with syntax highlighting
✅ **MUMPS Parser** - Custom lexer, AST, and validation for M language
✅ **Code Linting** - Real-time syntax validation and error detection
✅ **Code Execution** - Run MUMPS routines directly from the editor
✅ **Auto-completion** - Context-aware code suggestions

### Debugging
✅ **AHMDBG Engine** - JSON-based debugger with stdin/stdout protocol
✅ **Breakpoints** - Set/clear breakpoints with line-level precision
✅ **Stepping** - Step Into, Step Over, Step Out, Continue
✅ **Variable Inspection** - Real-time local and system variable viewing
✅ **Call Stack** - View execution stack frames
✅ **REPL/Eval** - Evaluate arbitrary MUMPS code while paused

### Remote Connections
✅ **Docker Support** - Connect to running Docker containers
✅ **SSH Support** - Secure shell connections with password authentication
✅ **Multi-Environment** - Save and switch between different environments
✅ **Routine Management** - Read, write, search, and ZLINK routines remotely

### User Interface
✅ **Project Explorer** - Tree view of routines with filtering
✅ **Multi-Tab Editor** - Work on multiple files simultaneously
✅ **Tool Windows** - Terminal, Debug, Git, Problems, Services panels
✅ **Status Bar** - Git branch, problems count, cursor position, connection status
✅ **Keyboard Shortcuts** - Configurable keybindings

### Version Control (Git)
✅ **Status & Changes** - View modified, staged, and unstaged files
✅ **Commit & Push** - Commit changes and push to remote
✅ **Diff Viewer** - Side-by-side file comparison
✅ **Branch Management** - Create and switch branches
✅ **History** - View commit log and file history

---

## Architecture

### Technology Stack

```
┌─────────────────────────────────────────────────────┐
│                  Mumps Studio                       │
├─────────────────────────────────────────────────────┤
│  Electron (v27.1.0)                                 │
│  ├─ Main Process (Node.js)                          │
│  │   ├─ main.js         - App lifecycle             │
│  │   ├─ bridge.js       - MUMPS runtime bridge      │
│  │   ├─ preload.js      - IPC security layer        │
│  │   └─ utils/          - Logging & debug utils     │
│  │                                                   │
│  └─ Renderer Process (Browser)                      │
│      ├─ renderer.js     - UI logic & state          │
│      ├─ index.html      - Main UI layout            │
│      ├─ styles.css      - style theme      │
│      └─ assets/mumps/   - MUMPS language support    │
│                                                      │
├─────────────────────────────────────────────────────┤
│  External Libraries                                 │
│  ├─ Monaco Editor   - Code editor                   │
│  ├─ xterm.js        - Terminal emulator             │
│  ├─ node-pty        - PTY for terminal (optional)   │
│  ├─ ssh2            - SSH client                    │
│  └─ jQuery          - DOM manipulation              │
├─────────────────────────────────────────────────────┤
│  Remote MUMPS Runtime                               │
│  ├─ YottaDB/GT.M    - MUMPS database engine         │
│  ├─ Docker          - Container runtime             │
│  ├─ SSH             - Remote shell access           │
│  └─ AHMDBG.m        - Debugger backend (M routine)  │
└─────────────────────────────────────────────────────┘
```

### Process Architecture

```
┌───────────────────────────────────────────────────────────┐
│  MAIN PROCESS (Node.js)                                   │
│  ┌─────────────────────────────────────────────────────┐  │
│  │  IPC Handlers                                       │  │
│  │  • env:get        • debug:start   • git:run        │  │
│  │  • lint:run       • debug:step    • routines:list  │  │
│  │  • exec:run       • debug:stop    • ssh:connect    │  │
│  │  • terminal:*     • docker:list   • project:*      │  │
│  └─────────────────────────────────────────────────────┘  │
│                           ↕ IPC                           │
│  ┌─────────────────────────────────────────────────────┐  │
│  │  Bridge (bridge.js)                                 │  │
│  │  • SSH Client Manager                               │  │
│  │  • Docker Command Executor                          │  │
│  │  • MUMPS Code Execution                             │  │
│  │  • AHMDBG Debugger Controller                       │  │
│  │  • Routine File I/O                                 │  │
│  └─────────────────────────────────────────────────────┘  │
└───────────────────────────────────────────────────────────┘
                            ↕
┌───────────────────────────────────────────────────────────┐
│  RENDERER PROCESS (Chromium)                              │
│  ┌─────────────────────────────────────────────────────┐  │
│  │  UI Controllers (renderer.js)                       │  │
│  │  • Editor Manager    • Debug Controller            │  │
│  │  • Terminal Manager  • Git UI                      │  │
│  │  • Tab Manager       • Connection Manager          │  │
│  │  • Menu System       • Keyboard Shortcuts          │  │
│  └─────────────────────────────────────────────────────┘  │
│                                                           │
│  ┌─────────────────────────────────────────────────────┐  │
│  │  Monaco Editor                                      │  │
│  │  • Syntax Highlighting  • Breakpoint Decorations   │  │
│  │  • Code Completion      • Error Markers            │  │
│  └─────────────────────────────────────────────────────┘  │
└───────────────────────────────────────────────────────────┘
                            ↕
        ┌───────────────────────────────────────┐
        │  REMOTE MUMPS ENVIRONMENT             │
        │  ┌─────────────────────────────────┐  │
        │  │  Docker Container / SSH Host    │  │
        │  │  • YottaDB/GT.M Runtime         │  │
        │  │  • AHMDBG.m (Debug Server)      │  │
        │  │  • Routine Files (.m)           │  │
        │  │  • Global Database              │  │
        │  └─────────────────────────────────┘  │
        └───────────────────────────────────────┘
```

---

## Installation & Setup

### Prerequisites

- **Node.js** v16+ and npm
- **Docker** (for Docker-based connections) with `sg docker` permissions
- **SSH server** (for SSH-based connections)
- **YottaDB or GT.M** installed in target environment

### Installation Steps

1. **Clone or extract the project:**
   ```bash
   cd ~/Desktop
   cd "Mumps Studio"
   ```

2. **Install dependencies:**
   ```bash
   npm install
   ```

3. **Start the application:**
   ```bash
   npm start
   ```

   Or with custom flags:
   ```bash
   AHMAD_IDE_USE_SG=1 electron .
   ```

### Environment Variables

| Variable | Description | Default |
|----------|-------------|---------|
| `AHMAD_IDE_USE_SG` | Use `sg docker` wrapper for Docker commands | `0` |
| `AHMAD_IDE_ENABLE_NODE_PTY` | Enable node-pty for terminal (requires compilation) | `0` |
| `AHMAD_IDE_DEBUG_ENGINE` | Debug engine mode (ZSTEP only) | `zstep` |

---

## User Interface Guide

### Main Window Layout

![Main Window](screenshots/01-main-window.png)

The IDE follows a inspired layout with these major sections:

1. **Menu Bar** (Top) - Application menu and branding
2. **Toolbar** (Below menu) - Save, Run, Debug, Lint, Git controls
3. **Left Tool Window Bar** - Project, Structure panels
4. **Project Explorer** - Tree view of routines/files
5. **Editor Area** (Center) - Multi-tab code editor with Monaco
6. **Bottom Tool Window Bar** - Terminal, Debug, Problems, Services
7. **Status Bar** (Bottom) - Git branch, problems, cursor position, connection status

---

### 1. Main Interface Components

#### Menu Bar
![Menu Bar](screenshots/02-menu-bar.png)

**Location:** Top of window
**Components:**
- **Brand Icon** 🛰️ "Mumps Studio -  Brown"
- **Main Menu** - File, Edit, View, Tools, Git, Help (dynamically populated)

#### Toolbar
![Toolbar](screenshots/03-toolbar.png)

**Left Section:**
- **Save Button** (💾) - Save current routine (Ctrl+S)
- **Lint Button** (🔍) - Validate MUMPS code
- **Run Button** (▶️) - Execute current routine (Ctrl+Enter)
- **Debug Button** (🐛) - Start debugger (Shift+F9)
- **Run Config Dropdown** - Select run/debug configuration

**Right Section:**
- **VCS Widget** - Git branch and quick actions

---

### 2. Project Explorer (Left Panel)

![Project Panel](screenshots/04-project-panel.png)

**Features:**
- **Filter Input** - Search/filter routines by name
- **Tree View** - Hierarchical display of MUMPS routines
- **Context Menu** - Right-click for actions
- **Collapse All** - Collapse entire tree
- **Hide Button** - Toggle panel visibility

**Usage:**
1. Click routine name to open in editor
2. Use filter box to search (e.g., type "USER" to find USER*.m)
3. Right-click for Git operations, rename, delete

---

### 3. Editor Area (Center)

![Editor](screenshots/05-editor-main.png)

**Monaco Editor Features:**
- **Syntax Highlighting** - MUMPS-specific tokenization
- **Line Numbers** - Gutter with line numbers
- **Minimap** - Code overview on right side
- **Tabs** - Multiple files open simultaneously
- **Breadcrumbs** - File path navigation
- **Status Info** - Line/column position in status bar

**Keyboard Shortcuts:**
- `Ctrl+S` - Save
- `Ctrl+Enter` - Run code
- `Ctrl+F` - Find in file
- `Ctrl+Shift+F` - Find in all files
- `Ctrl+D` - Duplicate line
- `F9` - Toggle breakpoint
- `Shift+F9` - Start debugging

---

### 4. Terminal Panel (Bottom)

![Terminal](screenshots/06-terminal-panel.png)

**Features:**
- **Multi-Tab Support** - Open multiple terminal sessions
- **xterm.js** - Full-featured terminal emulator
- **New Tab Button** (+) - Create new terminal
- **Clear Button** - Clear terminal output
- **Hide Button** (✕) - Close terminal panel

**Keyboard Shortcuts:**
- `Alt+F12` - Toggle terminal
- `Ctrl+Shift+T` - New terminal tab

**Terminal Sessions:**
- Each tab runs an independent shell (bash/zsh/etc.)
- Supports colors, cursor positioning, and control sequences
- Persistent sessions until closed

---

### 5. Debug Panel (Bottom)

![Debug Panel](screenshots/07-debug-panel.png)

**Tabs:**
1. **Breakpoints** - List of all breakpoints with file:line
2. **Variables** - Local variables when paused
3. **Watches** - Watched expressions (not implemented)
4. **Call Stack** - Execution stack frames
5. **Console** - Debug output and events
6. **Problems** - Syntax errors and warnings

**Debug Toolbar (Floating):**
![Debug Toolbar](screenshots/08-debug-toolbar.png)

When debugging, a floating toolbar appears with:
- **Continue** (▶️) - Resume execution (F9)
- **Pause** (⏸️) - Pause execution
- **Step Over** (↷) - Execute next line (F8)
- **Step Into** (↓) - Step into function (F7)
- **Step Out** (↑) - Step out of function (Shift+F8)
- **Restart** (🔄) - Restart debug session
- **Stop** (■) - Stop debugger (Ctrl+F2)

---

### 6. Git Tool Window

![Git Panel](screenshots/09-git-panel.png)

Access via: VCS menu or toolbar Git widget

**Left Side - Local Changes:**
- **Unstaged Files** - Modified files not staged
- **Staged Files** - Files ready to commit
- **Action Buttons:**
  - Refresh, Stage Selected, Unstage Selected, Diff Selected
- **Commit Section:**
  - Commit message textarea
  - Commit, Push, Pull, Fetch buttons
- **Branch Management:**
  - Branch selector dropdown
  - New branch input
  - Checkout button

**Right Side - History & Diff:**
- **Status/Log/Diff Buttons** - View Git information
- **Console Output** - Git command results
- **File Diff:**
  - Path input for specific file diff
  - Show Diff, File History buttons
- **Compare Two Files:**
  - Side-by-side diff viewer
  - Path A and Path B inputs

---

### 7. Connections Panel

![Connections Panel](screenshots/10-connections-panel.png)

Access via: Status bar "Connections" button

**Docker Section:**
- **Container List** - Running Docker containers
- **Refresh Docker** - Update container list
- **Use Default** - Use default container configuration

**SSH Section:**
- **Connection Form:**
  - Host (IP address)
  - Port (default 22)
  - Environment key (e.g., "cc")
  - Username
  - Password
- **Connect SSH** - Establish SSH connection
- **Saved Environments** - Previously saved configurations
- **Save Environment** - Store current connection for reuse

**Connection Status:** Displayed in status bar (Ready/Connected/Error)

---

### 8. Services Panel

![Services Panel](screenshots/11-services-panel.png)

Access via: Bottom toolbar "Services" button (Alt+8)

**Docker Services:**
- List running containers with ID, name, and status
- Refresh container list
- Quick access to Docker management

**SSH Services:**
- Run one-off SSH commands
- Command input field
- Execute button
- Output display

---

### 9. Settings Panel

![Settings Panel](screenshots/12-settings-panel.png)

Access via: Menu → Settings

**Appearance:**
- **IDE Theme** - Earth Dark, Desert Contrast
- **Code Theme** - Earthy, Carbon (MUMPS syntax themes)

**Git Configuration:**
- User name (`git config user.name`)
- Email (`git config user.email`)
- Remote URL (origin)
- Save and Test buttons

**Developer Tools:**
- Toggle DevTools button for debugging the IDE itself

**Terminal Settings:**
- Shell configuration (stub)

**SSH/Docker Settings:**
- Remote connection configurations (stub)

---

### 10. Keyboard Shortcuts Panel

![Shortcuts Panel](screenshots/13-shortcuts-panel.png)

Access via: Menu → Keyboard Shortcuts

**Registered Commands:**
- Complete list of all keyboard bindings
- Click to copy command details

**Update Binding:**
- Command selector dropdown
- New keybinding input (e.g., "Ctrl+Shift+L")
- Save button
- Validation for key combinations

**Default Shortcuts:**
- `Ctrl+S` - Save
- `Ctrl+Enter` - Run
- `Ctrl+Shift+L` - Lint
- `Alt+1` - Toggle Project panel
- `Alt+F12` - Toggle Terminal
- `Ctrl+Shift+T` - New Terminal
- `Ctrl+D` - Duplicate line
- `F9` - Toggle breakpoint
- `Shift+Shift` (double tap) - Search Everywhere

---

### 11. Find/Replace Dialog

![Find Dialog](screenshots/14-find-dialog.png)

Access via: `Ctrl+Shift+F`

**Find Mode:**
- Text to find input
- Options: Case sensitive, Whole words, Regex
- Scope indicator (current project/file)
- Results list with file:line matches

**Replace Mode:**
- Switch to Replace button
- Replace with input
- Replace All button
- Preview changes before applying

---

### 12. Search Everywhere

![Search Everywhere](screenshots/15-search-everywhere.png)

Access via: Press `Shift` twice quickly

**Features:**
- Global search across all files
- Type filename to find quickly
- Navigate to files instantly
- Similar to 's "Search Everywhere"

---

## Core Features

### Feature 1: Code Editing

#### Opening Files
1. **From Project Explorer:** Click routine name in left panel
2. **From Menu:** File → Open
3. **Search Everywhere:** Double-tap Shift, type filename

#### Editing Features
- **Syntax Highlighting:** MUMPS keywords, functions, operators
- **Auto-Indentation:** Automatic code formatting
- **Line Numbers:** Visible in gutter
- **Minimap:** Code overview on right
- **Find/Replace:** `Ctrl+F` for current file
- **Multi-Cursor:** `Alt+Click` for multiple cursors

#### Saving Files
- **Save Current:** `Ctrl+S` or Save button
- **Auto-Save:** (Not implemented - manual save required)

---

### Feature 2: Running MUMPS Code

![Run Execution](screenshots/16-run-execution.png)

**Methods:**
1. **Run Button** (▶️) in toolbar
2. **Keyboard Shortcut:** `Ctrl+Enter`
3. **Menu:** Run → Execute Current Routine

**Execution Flow:**
1. IDE sends code to `bridge.js`
2. Bridge selects connection (Docker or SSH)
3. Code is written to temporary file on remote system
4. MUMPS runtime (`mumps -run`) executes the routine
5. Output appears in Terminal panel

**Error Handling:**
- Syntax errors shown in Problems panel
- Runtime errors displayed in terminal
- Stack traces preserved

**Example:**
```mumps
TEST    ; Simple test routine
        WRITE "Hello from Mumps Studio!",!
        SET X=42
        WRITE "X = ",X,!
        QUIT
```

---

### Feature 3: Code Linting

![Linting Results](screenshots/17-linting-results.png)

**Trigger Lint:**
1. Click Lint button (🔍) in toolbar
2. Automatic linting on file save (if enabled)

**Linter Features:**
- **Syntax Validation:** Checks MUMPS grammar
- **Semantic Analysis:** Variable usage, undefined labels
- **Best Practices:** Code style recommendations
- **Real-Time Markers:** Error/warning squiggles in editor

**Linter Components:**
- `mumps-lexer.js` - Tokenizes MUMPS code
- `mumps-parser.js` - Builds Abstract Syntax Tree (AST)
- `mumps-validator.js` - Validates AST against rules
- `mumps-linter.js` - Reports issues to UI

**Example Errors:**
- Undefined label reference
- Missing closing parenthesis
- Invalid command syntax
- Unreachable code

---

### Feature 4: Debugging with AHMDBG

![Debug Session](screenshots/18-debug-session.png)

Mumps Studio includes a custom JSON-based debugger called **AHMDDG** (Ahmad JSON Debugger) implemented in pure MUMPS.

#### Starting Debug Session

**Method 1: Debug Button**
1. Set breakpoints by clicking line number gutter (or F9)
2. Click Debug button (🐛) or press `Shift+F9`
3. Debugger starts and pauses at first breakpoint

**Method 2: Run Configuration**
1. Click Run Config dropdown
2. Select "Current file (Debug)"
3. Click Run

#### Debugger Features

**Breakpoints:**
- Click line number to set/clear
- Red dot indicates active breakpoint
- Listed in Breakpoints tab
- Persist across debug sessions

**Stepping:**
- **Step Into (F7):** Enter function calls
- **Step Over (F8):** Execute without entering functions
- **Step Out (Shift+F8):** Exit current function
- **Continue (F9):** Run until next breakpoint

**Variable Inspection:**
![Variables Tab](screenshots/19-debug-variables.png)

- **Variables Tab:** All local variables in current scope
- **Hover:** Hover over variable in editor to see value
- **ZSHOW Format:** Variables shown as `NAME=value`

**Call Stack:**
![Call Stack](screenshots/20-debug-callstack.png)

- **Stack Tab:** Shows execution frames
- **Frame Info:** Routine name, line number, depth
- Click frame to navigate to that position

**Debug Console (REPL):**
![Debug Console](screenshots/21-debug-console.png)

- **Eval Commands:** Execute arbitrary MUMPS code while paused
- **Inspect Globals:** `WRITE ^GLOBAL(1,2,3)`
- **Modify Locals:** `SET X=100`
- **Call Functions:** `DO SUBROUTINE^ROUTINE`

#### AHMDDG Protocol

**Architecture:**
```
┌──────────────┐          ┌──────────────┐          ┌──────────────┐
│ Mumps Studio │          │  bridge.js   │          │  AHMDDG.m    │
│  (renderer)  │  ◄────►  │  (stdio)     │  ◄────►  │  (YottaDB)   │
└──────────────┘   IPC    └──────────────┘   JSON   └──────────────┘
```

**Commands (IDE → AHMDDG):**
```json
INTO                                    # Step into
OVER                                    # Step over
OUTOF                                   # Step out
CONTINUE                                # Continue execution
HALT                                    # Stop debugger
GETVARS                                 # Request variables
SETBPJSON;{"routine":"TEST","tag":"MAIN","offset":3}
CLEARBP;TEST;5                          # Clear breakpoint at line 5
EVAL;WRITE X,!                          # Evaluate expression
```

**Events (AHMDDG → IDE):**
```json
{"event":"ready","routine":"TEST","tag":"MAIN"}
{"event":"stopped","routine":"TEST","line":5,"pos":"MAIN+3^TEST","depth":1,"tag":"MAIN","offset":3}
{"event":"vars","vars":{"X":"42","Y":"Hello"}}
{"event":"bp-set","routine":"TEST","tag":"MAIN","offset":3}
{"event":"bp-cleared","routine":"TEST","line":5}
{"event":"eval","ok":1,"output":"42\n","locals":{"X":"42"}}
{"event":"error","message":"<UNDEF>X"}
```

**ZSTEP Integration:**

AHMDDG uses YottaDB's `$ZSTEP` facility:
```mumps
SET $ZSTEP="ZSHOW ""V"":^%AHMDDG($J,""VARS"") SET %STP=$$STEPJSON^AHMDDG() ..."
```

This captures variables at each step and waits for debugger commands via stdin.

---

### Feature 5: Git Integration

![Git Workflow](screenshots/22-git-workflow.png)

**Opening Git Panel:**
- Click Git widget in toolbar
- Menu → Git → Show Tool Window
- VCS menu dropdown

#### Git Operations

**1. Status**
```bash
git status
```
Shows modified, staged, untracked files

**2. Staging Files**
- Select file(s) in Unstaged list
- Click "Stage Selected"
- Or use Git → Stage menu

**3. Committing**
![Git Commit](screenshots/23-git-commit.png)

1. Write commit message in textarea
2. Click "Commit" button
3. IDE runs: `git commit -m "Your message"`

**4. Push/Pull**
- **Push:** Send commits to remote (`git push`)
- **Pull:** Fetch and merge from remote (`git pull`)
- **Fetch:** Download without merging (`git fetch`)

**5. Branching**
![Git Branches](screenshots/24-git-branches.png)

- **View Branches:** Dropdown shows all branches
- **Create Branch:** Type name in input, click Checkout
- **Switch Branch:** Select from dropdown, click Checkout

**6. File Diff**
![Git Diff](screenshots/25-git-diff.png)

- Enter file path (relative to project root)
- Click "Show Diff"
- Side-by-side diff appears in right panel
- Red: Deleted lines
- Green: Added lines

**7. Compare Two Files**
- Enter Path A and Path B
- Click "Compare"
- Visual diff displayed

**8. History/Log**
```bash
git log --oneline --graph --decorate -20
```
Shows recent commits with graph visualization

#### Git Configuration

Configure Git user in Settings panel:
```bash
git config user.name "Ahmad"
git config user.email "ahmad@example.com"
git remote add origin https://github.com/user/repo.git
```

---

### Feature 6: Remote Connection Management

#### Docker Connections

![Docker Connection](screenshots/26-docker-connection.png)

**Setup:**
1. Ensure Docker daemon is running
2. Start target container: `docker run -d yottadb/yottadb`
3. Open Connections panel
4. Click "Refresh Docker"
5. Select container from list
6. Click container to connect

**Configuration:**
- **Container ID:** Detected automatically
- **YottaDB Path:** `/opt/fis-gtm/YDB136`
- **Env Key:** `prod` (default for Docker)
- **Globals Path:** `/var/worldvista/prod/globals/mumps.gld`
- **Routines Path:** `/var/worldvista/prod/localr`

**Docker Commands:**
IDE wraps commands with `sg docker -c` if `AHMAD_IDE_USE_SG=1`:
```bash
sg docker -c "docker exec <container_id> bash -c 'command'"
```

#### SSH Connections

![SSH Connection](screenshots/27-ssh-connection.png)

**Setup:**
1. Open Connections panel
2. Fill SSH form:
   - Host: `10.0.0.5` (example)
   - Port: `22`
   - Environment key: `cc`
   - Username: `ahmad-cc`
   - Password: `********`
3. Click "Connect SSH"

**Connection Flow:**
1. IDE uses `ssh2` library to establish connection
2. Session stored in `sshSessions` Map
3. Commands executed via `SSH.exec()`
4. Session persists until disconnect

**Saved Environments:**
- Click "Save environment" to store connection
- Saved configs appear in "Saved environments" list
- Click saved env to load credentials
- Stored in localStorage

**Environment Key Derivation:**
Username `ahmad-cc` → Env key `cc`
- Paths: `/var/worldvista/prod/cc/`
- Globals: `/var/worldvista/prod/cc/globals/mumps.gld`
- Routines: `/var/worldvista/prod/cc/localr`

---

### Feature 7: Routine Management

#### Listing Routines

![Routine List](screenshots/28-routine-list.png)

**Command:**
```bash
# Docker
docker exec <container> bash -c 'ls /var/worldvista/prod/localr/*.m'

# SSH
ssh user@host 'ls /var/worldvista/prod/cc/localr/*.m'
```

**Features:**
- Tree view in Project panel
- Filter box for searching
- Refresh button to reload
- Context menu for actions

#### Reading Routines

**Method:**
1. Click routine name in Project panel
2. IDE calls `bridge.readRoutine(name)`
3. Bridge reads file from remote system
4. Content loaded into Monaco editor

**Implementation:**
```javascript
// Docker
docker exec <container> cat /path/to/ROUTINE.m

// SSH
ssh user@host 'cat /path/to/ROUTINE.m'
```

#### Saving Routines

**Method:**
1. Edit code in Monaco editor
2. Press `Ctrl+S` or click Save button
3. IDE calls `bridge.saveRoutine(name, code)`
4. Code written to remote file

**Implementation:**
```bash
# Docker
echo "code content" | docker exec -i <container> bash -c 'cat > /path/to/ROUTINE.m'

# SSH
ssh user@host 'cat > /path/to/ROUTINE.m' <<< "code content"
```

#### Searching Routines

![Routine Search](screenshots/29-routine-search.png)

**Features:**
- Search by routine name (glob patterns)
- Search by content (grep)
- Case-sensitive/insensitive options
- Results with line numbers

**Implementation:**
```bash
# Search by name
ls /path/routines/*PATTERN*.m

# Search by content
grep -rn "search term" /path/routines/
```

#### ZLINK (Compile & Link)

**Purpose:** Compile and link routine into running MUMPS process

**Usage:**
1. Save routine file
2. Menu → Tools → ZLINK Routine
3. IDE executes: `ZLINK "ROUTINE"`

**Implementation:**
```bash
mumps -run %XCMD 'ZLINK "ROUTINE"'
```

---

### Feature 8: Terminal Integration

![Terminal](screenshots/30-terminal.png)

**xterm.js Terminal:**
- Full VT100/xterm compatibility
- ANSI color support
- Cursor positioning
- Scrollback buffer

**PTY vs Spawn:**
- **node-pty (Optional):** True PTY for interactive shells
- **spawn (Fallback):** Simple stdin/stdout pipes

**Terminal Sessions:**
- Each tab is independent process
- Persistent until tab closed
- Supports any shell (bash, zsh, fish, etc.)

**Features:**
- Copy/paste (Ctrl+Shift+C/V)
- Clear terminal
- Resize on window resize
- Multiple tabs

**Environment:**
```javascript
{
  name: 'xterm-color',
  cols: 80,
  rows: 24,
  cwd: '/home/user/project',
  env: process.env
}
```

---

## Technical Documentation

### File Structure

```
Mumps Studio/
├── main.js                 # Electron main process entry point
├── bridge.js              # MUMPS runtime bridge (3400+ lines)
├── preload.js             # IPC security layer
├── renderer.js            # Main UI controller
├── index.html             # Main UI layout
├── styles.css             # -inspired theme (2000+ lines)
├── AHMDDG.m               # MUMPS debugger backend (JSON-based)
├── package.json           # NPM dependencies
├── package-lock.json      # Locked dependency versions
│
├── src/
│   └── editor/            # Modular editor components
│       ├── debug/         # Debug panel and controls
│       ├── diagnostics/   # Linting and error reporting
│       ├── git/           # Git integration UI
│       ├── mumps/         # MUMPS Monaco renderer
│       ├── problems/      # Problems panel
│       ├── project/       # Project explorer
│       ├── routines/      # Routine management
│       ├── search/        # Search and find functionality
│       ├── tabs/          # Tab management
│       ├── terminal/      # Terminal integration
│       └── ui/            # UI utilities and components
│
├── assets/
│   └── mumps/
│       ├── mumps-lexer.js      # Tokenizer
│       ├── mumps-parser.js     # AST builder
│       ├── mumps-ast.js        # AST node definitions
│       ├── mumps-validator.js  # Semantic validator
│       └── mumps-linter.js     # Linting engine
│
├── libs/
│   └── jquery.min.js      # jQuery library
│
├── utils/
│   ├── logger.js          # Structured logging
│   └── debug-log.js       # Debug output helper
│
├── scripts/
│   └── perf-bench.js      # Performance benchmarking
│
├── node_modules/          # Dependencies
│   ├── electron/
│   ├── monaco-editor/
│   ├── xterm/
│   ├── ssh2/
│   └── node-pty/
│
└── docs/                  # Documentation (this folder)
    ├── README.md          # This file
    ├── API.md             # API reference
    ├── ARCHITECTURE.md    # Detailed architecture
    └── screenshots/       # UI screenshots
```

---

### IPC Communication

**Security Model:**
- `contextIsolation: true`
- `nodeIntegration: false`
- `sandbox: true`
- Preload script exposes safe API

**IPC Channels:**

| Channel | Handler | Purpose |
|---------|---------|---------|
| `env:get` | `getEnv()` | Get environment info |
| `lint:run` | `lint(code)` | Run linter |
| `exec:run` | `execute(code)` | Execute MUMPS code |
| `connection:set` | `setConnection(type, config)` | Configure Docker/SSH |
| `debug:start` | `debugStart(code, breakpoints, startLine)` | Start debug session |
| `debug:step` | `debugStep(sessionId, stepType)` | Step debugger |
| `debug:continue` | `debugContinue(sessionId)` | Continue execution |
| `debug:stop` | `debugStop(sessionId)` | Stop debugging |
| `debug:eval` | `debugEval(sessionId, code)` | Evaluate expression |
| `docker:list` | `listDocker()` | List Docker containers |
| `ssh:connect` | `sshConnect(config)` | Connect SSH |
| `ssh:exec` | `sshExec(sessionId, command)` | Execute SSH command |
| `ssh:disconnect` | `sshDisconnect(sessionId)` | Close SSH session |
| `routines:list` | `listRoutines(search)` | List routine files |
| `routines:search` | `searchRoutines(term, options)` | Search routines |
| `routines:read` | `readRoutine(name)` | Read routine content |
| `routines:save` | `saveRoutine(name, code)` | Save routine |
| `routines:zlink` | `zlinkRoutine(name)` | Compile & link |
| `terminal:create` | `terminalCreate(options)` | Create terminal session |
| `terminal:write` | `terminalWrite(id, data)` | Send input to terminal |
| `terminal:resize` | `terminalResize(id, cols, rows)` | Resize terminal |
| `terminal:close` | `terminalClose(id)` | Close terminal |
| `terminal:exec` | `hostExec(command)` | Execute one-shot command |
| `git:run` | `git(command)` | Run git command |
| `git:getConfig` | `getGitConfig(projectPath)` | Get git config |
| `project:create` | `createProject(config)` | Create new project |
| `project:open` | `openProject(path)` | Open existing project |
| `dialog:openFolder` | `openFolderDialog()` | Show folder picker |
| `shell:reveal` | `revealInExplorer(path)` | Show in file manager |
| `devtools:toggle` | `toggleDevTools()` | Toggle DevTools |
| `app:exit` | `exitApp()` | Quit application |

**Terminal Events (Renderer ← Main):**
- `terminal:data` - Output from terminal
- `terminal:exit` - Terminal process exited

---

### MUMPS Language Support

#### Lexer (mumps-lexer.js)

**Token Types:**
- Keywords: `SET`, `WRITE`, `DO`, `QUIT`, `IF`, `FOR`, etc.
- Operators: `+`, `-`, `*`, `/`, `=`, `<`, `>`, `&`, `!`
- Identifiers: Variable names, routine names
- Literals: Numbers, strings
- Commands: Intrinsic functions (`$PIECE`, `$GET`, etc.)

**Example:**
```mumps
SET X=42
```
Tokens:
```javascript
[
  { type: 'KEYWORD', value: 'SET' },
  { type: 'IDENTIFIER', value: 'X' },
  { type: 'OPERATOR', value: '=' },
  { type: 'NUMBER', value: '42' }
]
```

#### Parser (mumps-parser.js)

**AST Nodes:**
- `ProgramNode` - Root node
- `LineNode` - Single line of code
- `CommandNode` - MUMPS command
- `ExpressionNode` - Expression
- `FunctionNode` - Intrinsic function
- `VariableNode` - Variable reference

**Example AST:**
```javascript
{
  type: 'Program',
  lines: [
    {
      type: 'Line',
      label: 'TEST',
      commands: [
        {
          type: 'Command',
          name: 'SET',
          args: [
            { type: 'Assignment', var: 'X', value: { type: 'Number', value: 42 } }
          ]
        }
      ]
    }
  ]
}
```

#### Validator (mumps-validator.js)

**Validation Rules:**
1. Undefined label references
2. Syntax errors
3. Invalid function calls
4. Type mismatches
5. Unreachable code
6. Unused variables
7. Missing closing delimiters

**Error Format:**
```javascript
{
  line: 5,
  column: 10,
  severity: 'error',
  message: 'Undefined label: NOTFOUND',
  source: 'mumps-validator'
}
```

---

### Monaco Editor Integration

**Configuration:**
```javascript
monaco.editor.create(container, {
  value: code,
  language: 'mumps',
  theme: 'mumps-earth',
  fontSize: 13,
  fontFamily: 'ains Mono',
  minimap: { enabled: true },
  lineNumbers: 'on',
  rulers: [80, 120],
  scrollBeyondLastLine: false,
  automaticLayout: true
});
```

**Custom Language Definition:**
```javascript
monaco.languages.register({ id: 'mumps' });
monaco.languages.setMonarchTokensProvider('mumps', {
  keywords: ['SET', 'WRITE', 'DO', 'QUIT', 'IF', 'FOR', ...],
  operators: ['+', '-', '*', '/', '=', '<', '>', ...],
  symbols: /[=><!~?:&|+\-*\/\^%]+/,
  tokenizer: { ... }
});
```

**Themes:**
- `mumps-earth` - Earthy colors (default)
- `mumps-dark` - Carbon dark theme

**Breakpoint Decorations:**
```javascript
editor.deltaDecorations([], [
  {
    range: new monaco.Range(lineNum, 1, lineNum, 1),
    options: {
      isWholeLine: true,
      className: 'breakpoint-line',
      glyphMarginClassName: 'breakpoint-glyph'
    }
  }
]);
```

---

### Debugging Implementation

#### AHMDBG.m Structure

**Entry Points:**
1. `AHMDBGJSON^AHMDBG(routine, tag)` - JSON mode entry (stdin/stdout communication)

**Core Functions:**
- `STEPJSON()` - $ZSTEP handler, sends stopped event
- `READCMDJSON()` - Read debugger command from stdin
- `SETBPJSON(cmd)` - Set breakpoint via ZBREAK
- `CLEARBPJSON(cmd)` - Clear breakpoint
- `SENDVARS()` - Send local variables as JSON
- `EVJSON(cmd)` - Evaluate arbitrary M code
- `LINENUM(tag, offset, routine)` - Convert tag+offset to line number
- `ERRJSON()` - Error handler

**$ZSTEP Configuration:**
```mumps
SET $ZSTEP="ZSHOW ""V"":^%AHMDBG($J,""VARS"") SET %STP=$$STEPJSON^AHMDBG() ZSTEP:%STP=""I"" INTO ZSTEP:%STP=""O"" OVER ZSTEP:%STP=""F"" OUTOF ZCONTINUE:%STP=""C"" HALT:%STP=""H"""
```

**Execution Flow:**
```
1. User clicks Debug button
2. Renderer calls `ahmadIDE.debugStart(code, breakpoints, startLine)`
3. Main process calls `bridge.debugStart()`
4. Bridge spawns YottaDB process with ZSTEP engine
5. Bridge sends JSON commands via stdin
6. ZSTEP engine sets breakpoints using ZBREAK
7. Engine executes routine in debug mode
8. Each step triggers $ZSTEP action, sends JSON event to stdout
9. Bridge receives events, parses them
10. Updates UI via IPC to renderer
12. User clicks Step/Continue
13. Client sends command (INTO/OVER/OUTOF/CONTINUE)
14. AHMDBG executes next step
15. Repeat until QUIT or HALT
```

#### Debug Client

**Communication:**
- Uses stdin/stdout for JSON protocol
- No TCP connection required
- Direct process spawning

**Methods:**
- `start()` - Start AHMDDG process
- `step(type)` - Send step command ('into', 'over', 'out')
- `continue()` - Send CONTINUE command
- `stop()` - Send HALT command
- `setBreakpoint(file, line)` - Set breakpoint
- `clearBreakpoint(file, line)` - Clear breakpoint
- `getVariables()` - Request variables via GETVARS
- `eval(code)` - Evaluate code via EVAL;...
- `_onData(data)` - Process incoming JSON from stdout
- `_processLine(line)` - Parse JSON events

---

### Performance Optimizations

**CSS Animations Disabled:**
```css
*, *::before, *::after {
  transition: none !important;
  animation: none !important;
}
```

**Why:** Electron/Chromium can struggle with complex CSS transitions on some systems. Disabling animations improves responsiveness.

**Hardware Acceleration Disabled:**
```javascript
app.disableHardwareAcceleration();
app.commandLine.appendSwitch('disable-gpu');
app.commandLine.appendSwitch('disable-gpu-compositing');
```

**Why:** GPU issues on the development system. Software rendering is more stable.

**Large Buffer Support:**
```javascript
exec(command, { maxBuffer: 5 * 1024 * 1024 }, ...);
```

**Why:** MUMPS routines can be large; default 200KB buffer is insufficient.

**Terminal Optimization:**
- Use `node-pty` when available (true PTY)
- Fallback to `spawn` (simpler, fewer dependencies)
- Configurable via `AHMAD_IDE_ENABLE_NODE_PTY=1`

---

### Logging System

**logger.js:**
```javascript
const logger = {
  info: (event, data) => {
    const entry = {
      timestamp: new Date().toISOString(),
      level: 'INFO',
      event,
      ...data
    };
    console.log(JSON.stringify(entry));
  },
  error: (event, data) => { ... },
  warn: (event, data) => { ... }
};
```

**Log Events:**
- `IPC_REQUEST` - IPC call received
- `IPC_RESPONSE` - IPC call completed
- `IPC_RESPONSE_ERROR` - IPC call failed
- `TERMINAL_CREATE_ERROR` - Terminal creation failed
- `MAIN_UNCAUGHT_EXCEPTION` - Unhandled exception
- `MAIN_UNHANDLED_REJECTION` - Unhandled promise rejection
- `AHMDDG` - Debug server events

**Output:**
```json
{"timestamp":"2024-12-12T15:30:00.000Z","level":"INFO","event":"IPC_REQUEST","channel":"debug:start","payload":{"code":"[len:150]"}}
{"timestamp":"2024-12-12T15:30:01.000Z","level":"INFO","event":"IPC_RESPONSE","channel":"debug:start","response":{"ok":true}}
```

---

## API Reference

### Exposed APIs (preload.js)

**Environment:**
```javascript
await window.mumpsStudio.getEnv()
// Returns: { platform: 'linux', versions: {...}, cwd: '/path' }
```

**Code Execution:**
```javascript
await window.ahmadIDE.lint(code)
// Returns: { ok: true, errors: [...] }

await window.ahmadIDE.execute(code)
// Returns: { ok: true, stdout: '...', stderr: '...' }
```

**Debugging:**
```javascript
await window.ahmadIDE.debugStart(code, breakpoints, startLine)
// Returns: { ok: true, sessionId: '...' }

await window.ahmadIDE.debugStep(sessionId, stepType)
// stepType: 'into' | 'over' | 'out'
// Returns: { ok: true, stopped: true, file: '...', line: 5, vars: {...} }

await window.ahmadIDE.debugContinue(sessionId)
// Returns: { ok: true }

await window.ahmadIDE.debugStop(sessionId)
// Returns: { ok: true }

await window.ahmadIDE.debugEval(sessionId, code)
// Returns: { ok: true, output: '...', locals: {...} }
```

**Docker:**
```javascript
await window.ahmadIDE.listDocker()
// Returns: { ok: true, containers: [{ id, name, status }, ...] }
```

**SSH:**
```javascript
await window.ahmadIDE.sshConnect({ host, port, username, password, envKey })
// Returns: { ok: true, sessionId: '...' }

await window.ahmadIDE.sshExec(sessionId, command)
// Returns: { ok: true, stdout: '...', stderr: '...' }

await window.ahmadIDE.sshDisconnect(sessionId)
// Returns: { ok: true }
```

**Routines:**
```javascript
await window.ahmadIDE.listRoutines(search)
// Returns: { ok: true, routines: ['ROUTINE1', 'ROUTINE2', ...] }

await window.ahmadIDE.searchRoutines(term, options)
// Returns: { ok: true, results: [{ file, line, text }, ...] }

await window.ahmadIDE.readRoutine(name)
// Returns: { ok: true, code: '...' }

await window.ahmadIDE.saveRoutine(name, code)
// Returns: { ok: true }

await window.ahmadIDE.zlinkRoutine(name)
// Returns: { ok: true, output: '...' }
```

**Terminal:**
```javascript
await window.ahmadIDE.terminalCreate({ shell, cwd, cols, rows })
// Returns: { ok: true, id: 'term_...' }

await window.ahmadIDE.terminalWrite(id, data)
// Returns: { ok: true }

await window.ahmadIDE.terminalResize(id, cols, rows)
// Returns: { ok: true }

await window.ahmadIDE.terminalClose(id)
// Returns: { ok: true }

window.ahmadIDE.onTerminalData(({ id, data }) => {
  console.log(`Terminal ${id}: ${data}`);
});

window.ahmadIDE.onTerminalExit(({ id, code }) => {
  console.log(`Terminal ${id} exited with code ${code}`);
});
```

**Git:**
```javascript
await window.ahmadIDE.git(command)
// command: 'status', 'log', 'diff', 'commit -m "msg"', etc.
// Returns: { ok: true, stdout: '...', stderr: '...' }

await window.ahmadIDE.getGitConfig(projectPath)
// Returns: { ok: true, config: { name, email, remote } }
```

**Dialogs:**
```javascript
await window.ahmadIDE.openFolderDialog()
// Returns: { ok: true, path: '/selected/folder' } or { ok: false, canceled: true }

await window.ahmadIDE.revealInExplorer(path)
// Opens file manager at path
```

**Application:**
```javascript
await window.ahmadIDE.toggleDevTools()
// Toggles Chrome DevTools

await window.ahmadIDE.exitApp()
// Quits application
```

---

## Development Guide

### Setting Up Development Environment

1. **Install Node.js 16+**
2. **Clone project:**
   ```bash
   git clone <repository>
   cd "Ahmad IDE 2"
   ```
3. **Install dependencies:**
   ```bash
   npm install
   ```
4. **Start in dev mode:**
   ```bash
   npm start
   ```

### Project Structure Guidelines

- **main.js** - Minimal, delegates to bridge.js
- **bridge.js** - All MUMPS/Docker/SSH logic
- **renderer.js** - All UI logic, no direct Node.js access
- **preload.js** - Only expose safe APIs, never expose `require` or Node.js modules
- **assets/mumps/** - MUMPS language tooling (parser, lexer, validator)

### Adding New IPC Handlers

**1. Define handler in main.js:**
```javascript
ipcHandle('feature:action', async (_event, payload) => {
  return bridge.featureAction(payload?.param || '');
});
```

**2. Implement in bridge.js:**
```javascript
async function featureAction(param) {
  // Implementation
  return { ok: true, result: '...' };
}
module.exports = { ..., featureAction };
```

**3. Expose in preload.js:**
```javascript
contextBridge.exposeInMainWorld('mumpsStudio', {
  // ...
  featureAction: (param) => ipcRenderer.invoke('feature:action', { param })
});
```

**4. Call from renderer.js:**
```javascript
const result = await window.mumpsStudio.featureAction(param);
if (result.ok) {
  // Success
}
```

### Debugging the IDE

**Enable DevTools:**
1. Settings → Developer Tools → Toggle DevTools
2. Or uncomment in main.js: `win.webContents.openDevTools();`

**Console Logging:**
```javascript
console.log('[FEATURE]', data); // Shows in DevTools console
logger.info('FEATURE_EVENT', { data }); // Structured log in terminal
```

**Inspect IPC Calls:**
All IPC calls are logged:
```json
{"event":"IPC_REQUEST","channel":"debug:start","payload":{"code":"[len:150]"}}
{"event":"IPC_RESPONSE","channel":"debug:start","response":{"ok":true}}
```

### Performance Profiling

**Use scripts/perf-bench.js:**
```bash
node scripts/perf-bench.js
```

**Metrics:**
- Startup time
- Editor rendering
- IPC latency
- Terminal responsiveness

### Building for Production

**Package for Linux:**
```bash
npm install electron-builder --save-dev
npx electron-builder --linux
```

**Package for Windows:**
```bash
npx electron-builder --win
```

**Package for macOS:**
```bash
npx electron-builder --mac
```

---

## Troubleshooting

### Common Issues

#### 1. Terminal not working

**Symptoms:** Terminal shows blank or "Session not found" error

**Solutions:**
- Ensure shell exists: `which bash`
- Try spawn fallback: `AHMAD_IDE_ENABLE_NODE_PTY=0 npm start`
- Check permissions on shell binary

#### 2. Docker connection fails

**Symptoms:** "Container not found" or permission denied

**Solutions:**
- Check Docker daemon: `systemctl status docker`
- Verify container is running: `docker ps`
- Use `sg docker` wrapper: `AHMAD_IDE_USE_SG=1 npm start`
- Add user to docker group: `sudo usermod -aG docker $USER`

#### 3. SSH connection timeout

**Symptoms:** "Connection timeout" or "ECONNREFUSED"

**Solutions:**
- Verify SSH server is running: `ssh user@host`
- Check firewall rules
- Confirm port is correct (default 22)
- Test with `sshpass`: `sshpass -p 'password' ssh user@host`

#### 4. Debugger not stopping at breakpoints

**Symptoms:** Debug session runs without pausing

**Solutions:**
- Ensure AHMDDG.m is compiled: Check `/tmp/ahmad_dbg/AHMDDG.o`
- Check debug process is running: `ps aux | grep AHMDDG`
- Restart debug session: Stop and start again
- Check YottaDB environment variables

#### 5. Monaco editor not loading

**Symptoms:** Blank editor area or "Failed to load editor" error

**Solutions:**
- Check console for errors (F12)
- Verify Monaco files exist: `node_modules/monaco-editor/`
- Clear cache: Delete `~/.config/ahmad-ide/`
- Reinstall: `rm -rf node_modules && npm install`

#### 6. Git operations fail

**Symptoms:** "Git not found" or "Not a git repository"

**Solutions:**
- Install git: `sudo apt install git`
- Initialize repo: `git init`
- Configure user: `git config user.name "Name"`
- Check git path: `which git`

#### 7. Performance issues / lag

**Symptoms:** Slow UI, delayed keystrokes

**Solutions:**
- Animations already disabled in styles.css
- Reduce minimap size in editor settings
- Close unused tool windows
- Limit terminal scrollback buffer
- Use hardware acceleration if GPU is stable

#### 8. AHMDDG debugger fails to start

**Symptoms:** Debug session fails to start

**Solutions:**
1. Check debug process: `ps aux | grep AHMDDG`
2. Kill zombie processes: `pkill -9 -f AHMDDG`
3. Wait for cleanup: `sleep 2` then retry
4. Check YottaDB log: `/tmp/ahmad_dbg/`
5. Verify YottaDB path: `ls /opt/fis-gtm/YDB136/mumps`

#### 9. Routine save fails

**Symptoms:** "Failed to save routine" error

**Solutions:**
- Check write permissions on routines directory
- Verify connection to Docker/SSH is active
- Check disk space: `df -h`
- Ensure routine name is valid (no special chars)

#### 10. Linter shows false errors

**Symptoms:** Red squiggles on valid MUMPS code

**Solutions:**
- MUMPS parser is custom and may have limitations
- Check parser rules in `assets/mumps/mumps-parser.js`
- Report parser bugs for improvement
- Disable linter temporarily if needed

---

### Log Files

**Location:** Console output (stdout/stderr)

**Enable verbose logging:**
```bash
DEBUG=* npm start
```

**Key log patterns:**
```
[AHMDDG] - Debug server events
[IPC_REQUEST] - IPC calls from renderer
[SSH] - SSH connection events
[TERMINAL_CREATE_ERROR] - Terminal issues
```

---

### Getting Help

**Resources:**
- Check console logs (DevTools → Console)
- Review IPC logs in terminal
- Inspect network tab for remote operations
- Read MUMPS/YottaDB documentation

**Reporting Issues:**
1. Capture error message
2. Note reproduction steps
3. Include environment info (`env:get`)
4. Check console logs
5. Test with minimal case

---

## Screenshots Reference

Below is a complete list of all screenshots that should be captured and placed in `docs/screenshots/`:

| File | Description | How to Capture |
|------|-------------|----------------|
| `01-main-window.png` | Full IDE window showing all panels | Default view after startup |
| `02-menu-bar.png` | Top menu bar with branding | Crop top 50px of window |
| `03-toolbar.png` | Toolbar with Save/Run/Debug buttons | Crop toolbar area |
| `04-project-panel.png` | Left project explorer with routines | Show with routines loaded |
| `05-editor-main.png` | Monaco editor with MUMPS code | Open a .m file |
| `06-terminal-panel.png` | Terminal with multi-tab view | Show terminal with 2-3 tabs |
| `07-debug-panel.png` | Debug panel with all tabs | Open debug panel |
| `08-debug-toolbar.png` | Floating debug control bar | Start debug session |
| `09-git-panel.png` | Git tool window full view | Open Git panel from VCS |
| `10-connections-panel.png` | Connections dialog | Click "Connections" in status bar |
| `11-services-panel.png` | Services panel | Click Services button (Alt+8) |
| `12-settings-panel.png` | Settings dialog | Menu → Settings |
| `13-shortcuts-panel.png` | Keyboard shortcuts dialog | Menu → Keyboard Shortcuts |
| `14-find-dialog.png` | Find/Replace in files | Ctrl+Shift+F |
| `15-search-everywhere.png` | Search everywhere dialog | Press Shift twice |
| `16-run-execution.png` | Code execution with output | Click Run, show terminal output |
| `17-linting-results.png` | Problems panel with errors | Run lint on code with errors |
| `18-debug-session.png` | Active debug session paused at breakpoint | Debug with breakpoint hit |
| `19-debug-variables.png` | Variables tab showing locals | Debug paused, show Variables tab |
| `20-debug-callstack.png` | Call stack tab | Debug paused, show Stack tab |
| `21-debug-console.png` | Debug console with eval | Execute EVAL in console |
| `22-git-workflow.png` | Git status and changes | Run git status |
| `23-git-commit.png` | Commit dialog with message | Enter commit message |
| `24-git-branches.png` | Branch dropdown open | Click branch selector |
| `25-git-diff.png` | Side-by-side file diff | Show file diff view |
| `26-docker-connection.png` | Docker container list | Open connections, show Docker |
| `27-ssh-connection.png` | SSH connection form | Open connections, show SSH |
| `28-routine-list.png` | Project tree with routines | Load routines from Docker/SSH |
| `29-routine-search.png` | Search results in routines | Use Find in Files |
| `30-terminal.png` | Terminal with command output | Run commands in terminal |

**Screenshot Guidelines:**
- Use 1920x1080 resolution or similar
- Capture in PNG format
- Use descriptive sample data (avoid real passwords/IPs)
- Show realistic MUMPS code examples
- Include both light and dark states where applicable
- Annotate key UI elements if helpful

---

## Appendix

### Keyboard Shortcuts Reference

| Shortcut | Command | Description |
|----------|---------|-------------|
| `Ctrl+S` | Save | Save current file |
| `Ctrl+Enter` | Run | Execute current routine |
| `Ctrl+F` | Find | Find in current file |
| `Ctrl+Shift+F` | Find in Files | Global search |
| `Ctrl+D` | Duplicate Line | Duplicate current line |
| `Alt+1` | Project Panel | Toggle project explorer |
| `Alt+F12` | Terminal | Toggle terminal |
| `Ctrl+Shift+T` | New Terminal | Create new terminal tab |
| `F9` | Toggle Breakpoint | Set/clear breakpoint |
| `Shift+F9` | Debug | Start debugging |
| `F7` | Step Into | Step into function |
| `F8` | Step Over | Step over line |
| `Shift+F8` | Step Out | Step out of function |
| `Ctrl+F2` | Stop Debug | Stop debugger |
| `Shift+Shift` | Search Everywhere | Global file search |
| `Ctrl+/` | Comment | Toggle line comment |

### Color Palette ( Brown/Earth Theme)

```css
--bg: #2b2b2b              /* Main background */
--panel: #3c3f41           /* Panel background */
--panel-2: #313335         /* Secondary panel */
--editor-bg: #2b2b2b       /* Editor background */
--text: #bbbbbb            /* Default text */
--text-bright: #ffffff     /* Bright text */
--muted: #808080           /* Muted text */
--accent: #4a9eff          /* Blue accent */
--accent-green: #6a8759    /* Green (strings) */
--accent-orange: #cc7832   /* Orange (keywords) */
--border: #323232          /* Borders */
--tree-selected: #214283   /* Selected item */
--hover-bg: #2d2f30        /* Hover background */
```

### MUMPS Syntax Highlighting

**Keywords:** `SET`, `WRITE`, `DO`, `QUIT`, `IF`, `FOR`, `WHILE`, `KILL`, `NEW`, `MERGE`, `LOCK`, `GOTO`, `HALT`, `HANG`, `READ`, `CLOSE`, `OPEN`, `USE`, `JOB`, `XECUTE`, `TSTART`, `TCOMMIT`, `TROLLBACK`, `ZBREAK`, `ZSTEP`, `ZCONTINUE`, `ZLINK`, `ZSHOW`, `ZSYSTEM`, `ZWRITE`

**Intrinsic Functions:** `$PIECE`, `$GET`, `$DATA`, `$ORDER`, `$QUERY`, `$LENGTH`, `$EXTRACT`, `$FIND`, `$TEXT`, `$STACK`, `$ZSTATUS`, `$ZPOSITION`, `$JOB`, `$HOROLOG`, `$RANDOM`, `$SELECT`, `$JUSTIFY`, `$TRANSLATE`, `$CHAR`, `$ASCII`

**Special Variables:** `$IO`, `$PRINCIPAL`, `$TEST`, `$X`, `$Y`, `$ZTRAP`, `$ETRAP`, `$ZSTEP`, `$ZCMDLINE`, `$ZEOF`, `$ZINTERRUPT`

### Environment Paths

**Docker (default):**
```
Container ID: 8c21cf79fb67
YottaDB: /opt/fis-gtm/YDB136
Env Key: prod
Base: /var/worldvista/prod
Globals: /var/worldvista/prod/globals/mumps.gld
Routines: /var/worldvista/prod/localr
```

**SSH (example):**
```
Host: 10.0.0.5
Port: 22
Username: ahmad-cc
Env Key: cc (derived from username)
YottaDB: /opt/fis-gtm/YDB136
Base: /var/worldvista/prod/cc
Globals: /var/worldvista/prod/cc/globals/mumps.gld
Routines: /var/worldvista/prod/cc/localr
```

### License

This project is licensed under **ISC License** (see package.json).

### Credits

**Built with:**
- Electron - Desktop app framework
- Monaco Editor - VS Code editor engine
- xterm.js - Terminal emulator
- ssh2 - SSH client library
- YottaDB - MUMPS database engine

**Inspired by:**
- ains  - UI/UX design
- VS Code - Editor experience
- mumps-debug (vscode extension) - Debugging protocol

---

**End of Documentation**

For technical details, see [API.md](API.md) and [ARCHITECTURE.md](ARCHITECTURE.md).

For screenshots, see [screenshots/](screenshots/) directory.

**Version:** 1.0.0
**Last Updated:** December 12, 2024
**Author:** Ahmad
