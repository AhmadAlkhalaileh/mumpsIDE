const { app, BrowserWindow, ipcMain, dialog } = require('electron');
const path = require('path');
const { exec, spawn } = require('child_process');
const fs = require('fs');
const bridge = require('./bridge');
let nodePty = null;
try {
    // Optional: node-pty for true PTY behavior
    nodePty = require('node-pty');
} catch (e) {
    nodePty = null;
}

// Terminal sessions (simple persistent shell per tab)
const terminalSessions = new Map();

// Reduce GPU-related errors on some Linux environments
app.disableHardwareAcceleration();

function createWindow() {
    const win = new BrowserWindow({
        width: 1400,
        height: 900,
        show: false,
        autoHideMenuBar: true,
        backgroundColor: '#1b120e',
        webPreferences: {
            preload: path.join(__dirname, 'preload.js'),
            contextIsolation: true,
            nodeIntegration: false,
            sandbox: true
        }
    });

    win.loadFile('index.html');
    win.once('ready-to-show', () => {
        win.maximize();
        win.show();
    });

    // DevTools disabled by default - can be toggled from Settings
    // win.webContents.openDevTools();
}

// Simple exec helper
function runCommand(cmd) {
    return new Promise((resolve) => {
        exec(cmd, { timeout: 8000 }, (err, stdout, stderr) => {
            if (err) {
                resolve({ ok: false, error: err.message, stderr, stdout });
            } else {
                resolve({ ok: true, stdout, stderr });
            }
        });
    });
}

// Wrap docker commands with sg docker
function wrapDockerCmd(cmd) {
    return `sg docker -c "${cmd.replace(/"/g, '\\"')}"`;
}

// IPC handlers
ipcMain.handle('env:get', () => {
    return {
        platform: process.platform,
        versions: process.versions,
    cwd: process.cwd()
  };
});

ipcMain.handle('lint:run', async (_event, payload) => {
    return bridge.lint(payload?.code || '');
});

ipcMain.handle('lint:analyze', async (_event, payload) => {
  return bridge.lintCode(payload?.code || '');
});

ipcMain.handle('parse:code', async (_event, payload) => {
  return bridge.parseCode(payload?.code || '');
});

ipcMain.handle('parse:labels', async (_event, payload) => {
  return bridge.getLabels(payload?.code || '');
});

ipcMain.handle('exec:run', async (_event, payload) => {
  return bridge.execute(payload?.code || '');
});

ipcMain.handle('connection:set', async (_event, payload) => {
  return bridge.setConnection(payload?.type, payload?.config);
});

ipcMain.handle('debug:start', async (_event, payload) => {
  return bridge.debugStart(payload?.code || '', payload?.breakpoints || [], payload?.startLine || null);
});

ipcMain.handle('debug:step', async (_event, payload) => {
  return bridge.debugStep(payload?.sessionId || '', payload?.stepType || 'into');
});

ipcMain.handle('debug:continue', async (_event, payload) => {
  return bridge.debugContinue(payload?.sessionId || '');
});

ipcMain.handle('debug:stop', async (_event, payload) => {
  return bridge.debugStop(payload?.sessionId || '');
});

ipcMain.handle('docker:list', async () => {
  const res = await runCommand(wrapDockerCmd('docker ps --format "{{.ID}}|{{.Names}}|{{.Status}}"'));
  if (!res.ok) return res;
  const containers = res.stdout
        .trim()
        .split('\n')
        .filter(Boolean)
        .map(line => {
            const [id, name, status] = line.split('|');
            return { id, name, status };
        });
  return { ok: true, containers };
});

ipcMain.handle('ssh:connect', async (_event, payload) => {
  return bridge.sshConnect(payload || {});
});

ipcMain.handle('ssh:exec', async (_event, payload) => {
  return bridge.sshExec(payload?.sessionId || '', payload?.command || '');
});

ipcMain.handle('ssh:disconnect', async (_event, payload) => {
  return bridge.sshDisconnect(payload?.sessionId || '');
});

ipcMain.handle('routines:list', async (_event, payload) => {
  return bridge.listRoutines(payload?.search || '');
});

ipcMain.handle('routines:read', async (_event, payload) => {
  return bridge.readRoutine(payload?.name || '');
});

ipcMain.handle('routines:save', async (_event, payload) => {
  return bridge.saveRoutine(payload?.name || '', payload?.code || '');
});

ipcMain.handle('routines:zlink', async (_event, payload) => {
  return bridge.zlinkRoutine(payload?.name || '');
});

ipcMain.handle('terminal:exec', async (_event, payload) => {
  return bridge.hostExec(payload?.command || '');
});

ipcMain.handle('git:run', async (_event, payload) => {
    return bridge.git(payload?.command || '');
});

ipcMain.handle('git:getConfig', async (_event, payload) => {
    return bridge.getGitConfig(payload?.projectPath || '');
});

ipcMain.handle('project:create', async (_event, payload) => {
    return bridge.createProject(payload || {});
});

ipcMain.handle('project:open', async (_event, payload) => {
    return bridge.openProject(payload?.projectPath || '');
});

ipcMain.handle('dialog:openFolder', async () => {
    const result = await dialog.showOpenDialog({
        properties: ['openDirectory']
    });
    if (result.canceled) {
        return { ok: false, canceled: true };
    }
    return { ok: true, path: result.filePaths[0] };
});

ipcMain.handle('devtools:toggle', async (event) => {
    const win = BrowserWindow.fromWebContents(event.sender);
    if (win) {
        if (win.webContents.isDevToolsOpened()) {
            win.webContents.closeDevTools();
        } else {
            win.webContents.openDevTools();
        }
    }
    return { ok: true };
});

// Reveal file/folder in native file explorer
ipcMain.handle('shell:reveal', async (event, { path }) => {
    const { shell } = require('electron');
    try {
        if (path) {
            shell.showItemInFolder(path);
        }
        return { ok: true };
    } catch (err) {
        return { ok: false, error: err.message };
    }
});

function createTerminalSession(sender) {
    const shell = process.env.SHELL || (process.platform === 'win32' ? 'powershell.exe' : '/bin/bash');
    const id = `term_${Date.now()}_${Math.random().toString(16).slice(2)}`;

    if (nodePty) {
        const ptyProc = nodePty.spawn(shell, [], {
            name: 'xterm-color',
            cols: 80,
            rows: 24,
            cwd: process.cwd(),
            env: process.env
        });
        ptyProc.onData((data) => sender.send('terminal:data', { id, data }));
        ptyProc.onExit((evt) => {
            sender.send('terminal:exit', { id, code: evt.exitCode });
            terminalSessions.delete(id);
        });
        terminalSessions.set(id, ptyProc);
    } else {
        const child = spawn(shell, [], {
            env: process.env,
            cwd: process.cwd(),
            stdio: 'pipe'
        });
        child.stdout.on('data', (data) => sender.send('terminal:data', { id, data: data.toString() }));
        child.stderr.on('data', (data) => sender.send('terminal:data', { id, data: data.toString() }));
        child.on('close', (code) => {
            sender.send('terminal:exit', { id, code });
            terminalSessions.delete(id);
        });
        terminalSessions.set(id, child);
    }
    return id;
}

ipcMain.handle('terminal:create', async (event) => {
    try {
        const id = createTerminalSession(event.sender);
        return { ok: true, id };
    } catch (e) {
        return { ok: false, error: e.message };
    }
});

ipcMain.handle('terminal:write', async (_event, payload) => {
    const id = payload?.id;
    const data = payload?.data || '';
    if (!id || !terminalSessions.has(id)) {
        return { ok: false, error: 'Session not found' };
    }
    try {
        const term = terminalSessions.get(id);
        if (term.write) {
            term.write(data);
        } else if (term.stdin) {
            term.stdin.write(data);
        }
        return { ok: true };
    } catch (e) {
        return { ok: false, error: e.message };
    }
});

ipcMain.handle('terminal:resize', async (_event, payload) => {
    const id = payload?.id;
    if (!id || !terminalSessions.has(id)) {
        return { ok: false, error: 'Session not found' };
    }
    const cols = payload?.cols || 80;
    const rows = payload?.rows || 24;
    try {
        const term = terminalSessions.get(id);
        if (term.resize) term.resize(cols, rows);
        return { ok: true };
    } catch (e) {
        return { ok: false, error: e.message };
    }
});

ipcMain.handle('terminal:close', async (_event, payload) => {
    const id = payload?.id;
    if (!id || !terminalSessions.has(id)) {
        return { ok: false, error: 'Session not found' };
    }
    try {
        const term = terminalSessions.get(id);
        if (term.kill) term.kill();
        terminalSessions.delete(id);
        return { ok: true };
    } catch (e) {
        return { ok: false, error: e.message };
    }
});

app.whenReady().then(() => {
    createWindow();

    app.on('activate', () => {
        if (BrowserWindow.getAllWindows().length === 0) {
            createWindow();
        }
    });
});

app.on('window-all-closed', () => {
    if (process.platform !== 'darwin') {
        app.quit();
    }
});
