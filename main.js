// main.js
const { app, BrowserWindow, ipcMain, dialog } = require('electron');
const path = require('path');
const { exec, spawn } = require('child_process');
const fs = require('fs');
const { logger } = require('./utils/logger');
const bridge = require('./bridge');

// Suppress harmless Electron warnings
process.env.ELECTRON_DISABLE_SECURITY_WARNINGS = 'true';
app.commandLine.appendSwitch('disable-gpu-sandbox');

let nodePty = null;
const enableNodePty = process.env.AHMAD_IDE_ENABLE_NODE_PTY === '1';

// Terminal sessions (simple persistent shell per tab)
const terminalSessions = new Map();

// GPU has issues on some systems; in snaps it also depends on interfaces.
// Default to software rendering in snaps for reliability unless explicitly enabled.
const isSnap = !!process.env.SNAP;
const forceSoftwareRendering = process.env.AHMAD_IDE_FORCE_SOFT_RENDER === '1';
const enableGpu = process.env.AHMAD_IDE_ENABLE_GPU === '1';
if (forceSoftwareRendering || (isSnap && !enableGpu)) {
    app.disableHardwareAcceleration();
    app.commandLine.appendSwitch('disable-gpu');
    app.commandLine.appendSwitch('disable-gpu-compositing');
    app.commandLine.appendSwitch('disable-gpu-sandbox');
}

// Handle uncaught errors
process.on('uncaughtException', (err) => {
    logger.error('MAIN_UNCAUGHT_EXCEPTION', {
        message: err?.message,
        stack: err?.stack
    });
});

process.on('unhandledRejection', (reason) => {
    logger.error('MAIN_UNHANDLED_REJECTION', {
        reason: reason?.stack || reason
    });
});

// Try to load node-pty (optional)
let nodePtyLoadError = null;
if (enableNodePty) {
    try {
        nodePty = require('node-pty');
    } catch (e) {
        nodePtyLoadError = e;
        nodePty = null;
        console.warn('node-pty unavailable, falling back to spawn():', e?.message || e);
    }
} else {
    console.warn('node-pty disabled by default (set AHMAD_IDE_ENABLE_NODE_PTY=1 to attempt loading). Using spawn() fallback.');
}

function createWindow() {
    const win = new BrowserWindow({
        width: 1400,
        height: 900,
        show: false,  // Restore v1.3 behavior
        autoHideMenuBar: true,
        backgroundColor: '#1b120e',
        webPreferences: {
            preload: path.join(__dirname, 'preload.js'),
            contextIsolation: true,
            nodeIntegration: false,
            sandbox: false  // Disable Electron sandbox in snap (snap provides confinement)
        }
    });

    const indexHtmlPath = path.join(__dirname, 'index.html');
    win.loadFile(indexHtmlPath).catch((err) => {
        const message = err?.message || String(err);
        logger.error('UI_LOAD_FAILED', { message, stack: err?.stack, indexHtmlPath });

        const safePath = indexHtmlPath.replace(/</g, '&lt;').replace(/>/g, '&gt;');
        const safeMessage = message.replace(/</g, '&lt;').replace(/>/g, '&gt;');
        const html = `<!doctype html>
<meta charset="utf-8" />
<title>Mumps Studio - Startup Error</title>
<body style="font-family: sans-serif; padding: 16px;">
  <h2>Mumps Studio failed to start</h2>
  <p>Could not load the UI file:</p>
  <pre>${safePath}</pre>
  <p>Error:</p>
  <pre>${safeMessage}</pre>
</body>`;

        win.loadURL(`data:text/html;charset=utf-8,${encodeURIComponent(html)}`).catch(() => {});
        win.show();
    });
    win.once('ready-to-show', () => {
        win.maximize();
        win.show();
    });

    // DevTools can be toggled from the UI
    // win.webContents.openDevTools();
}

// Simple exec helper
function runCommand(cmd) {
    return new Promise((resolve) => {
        // In snap environment, explicitly set PATH to include system binaries
        const env = {
            ...process.env,
            PATH: '/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin' + (process.env.PATH ? ':' + process.env.PATH : '')
        };
        exec(cmd, { timeout: 8000, env }, (err, stdout, stderr) => {
            if (err) {
                resolve({ ok: false, error: err.message, stderr, stdout });
            } else {
                resolve({ ok: true, stdout, stderr });
            }
        });
    });
}

// Run docker commands directly (no sg wrapper needed in snap)
function wrapDockerCmd(cmd) {
    // In snap, docker is bundled at $SNAP/usr/bin/docker
    const dockerPath = process.env.SNAP ? `${process.env.SNAP}/usr/bin/docker` : 'docker';
    return cmd.replace(/^docker\s/, `${dockerPath} `);
}

const summarizePayload = (payload) => {
    if (!payload || typeof payload !== 'object') return payload;
    const copy = { ...payload };
    if (copy.code && typeof copy.code === 'string') {
        copy.code = `[len:${copy.code.length}]`;
    }
    if (copy.command && typeof copy.command === 'string' && copy.command.length > 180) {
        copy.command = `${copy.command.slice(0, 180)}...`;
    }
    return copy;
};

const summarizeResponse = (res) => {
    if (res === undefined) return 'undefined';
    if (res === null) return 'null';
    if (typeof res !== 'object') return res;
    const base = { ok: res.ok };
    if (res.error) base.error = res.error;
    if (res.stderr) base.stderr = res.stderr?.slice(0, 120);
    if (res.message) base.message = res.message;
    if (res.output) base.output = res.output?.slice?.(0, 120);
    return base;
};

const ipcHandle = (channel, handler) => {
    ipcMain.handle(channel, async (event, payload) => {
        logger.info('IPC_REQUEST', {
            channel,
            payload: summarizePayload(payload)
        });
        try {
            const res = await handler(event, payload);
            logger.info('IPC_RESPONSE', {
                channel,
                response: summarizeResponse(res)
            });
            return res;
        } catch (err) {
            logger.error('IPC_RESPONSE_ERROR', {
                channel,
                message: err?.message,
                stack: err?.stack
            });
            throw err;
        }
    });
};

// ---------------- IPC HANDLERS ----------------

// Env info
ipcHandle('env:get', () => {
    return {
        platform: process.platform,
        versions: process.versions,
        cwd: process.cwd()
    };
});

// Lint / Exec (Tools)
ipcHandle('lint:run', async (_event, payload) => {
    return bridge.lint(payload?.code || '');
});

ipcHandle('exec:run', async (_event, payload) => {
    return bridge.execute(payload?.code || '');
});

// Connection (docker / ssh)
ipcHandle('connection:set', async (_event, payload) => {
    return bridge.setConnection(payload?.type, payload?.config);
});

// JSON/ZSTEP DEBUG (AHMDBG) – inline code debugger
// Strict mapping to ZSTEP engine functions in bridge
ipcHandle('debug:start', async (_event, payload) => {
    return bridge.debugStart(
        payload?.code || '',
        payload?.breakpoints || [],
        payload?.startLine || null
    );
});

ipcHandle('debug:step', async (_event, payload) => {
    return bridge.debugStep(
        payload?.sessionId || '',
        payload?.stepType || 'into'
    );
});

ipcHandle('debug:continue', async (_event, payload) => {
    return bridge.debugContinue(payload?.sessionId || '');
});

ipcHandle('debug:stop', async (_event, payload) => {
    return bridge.debugStop(payload?.sessionId || '');
});

ipcHandle('debug:eval', async (_event, payload) => {
    return bridge.debugEval(payload?.sessionId || '', payload?.code || '');
});

// ---------------------------------------------------------
// LEGACY MDEBUG TCP HANDLERS (Isolated / Optional)
// These are not exposed by preload.js anymore but kept if needed for reference
// ---------------------------------------------------------
ipcHandle('debug:start:mdebug', async (_event, payload) => {
    return bridge.debugStartMdebug(
        payload?.routine || payload?.file || '',
        payload?.breakpoints || [],
        { stopOnEntry: !!payload?.stopOnEntry }
    );
});

ipcHandle('debug:step:mdebug', async (_event, payload) => {
    return bridge.debugStepMdebug(
        payload?.sessionId || '',
        payload?.stepType || 'over'
    );
});

ipcHandle('debug:continue:mdebug', async (_event, payload) => {
    return bridge.debugContinueMdebug(payload?.sessionId || '');
});

ipcHandle('debug:stop:mdebug', async (_event, payload) => {
    return bridge.debugStopMdebug(payload?.sessionId || '');
});
// ---------------------------------------------------------

// Docker list
ipcHandle('docker:list', async () => {
    const dockerCmd = wrapDockerCmd('docker ps --format "{{.ID}}|{{.Names}}|{{.Status}}"');
    console.log('[DEBUG] Docker command:', dockerCmd);

    const res = await runCommand(dockerCmd);

    console.log('[DEBUG] Docker result:', JSON.stringify({
        ok: res.ok,
        error: res.error,
        stderr: res.stderr,
        stdout: res.stdout?.substring(0, 100)
    }));

    // Check for permission errors
    if (!res.ok) {
        const errorMsg = (res.error || res.stderr || '').toLowerCase();
        if (errorMsg.includes('permission denied') || errorMsg.includes('connect') || errorMsg.includes('socket')) {
            return {
                ok: false,
                permissionError: true,
                error: 'Docker permission denied',
                message: 'Docker permission denied. Quick fix:\n\n' +
                        '1. Add yourself to docker group:\n' +
                        '   sudo usermod -aG docker $USER\n' +
                        '   newgrp docker\n\n' +
                        '2. Restart Mumps Studio\n\n' +
                        'See DOCKER-SETUP.md for detailed instructions.',
                details: res.stderr || res.error
            };
        }
        if (errorMsg.includes('not found') || errorMsg.includes('command not found')) {
            return {
                ok: false,
                error: 'Docker not installed',
                message: 'DEBUG INFO:\n' +
                        'Command: ' + dockerCmd + '\n' +
                        'Error: ' + res.error + '\n' +
                        'Stderr: ' + res.stderr + '\n' +
                        'Stdout: ' + res.stdout + '\n\n' +
                        'PATH: ' + process.env.PATH,
                details: res.stderr || res.error
            };
        }
        return res;
    }

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

// SSH
ipcHandle('ssh:connect', async (_event, payload) => {
    console.log('[DEBUG] SSH connect payload:', JSON.stringify({
        host: payload?.host,
        port: payload?.port,
        username: payload?.username,
        hasPassword: !!payload?.password
    }));

    const res = await bridge.sshConnect(payload || {});

    console.log('[DEBUG] SSH result:', JSON.stringify({
        ok: res.ok,
        error: res.error,
        sessionId: res.sessionId
    }));

    // Check for SSH permission errors
    if (!res.ok && res.error) {
        const errorMsg = res.error.toLowerCase();
        if (errorMsg.includes('permission denied') || errorMsg.includes('eacces')) {
            return {
                ok: false,
                permissionError: true,
                error: 'SSH permission denied',
                message: 'SSH access requires permissions. Please run:\n\n' +
                        '1. Connect snap interface:\n' +
                        '   sudo snap connect mumps-studio:ssh-keys\n\n' +
                        '2. Check SSH key permissions:\n' +
                        '   chmod 600 ~/.ssh/id_rsa\n' +
                        '   chmod 644 ~/.ssh/id_rsa.pub\n\n' +
                        '3. Restart the IDE',
                details: res.error
            };
        }
    }

    return res;
});

ipcHandle('ssh:exec', async (_event, payload) => {
    return bridge.sshExec(payload?.sessionId || '', payload?.command || '');
});

ipcHandle('ssh:disconnect', async (_event, payload) => {
    return bridge.sshDisconnect(payload?.sessionId || '');
});

// Environment operations
ipcHandle('env:createDirectory', async (_event, payload) => {
    return bridge.createDirectoryInCurrentEnv(payload?.dirPath || '');
});

// Routines
ipcHandle('routines:list', async (_event, payload) => {
    return bridge.listRoutines(payload?.search || '');
});

ipcHandle('routines:search', async (_event, payload) => {
    return bridge.searchRoutines(payload?.term || '', payload?.options || {});
});

ipcHandle('routines:read', async (_event, payload) => {
    return bridge.readRoutine(payload?.name || '');
});

ipcHandle('routines:save', async (_event, payload) => {
    return bridge.saveRoutine(payload?.name || '', payload?.code || '');
});

ipcHandle('routines:zlink', async (_event, payload) => {
    return bridge.zlinkRoutine(payload?.name || '');
});

// One-shot host command via terminal
ipcHandle('terminal:exec', async (_event, payload) => {
    return bridge.hostExec(payload?.command || '');
});

// Git
ipcHandle('git:run', async (_event, payload) => {
    return bridge.git(payload?.command || '');
});

ipcHandle('git:detectRepo', async (_event, payload) => {
    return bridge.detectGitRepo(payload?.projectRoot || '', payload?.opts || {});
});

ipcHandle('git:getConfig', async (_event, payload) => {
    return bridge.getGitConfig(payload?.projectPath || '');
});

// Project create/open
ipcHandle('project:create', async (_event, payload) => {
    return bridge.createProject(payload || {});
});

ipcHandle('project:open', async (_event, payload) => {
    return bridge.openProject(payload?.path || '');
});

// Dialogs
ipcHandle('dialog:openFolder', async () => {
    const result = await dialog.showOpenDialog({
        properties: ['openDirectory']
    });
    if (result.canceled) {
        return { ok: false, canceled: true };
    }
    return { ok: true, path: result.filePaths[0] };
});

// DevTools toggle
ipcHandle('devtools:toggle', async (event) => {
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

// Reveal file/folder in system file explorer
ipcHandle('shell:reveal', async (event, { path: targetPath }) => {
    const { shell } = require('electron');
    try {
        if (targetPath) {
            shell.showItemInFolder(targetPath);
        }
        return { ok: true };
    } catch (err) {
        return { ok: false, error: err.message };
    }
});

// App exit
ipcHandle('app:exit', async () => {
    app.quit();
    return { ok: true };
});

// ---------------- TERMINAL (PTY or spawn) ----------------

function resolveShell(shellOverride) {
    if (shellOverride && typeof shellOverride === 'string' && shellOverride.trim().length) {
        return shellOverride.trim();
    }
    if (process.platform === 'win32') {
        return process.env.COMSPEC || 'powershell.exe';
    }
    return process.env.SHELL || '/bin/bash';
}

function resolveCwd(dirOverride) {
    const candidate = dirOverride && typeof dirOverride === 'string'
        ? dirOverride
        : process.cwd();
    try {
        if (fs.existsSync(candidate) && fs.statSync(candidate).isDirectory()) {
            return candidate;
        }
    } catch (_) {
        // ignore and fall through
    }
    return process.cwd();
}

function createTerminalSession(sender, options = {}) {
    const shell = resolveShell(options.shell);
    if (options.shell && (options.shell.includes(path.sep)) && !fs.existsSync(shell)) {
        throw new Error(`Shell not found at ${shell}`);
    }
    const cwd = resolveCwd(options.cwd);
    const parsedCols = parseInt(options.cols, 10);
    const parsedRows = parseInt(options.rows, 10);
    const cols = Number.isFinite(parsedCols) && parsedCols > 0 ? parsedCols : 80;
    const rows = Number.isFinite(parsedRows) && parsedRows > 0 ? parsedRows : 24;
    const id = `term_${Date.now()}_${Math.random().toString(16).slice(2)}`;

    if (nodePty) {
        const ptyProc = nodePty.spawn(shell, [], {
            name: 'xterm-color',
            cols,
            rows,
            cwd,
            env: process.env
        });
        ptyProc.onData((data) =>
            sender.send('terminal:data', { id, data })
        );
        ptyProc.onExit((evt) => {
            sender.send('terminal:exit', { id, code: evt.exitCode });
            terminalSessions.delete(id);
        });
        terminalSessions.set(id, ptyProc);
    } else {
        const child = spawn(shell, [], {
            env: process.env,
            cwd,
            stdio: 'pipe'
        });
        child.stdout.on('data', (data) =>
            sender.send('terminal:data', { id, data: data.toString() })
        );
        child.stderr.on('data', (data) =>
            sender.send('terminal:data', { id, data: data.toString() })
        );
        child.on('close', (code) => {
            sender.send('terminal:exit', { id, code });
            terminalSessions.delete(id);
        });
        terminalSessions.set(id, child);
    }

    return id;
}

ipcHandle('terminal:create', async (event, payload) => {
    try {
        const id = createTerminalSession(event.sender, payload || {});
        return { ok: true, id };
    } catch (e) {
        logger.error('TERMINAL_CREATE_ERROR', {
            message: e?.message,
            stack: e?.stack
        });
        return { ok: false, error: e.message };
    }
});

ipcHandle('terminal:write', async (_event, payload) => {
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

ipcHandle('terminal:resize', async (_event, payload) => {
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

ipcHandle('terminal:close', async (_event, payload) => {
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

// ---------------- APP LIFECYCLE ----------------

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
