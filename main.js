// main.js
const path = require('path');
const { exec, spawn } = require('child_process');
const fs = require('fs');
const { logger } = require('./utils/logger');
const bridge = require('./bridge');
const patchTrackerService = require('./src/services/patchTracking/patchTrackerService');
const { createWorkspaceFileService } = require('./src/services/workspaceFileService');

const workspaceFileService = createWorkspaceFileService({ logger });

// Suppress harmless Electron warnings
process.env.ELECTRON_DISABLE_SECURITY_WARNINGS = 'true';

let nodePty = null;
// PhpStorm-like Terminal requires a real PTY by default.
// Allow opting out for environments where native modules are unavailable.
const enableNodePty = process.env.AHMAD_IDE_DISABLE_NODE_PTY !== '1';

// Terminal sessions (simple persistent shell per tab)
const terminalSessions = new Map();

// GPU has issues on some systems; in snaps it also depends on interfaces.
// Default to software rendering in snaps for reliability unless explicitly enabled.
const isSnap = !!process.env.SNAP;
const forceSoftwareRendering = process.env.AHMAD_IDE_FORCE_SOFT_RENDER === '1';
const enableGpu = process.env.AHMAD_IDE_ENABLE_GPU === '1';

// Declare electron variables
let app, BrowserWindow, ipcMain, dialog;

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
    console.warn('node-pty disabled via AHMAD_IDE_DISABLE_NODE_PTY=1. Using spawn() fallback.');
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
    const env = (() => {
        const base = {
            ...process.env,
            ...(options.env && typeof options.env === 'object' ? options.env : {})
        };
        // Improve Linux terminal compatibility (colors/vim) by default.
        if (!base.TERM) base.TERM = 'xterm-256color';
        if (!base.COLORTERM) base.COLORTERM = 'truecolor';
        // Ensure system binaries are discoverable (snap PATH can be minimal).
        base.PATH = '/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin' + (base.PATH ? ':' + base.PATH : '');
        return base;
    })();

    if (nodePty) {
        const ptyProc = nodePty.spawn(shell, [], {
            name: env.TERM || 'xterm-256color',
            cols,
            rows,
            cwd,
            env
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
            env,
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


// ---------------- APP LIFECYCLE ----------------

function initializeElectron() {
    console.log('[INIT] Initializing Electron...');
    try {
        const electronModule = require('electron');
        if (typeof electronModule === 'string') {
            console.error('[INIT] ERROR: require("electron") returned a string (path). The Electron module system is not properly initialized.');
            console.error('[INIT] This may indicate a critical issue with the Electron environment.');
            throw new Error('Electron module not properly initialized');
        }
        app = electronModule.app;
        BrowserWindow = electronModule.BrowserWindow;
        ipcMain = electronModule.ipcMain;
        dialog = electronModule.dialog;
        if (!app || !BrowserWindow || !ipcMain) {
            console.error('[INIT] ERROR: Failed to get Electron modules from require("electron")');
            throw new Error('Failed to initialize Electron modules');
        }
        console.log('[INIT] Electron modules loaded successfully');
        return true;
    } catch (err) {
        console.error('[INIT] FATAL ERROR:', err.message);
        process.exit(1);
    }
}

// Initialize Electron and start app
if (!initializeElectron()) {
    process.exit(1);
}

app.whenReady().then(() => {
    // Apply GPU settings after app is ready
    app.commandLine.appendSwitch('disable-gpu-sandbox');
    if (forceSoftwareRendering || (isSnap && !enableGpu)) {
        app.disableHardwareAcceleration();
        app.commandLine.appendSwitch('disable-gpu');
        app.commandLine.appendSwitch('disable-gpu-compositing');
        app.commandLine.appendSwitch('disable-gpu-sandbox');
    }

    // Register IPC handlers
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

    ipcHandle('connection:get', async (_event, payload) => {
        return bridge.getConnection();
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

    // Patch Tracking
    ipcHandle('patchTracking:uploadPatch', async (_event, payload) => {
        try {
            const { content } = payload || {};
            if (!content) {
                throw new Error('No KIDS content provided');
            }

            // Initialize if needed
            if (!patchTrackerService.initialized) {
                await patchTrackerService.init();
            }

            // Parse KIDS content directly (since we receive content from renderer)
            const kidsParser = require('./src/services/patchTracking/kidsParser');

            if (!kidsParser.isValidKIDS(content)) {
                throw new Error('Invalid KIDS file format');
            }

            const patchMetadata = kidsParser.parse(content);
            const patchRegistry = require('./src/services/patchTracking/patchRegistry');

            await patchRegistry.addPatch(patchMetadata);

            return {
                success: true,
                patchId: patchMetadata.patchId,
                metadata: patchMetadata
            };
        } catch (error) {
            logger.error('PATCH_UPLOAD_ERROR', error);
            return { success: false, error: error.message };
        }
    });

    ipcHandle('patchTracking:scanEnvironment', async (_event, payload) => {
        try {
            const { connectionId, envName, localrPath, routinesPath, patchId } = payload || {};

            if (!patchTrackerService.initialized) {
                await patchTrackerService.init();
            }

            const result = await patchTrackerService.scanEnvironment({
                connectionId,
                envName: envName || 'docker',
                localrPath: localrPath || '/var/worldvista/prod/hakeem/localr',
                routinesPath: routinesPath || '/var/worldvista/prod/hakeem/routines',
                patchId: patchId // Pass through the patchId
            });

            return result;
        } catch (error) {
            logger.error('PATCH_SCAN_ERROR', error);
            return { success: false, error: error.message };
        }
    });

    ipcHandle('patchTracking:correlate', async (_event, payload) => {
        try {
            const { patchId, changeId } = payload || {};

            if (!patchTrackerService.initialized) {
                await patchTrackerService.init();
            }

            const result = await patchTrackerService.correlatePatchWithChanges(patchId, changeId);

            return result;
        } catch (error) {
            logger.error('PATCH_CORRELATE_ERROR', error);
            return { success: false, error: error.message };
        }
    });

    ipcHandle('patchTracking:prepareCommit', async (_event, payload) => {
        try {
            const { patchId, correlation } = payload || {};

            if (!patchTrackerService.initialized) {
                await patchTrackerService.init();
            }

            const result = await patchTrackerService.prepareCommit(patchId, correlation);

            return result;
        } catch (error) {
            logger.error('PATCH_PREPARE_COMMIT_ERROR', error);
            return { success: false, error: error.message };
        }
    });

    ipcHandle('patchTracking:executeCommit', async (_event, payload) => {
        try {
            if (!patchTrackerService.initialized) {
                await patchTrackerService.init();
            }

            const result = await patchTrackerService.executeCommit();

            return result;
        } catch (error) {
            logger.error('PATCH_EXECUTE_COMMIT_ERROR', error);
            return { success: false, error: error.message };
        }
    });

    ipcHandle('patchTracking:cancelCommit', async (_event, payload) => {
        try {
            if (!patchTrackerService.initialized) {
                await patchTrackerService.init();
            }

            const result = await patchTrackerService.cancelCommit();

            return result;
        } catch (error) {
            logger.error('PATCH_CANCEL_COMMIT_ERROR', error);
            return { success: false, error: error.message };
        }
    });

    ipcHandle('patchTracking:setRepoPath', async (_event, payload) => {
        try {
            const { repoPath } = payload || {};

            if (!repoPath) {
                throw new Error('Repository path is required');
            }

            if (!patchTrackerService.initialized) {
                await patchTrackerService.init();
            }

            const result = await patchTrackerService.setRepoPath(repoPath);

            return result;
        } catch (error) {
            logger.error('PATCH_SET_REPO_ERROR', error);
            return { success: false, error: error.message };
        }
    });

    // Git Blame handlers
    const gitBlameService = require('./src/services/gitBlameService');

    ipcHandle('gitBlame:getBlame', async (_event, payload) => {
        try {
            const { filePath } = payload || {};
            if (!filePath) {
                throw new Error('File path is required');
            }

            const blameData = await gitBlameService.getBlame(filePath);
            return { success: true, blameData };
        } catch (error) {
            logger.error('GIT_BLAME_ERROR', error);
            return { success: false, error: error.message };
        }
    });

    ipcHandle('gitBlame:getBlameForLine', async (_event, payload) => {
        try {
            const { filePath, lineNumber } = payload || {};
            if (!filePath || !lineNumber) {
                throw new Error('File path and line number are required');
            }

            const blame = await gitBlameService.getBlameForLine(filePath, lineNumber);
            return { success: true, blame };
        } catch (error) {
            logger.error('GIT_BLAME_LINE_ERROR', error);
            return { success: false, error: error.message };
        }
    });

    ipcHandle('gitBlame:clearCache', async (_event, payload) => {
        try {
            const { filePath } = payload || {};
            if (filePath) {
                gitBlameService.clearCache(filePath);
            } else {
                gitBlameService.clearAllCaches();
            }
            return { success: true };
        } catch (error) {
            logger.error('GIT_BLAME_CLEAR_CACHE_ERROR', error);
            return { success: false, error: error.message };
        }
    });

    ipcHandle('gitBlame:setRepoPath', async (_event, payload) => {
        try {
            const { repoPath } = payload || {};
            if (!repoPath) {
                throw new Error('Repository path is required');
            }

            gitBlameService.setGitRepoPath(repoPath);
            return { success: true };
        } catch (error) {
            logger.error('GIT_BLAME_SET_REPO_PATH_ERROR', error);
            return { success: false, error: error.message };
        }
    });

    ipcHandle('patchTracking:getStatistics', async (_event, payload) => {
        try {
            if (!patchTrackerService.initialized) {
                await patchTrackerService.init();
            }

            const stats = await patchTrackerService.getStatistics();

            return { success: true, stats };
        } catch (error) {
            logger.error('PATCH_GET_STATS_ERROR', error);
            return { success: false, error: error.message };
        }
    });

    // Project create/open
    ipcHandle('project:create', async (_event, payload) => {
        return bridge.createProject(payload || {});
    });

    ipcHandle('project:open', async (_event, payload) => {
        return bridge.openProject(payload?.path || '');
    });

    // Workspace filesystem access (safe offline indexing)
    ipcHandle('workspace:listFiles', async (_event, payload) => {
        const dir = payload?.dir || payload?.root || '';
        const pattern = payload?.pattern || payload?.glob || '*';
        const opts = payload?.opts || {};
        return workspaceFileService.listFiles(dir, pattern, opts);
    });

    ipcHandle('workspace:readFile', async (_event, payload) => {
        const filePath = payload?.filePath || payload?.path || '';
        const opts = payload?.opts || {};
        return workspaceFileService.readFile(filePath, opts);
    });

    ipcHandle('workspace:watch', async (event, payload) => {
        return workspaceFileService.watch({
            root: payload?.root || payload?.dir || '',
            pattern: payload?.pattern || '*.m',
            sender: event.sender,
            opts: payload?.opts || {}
        });
    });

    ipcHandle('workspace:unwatch', async (_event, payload) => {
        return workspaceFileService.unwatch(payload?.watchId || '');
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

    // Keychain handlers (for secure password storage)
    let keytar;
    try {
        keytar = require('keytar');
    } catch (e) {
        console.warn('[Keychain] keytar not available:', e.message);
    }

    if (keytar) {
        ipcHandle('keychain:setPassword', async (_event, payload) => {
            try {
                const { service, account, password } = payload || {};
                if (!service || !account || !password) {
                    throw new Error('Service, account, and password are required');
                }
                await keytar.setPassword(service, account, password);
                console.log(`[Keychain] Password set for ${service}:${account}`);
                return { success: true };
            } catch (error) {
                console.error('[Keychain] Set password error:', error);
                throw error;
            }
        });

        ipcHandle('keychain:getPassword', async (_event, payload) => {
            try {
                const { service, account } = payload || {};
                if (!service || !account) {
                    throw new Error('Service and account are required');
                }
                const password = await keytar.getPassword(service, account);
                return password; // null if not found
            } catch (error) {
                console.error('[Keychain] Get password error:', error);
                throw error;
            }
        });

        ipcHandle('keychain:deletePassword', async (_event, payload) => {
            try {
                const { service, account } = payload || {};
                if (!service || !account) {
                    throw new Error('Service and account are required');
                }
                const success = await keytar.deletePassword(service, account);
                console.log(`[Keychain] Password deleted for ${service}:${account}`);
                return { success };
            } catch (error) {
                console.error('[Keychain] Delete password error:', error);
                throw error;
            }
        });

        ipcHandle('keychain:findPasswords', async (_event, payload) => {
            try {
                const { service } = payload || {};
                if (!service) {
                    throw new Error('Service is required');
                }
                const credentials = await keytar.findCredentials(service);
                // Return without passwords for security
                return credentials.map(c => ({ account: c.account }));
            } catch (error) {
                console.error('[Keychain] Find passwords error:', error);
                throw error;
            }
        });

        console.log('[Keychain] Handlers registered');
    }

    // Terminal handlers
    ipcHandle('terminal:create', async (event, payload) => {
        try {
            const id = createTerminalSession(event.sender, payload || {});
            return {
                ok: true,
                id,
                backend: nodePty ? 'pty' : 'pipe',
                pty: !!nodePty,
                ptyError: nodePty ? null : (nodePtyLoadError ? (nodePtyLoadError.message || String(nodePtyLoadError)) : 'node-pty unavailable')
            };
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

    // Fire-and-forget input path for interactive terminals (avoids invoke overhead and log spam).
    ipcMain.on('terminal:writeFast', (_event, payload) => {
        const id = payload?.id;
        const data = payload?.data || '';
        if (!id || !terminalSessions.has(id)) return;
        try {
            const term = terminalSessions.get(id);
            if (term?.write) {
                term.write(data);
            } else if (term?.stdin) {
                term.stdin.write(data);
            }
        } catch (_) {
            // best-effort
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

    // Fire-and-forget resize path (used during split drags / continuous resizes).
    ipcMain.on('terminal:resizeFast', (_event, payload) => {
        const id = payload?.id;
        if (!id || !terminalSessions.has(id)) return;
        const cols = payload?.cols || 80;
        const rows = payload?.rows || 24;
        try {
            const term = terminalSessions.get(id);
            if (term?.resize) term.resize(cols, rows);
        } catch (_) {
            // best-effort
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
