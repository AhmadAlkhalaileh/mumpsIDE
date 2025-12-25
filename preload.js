const { contextBridge, ipcRenderer } = require('electron');

// Note: xterm is a browser library loaded via <script> tags in the renderer
// Do NOT try to require it here - it's not a Node.js module

contextBridge.exposeInMainWorld('ahmadIDE', {
    // Runtime hints (safe, no secrets)
    runtime: {
        isSnap: !!process.env.SNAP,
        enableGpuRequested: process.env.AHMAD_IDE_ENABLE_GPU === '1',
        forceSoftwareRendering: process.env.AHMAD_IDE_FORCE_SOFT_RENDER === '1'
    },

    // Environment
    getEnv: () => ipcRenderer.invoke('env:get'),

    // Code execution / Tools
    lint: (code) => ipcRenderer.invoke('lint:run', { code }),
    execute: (code) => ipcRenderer.invoke('exec:run', { code }),

    // Connection Management
    setConnection: (type, config) => ipcRenderer.invoke('connection:set', { type, config }),
    getConnection: () => ipcRenderer.invoke('connection:get'),

    // Debugging (ZSTEP/JSON Engine Only)
    debugStart: (code, breakpoints, startLine) =>
        ipcRenderer.invoke('debug:start', { code, breakpoints, startLine }),
    debugStep: (sessionId, stepType) =>
        ipcRenderer.invoke('debug:step', { sessionId, stepType }),
    debugContinue: (sessionId) =>
        ipcRenderer.invoke('debug:continue', { sessionId }),
    debugStop: (sessionId) =>
        ipcRenderer.invoke('debug:stop', { sessionId }),
    debugEval: (sessionId, code) =>
        ipcRenderer.invoke('debug:eval', { sessionId, code }),

    // Docker
    listDocker: () => ipcRenderer.invoke('docker:list'),

    // SSH
    sshConnect: (config) => ipcRenderer.invoke('ssh:connect', config),
    sshExec: (sessionId, command) => ipcRenderer.invoke('ssh:exec', { sessionId, command }),
    sshDisconnect: (sessionId) => ipcRenderer.invoke('ssh:disconnect', { sessionId }),

    // Environment operations (works with current Docker or SSH connection)
    createDirectoryInCurrentEnv: (dirPath) => ipcRenderer.invoke('env:createDirectory', { dirPath }),

    // Routines
    listRoutines: (search) => ipcRenderer.invoke('routines:list', { search }),
    searchRoutines: (term, options) => ipcRenderer.invoke('routines:search', { term, options }),
    readRoutine: (name) => ipcRenderer.invoke('routines:read', { name }),
    saveRoutine: (name, code) => ipcRenderer.invoke('routines:save', { name, code }),
    zlinkRoutine: (name) => ipcRenderer.invoke('routines:zlink', { name }),

    // Host Terminal / git
    hostExec: (command) => ipcRenderer.invoke('terminal:exec', { command }),
    git: (command) => ipcRenderer.invoke('git:run', { command }),
    detectGitRepo: (projectRoot, opts) => ipcRenderer.invoke('git:detectRepo', { projectRoot, opts }),
    getGitConfig: (projectPath) => ipcRenderer.invoke('git:getConfig', { projectPath }),

    // Project
    createProject: (config) => ipcRenderer.invoke('project:create', config),
    openProject: (projectPath) => ipcRenderer.invoke('project:open', { path: projectPath }),
    openFolderDialog: () => ipcRenderer.invoke('dialog:openFolder'),
    revealInExplorer: (path) => ipcRenderer.invoke('shell:reveal', { path }),

    // App
    toggleDevTools: () => ipcRenderer.invoke('devtools:toggle'),
    exitApp: () => ipcRenderer.invoke('app:exit'),

    // Patch Tracking
    patchTracking: {
        uploadPatch: (content) => ipcRenderer.invoke('patchTracking:uploadPatch', { content }),
        scanEnvironment: (options) => ipcRenderer.invoke('patchTracking:scanEnvironment', options),
        correlate: (patchId, changeId) => ipcRenderer.invoke('patchTracking:correlate', { patchId, changeId }),
        prepareCommit: (patchId, correlation) => ipcRenderer.invoke('patchTracking:prepareCommit', { patchId, correlation }),
        executeCommit: () => ipcRenderer.invoke('patchTracking:executeCommit'),
        cancelCommit: () => ipcRenderer.invoke('patchTracking:cancelCommit'),
        getStatistics: () => ipcRenderer.invoke('patchTracking:getStatistics'),
        setRepoPath: (repoPath) => ipcRenderer.invoke('patchTracking:setRepoPath', { repoPath })
    },

    // Git Blame
    gitBlame: {
        getBlame: (filePath) => ipcRenderer.invoke('gitBlame:getBlame', { filePath }),
        getBlameForLine: (filePath, lineNumber) => ipcRenderer.invoke('gitBlame:getBlameForLine', { filePath, lineNumber }),
        clearCache: (filePath) => ipcRenderer.invoke('gitBlame:clearCache', { filePath }),
        setRepoPath: (repoPath) => ipcRenderer.invoke('gitBlame:setRepoPath', { repoPath })
    },

    // Integrated Terminal (Pty)
    terminalCreate: (options) => ipcRenderer.invoke('terminal:create', options),
    terminalWrite: (id, data) => ipcRenderer.invoke('terminal:write', { id, data }),
    // High-frequency path for xterm input (fire-and-forget; avoids invoke overhead).
    terminalWriteFast: (id, data) => ipcRenderer.send('terminal:writeFast', { id, data }),
    terminalResize: (id, cols, rows) => ipcRenderer.invoke('terminal:resize', { id, cols, rows }),
    terminalResizeFast: (id, cols, rows) => ipcRenderer.send('terminal:resizeFast', { id, cols, rows }),
    terminalClose: (id) => ipcRenderer.invoke('terminal:close', { id }),
    onTerminalData: (callback) => {
        ipcRenderer.on('terminal:data', (_event, payload) => callback(payload));
    },
    onTerminalExit: (callback) => {
        ipcRenderer.on('terminal:exit', (_event, payload) => callback(payload));
    }
});

// Generic bridge for IPC invoke (used by extensions)
contextBridge.exposeInMainWorld('bridge', {
    invoke: (channel, data) => ipcRenderer.invoke(channel, data)
});
