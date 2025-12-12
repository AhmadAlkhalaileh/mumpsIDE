const { contextBridge, ipcRenderer } = require('electron');

// Try to expose xterm Terminal class if available
try {
    const { Terminal } = require('xterm');
    contextBridge.exposeInMainWorld('Terminal', Terminal);
} catch (e) {
    console.warn('xterm not found in preload', e);
}

contextBridge.exposeInMainWorld('ahmadIDE', {
    // Environment
    getEnv: () => ipcRenderer.invoke('env:get'),

    // Code execution / Tools
    lint: (code) => ipcRenderer.invoke('lint:run', { code }),
    execute: (code) => ipcRenderer.invoke('exec:run', { code }),

    // Connection Management
    setConnection: (type, config) => ipcRenderer.invoke('connection:set', { type, config }),

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

    // Routines
    listRoutines: (search) => ipcRenderer.invoke('routines:list', { search }),
    searchRoutines: (term, options) => ipcRenderer.invoke('routines:search', { term, options }),
    readRoutine: (name) => ipcRenderer.invoke('routines:read', { name }),
    saveRoutine: (name, code) => ipcRenderer.invoke('routines:save', { name, code }),
    zlinkRoutine: (name) => ipcRenderer.invoke('routines:zlink', { name }),

    // Host Terminal / git
    hostExec: (command) => ipcRenderer.invoke('terminal:exec', { command }),
    git: (command) => ipcRenderer.invoke('git:run', { command }),
    getGitConfig: (projectPath) => ipcRenderer.invoke('git:getConfig', { projectPath }),

    // Project
    createProject: (config) => ipcRenderer.invoke('project:create', config),
    openProject: (projectPath) => ipcRenderer.invoke('project:open', { path: projectPath }),
    openFolderDialog: () => ipcRenderer.invoke('dialog:openFolder'),
    revealInExplorer: (path) => ipcRenderer.invoke('shell:reveal', { path }),

    // App
    toggleDevTools: () => ipcRenderer.invoke('devtools:toggle'),
    exitApp: () => ipcRenderer.invoke('app:exit'),

    // Integrated Terminal (Pty)
    terminalCreate: (options) => ipcRenderer.invoke('terminal:create', options),
    terminalWrite: (id, data) => ipcRenderer.invoke('terminal:write', { id, data }),
    terminalResize: (id, cols, rows) => ipcRenderer.invoke('terminal:resize', { id, cols, rows }),
    terminalClose: (id) => ipcRenderer.invoke('terminal:close', { id }),
    onTerminalData: (callback) => {
        ipcRenderer.on('terminal:data', (_event, payload) => callback(payload));
    },
    onTerminalExit: (callback) => {
        ipcRenderer.on('terminal:exit', (_event, payload) => callback(payload));
    }
});
