/**
 * Compare with Release Extension
 * Build #PS-253.28294.345 compatible
 *
 * Features:
 * - SSH connection to release server
 * - Find and compare routine files
 * - Modern diff UI with  Dracula theme
 * - Whitespace-aware comparison
 * - Problems panel with navigation
 */

(() => {
    const EXTENSION_ID = 'compare-with-release';
    const EXTENSION_NAME = 'Compare with Release';
    const EXTENSION_VERSION = '1.0.0';
    const utils = window.AhmadIDEModules?.extensions?.compareWithReleaseUtils || {};

    const runtime = {
        active: false,
        ignoreWhitespace: false,
        currentChangeIndex: -1,
        abortController: null
    };

    /**
     * Extension activation
     */
    async function activate(context) {
        const extensionsService = window.AhmadIDEModules?.services?.extensionsService;
        const menuRegistry = window.AhmadIDEModules?.app?.menuRegistry;

        if (!extensionsService || !menuRegistry) {
            console.error('Compare with Release: Required services not available');
            return [];
        }

        runtime.active = true;
        setCompareToolWindowVisible(true);

        const disposables = [];

        // Register context menu action
        const contextMenuDisposable = registerContextMenu(menuRegistry);
        if (contextMenuDisposable) disposables.push(contextMenuDisposable);

        // Register keyboard shortcuts (Ctrl/Cmd+Alt+R, F7/Shift+F7)
        const shortcutDisposable = registerKeyboardShortcuts();
        if (shortcutDisposable) disposables.push(shortcutDisposable);

        console.log('[Compare with Release] Extension activated');
        return disposables;
    }

    /**
     * Extension deactivation
     */
    async function deactivate() {
        runtime.active = false;
        runtime.ignoreWhitespace = false;
        runtime.currentChangeIndex = -1;
        try { runtime.abortController?.abort?.(); } catch (_) { }
        runtime.abortController = null;
        setCompareToolWindowVisible(false);

        // Close diff editor
        closeDiffEditor();

        // Clear cache
        if (window.AhmadIDEModules?.compareWithRelease?.cache) {
            window.AhmadIDEModules.compareWithRelease.cache.clear();
        }
        console.log('[Compare with Release] Extension deactivated');
    }

    /**
     * Register context menu action
     */
    function registerContextMenu(menuRegistry) {
        if (!menuRegistry || !menuRegistry._registry) {
            console.warn('[Compare with Release] Menu registry not available');
            return { dispose: () => { } };
        }

        // Hook into editor context menu
        const originalEditorGetter = menuRegistry._registry['context.editor'];

        if (originalEditorGetter) {
            const wrappedEditorGetter = (ctx) => {
                const baseItems = typeof originalEditorGetter === 'function' ? originalEditorGetter(ctx) : [];

                // Check if current file is a .m file
                const editor = window.activeEditor;
                let isMumpsFile = false;
                if (editor) {
                    try {
                        const model = editor.getModel();
                        if (model) {
                            const uri = model.uri.toString();
                            isMumpsFile = uri.endsWith('.m');
                        }
                    } catch (e) { }
                }

                if (isMumpsFile) {
                    const compareItem = {
                        id: 'ed.compareWithRelease',
                        label: 'Compare with Release',
                        action: 'compare-with-release',
                        icon: 'compare'
                    };

                    // Insert after Git items
                    const gitIndex = baseItems.findIndex(item => item.id && item.id.startsWith('gitctx:'));
                    if (gitIndex !== -1) {
                        baseItems.splice(gitIndex + 1, 0, compareItem);
                    } else {
                        baseItems.push({ type: 'separator' });
                        baseItems.push(compareItem);
                    }
                }

                return baseItems;
            };

            menuRegistry._registry['context.editor'] = wrappedEditorGetter;
        }

        // Hook into project tree context menu
        const originalProjectGetter = menuRegistry._registry['context.project'];

        if (originalProjectGetter) {
            const wrappedProjectGetter = (ctx) => {
                const baseItems = typeof originalProjectGetter === 'function' ? originalProjectGetter(ctx) : [];

                // Check if it's a .m file in project tree
                const path = ctx?.path || '';
                const isMumpsFile = path.endsWith('.m');
                const isFile = ctx?.type === 'file';

                if (isMumpsFile && isFile) {
                    const compareItem = {
                        id: 'prj.compareWithRelease',
                        label: 'Compare with Release',
                        action: 'compare-with-release',
                        icon: 'compare'
                    };

                    // Insert after Git items
                    const gitIndex = baseItems.findIndex(item => item.id && (item.id.startsWith('prj.git') || item.id.startsWith('gitctx:')));
                    if (gitIndex !== -1) {
                        // Find the last git item
                        let lastGitIndex = gitIndex;
                        for (let i = gitIndex + 1; i < baseItems.length; i++) {
                            if (baseItems[i].id && (baseItems[i].id.startsWith('prj.git') || baseItems[i].id.startsWith('gitctx:'))) {
                                lastGitIndex = i;
                            } else {
                                break;
                            }
                        }
                        baseItems.splice(lastGitIndex + 1, 0, compareItem);
                    } else {
                        baseItems.push({ type: 'separator' });
                        baseItems.push(compareItem);
                    }
                }

                return baseItems;
            };

            menuRegistry._registry['context.project'] = wrappedProjectGetter;
        }

        return {
            dispose: () => {
                if (menuRegistry._registry) {
                    if (originalEditorGetter) {
                        menuRegistry._registry['context.editor'] = originalEditorGetter;
                    }
                    if (originalProjectGetter) {
                        menuRegistry._registry['context.project'] = originalProjectGetter;
                    }
                }
            }
        };
    }

    function setCompareToolWindowVisible(visible) {
        const show = !!visible;
        const stripeBtn = document.querySelector('.tool-window-stripe-btn[data-panel="comparePanel"]');
        stripeBtn?.classList.toggle('hidden', !show);

        if (!show) {
            try {
                // If Compare panel is currently visible, switch away.
                const panel = document.getElementById('comparePanel');
                if (panel && !panel.classList.contains('hidden') && typeof toggleToolWindowPanel === 'function') {
                    toggleToolWindowPanel('terminalPanel', 'bottom');
                }
            } catch (_) { }
        }
    }

    function registerKeyboardShortcuts() {
        const isEditable = (el) => {
            const t = el?.tagName?.toLowerCase?.();
            if (!t) return false;
            if (t === 'input' || t === 'textarea' || t === 'select') return true;
            return !!el?.isContentEditable;
        };

        const onKeyDown = (e) => {
            if (!runtime.active) return;
            if (isEditable(e.target) && (e.key !== 'F7')) return;

            // Ctrl/Cmd + Alt + R
            if ((e.ctrlKey || e.metaKey) && e.altKey && !e.shiftKey && (e.code === 'KeyR' || e.key === 'r' || e.key === 'R')) {
                e.preventDefault();
                handleCompareWithRelease({ source: 'shortcut' }).catch(() => { });
                return;
            }

            // F7 / Shift+F7 navigation
            if (e.key === 'F7') {
                e.preventDefault();
                if (e.shiftKey) navigateToPrevChange();
                else navigateToNextChange();
            }
        };

        document.addEventListener('keydown', onKeyDown, true);
        return {
            dispose: () => document.removeEventListener('keydown', onKeyDown, true)
        };
    }

    /**
     * Handle Compare with Release action
     */
    async function handleCompareWithRelease(context) {
        if (!runtime.active) {
            showNotification('info', EXTENSION_NAME, 'Enable the extension to use this feature.');
            return;
        }

        let routineName = '';
        let localContent = '';
        let fileName = '';

        // Priority 1: Context from Project Tree
        if (context?.path && typeof context.path === 'string' && context.path.endsWith('.m')) {
            fileName = getFileNameFromUri(context.path);
            routineName = fileName.replace(/\.m$/i, '');

            try {
                // Attempt to read routine content using IDE API
                if (window.ahmadIDE && typeof window.ahmadIDE.readRoutine === 'function') {
                    const res = await window.ahmadIDE.readRoutine(routineName);
                    if (res && res.ok) {
                        localContent = res.code || '';
                    } else {
                        throw new Error(res?.error || 'Failed to read routine');
                    }
                } else {
                    // Fallback: This shouldn't happen if API is consistent, but safeguard
                    throw new Error('Read API not available');
                }
            } catch (e) {
                showNotification('error', EXTENSION_NAME, `Could not read ${routineName}: ${e.message}`);
                return;
            }
        }
        // Priority 2: Active Editor
        else {
            const editor = window.activeEditor || context?.editor;
            if (!editor) {
                showNotification('error', EXTENSION_NAME, 'No active editor. Select a file in the Project tree or open a file.');
                return;
            }

            const model = editor.getModel();
            if (!model) {
                showNotification('error', EXTENSION_NAME, 'No active file');
                return;
            }

            const uri = model.uri.toString();
            fileName = getFileNameFromUri(uri);

            if (!fileName.endsWith('.m')) {
                showNotification('error', EXTENSION_NAME, 'This action is only available for MUMPS routine files (.m)');
                return;
            }

            routineName = fileName.replace(/\.m$/, '');
            localContent = model.getValue();
        }

        // Check if Release Connection is configured
        const releaseConnection = getReleaseConnection();
        if (!releaseConnection) {
            showNotification('error', 'Release Connection not configured', 'Please configure in File → Connections');
            focusReleaseConnection();
            return;
        }

        // Cancel any in-flight run
        try { runtime.abortController?.abort?.(); } catch (_) { }
        const controller = new AbortController();
        runtime.abortController = controller;
        const { signal } = controller;

        // Ensure Compare tool window is open first
        try {
            const panel = document.getElementById('comparePanel');
            const isVisible = panel && !panel.classList.contains('hidden');
            if (!isVisible && typeof toggleToolWindowPanel === 'function') {
                toggleToolWindowPanel('comparePanel', 'bottom');
            }
        } catch (_) { }

        // Open compare tool window instantly (loading state)
        openCompareUI({
            routineName,
            localContent,
            remoteContents: [],
            selectedPaths: [],
            activeTab: 0,
            status: { phase: 'search', message: 'Searching for routine on release server…' }
        });

        try {
            // Find remote routine
            const remotePaths = await findRemoteRoutine(routineName, releaseConnection, { signal });

            if (!remotePaths || remotePaths.length === 0) {
                showNotification('info', 'Routine not found', `No matches found for ${fileName} on release server`);
                openCompareUI({ routineName, localContent, remoteContents: [], selectedPaths: [], activeTab: 0, status: { phase: 'idle' } });
                return;
            }

            // Let user select up to 2 paths
            openCompareUI({
                routineName,
                localContent,
                remoteContents: [],
                selectedPaths: [],
                activeTab: 0,
                status: { phase: 'select', message: 'Select release version(s)…' }
            });

            const selectedPaths = await showPathSelectionDialog(remotePaths, fileName, { signal });
            if (!selectedPaths || selectedPaths.length === 0) {
                openCompareUI({ routineName, localContent, remoteContents: [], selectedPaths: [], activeTab: 0, status: { phase: 'idle' } });
                return; // User cancelled
            }

            // Fetch remote content(s)
            openCompareUI({
                routineName,
                localContent,
                remoteContents: [],
                selectedPaths,
                activeTab: 0,
                status: { phase: 'fetch', message: `Fetching ${selectedPaths.length} file(s)…` }
            });

            const remoteContents = await fetchRemoteContents(selectedPaths, releaseConnection, { signal });

            // Open compare UI
            openCompareUI({
                routineName,
                localContent,
                remoteContents,
                selectedPaths
            });

        } catch (error) {
            if (signal?.aborted || error?.name === 'AbortError') {
                showNotification('info', EXTENSION_NAME, 'Comparison canceled');
                openCompareUI({ routineName, localContent, remoteContents: [], selectedPaths: [], activeTab: 0, status: { phase: 'idle' } });
                return;
            }
            showNotification('error', 'Comparison failed', error?.message || 'Unknown error');
            openCompareUI({
                routineName,
                localContent,
                remoteContents: [],
                selectedPaths: [],
                activeTab: 0,
                status: { phase: 'error', message: error?.message || 'Comparison failed' }
            });
        } finally {
            if (runtime.abortController === controller) runtime.abortController = null;
        }
    }

    /**
     * Get Release Connection settings
     */
    function getReleaseConnection() {
        try {
            const connections = JSON.parse(localStorage.getItem('ahmadIDE:connectionProfiles') || '{}');
            const cfg = connections?.release || null;
            if (!cfg) return null;
            if (!cfg.host || !cfg.username) return null;
            return cfg;
        } catch (_) {
            return null;
        }
    }

    function focusReleaseConnection() {
        try {
            if (typeof runMenuAction === 'function') {
                runMenuAction('connections');
            } else {
                document.getElementById('toggleConnections')?.dispatchEvent?.(new MouseEvent('click', { bubbles: true, cancelable: true, shiftKey: true }));
            }
        } catch (_) { }

        setTimeout(() => {
            const card = document.getElementById('releaseConnectionCard');
            if (!card) return;
            card.scrollIntoView({ block: 'start', behavior: 'smooth' });
            card.classList.add('pulse-focus');
            setTimeout(() => card.classList.remove('pulse-focus'), 900);
        }, 80);
    }

    /**
     * Find remote routine via SSH
     */
    async function findRemoteRoutine(routineName, connection, { signal } = {}) {
        const cacheKey = `find:${String(connection?.host || '').trim()}:${routineName}`;
        const cached = getFromCache(cacheKey);
        if (cached) return cached;

        const ssh = await createSSHConnection(connection, { signal, timeoutMs: 15000 });

        try {
            // Search in current directory (likely home or project root) instead of root /
            const command = `find . -name ${shellQuote(`${routineName}.m`)} 2>/dev/null | head -n 50`;
            const result = await ssh.execCommand(command, { timeoutMs: 45000, signal });

            if (result.code !== 0 && result.code !== null) {
                throw new Error(`SSH command failed: ${result.stderr}`);
            }

            const parse = typeof utils.parseFindOutput === 'function'
                ? utils.parseFindOutput
                : (out) => String(out || '').split('\n').map((l) => l.trim()).filter((l) => l && l.endsWith('.m'));
            const paths = parse(result.stdout);

            setCache(cacheKey, paths, 300000); // 5 minutes
            return paths;

        } finally {
            await ssh.dispose();
        }
    }

    /**
     * Fetch remote file contents
     */
    async function fetchRemoteContents(paths, connection, { signal } = {}) {
        const ssh = await createSSHConnection(connection, { signal, timeoutMs: 15000 });
        const contents = [];

        try {
            for (const path of paths) {
                const result = await ssh.execCommand(`cat -- ${shellQuote(path)}`, { timeoutMs: 10000, signal });
                if (result.code === 0) {
                    contents.push({
                        path,
                        content: result.stdout
                    });
                } else {
                    contents.push({
                        path,
                        content: null,
                        error: result.stderr || 'Failed to read file'
                    });
                }
            }
            return contents;

        } finally {
            await ssh.dispose();
        }
    }

    /**
     * Create SSH connection
     */
    async function createSSHConnection(config, { signal, timeoutMs = 15000 } = {}) {
        const makeAbortError = () => {
            const err = new Error('Aborted');
            err.name = 'AbortError';
            return err;
        };

        const withTimeout = (promise, ms, sig) => new Promise((resolve, reject) => {
            let timer = null;
            let aborted = false;

            const onAbort = () => {
                aborted = true;
                cleanup();
                reject(makeAbortError());
            };

            const cleanup = () => {
                if (timer) clearTimeout(timer);
                timer = null;
                try { sig?.removeEventListener?.('abort', onAbort); } catch (_) { }
            };

            if (sig?.aborted) return reject(makeAbortError());
            try { sig?.addEventListener?.('abort', onAbort, { once: true }); } catch (_) { }

            timer = setTimeout(() => {
                cleanup();
                reject(new Error('Timeout'));
            }, Math.max(0, Number(ms || 0) || 0));

            Promise.resolve(promise)
                .then((v) => { cleanup(); resolve(v); })
                .catch((e) => { cleanup(); reject(e); });
        });

        const password = await getSecurePassword();
        if (!password) {
            throw new Error('Release Connection password not set. Configure it in File → Connections → Release Connection.');
        }

        // Use existing bridge SSH implementation (ssh2 library)
        const response = await withTimeout(window.bridge?.invoke('ssh:connect', {
            host: config.host,
            port: config.port || 22,
            username: config.username,
            password,
            purpose: 'compare-with-release',
            skipYdbDetect: true,
            setAsActiveConnection: false
        }), timeoutMs, signal);

        if (!response || !response.ok || !response.sessionId) {
            throw new Error(response?.error || 'SSH connection failed');
        }

        const sessionId = response.sessionId;

        const disconnect = async () => {
            try { await window.bridge?.invoke('ssh:disconnect', { sessionId }); } catch (_) { }
        };

        return {
            sessionId,
            execCommand: async (command, options = {}) => {
                const ms = Number(options.timeoutMs ?? options.timeout ?? 30000) || 30000;
                const sig = options.signal || null;
                try {
                    const result = await withTimeout(window.bridge?.invoke('ssh:exec', { sessionId, command }), ms, sig);
                    return {
                        code: result?.ok ? 0 : 1,
                        stdout: result?.stdout || '',
                        stderr: result?.stderr || result?.error || ''
                    };
                } catch (e) {
                    // Best-effort abort: disconnect session to stop remote exec.
                    if (sig?.aborted || e?.name === 'AbortError' || String(e?.message || '').toLowerCase().includes('timeout')) {
                        await disconnect();
                    }
                    if (String(e?.message || '') === 'Timeout') {
                        throw new Error('SSH command timed out');
                    }
                    throw e;
                }
            },
            dispose: disconnect
        };
    }

    /**
     * Get secure password from keychain
     */
    async function getSecurePassword() {
        return await window.bridge?.invoke('keychain:getPassword', {
            service: 'AhmadIDE',
            account: 'release-connection'
        });
    }

    /**
     * Show path selection dialog
     */
    async function showPathSelectionDialog(paths, fileName, { signal } = {}) {
        const createDialog = window.AhmadIDEModules?.ui?.createDialog;
        const createButton = window.AhmadIDEModules?.ui?.primitives?.createButton;
        if (!createDialog || !createButton) {
            // Fallback: pick first two.
            return (paths || []).slice(0, 2);
        }

        return new Promise((resolve) => {
            let settled = false;
            const finish = (value) => {
                if (settled) return;
                settled = true;
                resolve(value);
            };

            const title = `Select Release Version(s) for ${fileName}`;
            const dialog = createDialog({
                ariaLabel: title,
                closeOnEscape: true,
                closeOnBackdrop: false,
                onClose: () => finish(null)
            });

            const wrapper = document.createElement('div');
            wrapper.className = 'ui-dialog-layout';

            const header = document.createElement('div');
            header.className = 'ui-dialog-header';
            const headerLeft = document.createElement('div');
            headerLeft.className = 'ui-dialog-header__left';
            const titleEl = document.createElement('div');
            titleEl.className = 'ui-dialog-title';
            titleEl.textContent = title;
            headerLeft.appendChild(titleEl);
            const headerRight = document.createElement('div');
            headerRight.className = 'ui-dialog-header__right';
            const closeBtn = document.createElement('button');
            closeBtn.className = 'ui-dialog-close';
            closeBtn.type = 'button';
            closeBtn.title = 'Close';
            closeBtn.textContent = '✕';
            headerRight.appendChild(closeBtn);
            header.appendChild(headerLeft);
            header.appendChild(headerRight);

            const body = document.createElement('div');
            body.style.cssText = 'padding: 24px; min-width: 720px; max-width: 85vw; background: var(--ps-bg-main, var(--bg, #282a36));';

            // Header section with icon and description
            const headerSection = document.createElement('div');
            headerSection.style.cssText = 'margin-bottom: 24px; text-align: center;';
            headerSection.innerHTML = `
                <div style="display: inline-flex; align-items: center; justify-content: center; width: 56px; height: 56px; border-radius: 50%; background: linear-gradient(135deg, var(--ps-accent-blue, var(--accent, #bd93f9)) 0%, var(--accent-blue, #8be9fd) 100%); margin-bottom: 16px; box-shadow: 0 4px 16px rgba(189, 147, 249, 0.22);">
                    <svg width="28" height="28" fill="white">
                        <path d="M4 4h9l2 2h9v16H4V4z" fill="none" stroke="currentColor" stroke-width="2" stroke-linejoin="round"/>
                        <path d="M8 11h12M8 15h8" stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
                    </svg>
                </div>
                <div style="font-size: 15px; font-weight: 600; color: var(--ps-text-main, var(--text, #f8f8f2)); margin-bottom: 8px;">
                    Choose Release Versions
                </div>
                <div style="font-size: 13px; color: var(--ps-text-muted, var(--muted, #6272a4)); max-width: 480px; margin: 0 auto;">
                    Select up to <span style="color: var(--ps-accent-blue, var(--accent, #bd93f9)); font-weight: 600;">2 versions</span> to compare with your local copy
                </div>
            `;
            body.appendChild(headerSection);

            // Stats bar
            const statsBar = document.createElement('div');
            statsBar.style.cssText = 'display: flex; gap: 12px; margin-bottom: 20px; padding: 12px; background: rgba(0,0,0,0.2); border-radius: 8px; border: 1px solid rgba(255,255,255,0.05);';
            statsBar.innerHTML = `
                <div style="flex: 1; text-align: center;">
                    <div style="font-size: 24px; font-weight: 700; color: var(--ps-accent-blue, var(--accent, #bd93f9));">${paths.length}</div>
                    <div style="font-size: 11px; color: var(--ps-text-muted, var(--muted, #6272a4)); text-transform: uppercase; letter-spacing: 0.5px;">Found</div>
                </div>
                <div style="width: 1px; background: rgba(255,255,255,0.1);"></div>
                <div style="flex: 1; text-align: center;">
                    <div style="font-size: 24px; font-weight: 700; color: var(--ps-accent-green, var(--accent-green, #50fa7b));" id="selectedCount">0</div>
                    <div style="font-size: 11px; color: var(--ps-text-muted, var(--muted, #6272a4)); text-transform: uppercase; letter-spacing: 0.5px;">Selected</div>
                </div>
                <div style="width: 1px; background: rgba(255,255,255,0.1);"></div>
                <div style="flex: 1; text-align: center;">
                    <div style="font-size: 24px; font-weight: 700; color: var(--ps-accent-orange, var(--accent-orange, #ffb86c));">2</div>
                    <div style="font-size: 11px; color: var(--ps-text-muted, var(--muted, #6272a4)); text-transform: uppercase; letter-spacing: 0.5px;">Max</div>
                </div>
            `;
            body.appendChild(statsBar);

            const list = document.createElement('div');
            list.className = 'release-path-list';
            list.style.cssText = 'max-height: 380px; overflow-y: auto; overflow-x: hidden; padding-right: 8px;';

            list.innerHTML = (paths || []).map((p, idx) => {
                const env = extractEnvironmentFromPath(p);
                const envColors = {
                    'MOH': { bg: 'rgba(45, 79, 108, 0.15)', border: '#2D4F6C', text: '#5B7D9C' },
                    'KHCC': { bg: 'rgba(106, 135, 89, 0.15)', border: '#6A8759', text: '#6A8759' },
                    'RMS': { bg: 'rgba(204, 120, 50, 0.15)', border: '#CC7832', text: '#CC7832' },
                    'WHCC': { bg: 'rgba(156, 95, 180, 0.15)', border: '#9C5FB4', text: '#9C5FB4' }
                };
                const color = envColors[env] || { bg: 'rgba(139, 140, 142, 0.15)', border: '#8B8C8E', text: '#8B8C8E' };

                return `
                    <label class="release-path-item" data-index="${idx}"
                           style="display: block; margin-bottom: 12px; cursor: pointer; position: relative; transition: all 0.2s cubic-bezier(0.4, 0, 0.2, 1);">
                        <input type="checkbox" value="${idx}" style="position: absolute; opacity: 0; pointer-events: none;">

                        <div class="path-card" style="
                            padding: 16px 20px;
                            background: linear-gradient(135deg, rgba(60, 63, 65, 0.4) 0%, rgba(50, 53, 55, 0.4) 100%);
                            border: 2px solid transparent;
                            border-radius: 10px;
                            transition: all 0.25s cubic-bezier(0.4, 0, 0.2, 1);
                            position: relative;
                            overflow: hidden;
                        ">
                            <!-- Glow effect -->
                            <div class="card-glow" style="
                                position: absolute;
                                top: -2px;
                                left: -2px;
                                right: -2px;
                                bottom: -2px;
                                background: linear-gradient(135deg, ${color.border}40 0%, ${color.border}20 100%);
                                border-radius: 10px;
                                opacity: 0;
                                transition: opacity 0.25s ease;
                                z-index: -1;
                            "></div>

                            <div style="display: flex; align-items: center; gap: 16px;">
                                <!-- Custom checkbox -->
                                <div class="custom-checkbox" style="
                                    flex-shrink: 0;
                                    width: 24px;
                                    height: 24px;
                                    border: 2px solid var(--ps-accent-blue, var(--accent, #bd93f9));
                                    border-radius: 6px;
                                    background: rgba(0,0,0,0.3);
                                    position: relative;
                                    transition: all 0.2s ease;
                                ">
                                    <svg class="check-icon" width="16" height="16" viewBox="0 0 16 16" style="
                                        position: absolute;
                                        top: 50%;
                                        left: 50%;
                                        transform: translate(-50%, -50%) scale(0);
                                        transition: transform 0.2s cubic-bezier(0.68, -0.55, 0.265, 1.55);
                                    ">
                                        <path d="M3 8l3 3 7-7" stroke="white" stroke-width="2.5" fill="none" stroke-linecap="round" stroke-linejoin="round"/>
                                    </svg>
                                </div>

                                <!-- Environment badge -->
                                <div style="
                                    flex-shrink: 0;
                                    padding: 6px 14px;
                                    background: ${color.bg};
                                    border: 1px solid ${color.border};
                                    border-radius: 20px;
                                    font-size: 12px;
                                    font-weight: 700;
                                    color: ${color.text};
                                    text-transform: uppercase;
                                    letter-spacing: 0.5px;
                                    box-shadow: 0 2px 8px ${color.border}30;
                                ">
                                    ${escapeHtml(env || 'RELEASE')}
                                </div>

                                <!-- Path -->
                                <div style="flex: 1; min-width: 0;">
                                    <div style="
                                        font-family: 'ains Mono', 'Fira Code', 'Consolas', monospace;
                                        font-size: 13px;
                                        color: var(--ps-text-main, var(--text, #f8f8f2));
                                        white-space: nowrap;
                                        overflow: hidden;
                                        text-overflow: ellipsis;
                                        font-weight: 500;
                                    " title="${escapeHtml(p)}">
                                        ${escapeHtml(p)}
                                    </div>
                                    <div style="
                                        font-size: 11px;
                                        color: var(--ps-text-muted, var(--muted, #6272a4));
                                        margin-top: 4px;
                                    ">
                                        ${p.split('/').length - 1} folders deep
                                    </div>
                                </div>

                                <!-- Arrow indicator -->
                                <div class="arrow-indicator" style="
                                    flex-shrink: 0;
                                    width: 32px;
                                    height: 32px;
                                    border-radius: 50%;
                                    background: var(--accent-soft);
                                    display: flex;
                                    align-items: center;
                                    justify-content: center;
                                    opacity: 0.6;
                                    transition: all 0.2s ease;
                                ">
                                    <svg width="16" height="16" fill="none" stroke="var(--ps-accent-blue, var(--accent, #bd93f9))" stroke-width="2">
                                        <path d="M6 4l4 4-4 4" stroke-linecap="round" stroke-linejoin="round"/>
                                    </svg>
                                </div>
                            </div>
                        </div>
                    </label>
                `;
            }).join('');
            body.appendChild(list);

            const footer = document.createElement('div');
            footer.className = 'ui-dialog-footer';

            const cancelBtn = createButton({
                label: 'Cancel',
                variant: 'ghost',
                onClick: () => {
                    finish(null);
                    dialog.close('cancel');
                }
            });
            const selectBtn = createButton({
                label: 'Select',
                variant: 'primary',
                onClick: () => {
                    const checked = Array.from(list.querySelectorAll('input[type="checkbox"]')).filter((c) => c.checked);
                    const selected = checked.map((c) => paths[parseInt(c.value || '0', 10)]);
                    finish(selected);
                    dialog.close('ok');
                }
            });

            footer.appendChild(cancelBtn);
            footer.appendChild(selectBtn);

            wrapper.appendChild(header);
            wrapper.appendChild(body);
            wrapper.appendChild(footer);

            // Add interactive checkbox behavior with animations
            const checkboxes = Array.from(list.querySelectorAll('input[type="checkbox"]'));
            const selectedCountEl = document.getElementById('selectedCount');

            const updateSelection = () => {
                const checked = checkboxes.filter((c) => c.checked);
                selectedCountEl.textContent = checked.length;

                // Animate count change
                selectedCountEl.style.transform = 'scale(1.2)';
                setTimeout(() => {
                    selectedCountEl.style.transform = 'scale(1)';
                }, 150);
            };

            checkboxes.forEach((cb, idx) => {
                const label = cb.closest('.release-path-item');
                const card = label.querySelector('.path-card');
                const customCheckbox = label.querySelector('.custom-checkbox');
                const checkIcon = label.querySelector('.check-icon');
                const glow = label.querySelector('.card-glow');
                const arrow = label.querySelector('.arrow-indicator');

                // Hover effects
                label.addEventListener('mouseenter', () => {
                    card.style.transform = 'translateY(-2px)';
                    card.style.boxShadow = '0 8px 24px rgba(0, 0, 0, 0.3)';
                    arrow.style.opacity = '1';
                    arrow.style.transform = 'translateX(2px)';
                });

                label.addEventListener('mouseleave', () => {
                    if (!cb.checked) {
                        card.style.transform = 'translateY(0)';
                        card.style.boxShadow = 'none';
                    }
                    arrow.style.opacity = '0.6';
                    arrow.style.transform = 'translateX(0)';
                });

                // Click handling
                label.addEventListener('click', (e) => {
                    e.preventDefault();

                    const checked = checkboxes.filter((c) => c.checked);

                    // Check if trying to select more than 2
                    if (!cb.checked && checked.length >= 2) {
                        // Shake animation for max limit
                        card.style.animation = 'shake 0.4s ease';
                        setTimeout(() => {
                            card.style.animation = '';
                        }, 400);
                        showNotification('warning', EXTENSION_NAME, 'You can select up to 2 versions for comparison.');
                        return;
                    }

                    // Toggle checkbox
                    cb.checked = !cb.checked;

                    // Update visual state
                    if (cb.checked) {
                        customCheckbox.style.background = 'linear-gradient(135deg, var(--ps-accent-blue, var(--accent, #bd93f9)) 0%, var(--accent-blue, #8be9fd) 100%)';
                        customCheckbox.style.borderColor = 'var(--ps-accent-blue, var(--accent, #bd93f9))';
                        checkIcon.style.transform = 'translate(-50%, -50%) scale(1)';
                        card.style.border = '2px solid var(--ps-accent-blue, var(--accent, #bd93f9))';
                        card.style.transform = 'translateY(-2px)';
                        card.style.boxShadow = '0 8px 24px rgba(189, 147, 249, 0.24)';
                        glow.style.opacity = '1';
                    } else {
                        customCheckbox.style.background = 'rgba(0,0,0,0.3)';
                        customCheckbox.style.borderColor = 'var(--ps-accent-blue, var(--accent, #bd93f9))';
                        checkIcon.style.transform = 'translate(-50%, -50%) scale(0)';
                        card.style.border = '2px solid transparent';
                        card.style.transform = 'translateY(0)';
                        card.style.boxShadow = 'none';
                        glow.style.opacity = '0';
                    }

                    updateSelection();
                });
            });

            // Add custom styles and animations
            const style = document.createElement('style');
            style.textContent = `
                @keyframes shake {
                    0%, 100% { transform: translateX(0); }
                    25% { transform: translateX(-8px); }
                    75% { transform: translateX(8px); }
                }
                #selectedCount {
                    transition: transform 0.15s cubic-bezier(0.68, -0.55, 0.265, 1.55);
                }

                /* Beautiful scrollbar for release list */
                .release-path-list::-webkit-scrollbar {
                    width: 10px;
                }
                .release-path-list::-webkit-scrollbar-track {
                    background: rgba(0, 0, 0, 0.2);
                    border-radius: 10px;
                    margin: 4px 0;
                }
                .release-path-list::-webkit-scrollbar-thumb {
                    background: linear-gradient(180deg, var(--accent-blue, #8be9fd) 0%, var(--ps-accent-blue, var(--accent, #bd93f9)) 100%);
                    border-radius: 10px;
                    border: 2px solid rgba(0, 0, 0, 0.2);
                }
                .release-path-list::-webkit-scrollbar-thumb:hover {
                    background: linear-gradient(180deg, var(--accent-blue, #8be9fd) 0%, var(--ps-accent-blue, var(--accent, #bd93f9)) 100%);
                }

                /* Smooth card entrance animation */
                .release-path-item {
                    animation: slideInCard 0.3s ease-out backwards;
                }
                @keyframes slideInCard {
                    from {
                        opacity: 0;
                        transform: translateY(10px);
                    }
                    to {
                        opacity: 1;
                        transform: translateY(0);
                    }
                }
                ${(paths || []).map((_, idx) => `
                    .release-path-item:nth-child(${idx + 1}) {
                        animation-delay: ${idx * 0.05}s;
                    }
                `).join('')}
            `;
            document.head.appendChild(style);

            closeBtn.addEventListener('click', () => {
                finish(null);
                dialog.close('x');
            });

            const onAbort = () => {
                finish(null);
                dialog.close('abort');
            };
            if (signal?.aborted) return onAbort();
            try { signal?.addEventListener?.('abort', onAbort, { once: true }); } catch (_) { }

            dialog.setContent(wrapper);
            dialog.open();
        });
    }

    /**
     * Open compare UI
     */
    function openCompareUI(data) {
        const { routineName } = data;

        // Initialize compare module
        if (!window.AhmadIDEModules.compareWithRelease) {
            window.AhmadIDEModules.compareWithRelease = {};
        }

        // FORCE Compare tool window to open - multiple methods for reliability
        let panelOpened = false;
        try {
            const panel = document.getElementById('comparePanel');
            const isVisible = panel && !panel.classList.contains('hidden');

            if (!isVisible) {
                // Method 1: Use toggleToolWindowPanel function
                if (typeof toggleToolWindowPanel === 'function') {
                    toggleToolWindowPanel('comparePanel', 'bottom');
                    panelOpened = true;
                }

                // Method 2: Fallback - manually click the stripe button
                if (!panelOpened) {
                    const stripeBtn = document.querySelector('.tool-window-stripe-btn[data-panel="comparePanel"]');
                    if (stripeBtn && !stripeBtn.classList.contains('active')) {
                        stripeBtn.click();
                        panelOpened = true;
                    }
                }

                // Method 3: Direct DOM manipulation as last resort
                if (!panelOpened && panel) {
                    panel.classList.remove('hidden');
                    const stripeBtn = document.querySelector('.tool-window-stripe-btn[data-panel="comparePanel"]');
                    if (stripeBtn) {
                        stripeBtn.classList.add('active');
                    }
                    panelOpened = true;
                }
            } else {
                panelOpened = true;
            }
        } catch (e) {
            console.error('[Compare] Failed to open panel:', e);
        }

        window.AhmadIDEModules.compareWithRelease.currentComparison = {
            routineName: String(routineName || ''),
            localContent: String(data.localContent || ''),
            remoteContents: Array.isArray(data.remoteContents) ? data.remoteContents : [],
            selectedPaths: Array.isArray(data.selectedPaths) ? data.selectedPaths : [],
            activeTab: Number(data.activeTab || 0) || 0,
            status: data.status || null
        };

        renderComparePanel();
    }

    /**
     * Render compare panel
     */
    function renderComparePanel() {
        const data = window.AhmadIDEModules.compareWithRelease?.currentComparison;
        if (!data) return;

        // Ensure compare panel content div exists
        let panel = document.getElementById('releaseComparePanel');
        if (!panel) {
            const comparePanel = document.getElementById('comparePanel');
            if (comparePanel) {
                panel = document.createElement('div');
                panel.id = 'releaseComparePanel';
                panel.style.cssText = 'width: 100%; height: 100%;';
                comparePanel.appendChild(panel);
            } else {
                console.error('[Compare] comparePanel not found in DOM');
                return;
            }
        }

        const phase = String(data?.status?.phase || '').toLowerCase();

        if (!data?.routineName) {
            panel.innerHTML = `
                <div class="ps-compare-empty">
                    <p>Right-click on a routine file and select "Compare with Release"</p>
                </div>
            `;
            return;
        }

        if (phase && phase !== 'idle') {
            const msg = String(data?.status?.message || 'Working…');
            const progressSteps = {
                'search': 1,
                'select': 2,
                'fetch': 3
            };
            const currentStep = progressSteps[phase] || 1;
            const progressPercent = (currentStep / 3) * 100;

            panel.innerHTML = `
                <div class="ps-compare-container">
                    <div class="ps-compare-toolbar">
                        <span class="ps-compare-title">${escapeHtml(data.routineName)}.m</span>
                        <div class="ps-spacer"></div>
                        <button class="ps-icon-btn" id="compareCancel" title="Cancel">
                            <svg width="16" height="16" fill="currentColor"><path d="M4 4l8 8m0-8l-8 8" stroke="currentColor" stroke-width="2"/></svg>
                        </button>
                    </div>
                    <div class="ps-compare-loading">
                        <div style="text-align: center; max-width: 400px;">
                            <div style="font-size: 14px; margin-bottom: 16px; color: var(--ps-text-main, var(--text, #f8f8f2));">
                                <svg width="24" height="24" fill="none" stroke="currentColor" stroke-width="2" style="animation: spin 1s linear infinite; display: inline-block; vertical-align: middle; margin-right: 8px;">
                                    <circle cx="12" cy="12" r="10" opacity="0.25"/>
                                    <path d="M12 2a10 10 0 0 1 10 10" opacity="0.75"/>
                                </svg>
                                ${escapeHtml(msg)}
                            </div>
                            <div style="width: 100%; height: 4px; background: var(--ps-bg-hover, #343746); border-radius: 2px; overflow: hidden;">
                                <div style="width: ${progressPercent}%; height: 100%; background: linear-gradient(90deg, var(--ps-accent-blue, var(--accent, #bd93f9)), var(--accent-blue, #8be9fd)); transition: width 0.3s ease;"></div>
                            </div>
                        </div>
                    </div>
                </div>
            `;
            wireCompareUIEvents(panel);
            panel.querySelector('#compareCancel')?.addEventListener('click', () => {
                try { runtime.abortController?.abort?.(); } catch (_) { }
            });
            return;
        }

        if (phase === 'idle' && (!data.remoteContents || data.remoteContents.length === 0)) {
            panel.innerHTML = `
                <div class="ps-compare-empty">
                    <p>No comparison loaded.</p>
                </div>
            `;
            return;
        }

        panel.innerHTML = createCompareUI(data);
        wireCompareUIEvents(panel);
        performDiff(data);
    }

    /**
     * Create compare UI with diff view
     */
    function createCompareUI(data) {
        const { routineName, remoteContents } = data;
        const activeTab = Number(data.activeTab || 0) || 0;

        // Get environment label for active tab
        const activeRemote = remoteContents[activeTab];
        const envLabel = activeRemote ? extractEnvironmentFromPath(activeRemote.path || '') : '';
        const envDisplay = envLabel ? `<span style="color: var(--ps-text-muted, var(--muted, #6272a4)); font-weight: normal; margin-left: 8px;">(${envLabel})</span>` : '';

        return `
            <div class="ps-compare-container">
                <!-- Toolbar -->
                <div class="ps-compare-toolbar">
                    <span class="ps-compare-title">${escapeHtml(routineName)}.m ${envDisplay}</span>
                    <div class="ps-spacer"></div>
                    ${remoteContents.length > 1 ? createTabButtons(remoteContents, activeTab) : ''}
                    <button class="ps-icon-btn" id="comparePrevChange" title="Previous Change (Shift+F7)">
                        <svg width="16" height="16" fill="currentColor"><path d="M12 4l-8 4 8 4V4z"/></svg>
                    </button>
                    <button class="ps-icon-btn" id="compareNextChange" title="Next Change (F7)">
                        <svg width="16" height="16" fill="currentColor"><path d="M4 4l8 4-8 4V4z"/></svg>
                    </button>
                    <button class="ps-icon-btn" id="compareToggleWhitespace" title="Toggle Whitespace">
                        <svg width="16" height="16" fill="currentColor"><path d="M2 4h12M2 8h12M2 12h12" stroke="currentColor" stroke-width="1.5"/></svg>
                    </button>
                    <button class="ps-icon-btn" id="compareRefresh" title="Refresh">
                        <svg width="16" height="16" fill="currentColor"><path d="M13.5 2.5A7 7 0 1 0 15 8h-2a5 5 0 1 1-1.5-3.6L9 6h6V0l-1.5 2.5z"/></svg>
                    </button>
                    <button class="ps-icon-btn" id="compareCopyPath" title="Copy Remote Path">
                        <svg width="16" height="16" fill="currentColor"><path d="M4 1.5H3a2 2 0 0 0-2 2V14a2 2 0 0 0 2 2h10a2 2 0 0 0 2-2V3.5a2 2 0 0 0-2-2h-1"/></svg>
                    </button>
                    <button class="ps-icon-btn" id="compareSettings" title="Settings">
                        <svg width="16" height="16" fill="currentColor"><circle cx="8" cy="8" r="2"/></svg>
                    </button>
                </div>

                <!-- Main content area -->
                <div class="ps-compare-main">
                    <!-- Diff view -->
                    <div class="ps-compare-diff" id="compareDiffView"></div>

                    <!-- Problems sidebar -->
                    <div class="ps-compare-problems" id="compareProblems">
                        <div class="ps-compare-problems-header">
                            <span>Changes</span>
                            <span class="ps-compare-problems-count" id="compareProblemsCount">0</span>
                        </div>
                        <div class="ps-compare-problems-list" id="compareProblemsList"></div>
                    </div>
                </div>

                <!-- Minimap -->
                <div class="ps-compare-minimap" id="compareMinimap"></div>
            </div>
        `;
    }

    /**
     * Create tab buttons for multiple remote versions
     */
    function createTabButtons(remoteContents, activeTab = 0) {
        if (!remoteContents || remoteContents.length <= 1) return '';

        return `
            <div class="ps-compare-tabs" style="display: flex; gap: 4px; margin: 0 12px; padding: 4px; background: rgba(0,0,0,0.2); border-radius: 6px;">
                ${remoteContents.map((content, idx) => {
                    const env = extractEnvironmentFromPath(content.path || '');
                    const label = env || `Release ${String.fromCharCode(65 + idx)}`;
                    const isActive = idx === activeTab;
                    return `
                        <button class="ps-compare-tab ${isActive ? 'active' : ''}" data-tab="${idx}"
                                title="Compare Local with ${escapeHtml(content.path || '')}"
                                style="padding: 6px 16px; font-size: 13px; font-weight: 600; background: ${isActive ? 'var(--ps-bg-selected, var(--selection-bg, #44475a))' : 'transparent'};
                                       color: ${isActive ? 'var(--ps-text-main, var(--text, #f8f8f2))' : 'var(--ps-text-muted, var(--muted, #6272a4))'}; border: none; border-radius: 4px; cursor: pointer;
                                       transition: all 0.2s ease; white-space: nowrap;">
                            ${escapeHtml(label)}
                            ${isActive ? '<span style="margin-left: 6px; color: var(--ps-accent-green, var(--accent-green, #50fa7b));">✓</span>' : ''}
                        </button>
                    `;
                }).join('')}
            </div>
        `;
    }

    /**
     * Wire compare UI events
     */
    function wireCompareUIEvents(panel) {
        if (!panel || panel.dataset.cwrWired === '1') return;
        panel.dataset.cwrWired = '1';

        panel.addEventListener('click', (e) => {
            const t = e.target;

            // Tab switching
            const tab = t?.closest?.('.ps-compare-tab');
            if (tab) {
                e.preventDefault();
                e.stopPropagation();
                const tabIndex = parseInt(tab.dataset.tab || '0', 10);
                console.log('[Compare] Tab clicked:', tabIndex);
                switchCompareTab(tabIndex);
                return;
            }

            if (t?.closest?.('#comparePrevChange')) return navigateToPrevChange();
            if (t?.closest?.('#compareNextChange')) return navigateToNextChange();
            if (t?.closest?.('#compareToggleWhitespace')) return toggleWhitespace();
            if (t?.closest?.('#compareRefresh')) return refreshComparison();
            if (t?.closest?.('#compareCopyPath')) return copyRemotePath();

            const problem = t?.closest?.('.ps-compare-problem-item');
            if (problem) {
                const lineNumber = parseInt(problem.dataset.line || '0', 10) || 0;
                if (lineNumber) scrollToLine(lineNumber, { setIndex: true });
                return;
            }

            const marker = t?.closest?.('.ps-minimap-marker');
            if (marker) {
                const lineNumber = parseInt(marker.dataset.line || '0', 10) || 0;
                if (lineNumber) scrollToLine(lineNumber, { setIndex: true });
            }
        });
    }

    /**
     * Perform diff calculation
     */
    function performDiff(data) {
        const { localContent, remoteContents, activeTab = 0 } = data;
        const remoteContent = remoteContents[activeTab]?.content || '';

        // Use diff library (jsdiff or similar)
        const raw = calculateDiff(localContent, remoteContent);
        const changes = runtime.ignoreWhitespace ? raw.filter((c) => !c.isWhitespaceOnly) : raw;

        try {
            if (window.AhmadIDEModules?.compareWithRelease?.currentComparison) {
                window.AhmadIDEModules.compareWithRelease.currentComparison.changes = changes;
            }
        } catch (_) { }

        // Render everything
        renderDiffView(changes, localContent, remoteContent);
        renderProblemsList(changes);
        const maxLines = Math.max(
            String(localContent || '').split('\n').length,
            String(remoteContent || '').split('\n').length
        );
        renderMinimap(changes, maxLines);

        if (changes.length) {
            if (runtime.currentChangeIndex < 0 || runtime.currentChangeIndex >= changes.length) {
                runtime.currentChangeIndex = 0;
            }
            highlightActiveChange();
        } else {
            runtime.currentChangeIndex = -1;
        }

        // Toolbar state
        const wsBtn = document.getElementById('compareToggleWhitespace');
        wsBtn?.classList.toggle('active', !!runtime.ignoreWhitespace);
    }

    /**
     * Calculate diff using jsdiff
     */
    function calculateDiff(localContent, remoteContent) {
        if (typeof utils.calculateLineDiff === 'function') {
            return utils.calculateLineDiff(localContent, remoteContent);
        }

        // Fallback: simplified line diff
        const localLines = String(localContent || '').replace(/\r\n/g, '\n').split('\n');
        const remoteLines = String(remoteContent || '').replace(/\r\n/g, '\n').split('\n');
        const changes = [];
        const maxLines = Math.max(localLines.length, remoteLines.length);
        for (let i = 0; i < maxLines; i += 1) {
            const localLine = localLines[i] || '';
            const remoteLine = remoteLines[i] || '';
            if (localLine === remoteLine) continue;
            changes.push({
                lineNumber: i + 1,
                type: !remoteLine ? 'added' : !localLine ? 'removed' : 'modified',
                localLine,
                remoteLine,
                isWhitespaceOnly: localLine.trim() === remoteLine.trim()
            });
        }
        return changes;
    }

    /**
     * Render diff view - Show clickable changes list
     */
    function renderDiffView(changes, localContent, remoteContent) {
        const diffView = document.getElementById('compareDiffView');
        if (!diffView) {
            console.warn('[Compare] Diff view container not found');
            return;
        }

        // Render beautiful changes list
        renderChangesList(diffView, changes);

        // Check if we have multiple release versions for 3-column view
        const data = window.AhmadIDEModules?.compareWithRelease?.currentComparison;
        const hasMultipleReleases = data?.remoteContents?.length > 1;

        // Auto-open diff editor immediately
        if (hasMultipleReleases) {
            openMultiColumnDiffEditor(localContent, data.remoteContents);
        } else {
            openInDiffEditor(localContent, remoteContent);
        }
    }

    /**
     * Render clickable changes list with UNBELIEVABLE DESIGN
     */
    function renderChangesList(container, changes) {
        if (!changes || changes.length === 0) {
            container.innerHTML = `
                <div style="display: flex; flex-direction: column; align-items: center; justify-content: center; height: 100%; gap: 20px; background: linear-gradient(135deg, rgba(40, 42, 54, 0.95) 0%, rgba(68, 71, 90, 0.95) 100%);">
                    <div style="position: relative;">
                        <svg width="96" height="96" fill="none" stroke="#50fa7b" stroke-width="2.5" style="filter: drop-shadow(0 0 20px rgba(80, 250, 123, 0.5));">
                            <circle cx="48" cy="48" r="40" opacity="0.2" stroke-width="3"/>
                            <path d="M28 48l14 14 28-28" stroke-linecap="round" stroke-linejoin="round" stroke-width="4"/>
                        </svg>
                        <div style="position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%); width: 100px; height: 100px; background: radial-gradient(circle, rgba(80, 250, 123, 0.2) 0%, transparent 70%); border-radius: 50%; animation: pulse 2s infinite;"></div>
                    </div>
                    <div style="text-align: center;">
                        <div style="font-size: 18px; color: #50fa7b; font-weight: 700; margin-bottom: 8px; text-shadow: 0 0 10px rgba(80, 250, 123, 0.5);">Perfect Match!</div>
                        <div style="font-size: 13px; color: #8be9fd; opacity: 0.9;">No differences detected between files</div>
                    </div>
                </div>
            `;
            return;
        }

        const changesByType = {
            added: changes.filter(c => c.type === 'added'),
            removed: changes.filter(c => c.type === 'removed'),
            modified: changes.filter(c => c.type === 'modified')
        };

        const totalChanges = changes.length;
        const addedPercent = (changesByType.added.length / totalChanges) * 100;
        const removedPercent = (changesByType.removed.length / totalChanges) * 100;
        const modifiedPercent = (changesByType.modified.length / totalChanges) * 100;

        container.innerHTML = `
            <div style="display: flex; flex-direction: column; height: 100%; background: linear-gradient(180deg, #1a1b26 0%, #282a36 100%); overflow: hidden;">

                <!-- Compact Header with Animated Background -->
                <div style="position: relative; padding: 16px 20px; background: linear-gradient(135deg, rgba(139, 233, 253, 0.1) 0%, rgba(189, 147, 249, 0.1) 100%); backdrop-filter: blur(10px); border-bottom: 1px solid rgba(139, 233, 253, 0.2); box-shadow: 0 4px 16px rgba(0, 0, 0, 0.3); overflow: hidden;">
                    <!-- Animated gradient overlay -->
                    <div style="position: absolute; top: 0; left: 0; right: 0; bottom: 0; background: linear-gradient(45deg, rgba(189, 147, 249, 0.05), rgba(139, 233, 253, 0.05), rgba(80, 250, 123, 0.05)); background-size: 300% 300%; animation: gradientShift 8s ease infinite; pointer-events: none;"></div>

                    <!-- Title Section - Compact -->
                    <div style="position: relative; z-index: 1; display: flex; align-items: center; justify-content: space-between; margin-bottom: 12px;">
                        <div style="display: flex; align-items: center; gap: 12px;">
                            <svg width="24" height="24" fill="none" stroke="url(#headerGradient)" stroke-width="2.5" style="filter: drop-shadow(0 0 6px rgba(139, 233, 253, 0.5));">
                                <defs>
                                    <linearGradient id="headerGradient" x1="0%" y1="0%" x2="100%" y2="100%">
                                        <stop offset="0%" style="stop-color:#8be9fd;stop-opacity:1" />
                                        <stop offset="100%" style="stop-color:#bd93f9;stop-opacity:1" />
                                    </linearGradient>
                                </defs>
                                <path d="M6 6h10l3 3h9v16H6V6z" stroke-linecap="round" stroke-linejoin="round"/>
                                <path d="M12 15h8M12 19h5" stroke-linecap="round"/>
                            </svg>
                            <div style="font-size: 16px; font-weight: 700; background: linear-gradient(135deg, #8be9fd 0%, #bd93f9 100%); -webkit-background-clip: text; -webkit-text-fill-color: transparent; background-clip: text; letter-spacing: 0.3px;">Changes Summary</div>
                        </div>
                        <div style="font-size: 11px; color: rgba(139, 233, 253, 0.7); font-weight: 600; padding: 4px 10px; background: rgba(139, 233, 253, 0.1); border-radius: 12px; border: 1px solid rgba(139, 233, 253, 0.3);">${totalChanges} Total</div>
                    </div>

                    <!-- Compact Statistics Cards - Interactive Filters -->
                    <div style="position: relative; z-index: 1; display: grid; grid-template-columns: repeat(4, 1fr); gap: 8px;">
                        <!-- All Changes Card -->
                        <div class="stat-card filter-card active" data-filter="all" style="position: relative; padding: 10px 12px; background: linear-gradient(135deg, rgba(139, 233, 253, 0.15) 0%, rgba(139, 233, 253, 0.05) 100%); border: 2px solid rgba(139, 233, 253, 0.5); border-radius: 8px; overflow: hidden; backdrop-filter: blur(10px); box-shadow: 0 2px 8px rgba(139, 233, 253, 0.3); transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1); cursor: pointer;">
                            <div style="position: absolute; top: 0; right: 0; width: 40px; height: 40px; background: radial-gradient(circle, rgba(139, 233, 253, 0.2) 0%, transparent 70%); border-radius: 50%;"></div>
                            <div style="position: relative; z-index: 1; display: flex; align-items: center; justify-content: space-between;">
                                <div style="display: flex; align-items: center; gap: 6px;">
                                    <div style="width: 8px; height: 8px; border-radius: 50%; background: #8be9fd; box-shadow: 0 0 8px rgba(139, 233, 253, 0.8); animation: pulse 2s infinite;"></div>
                                    <span style="font-size: 10px; color: rgba(139, 233, 253, 0.9); font-weight: 600; text-transform: uppercase; letter-spacing: 0.3px;">All</span>
                                </div>
                                <div style="font-size: 20px; font-weight: 800; color: #8be9fd; line-height: 1; text-shadow: 0 0 12px rgba(139, 233, 253, 0.5);">${totalChanges}</div>
                            </div>
                        </div>

                        <!-- Added Card -->
                        <div class="stat-card filter-card" data-filter="added" style="position: relative; padding: 10px 12px; background: linear-gradient(135deg, rgba(80, 250, 123, 0.15) 0%, rgba(80, 250, 123, 0.05) 100%); border: 2px solid rgba(80, 250, 123, 0.3); border-radius: 8px; overflow: hidden; backdrop-filter: blur(10px); box-shadow: 0 2px 8px rgba(80, 250, 123, 0.2); transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1); cursor: pointer;">
                            <div style="position: absolute; top: 0; right: 0; width: 40px; height: 40px; background: radial-gradient(circle, rgba(80, 250, 123, 0.2) 0%, transparent 70%); border-radius: 50%;"></div>
                            <div style="position: relative; z-index: 1; display: flex; align-items: center; justify-content: space-between;">
                                <div style="display: flex; align-items: center; gap: 6px;">
                                    <div style="width: 8px; height: 8px; border-radius: 50%; background: #50fa7b; box-shadow: 0 0 8px rgba(80, 250, 123, 0.8); animation: pulse 2s infinite;"></div>
                                    <span style="font-size: 10px; color: rgba(80, 250, 123, 0.9); font-weight: 600; text-transform: uppercase; letter-spacing: 0.3px;">Added</span>
                                </div>
                                <div style="font-size: 20px; font-weight: 800; color: #50fa7b; line-height: 1; text-shadow: 0 0 12px rgba(80, 250, 123, 0.5);">${changesByType.added.length}</div>
                            </div>
                        </div>

                        <!-- Removed Card -->
                        <div class="stat-card filter-card" data-filter="removed" style="position: relative; padding: 10px 12px; background: linear-gradient(135deg, rgba(255, 85, 85, 0.15) 0%, rgba(255, 85, 85, 0.05) 100%); border: 2px solid rgba(255, 85, 85, 0.3); border-radius: 8px; overflow: hidden; backdrop-filter: blur(10px); box-shadow: 0 2px 8px rgba(255, 85, 85, 0.2); transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1); cursor: pointer;">
                            <div style="position: absolute; top: 0; right: 0; width: 40px; height: 40px; background: radial-gradient(circle, rgba(255, 85, 85, 0.2) 0%, transparent 70%); border-radius: 50%;"></div>
                            <div style="position: relative; z-index: 1; display: flex; align-items: center; justify-content: space-between;">
                                <div style="display: flex; align-items: center; gap: 6px;">
                                    <div style="width: 8px; height: 8px; border-radius: 50%; background: #ff5555; box-shadow: 0 0 8px rgba(255, 85, 85, 0.8); animation: pulse 2s infinite;"></div>
                                    <span style="font-size: 10px; color: rgba(255, 85, 85, 0.9); font-weight: 600; text-transform: uppercase; letter-spacing: 0.3px;">Removed</span>
                                </div>
                                <div style="font-size: 20px; font-weight: 800; color: #ff5555; line-height: 1; text-shadow: 0 0 12px rgba(255, 85, 85, 0.5);">${changesByType.removed.length}</div>
                            </div>
                        </div>

                        <!-- Modified Card -->
                        <div class="stat-card filter-card" data-filter="modified" style="position: relative; padding: 10px 12px; background: linear-gradient(135deg, rgba(255, 184, 108, 0.15) 0%, rgba(255, 184, 108, 0.05) 100%); border: 2px solid rgba(255, 184, 108, 0.3); border-radius: 8px; overflow: hidden; backdrop-filter: blur(10px); box-shadow: 0 2px 8px rgba(255, 184, 108, 0.2); transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1); cursor: pointer;">
                            <div style="position: absolute; top: 0; right: 0; width: 40px; height: 40px; background: radial-gradient(circle, rgba(255, 184, 108, 0.2) 0%, transparent 70%); border-radius: 50%;"></div>
                            <div style="position: relative; z-index: 1; display: flex; align-items: center; justify-content: space-between;">
                                <div style="display: flex; align-items: center; gap: 6px;">
                                    <div style="width: 8px; height: 8px; border-radius: 50%; background: #ffb86c; box-shadow: 0 0 8px rgba(255, 184, 108, 0.8); animation: pulse 2s infinite;"></div>
                                    <span style="font-size: 10px; color: rgba(255, 184, 108, 0.9); font-weight: 600; text-transform: uppercase; letter-spacing: 0.3px;">Modified</span>
                                </div>
                                <div style="font-size: 20px; font-weight: 800; color: #ffb86c; line-height: 1; text-shadow: 0 0 12px rgba(255, 184, 108, 0.5);">${changesByType.modified.length}</div>
                            </div>
                        </div>
                    </div>
                </div>

                <!-- Changes List with Modern Cards -->
                <div id="changesListContainer" style="flex: 1; overflow-y: auto; padding: 12px; background: #282a36; position: relative;">
                    <!-- Empty state message (hidden by default) -->
                    <div id="emptyFilterMessage" style="display: none; position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%); text-align: center;">
                        <svg width="64" height="64" fill="none" stroke="#6272a4" stroke-width="2" style="margin: 0 auto 16px;">
                            <circle cx="32" cy="32" r="28" opacity="0.2"/>
                            <path d="M20 20l24 24M44 20l-24 24" stroke-linecap="round"/>
                        </svg>
                        <div style="font-size: 16px; color: #6272a4; font-weight: 600; margin-bottom: 8px;">No Changes Found</div>
                        <div style="font-size: 13px; color: #6272a4; opacity: 0.7;">Try selecting a different filter</div>
                    </div>

                    ${changes.map((change, idx) => {
                        const isAdded = change.type === 'added';
                        const isRemoved = change.type === 'removed';
                        const isModified = change.type === 'modified';

                        const borderColor = isAdded ? '#50fa7b' : isRemoved ? '#ff5555' : '#ffb86c';
                        const iconBg = isAdded ? 'linear-gradient(135deg, #50fa7b, #8be9fd)' :
                                      isRemoved ? 'linear-gradient(135deg, #ff5555, #ff79c6)' :
                                      'linear-gradient(135deg, #ffb86c, #f1fa8c)';
                        const icon = isAdded ? '+' : isRemoved ? '−' : '~';
                        const typeLabel = isAdded ? 'ADDED' : isRemoved ? 'REMOVED' : 'MODIFIED';
                        const typeLabelColor = isAdded ? '#50fa7b' : isRemoved ? '#ff5555' : '#ffb86c';

                        return `
                            <div class="change-item-modern visible" data-line="${change.lineNumber}" data-idx="${idx}" data-type="${change.type}"
                                 style="position: relative; margin-bottom: 10px; padding: 0; background: rgba(40, 42, 54, 0.5); border-radius: 8px; cursor: pointer; transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1); overflow: hidden; box-shadow: 0 2px 8px rgba(0, 0, 0, 0.3); border: 1px solid rgba(68, 71, 90, 0.5);">

                                <!-- Header Bar -->
                                <div style="display: flex; align-items: center; gap: 10px; padding: 8px 12px; background: linear-gradient(135deg, rgba(${isAdded ? '80, 250, 123' : isRemoved ? '255, 85, 85' : '255, 184, 108'}, 0.15) 0%, rgba(${isAdded ? '80, 250, 123' : isRemoved ? '255, 85, 85' : '255, 184, 108'}, 0.05) 100%); border-bottom: 1px solid rgba(68, 71, 90, 0.5);">
                                    <!-- Icon Badge -->
                                    <div style="flex-shrink: 0; width: 24px; height: 24px; border-radius: 6px; background: ${iconBg}; color: #1a1b26; display: flex; align-items: center; justify-content: center; font-weight: 800; font-size: 14px; box-shadow: 0 2px 6px ${borderColor}40;">
                                        ${icon}
                                    </div>

                                    <!-- Type Badge & Line Number -->
                                    <span style="padding: 2px 8px; background: ${borderColor}30; border: 1px solid ${borderColor}50; border-radius: 4px; font-size: 9px; font-weight: 700; color: ${typeLabelColor}; letter-spacing: 0.3px; text-shadow: 0 0 6px ${borderColor}60;">${typeLabel}</span>
                                    <span style="font-size: 10px; color: #8be9fd; font-weight: 600;">Line ${change.lineNumber}</span>

                                    <!-- Arrow indicator -->
                                    <div style="margin-left: auto; opacity: 0.5;">
                                        <svg width="14" height="14" fill="none" stroke="${borderColor}" stroke-width="2.5">
                                            <path d="M5 3l4 4-4 4" stroke-linecap="round" stroke-linejoin="round"/>
                                        </svg>
                                    </div>
                                </div>

                                <!-- Side-by-Side Code Comparison -->
                                <div style="display: grid; grid-template-columns: ${isAdded ? '1fr' : isRemoved ? '1fr' : '1fr 2px 1fr'}; min-height: 60px;">

                                    ${!isAdded ? `
                                        <!-- Release Version (Left) -->
                                        <div class="code-section" style="padding: 10px 12px; background: rgba(255, 85, 85, 0.05); position: relative; transition: background 0.3s ease;">
                                            <div style="position: absolute; top: 6px; left: 6px; font-size: 8px; font-weight: 700; color: rgba(255, 85, 85, 0.6); text-transform: uppercase; letter-spacing: 0.5px; text-shadow: 0 0 4px rgba(255, 85, 85, 0.3);">Release</div>
                                            <div style="font-family: 'Fira Code', 'Consolas', monospace; font-size: 11px; color: ${isRemoved ? '#ff9999' : '#f8f8f2'}; white-space: pre-wrap; overflow-wrap: break-word; line-height: 1.5; padding-top: 14px; ${isRemoved ? 'opacity: 0.8; text-decoration: line-through; text-decoration-color: rgba(255, 85, 85, 0.5); text-decoration-thickness: 2px;' : ''}">
                                                ${escapeHtml(change.remoteLine || '')}
                                            </div>
                                        </div>
                                    ` : ''}

                                    ${isModified ? `
                                        <!-- Divider with Arrow -->
                                        <div style="background: linear-gradient(180deg, ${borderColor}40 0%, ${borderColor}20 50%, ${borderColor}40 100%); position: relative;">
                                            <div style="position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%); width: 18px; height: 18px; background: #282a36; border-radius: 50%; display: flex; align-items: center; justify-content: center; border: 2px solid ${borderColor}; box-shadow: 0 0 8px ${borderColor}60;">
                                                <svg width="12" height="12" fill="none" stroke="${borderColor}" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
                                                    <path d="M3 6h6M6 3l3 3-3 3"/>
                                                </svg>
                                            </div>
                                        </div>
                                    ` : ''}

                                    ${!isRemoved ? `
                                        <!-- Local Version (Right) -->
                                        <div class="code-section" style="padding: 10px 12px; background: rgba(80, 250, 123, 0.05); position: relative; transition: background 0.3s ease;">
                                            <div style="position: absolute; top: 6px; ${isModified ? 'left' : 'right'}: 6px; font-size: 8px; font-weight: 700; color: rgba(80, 250, 123, 0.6); text-transform: uppercase; letter-spacing: 0.5px; text-shadow: 0 0 4px rgba(80, 250, 123, 0.3);">Local</div>
                                            <div style="font-family: 'Fira Code', 'Consolas', monospace; font-size: 11px; color: ${isAdded ? '#50fa7b' : '#f8f8f2'}; white-space: pre-wrap; overflow-wrap: break-word; line-height: 1.5; padding-top: 14px; ${isAdded ? 'font-weight: 600;' : ''}">
                                                ${escapeHtml(change.localLine || '')}
                                            </div>
                                        </div>
                                    ` : ''}

                                </div>
                            </div>
                        `;
                    }).join('')}
                </div>
            </div>

            <style>
                @keyframes pulse {
                    0%, 100% { opacity: 1; transform: scale(1); }
                    50% { opacity: 0.7; transform: scale(1.1); }
                }

                @keyframes gradientShift {
                    0% { background-position: 0% 50%; }
                    50% { background-position: 100% 50%; }
                    100% { background-position: 0% 50%; }
                }

                @keyframes slideIn {
                    from {
                        opacity: 0;
                        transform: translateY(-10px);
                    }
                    to {
                        opacity: 1;
                        transform: translateY(0);
                    }
                }

                .stat-card:hover {
                    transform: translateY(-2px) scale(1.02);
                    box-shadow: 0 4px 16px rgba(139, 233, 253, 0.4);
                }

                /* Active filter badge */
                .filter-card.active {
                    border-width: 3px;
                    transform: scale(1.03);
                    box-shadow: 0 4px 20px rgba(139, 233, 253, 0.5);
                }

                .filter-card.active[data-filter="added"] {
                    box-shadow: 0 6px 24px rgba(80, 250, 123, 0.6);
                }

                .filter-card.active[data-filter="removed"] {
                    box-shadow: 0 6px 24px rgba(255, 85, 85, 0.6);
                }

                .filter-card.active[data-filter="modified"] {
                    box-shadow: 0 6px 24px rgba(255, 184, 108, 0.6);
                }

                /* Hidden change items */
                .change-item-modern.hidden {
                    display: none !important;
                }

                /* Visible change items animation */
                .change-item-modern.visible {
                    animation: slideIn 0.3s ease-out forwards;
                }

                .change-item-modern:hover {
                    transform: translateY(-2px) scale(1.01);
                    box-shadow: 0 6px 24px rgba(139, 233, 253, 0.3);
                    border-color: rgba(139, 233, 253, 0.5);
                }

                .change-item-modern:active {
                    transform: translateY(0) scale(0.99);
                }

                /* Highlight code sections on hover */
                .change-item-modern:hover .code-section {
                    background: rgba(68, 71, 90, 0.3);
                }
            </style>
        `;

        // Add click handlers with amazing animations
        container.querySelectorAll('.change-item-modern').forEach(item => {
            item.addEventListener('click', () => {
                const lineNumber = parseInt(item.dataset.line || '0', 10);
                if (lineNumber) {
                    // Try both diff editor types
                    if (window._monacoCompareEditor) {
                        window._monacoCompareEditor.revealLineInCenter(lineNumber);
                    } else if (window._multiColumnEditors && window._multiColumnEditors.length > 0) {
                        // For multi-column, scroll all editors
                        window._multiColumnEditors.forEach(editor => {
                            try {
                                editor.revealLineInCenter(lineNumber);
                            } catch (e) {
                                console.log('[Compare] Could not scroll editor:', e);
                            }
                        });
                    }

                    // Pulse effect on click with glow
                    item.style.transform = 'translateY(-4px) scale(1.02)';
                    item.style.boxShadow = '0 12px 40px rgba(139, 233, 253, 0.6)';
                    item.style.borderColor = 'rgba(139, 233, 253, 0.8)';

                    setTimeout(() => {
                        item.style.transform = '';
                        item.style.boxShadow = '';
                        item.style.borderColor = '';
                    }, 400);
                }
            });
        });

        // Add filter functionality - Interactive badges
        const filterCards = container.querySelectorAll('.filter-card');
        const changeItems = container.querySelectorAll('.change-item-modern');
        const emptyMessage = container.querySelector('#emptyFilterMessage');

        filterCards.forEach(card => {
            card.addEventListener('click', () => {
                const filterType = card.dataset.filter;

                // Remove active class from all cards
                filterCards.forEach(c => c.classList.remove('active'));

                // Add active class to clicked card
                card.classList.add('active');

                // Filter changes with smooth animation
                let visibleCount = 0;
                changeItems.forEach((item, index) => {
                    const itemType = item.dataset.type;
                    const shouldShow = filterType === 'all' || itemType === filterType;

                    if (shouldShow) {
                        visibleCount++;
                        // Stagger animation for smooth reveal
                        setTimeout(() => {
                            item.classList.remove('hidden');
                            item.classList.add('visible');
                        }, index * 30); // 30ms delay between each item
                    } else {
                        item.classList.add('hidden');
                        item.classList.remove('visible');
                    }
                });

                // Show/hide empty state message
                if (emptyMessage) {
                    if (visibleCount === 0) {
                        setTimeout(() => {
                            emptyMessage.style.display = 'block';
                            emptyMessage.style.animation = 'slideIn 0.3s ease-out';
                        }, 200);
                    } else {
                        emptyMessage.style.display = 'none';
                    }
                }

                // Add ripple effect to clicked card
                const ripple = document.createElement('div');
                ripple.style.cssText = `
                    position: absolute;
                    border-radius: 50%;
                    background: rgba(255, 255, 255, 0.3);
                    width: 10px;
                    height: 10px;
                    animation: ripple 0.6s ease-out;
                    pointer-events: none;
                    left: 50%;
                    top: 50%;
                    transform: translate(-50%, -50%);
                `;
                card.appendChild(ripple);

                setTimeout(() => ripple.remove(), 600);
            });
        });

        // Add ripple animation
        const style = document.createElement('style');
        style.textContent = `
            @keyframes ripple {
                from {
                    width: 10px;
                    height: 10px;
                    opacity: 1;
                }
                to {
                    width: 200px;
                    height: 200px;
                    opacity: 0;
                }
            }
        `;
        document.head.appendChild(style);
    }

    /**
     * Smart Diff Algorithm - Better than VimDiff
     * Features:
     * - Character-level diff detection
     * - Syntax-aware (MUMPS labels, commands, variables)
     * - Smart whitespace handling
     * - Context-aware change classification
     */
    function computeSmartDiff(remoteLines, localLines, releaseIdx) {
        const releaseDecorations = [];
        const localDecorations = [];
        const maxLines = Math.max(remoteLines.length, localLines.length);

        // Build LCS (Longest Common Subsequence) table for better line matching
        const lcsTable = buildLCSTable(remoteLines, localLines);

        for (let i = 0; i < maxLines; i++) {
            const lineNumber = i + 1;
            const remoteLine = remoteLines[i] || '';
            const localLine = localLines[i] || '';

            // Skip identical lines
            if (remoteLine === localLine) continue;

            // Normalize for smart comparison
            const remoteNorm = remoteLine.trim();
            const localNorm = localLine.trim();

            // Case 1: Line added in local (not in release)
            if (!remoteLine && localLine) {
                localDecorations.push({
                    range: new monaco.Range(lineNumber, 1, lineNumber, 1),
                    options: {
                        isWholeLine: true,
                        className: 'diff-line-added',
                        glyphMarginClassName: 'diff-glyph-added',
                        linesDecorationsClassName: 'diff-line-decoration-added',
                        overviewRuler: {
                            color: '#50fa7b',
                            position: monaco.editor.OverviewRulerLane.Full
                        }
                    }
                });
                continue;
            }

            // Case 2: Line removed in local (exists in release)
            if (remoteLine && !localLine) {
                releaseDecorations.push({
                    range: new monaco.Range(lineNumber, 1, lineNumber, 1),
                    options: {
                        isWholeLine: true,
                        className: 'diff-line-removed',
                        glyphMarginClassName: 'diff-glyph-removed',
                        linesDecorationsClassName: 'diff-line-decoration-removed',
                        overviewRuler: {
                            color: '#ff5555',
                            position: monaco.editor.OverviewRulerLane.Full
                        }
                    }
                });
                continue;
            }

            // Case 3: Line modified - Detect type of modification
            if (remoteLine !== localLine) {
                // Whitespace-only change
                if (remoteNorm === localNorm) {
                    const whitespaceDeco = {
                        range: new monaco.Range(lineNumber, 1, lineNumber, 1),
                        options: {
                            isWholeLine: true,
                            className: 'diff-line-whitespace',
                            glyphMarginClassName: 'diff-glyph-whitespace',
                            overviewRuler: {
                                color: '#f1fa8c',
                                position: monaco.editor.OverviewRulerLane.Full
                            }
                        }
                    };
                    releaseDecorations.push(whitespaceDeco);
                    localDecorations.push(whitespaceDeco);
                    continue;
                }

                // Syntax-aware detection
                const changeType = detectMUMPSChangeType(remoteLine, localLine);

                // Character-level diff for modified lines
                const charDiffs = computeCharacterDiff(remoteLine, localLine);

                // Release editor: show what was changed/removed
                releaseDecorations.push({
                    range: new monaco.Range(lineNumber, 1, lineNumber, 1),
                    options: {
                        isWholeLine: true,
                        className: `diff-line-modified diff-${changeType}`,
                        glyphMarginClassName: 'diff-glyph-modified',
                        linesDecorationsClassName: 'diff-line-decoration-modified',
                        overviewRuler: {
                            color: changeType === 'code' ? '#ff79c6' : '#ffb86c',
                            position: monaco.editor.OverviewRulerLane.Full
                        }
                    }
                });

                // Add inline character decorations for release
                if (charDiffs.removed.length > 0) {
                    charDiffs.removed.forEach(({ start, end }) => {
                        if (start >= 0 && end > start && end <= remoteLine.length) {
                            releaseDecorations.push({
                                range: new monaco.Range(lineNumber, start + 1, lineNumber, end + 1),
                                options: {
                                    className: 'diff-char-removed',
                                    inlineClassName: 'diff-char-removed-inline'
                                }
                            });
                        }
                    });
                }

                // Local editor: show what was added/changed
                localDecorations.push({
                    range: new monaco.Range(lineNumber, 1, lineNumber, 1),
                    options: {
                        isWholeLine: true,
                        className: `diff-line-modified diff-${changeType}`,
                        glyphMarginClassName: 'diff-glyph-modified',
                        linesDecorationsClassName: 'diff-line-decoration-modified',
                        overviewRuler: {
                            color: changeType === 'code' ? '#8be9fd' : '#ffb86c',
                            position: monaco.editor.OverviewRulerLane.Full
                        }
                    }
                });

                // Add inline character decorations for local
                if (charDiffs.added.length > 0) {
                    charDiffs.added.forEach(({ start, end }) => {
                        if (start >= 0 && end > start && end <= localLine.length) {
                            localDecorations.push({
                                range: new monaco.Range(lineNumber, start + 1, lineNumber, end + 1),
                                options: {
                                    className: 'diff-char-added',
                                    inlineClassName: 'diff-char-added-inline'
                                }
                            });
                        }
                    });
                }
            }
        }

        return { releaseDecorations, localDecorations };
    }

    /**
     * Build LCS table for better line matching
     */
    function buildLCSTable(arr1, arr2) {
        const m = arr1.length;
        const n = arr2.length;
        const table = Array(m + 1).fill(null).map(() => Array(n + 1).fill(0));

        for (let i = 1; i <= m; i++) {
            for (let j = 1; j <= n; j++) {
                if (arr1[i - 1] === arr2[j - 1]) {
                    table[i][j] = table[i - 1][j - 1] + 1;
                } else {
                    table[i][j] = Math.max(table[i - 1][j], table[i][j - 1]);
                }
            }
        }
        return table;
    }

    /**
     * Detect type of change in MUMPS code
     */
    function detectMUMPSChangeType(oldLine, newLine) {
        // Check if label changed (starts with alphanumeric, no leading space)
        const oldIsLabel = /^[A-Za-z0-9]/.test(oldLine);
        const newIsLabel = /^[A-Za-z0-9]/.test(newLine);
        if (oldIsLabel || newIsLabel) return 'label';

        // Check if command changed (MUMPS commands like SET, DO, IF, etc.)
        const mumpsCommands = /\b(SET|DO|IF|ELSE|FOR|QUIT|KILL|NEW|WRITE|READ|GOTO|MERGE|LOCK)\b/i;
        if (mumpsCommands.test(oldLine) || mumpsCommands.test(newLine)) return 'code';

        // Check if variable assignment
        if (/SET\s+\w+/.test(oldLine) || /SET\s+\w+/.test(newLine)) return 'variable';

        // Check if comment changed
        if (oldLine.includes(';') || newLine.includes(';')) return 'comment';

        return 'code';
    }

    /**
     * Character-level diff using Myers algorithm (simplified)
     */
    function computeCharacterDiff(oldStr, newStr) {
        const removed = [];
        const added = [];

        // Simple character-by-character comparison with grouping
        let i = 0, j = 0;
        let removeStart = -1, addStart = -1;

        while (i < oldStr.length || j < newStr.length) {
            if (i < oldStr.length && j < newStr.length && oldStr[i] === newStr[j]) {
                // Characters match - close any open ranges
                if (removeStart >= 0) {
                    removed.push({ start: removeStart, end: i });
                    removeStart = -1;
                }
                if (addStart >= 0) {
                    added.push({ start: addStart, end: j });
                    addStart = -1;
                }
                i++;
                j++;
            } else if (i < oldStr.length && (j >= newStr.length || oldStr[i] !== newStr[j])) {
                // Character removed
                if (removeStart < 0) removeStart = i;
                i++;
            } else if (j < newStr.length) {
                // Character added
                if (addStart < 0) addStart = j;
                j++;
            }
        }

        // Close any remaining ranges
        if (removeStart >= 0) removed.push({ start: removeStart, end: oldStr.length });
        if (addStart >= 0) added.push({ start: addStart, end: newStr.length });

        return { removed, added };
    }

    /**
     * Add inline transfer arrows between diff editors for easy code transfer
     */
    function addInlineTransferArrows(diffEditor, originalModel, modifiedModel) {
        if (!diffEditor || !originalModel || !modifiedModel) return;

        const originalEditor = diffEditor.getOriginalEditor();
        const modifiedEditor = diffEditor.getModifiedEditor();

        // Get the diff editor container
        const diffContainer = document.getElementById('monacoCompareDiffEditor');
        if (!diffContainer) return;

        // Remove existing arrows
        const existingArrows = diffContainer.querySelectorAll('.inline-transfer-arrows');
        existingArrows.forEach(el => el.remove());

        // Create arrows container overlay
        const arrowsOverlay = document.createElement('div');
        arrowsOverlay.className = 'inline-transfer-arrows';
        arrowsOverlay.style.cssText = `
            position: absolute;
            top: 92px;
            left: 50%;
            transform: translateX(-50%);
            width: 40px;
            height: calc(100% - 92px);
            pointer-events: none;
            z-index: 120;
        `;
        diffContainer.appendChild(arrowsOverlay);

        // Function to update arrows based on diff changes
        function updateArrows() {
            arrowsOverlay.innerHTML = '';

            const lineChanges = diffEditor.getLineChanges() || [];
            if (lineChanges.length === 0) return;

            lineChanges.forEach((change, idx) => {
                const modifiedLineNumber = change.modifiedStartLineNumber || 1;
                const originalLineNumber = change.originalStartLineNumber || 1;

                // Get the center line number for arrow placement
                const centerLine = modifiedLineNumber || originalLineNumber;

                // Get pixel position for this line
                const lineTop = modifiedEditor.getTopForLineNumber(centerLine);
                const scrollTop = modifiedEditor.getScrollTop();
                const yPos = lineTop - scrollTop;

                // Only show arrows for visible lines
                if (yPos < 0 || yPos > modifiedEditor.getLayoutInfo().height) return;

                // Create arrow buttons container
                const arrowContainer = document.createElement('div');
                arrowContainer.style.cssText = `
                    position: absolute;
                    top: ${yPos}px;
                    left: 0;
                    width: 100%;
                    display: flex;
                    gap: 2px;
                    justify-content: center;
                    align-items: center;
                    pointer-events: auto;
                `;

                // Left arrow (copy from local to release)
                const leftArrow = document.createElement('button');
                leftArrow.innerHTML = `
                    <svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="3">
                        <path d="M19 12H5M5 12l7-7M5 12l7 7"/>
                    </svg>
                `;
                leftArrow.style.cssText = `
                    width: 20px;
                    height: 20px;
                    padding: 2px;
                    background: rgba(139, 233, 253, 0.85);
                    border: 1px solid rgba(139, 233, 253, 1);
                    border-radius: 3px;
                    cursor: pointer;
                    display: flex;
                    align-items: center;
                    justify-content: center;
                    transition: all 0.15s ease;
                    color: #282a36;
                    box-shadow: 0 2px 6px rgba(0,0,0,0.3);
                `;
                leftArrow.title = 'Copy Local → Release';
                leftArrow.addEventListener('click', () => {
                    copyCodeBetweenEditors(change, modifiedModel, originalModel, modifiedEditor, originalEditor, 'toRelease');
                });
                leftArrow.addEventListener('mouseenter', () => {
                    leftArrow.style.background = 'rgba(139, 233, 253, 1)';
                    leftArrow.style.transform = 'scale(1.15)';
                });
                leftArrow.addEventListener('mouseleave', () => {
                    leftArrow.style.background = 'rgba(139, 233, 253, 0.85)';
                    leftArrow.style.transform = 'scale(1)';
                });

                // Right arrow (copy from release to local)
                const rightArrow = document.createElement('button');
                rightArrow.innerHTML = `
                    <svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="3">
                        <path d="M5 12h14M19 12l-7 7M19 12l-7-7"/>
                    </svg>
                `;
                rightArrow.style.cssText = `
                    width: 20px;
                    height: 20px;
                    padding: 2px;
                    background: rgba(80, 250, 123, 0.85);
                    border: 1px solid rgba(80, 250, 123, 1);
                    border-radius: 3px;
                    cursor: pointer;
                    display: flex;
                    align-items: center;
                    justify-content: center;
                    transition: all 0.15s ease;
                    color: #282a36;
                    box-shadow: 0 2px 6px rgba(0,0,0,0.3);
                `;
                rightArrow.title = 'Copy Release → Local';
                rightArrow.addEventListener('click', () => {
                    copyCodeBetweenEditors(change, originalModel, modifiedModel, originalEditor, modifiedEditor, 'toLocal');
                });
                rightArrow.addEventListener('mouseenter', () => {
                    rightArrow.style.background = 'rgba(80, 250, 123, 1)';
                    rightArrow.style.transform = 'scale(1.15)';
                });
                rightArrow.addEventListener('mouseleave', () => {
                    rightArrow.style.background = 'rgba(80, 250, 123, 0.85)';
                    rightArrow.style.transform = 'scale(1)';
                });

                arrowContainer.appendChild(leftArrow);
                arrowContainer.appendChild(rightArrow);
                arrowsOverlay.appendChild(arrowContainer);
            });
        }

        // Helper function to copy code between editors
        function copyCodeBetweenEditors(change, sourceModel, targetModel, sourceEditor, targetEditor, direction) {
            const sourceStartLine = direction === 'toLocal' ? change.originalStartLineNumber : change.modifiedStartLineNumber;
            const sourceEndLine = direction === 'toLocal' ? change.originalEndLineNumber : change.modifiedEndLineNumber;
            const targetStartLine = direction === 'toLocal' ? change.modifiedStartLineNumber : change.originalStartLineNumber;
            const targetEndLine = direction === 'toLocal' ? change.modifiedEndLineNumber : change.originalEndLineNumber;

            if (!sourceStartLine || !sourceEndLine) return;

            // Get source code
            const sourceCode = sourceModel.getValueInRange({
                startLineNumber: sourceStartLine,
                startColumn: 1,
                endLineNumber: sourceEndLine,
                endColumn: sourceModel.getLineMaxColumn(sourceEndLine)
            });

            // Replace target code
            if (targetStartLine && targetEndLine) {
                const edit = {
                    range: {
                        startLineNumber: targetStartLine,
                        startColumn: 1,
                        endLineNumber: targetEndLine,
                        endColumn: targetModel.getLineMaxColumn(targetEndLine)
                    },
                    text: sourceCode
                };
                targetModel.pushEditOperations([], [edit], () => null);
            } else {
                // Insert at the target position
                const insertLine = targetStartLine || 1;
                const edit = {
                    range: {
                        startLineNumber: insertLine,
                        startColumn: 1,
                        endLineNumber: insertLine,
                        endColumn: 1
                    },
                    text: sourceCode + '\n'
                };
                targetModel.pushEditOperations([], [edit], () => null);
            }

            // Show toast
            const dirLabel = direction === 'toLocal' ? 'Local' : 'Release';
            showTransferToast(`Copied to ${dirLabel}`, direction);

            // Update arrows after a short delay to allow diff to recalculate
            setTimeout(updateArrows, 100);
        }

        // Show toast for transfers
        function showTransferToast(message, direction) {
            const toast = document.createElement('div');
            const color = direction === 'toLocal' ? '#50fa7b' : '#8be9fd';
            toast.style.cssText = `
                position: fixed;
                bottom: 24px;
                left: 50%;
                transform: translateX(-50%);
                background: rgba(40, 42, 54, 0.95);
                color: ${color};
                padding: 10px 18px;
                border-radius: 6px;
                border: 1px solid ${color};
                box-shadow: 0 4px 16px rgba(0, 0, 0, 0.5);
                z-index: 200;
                font-size: 13px;
                font-weight: 600;
                animation: slideInUp 0.3s ease-out;
            `;
            toast.textContent = message;
            document.body.appendChild(toast);

            setTimeout(() => {
                toast.style.animation = 'slideOutDown 0.3s ease-in';
                setTimeout(() => toast.remove(), 300);
            }, 1500);
        }

        // Update arrows initially and on scroll/model change
        setTimeout(updateArrows, 300);
        modifiedEditor.onDidScrollChange(updateArrows);
        originalEditor.onDidScrollChange(updateArrows);
        modifiedModel.onDidChangeContent(() => setTimeout(updateArrows, 100));
        originalModel.onDidChangeContent(() => setTimeout(updateArrows, 100));
    }

    /**
     * Add compact horizontal toolbar with view controls (view-only, no saving)
     */
    function addDiffEditorToolbar(container, diffEditor, multiEditors) {
        // Remove existing toolbar if any
        const existingToolbar = container.querySelector('#diffToolbar');
        if (existingToolbar) existingToolbar.remove();

        const editors = multiEditors || (diffEditor ? [diffEditor.getOriginalEditor(), diffEditor.getModifiedEditor()] : []);

        // Create compact horizontal toolbar
        const toolbar = document.createElement('div');
        toolbar.id = 'diffToolbar';
        toolbar.style.cssText = `
            position: absolute;
            top: 46px;
            right: 12px;
            z-index: 151;
            background: rgba(40, 42, 54, 0.92);
            backdrop-filter: blur(8px);
            border: 1px solid rgba(139, 233, 253, 0.25);
            border-radius: 6px;
            padding: 4px 6px;
            box-shadow: 0 2px 12px rgba(0, 0, 0, 0.4);
            display: flex;
            align-items: center;
            gap: 3px;
            transition: all 0.2s ease;
        `;

        toolbar.innerHTML = `
            <style>
                #diffToolbar .tb-btn {
                    width: 26px;
                    height: 26px;
                    border: none;
                    background: rgba(68, 71, 90, 0.4);
                    color: #8be9fd;
                    border-radius: 4px;
                    cursor: pointer;
                    display: flex;
                    align-items: center;
                    justify-content: center;
                    transition: all 0.15s ease;
                    position: relative;
                    font-size: 11px;
                    font-weight: 600;
                }

                #diffToolbar .tb-btn:hover {
                    background: rgba(139, 233, 253, 0.25);
                    transform: translateY(-1px);
                    box-shadow: 0 2px 8px rgba(139, 233, 253, 0.3);
                }

                #diffToolbar .tb-btn.active {
                    background: rgba(80, 250, 123, 0.25);
                    color: #50fa7b;
                }

                #diffToolbar .tb-btn svg {
                    width: 14px;
                    height: 14px;
                }

                #diffToolbar .tb-sep {
                    width: 1px;
                    height: 20px;
                    background: rgba(139, 233, 253, 0.15);
                    margin: 0 2px;
                }

                #diffToolbar .tb-tip {
                    position: absolute;
                    bottom: 100%;
                    left: 50%;
                    transform: translateX(-50%);
                    margin-bottom: 6px;
                    padding: 3px 7px;
                    background: rgba(40, 42, 54, 0.95);
                    color: #8be9fd;
                    border-radius: 3px;
                    font-size: 10px;
                    white-space: nowrap;
                    pointer-events: none;
                    opacity: 0;
                    transition: opacity 0.15s ease;
                    border: 1px solid rgba(139, 233, 253, 0.25);
                    box-shadow: 0 2px 6px rgba(0,0,0,0.3);
                }

                #diffToolbar .tb-btn:hover .tb-tip {
                    opacity: 1;
                }
            </style>

            <!-- Font Size -->
            <button class="tb-btn" id="fontSizeIncrease">
                <span class="tb-tip">Font +</span>
                A+
            </button>
            <button class="tb-btn" id="fontSizeDecrease">
                <span class="tb-tip">Font -</span>
                A-
            </button>

            <div class="tb-sep"></div>

            <!-- Navigation -->
            <button class="tb-btn" id="prevDiff">
                <span class="tb-tip">Previous</span>
                <svg fill="none" stroke="currentColor" stroke-width="2.5" stroke-linecap="round" viewBox="0 0 24 24">
                    <path d="M15 18l-6-6 6-6"/>
                </svg>
            </button>
            <button class="tb-btn" id="nextDiff">
                <span class="tb-tip">Next</span>
                <svg fill="none" stroke="currentColor" stroke-width="2.5" stroke-linecap="round" viewBox="0 0 24 24">
                    <path d="M9 18l6-6-6-6"/>
                </svg>
            </button>

            <div class="tb-sep"></div>

            <!-- Toggles -->
            <button class="tb-btn active" id="toggleDiffColors">
                <span class="tb-tip">Diff Colors</span>
                <svg fill="none" stroke="currentColor" stroke-width="2" viewBox="0 0 24 24">
                    <circle cx="12" cy="12" r="9"/><path d="M12 3v18M3 12h18"/>
                </svg>
            </button>
            <button class="tb-btn" id="toggleWhitespace">
                <span class="tb-tip">Whitespace</span>
                <svg fill="none" stroke="currentColor" stroke-width="2.5" viewBox="0 0 24 24">
                    <circle cx="6" cy="12" r="1.5"/><circle cx="12" cy="12" r="1.5"/><circle cx="18" cy="12" r="1.5"/>
                </svg>
            </button>
            <button class="tb-btn active" id="toggleLineNumbers">
                <span class="tb-tip">Line #</span>
                <svg fill="none" stroke="currentColor" stroke-width="2" viewBox="0 0 24 24">
                    <path d="M3 3h3v3H3zM10 4h11M10 12h11M3 11h3v3H3z"/>
                </svg>
            </button>
            <button class="tb-btn active" id="toggleMinimap">
                <span class="tb-tip">Minimap</span>
                <svg fill="none" stroke="currentColor" stroke-width="2" viewBox="0 0 24 24">
                    <rect x="3" y="3" width="7" height="18" rx="1"/><rect x="14" y="3" width="7" height="18" rx="1"/>
                </svg>
            </button>
            <button class="tb-btn" id="toggleWordWrap">
                <span class="tb-tip">Wrap</span>
                <svg fill="none" stroke="currentColor" stroke-width="2" viewBox="0 0 24 24">
                    <path d="M4 6h16M4 12h12M20 12l-3 3m3-3l-3-3"/>
                </svg>
            </button>

            <div class="tb-sep"></div>

            <!-- Copy -->
            <button class="tb-btn" id="copyLeft">
                <span class="tb-tip">Copy Release</span>
                <svg fill="none" stroke="currentColor" stroke-width="2" viewBox="0 0 24 24">
                    <rect x="9" y="9" width="13" height="13" rx="2"/>
                    <path d="M5 15H4a2 2 0 01-2-2V4a2 2 0 012-2h9a2 2 0 012 2v1"/>
                </svg>
            </button>
            <button class="tb-btn" id="copyRight">
                <span class="tb-tip">Copy Local</span>
                <svg fill="none" stroke="currentColor" stroke-width="2" viewBox="0 0 24 24">
                    <path d="M16 4h2a2 2 0 012 2v14a2 2 0 01-2 2H6a2 2 0 01-2-2V6a2 2 0 012-2h2"/>
                    <rect x="8" y="2" width="8" height="4" rx="1"/>
                </svg>
            </button>

            <div class="tb-sep"></div>

            <!-- Reset -->
            <button class="tb-btn" id="resetView">
                <span class="tb-tip">Reset</span>
                <svg fill="none" stroke="currentColor" stroke-width="2" viewBox="0 0 24 24">
                    <path d="M3 12a9 9 0 009-9 9 9 0 019 9 9 9 0 01-9 9"/>
                    <path d="M3 12l3-3m-3 3l3 3"/>
                </svg>
            </button>
        `;

        container.appendChild(toolbar);

        // State management
        const state = {
            fontSize: 13,
            diffColorsEnabled: true,
            whitespaceVisible: false,
            lineNumbersVisible: true,
            minimapVisible: true,
            wordWrapEnabled: false
        };

        // Font Size Controls
        toolbar.querySelector('#fontSizeIncrease').addEventListener('click', () => {
            state.fontSize = Math.min(state.fontSize + 1, 24);
            editors.forEach(ed => ed.updateOptions({ fontSize: state.fontSize }));
            showToast(`Font: ${state.fontSize}px`);
        });

        toolbar.querySelector('#fontSizeDecrease').addEventListener('click', () => {
            state.fontSize = Math.max(state.fontSize - 1, 8);
            editors.forEach(ed => ed.updateOptions({ fontSize: state.fontSize }));
            showToast(`Font: ${state.fontSize}px`);
        });

        // Navigation Controls (for 2-column diff editor)
        if (diffEditor) {
            toolbar.querySelector('#nextDiff').addEventListener('click', () => {
                diffEditor.getActions().find(a => a.id === 'editor.action.diffReview.next')?.run();
            });

            toolbar.querySelector('#prevDiff').addEventListener('click', () => {
                diffEditor.getActions().find(a => a.id === 'editor.action.diffReview.prev')?.run();
            });
        } else {
            // For multi-column, disable these buttons
            toolbar.querySelector('#nextDiff').disabled = true;
            toolbar.querySelector('#nextDiff').style.opacity = '0.3';
            toolbar.querySelector('#prevDiff').disabled = true;
            toolbar.querySelector('#prevDiff').style.opacity = '0.3';
        }

        // Toggle Diff Colors
        toolbar.querySelector('#toggleDiffColors').addEventListener('click', (e) => {
            state.diffColorsEnabled = !state.diffColorsEnabled;
            e.currentTarget.classList.toggle('active');

            if (diffEditor) {
                // For 2-column diff
                const styleId = 'diffColorsOverride';
                if (!state.diffColorsEnabled) {
                    const style = document.createElement('style');
                    style.id = styleId;
                    style.textContent = `
                        #monacoCompareContainer .line-insert,
                        #monacoCompareContainer .line-delete,
                        #monacoCompareContainer .char-insert,
                        #monacoCompareContainer .char-delete {
                            background-color: transparent !important;
                            border: none !important;
                        }
                    `;
                    document.head.appendChild(style);
                    showToast('Diff colors off');
                } else {
                    document.getElementById(styleId)?.remove();
                    showToast('Diff colors on');
                }
            } else {
                // For multi-column
                const styleId = 'multiDiffColorsOverride';
                if (!state.diffColorsEnabled) {
                    const style = document.createElement('style');
                    style.id = styleId;
                    style.textContent = `
                        .diff-line-added,
                        .diff-line-removed,
                        .diff-line-modified,
                        .diff-char-added,
                        .diff-char-removed {
                            background-color: transparent !important;
                            border: none !important;
                        }
                    `;
                    document.head.appendChild(style);
                    showToast('Diff colors off');
                } else {
                    document.getElementById(styleId)?.remove();
                    showToast('Diff colors on');
                }
            }
        });

        // Toggle Whitespace
        toolbar.querySelector('#toggleWhitespace').addEventListener('click', (e) => {
            state.whitespaceVisible = !state.whitespaceVisible;
            e.currentTarget.classList.toggle('active');
            editors.forEach(ed => ed.updateOptions({
                renderWhitespace: state.whitespaceVisible ? 'all' : 'selection'
            }));
            showToast(state.whitespaceVisible ? 'Whitespace on' : 'Whitespace off');
        });

        // Toggle Line Numbers
        toolbar.querySelector('#toggleLineNumbers').addEventListener('click', (e) => {
            state.lineNumbersVisible = !state.lineNumbersVisible;
            e.currentTarget.classList.toggle('active');
            editors.forEach(ed => ed.updateOptions({
                lineNumbers: state.lineNumbersVisible ? 'on' : 'off'
            }));
            showToast(state.lineNumbersVisible ? 'Line numbers on' : 'Line numbers off');
        });

        // Toggle Minimap
        toolbar.querySelector('#toggleMinimap').addEventListener('click', (e) => {
            state.minimapVisible = !state.minimapVisible;
            e.currentTarget.classList.toggle('active');
            editors.forEach(ed => ed.updateOptions({
                minimap: { enabled: state.minimapVisible }
            }));
            showToast(state.minimapVisible ? 'Minimap on' : 'Minimap off');
        });

        // Toggle Word Wrap
        toolbar.querySelector('#toggleWordWrap').addEventListener('click', (e) => {
            state.wordWrapEnabled = !state.wordWrapEnabled;
            e.currentTarget.classList.toggle('active');
            editors.forEach(ed => ed.updateOptions({
                wordWrap: state.wordWrapEnabled ? 'on' : 'off'
            }));
            showToast(state.wordWrapEnabled ? 'Word wrap on' : 'Word wrap off');
        });

        // Copy Controls
        toolbar.querySelector('#copyLeft').addEventListener('click', () => {
            if (diffEditor) {
                const text = diffEditor.getOriginalEditor().getValue();
                navigator.clipboard.writeText(text).then(() => {
                    showToast('Release code copied');
                });
            } else if (multiEditors && multiEditors.length > 0) {
                const text = multiEditors[0].getValue();
                navigator.clipboard.writeText(text).then(() => {
                    showToast('Release code copied');
                });
            }
        });

        toolbar.querySelector('#copyRight').addEventListener('click', () => {
            if (diffEditor) {
                const text = diffEditor.getModifiedEditor().getValue();
                navigator.clipboard.writeText(text).then(() => {
                    showToast('Local code copied');
                });
            } else if (multiEditors && multiEditors.length > 0) {
                const text = multiEditors[multiEditors.length - 1].getValue();
                navigator.clipboard.writeText(text).then(() => {
                    showToast('Local code copied');
                });
            }
        });

        // Reset View
        toolbar.querySelector('#resetView').addEventListener('click', () => {
            state.fontSize = 13;
            state.diffColorsEnabled = true;
            state.whitespaceVisible = false;
            state.lineNumbersVisible = true;
            state.minimapVisible = true;
            state.wordWrapEnabled = false;

            editors.forEach(ed => ed.updateOptions({
                fontSize: 13,
                renderWhitespace: 'selection',
                lineNumbers: 'on',
                minimap: { enabled: true },
                wordWrap: 'off'
            }));

            document.getElementById('diffColorsOverride')?.remove();
            document.getElementById('multiDiffColorsOverride')?.remove();

            // Reset button states
            toolbar.querySelectorAll('.tb-btn').forEach(btn => {
                const shouldBeActive = ['toggleDiffColors', 'toggleLineNumbers', 'toggleMinimap'].includes(btn.id);
                btn.classList.toggle('active', shouldBeActive);
            });

            showToast('Reset to defaults');
        });

        // Helper function to show toast notifications
        function showToast(message) {
            const toast = document.createElement('div');
            toast.style.cssText = `
                position: fixed;
                bottom: 24px;
                right: 24px;
                background: rgba(40, 42, 54, 0.95);
                color: #50fa7b;
                padding: 12px 20px;
                border-radius: 6px;
                border: 1px solid rgba(80, 250, 123, 0.3);
                box-shadow: 0 4px 16px rgba(0, 0, 0, 0.5);
                z-index: 200;
                font-size: 13px;
                font-weight: 600;
                animation: slideInUp 0.3s ease-out;
            `;
            toast.textContent = message;
            document.body.appendChild(toast);

            setTimeout(() => {
                toast.style.animation = 'slideOutDown 0.3s ease-in';
                setTimeout(() => toast.remove(), 300);
            }, 2000);
        }

        // Add animations
        const animStyle = document.createElement('style');
        animStyle.textContent = `
            @keyframes slideInUp {
                from { transform: translateY(100%); opacity: 0; }
                to { transform: translateY(0); opacity: 1; }
            }
            @keyframes slideOutDown {
                from { transform: translateY(0); opacity: 1; }
                to { transform: translateY(100%); opacity: 0; }
            }
        `;
        document.head.appendChild(animStyle);
    }

    /**
     * Open multi-column diff editor for comparing with multiple releases
     */
    function openMultiColumnDiffEditor(localContent, remoteContents) {
        const data = window.AhmadIDEModules?.compareWithRelease?.currentComparison;
        if (!data) return;

        const monaco = window.monaco;
        if (!monaco) {
            showNotification('error', EXTENSION_NAME, 'Monaco editor not available');
            return;
        }

        console.log(`[Compare] Opening ${remoteContents.length + 1}-column diff view for ${data.routineName}`);

        // Always recreate for different routine or after close
        // Dispose old editors first
        if (window._multiColumnEditors) {
            console.log('[Compare] Disposing old multi-column editors');
            window._multiColumnEditors.forEach(ed => {
                try { ed.dispose(); } catch (_) {}
            });
            window._multiColumnEditors = null;
        }

        // Dispose old models
        if (window._multiColumnModels) {
            window._multiColumnModels.forEach(model => {
                try { model.dispose(); } catch (_) {}
            });
            window._multiColumnModels = null;
        }

        // Get or create multi-column container
        let multiContainer = document.getElementById('monacoMultiCompareContainer');
        if (!multiContainer) {
            const editorArea = document.getElementById('editor');
            if (!editorArea) {
                console.error('[Compare] Editor container #editor not found');
                return;
            }

            const editorParent = editorArea.parentElement;
            multiContainer = document.createElement('div');
            multiContainer.id = 'monacoMultiCompareContainer';
            multiContainer.style.cssText = 'position: absolute; top: 0; left: 0; right: 0; bottom: 0; z-index: 100; background: var(--ps-bg-main, var(--bg, #282a36)); display: flex; flex-direction: column;';
            editorParent.appendChild(multiContainer);
        }

        // Track current routine
        window._lastComparedRoutine = data.routineName;

        // Create close button HTML
        const closeBtnHtml = `
            <button class="ps-btn ps-btn-ghost" id="multiCloseDiffBtn" style="position: absolute; top: 8px; right: 8px; z-index: 103; padding: 6px 12px; background: var(--ps-bg-secondary, var(--panel, #2f3141)); color: var(--ps-text-main, var(--text, #f8f8f2)); border: 1px solid var(--ps-border, var(--border, #44475a)); border-radius: 4px; font-size: 12px; cursor: pointer; transition: background 0.15s ease;">
                ✕ Close Diff
            </button>
        `;

        // Create headers - Enhanced with proper positioning and visibility
        const numColumns = remoteContents.length + 1; // +1 for local
        const headersHtml = `
            <div id="multiCompareHeaders" style="position: absolute; top: 40px; left: 0; right: 0; z-index: 150; display: flex; height: 52px; background: var(--ps-bg-secondary, var(--panel, #2f3141)); border-bottom: 2px solid var(--ps-border, var(--border, #44475a)); box-shadow: 0 2px 8px rgba(0, 0, 0, 0.3); pointer-events: none;">
                ${remoteContents.map((remote, idx) => {
                    const env = extractEnvironmentFromPath(remote.path || '');
                    return `
                        <div style="flex: 1; padding: 8px 16px; border-right: 1px solid var(--ps-border, var(--border, #44475a)); background: linear-gradient(180deg, rgba(189, 147, 249, 0.18) 0%, rgba(189, 147, 249, 0.10) 100%); border-bottom: 2px solid var(--ps-accent-blue, var(--accent, #bd93f9));">
                            <div style="font-size: 13px; font-weight: 700; color: var(--ps-accent-blue, var(--accent, #bd93f9)); text-transform: uppercase; letter-spacing: 0.5px;">
                                <svg width="16" height="16" fill="var(--ps-accent-blue, var(--accent, #bd93f9))" style="vertical-align: middle; margin-right: 6px; filter: drop-shadow(0 0 3px currentColor);">
                                    <rect x="3" y="3" width="10" height="10" rx="1" fill="none" stroke="currentColor" stroke-width="1.5"/>
                                    <path d="M5 6h6M5 8h6M5 10h4" stroke="currentColor" stroke-width="1.2"/>
                                </svg>
                                Release · ${escapeHtml(env || `ENV ${idx + 1}`)}
                            </div>
                            <div style="font-size: 10px; color: var(--ps-text-muted, var(--muted, #6272a4)); font-family: 'Consolas', monospace; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; opacity: 0.9;" title="${escapeHtml(remote.path || '')}">
                                ${escapeHtml(truncatePath(remote.path || '', 40))}
                            </div>
                        </div>
                    `;
                }).join('')}
                <div style="flex: 1; padding: 8px 16px; background: linear-gradient(180deg, rgba(80, 250, 123, 0.18) 0%, rgba(80, 250, 123, 0.10) 100%); border-bottom: 2px solid var(--ps-accent-green, var(--accent-green, #50fa7b));">
                    <div style="font-size: 13px; font-weight: 700; color: var(--ps-accent-green, var(--accent-green, #50fa7b)); text-transform: uppercase; letter-spacing: 0.5px;">
                        <svg width="16" height="16" fill="var(--ps-accent-green, var(--accent-green, #50fa7b))" style="vertical-align: middle; margin-right: 6px; filter: drop-shadow(0 0 3px currentColor);">
                            <path d="M9 2.5l-6 5v8h5v-5h2v5h5v-8l-6-5z" fill="currentColor"/>
                        </svg>
                        Local Working Copy
                    </div>
                    <div style="font-size: 10px; color: var(--ps-text-muted, var(--muted, #6272a4)); opacity: 0.9;">${escapeHtml(data.routineName)}.m</div>
                </div>
            </div>
        `;

        // Create editors grid with padding for headers
        const editorsHtml = `
            <div id="multiColumnEditors" style="position: absolute; top: 92px; left: 0; right: 0; bottom: 0; display: grid; grid-template-columns: repeat(${numColumns}, 1fr); overflow: hidden;">
                ${remoteContents.map((_, idx) => `<div id="multiEditor${idx}" style="border-right: 1px solid var(--ps-border, var(--border, #44475a));"></div>`).join('')}
                <div id="multiEditorLocal"></div>
            </div>
        `;

        multiContainer.innerHTML = closeBtnHtml + headersHtml + editorsHtml;

        // Attach close button event
        const closeBtn = document.getElementById('multiCloseDiffBtn');
        if (closeBtn) {
            closeBtn.addEventListener('click', () => closeDiffEditor());
            closeBtn.addEventListener('mouseenter', () => {
                closeBtn.style.background = 'var(--ps-bg-hover, var(--hover-bg, #343746))';
            });
            closeBtn.addEventListener('mouseleave', () => {
                closeBtn.style.background = 'var(--ps-bg-secondary, var(--panel, #2f3141))';
            });
        }

        // Create Monaco editors for each column
        const editors = [];
        const models = [];

        // Create release editors - EDITABLE (view-only, no saving)
        remoteContents.forEach((remote, idx) => {
            const editorDiv = document.getElementById(`multiEditor${idx}`);
            const model = monaco.editor.createModel(remote.content || '', 'mumps',
                monaco.Uri.parse(`inmemory://release${idx}/${data.routineName}-${Date.now()}.m`));

            const editor = monaco.editor.create(editorDiv, {
                model,
                readOnly: false,  // Allow editing
                theme: 'vs-dark',
                fontSize: 13,
                fontFamily: "'ains Mono', 'Fira Code', 'Consolas', monospace",
                lineNumbers: 'on',
                minimap: { enabled: true, maxColumn: 80 },
                scrollBeyondLastLine: false,
                renderWhitespace: 'selection',
                glyphMargin: true,
                folding: false,
                lineDecorationsWidth: 10,
                lineNumbersMinChars: 4,
                overviewRulerLanes: 3,
                overviewRulerBorder: true
            });

            editors.push(editor);
            models.push(model);
        });

        // Create local editor - EDITABLE (view-only, no saving)
        const localModel = monaco.editor.createModel(localContent, 'mumps',
            monaco.Uri.parse(`inmemory://local/${data.routineName}-${Date.now()}.m`));

        const localEditor = monaco.editor.create(document.getElementById('multiEditorLocal'), {
            model: localModel,
            readOnly: false,  // Allow editing
            theme: 'vs-dark',
            fontSize: 13,
            fontFamily: "'ains Mono', 'Fira Code', 'Consolas', monospace",
            lineNumbers: 'on',
            minimap: { enabled: true, maxColumn: 80 },
            scrollBeyondLastLine: false,
            renderWhitespace: 'selection',
            glyphMargin: true,
            folding: false,
            lineDecorationsWidth: 10,
            lineNumbersMinChars: 4,
            overviewRulerLanes: 3,
            overviewRulerBorder: true
        });

        editors.push(localEditor);
        models.push(localModel);

        // Sync scrolling across all editors
        editors.forEach((editor, idx) => {
            editor.onDidScrollChange((e) => {
                editors.forEach((otherEditor, otherIdx) => {
                    if (otherIdx !== idx) {
                        otherEditor.setScrollTop(e.scrollTop);
                    }
                });
            });
        });

        // Add SMART diff highlighting decorations
        console.log('[Compare] Adding smart diff decorations to multi-column view');

        // Compare each release with local using smart diff algorithm
        remoteContents.forEach((remote, idx) => {
            const releaseEditor = editors[idx];
            const localEditor = editors[editors.length - 1]; // Last editor is local

            // Calculate smart diff
            const remoteLines = (remote.content || '').split('\n');
            const localLines = localContent.split('\n');

            const { releaseDecorations, localDecorations } = computeSmartDiff(
                remoteLines,
                localLines,
                idx
            );

            // Apply decorations to release editor
            releaseEditor.deltaDecorations([], releaseDecorations);

            // Apply decorations to local editor (only from first release to avoid conflicts)
            if (idx === 0) {
                localEditor.deltaDecorations([], localDecorations);
            }
        });

        // Add SMART diff decoration styles - VimDiff inspired with better colors
        if (!document.getElementById('multiColumnDiffStyles')) {
            const diffStyles = document.createElement('style');
            diffStyles.id = 'multiColumnDiffStyles';
            diffStyles.textContent = `
                /* ========================================
                 * SMART DIFF COLORS - Line by Line
                 * Better than VimDiff with vibrant colors
                 * ======================================== */

                /* ADDED LINES - Bright Green */
                .diff-line-added {
                    background-color: rgba(80, 250, 123, 0.20) !important;
                    border-left: 3px solid #50fa7b !important;
                }
                .diff-glyph-added {
                    background-color: rgba(80, 250, 123, 0.30) !important;
                }
                .diff-line-decoration-added::before {
                    content: '+';
                    color: #50fa7b;
                    font-weight: bold;
                    margin-right: 4px;
                }

                /* REMOVED LINES - Bright Red */
                .diff-line-removed {
                    background-color: rgba(255, 85, 85, 0.20) !important;
                    border-left: 3px solid #ff5555 !important;
                }
                .diff-glyph-removed {
                    background-color: rgba(255, 85, 85, 0.30) !important;
                }
                .diff-line-decoration-removed::before {
                    content: '-';
                    color: #ff5555;
                    font-weight: bold;
                    margin-right: 4px;
                }

                /* MODIFIED LINES - Vibrant Pink/Orange */
                .diff-line-modified {
                    background-color: rgba(255, 184, 108, 0.18) !important;
                    border-left: 3px solid #ffb86c !important;
                }
                .diff-glyph-modified {
                    background-color: rgba(255, 184, 108, 0.30) !important;
                }
                .diff-line-decoration-modified::before {
                    content: '~';
                    color: #ffb86c;
                    font-weight: bold;
                    margin-right: 4px;
                }

                /* WHITESPACE-ONLY CHANGES - Yellow */
                .diff-line-whitespace {
                    background-color: rgba(241, 250, 140, 0.15) !important;
                    border-left: 3px dashed #f1fa8c !important;
                }
                .diff-glyph-whitespace {
                    background-color: rgba(241, 250, 140, 0.25) !important;
                }

                /* CHARACTER-LEVEL CHANGES - Inline highlighting */
                .diff-char-removed,
                .diff-char-removed-inline {
                    background-color: rgba(255, 121, 198, 0.45) !important;
                    border-bottom: 2px solid #ff79c6 !important;
                    border-radius: 3px;
                    padding: 0 2px;
                }

                .diff-char-added,
                .diff-char-added-inline {
                    background-color: rgba(139, 233, 253, 0.45) !important;
                    border-bottom: 2px solid #8be9fd !important;
                    border-radius: 3px;
                    padding: 0 2px;
                }

                /* SYNTAX-AWARE HIGHLIGHTING */
                .diff-label {
                    background-color: rgba(189, 147, 249, 0.20) !important;
                    border-left: 3px solid #bd93f9 !important;
                }

                .diff-variable {
                    background-color: rgba(255, 184, 108, 0.20) !important;
                }

                .diff-comment {
                    background-color: rgba(98, 114, 164, 0.15) !important;
                    font-style: italic;
                }

                /* Overview ruler colors */
                .monaco-editor .decorationsOverviewRuler {
                    opacity: 0.8;
                }

                /* Line numbers for changed lines */
                .diff-line-added .line-numbers,
                .diff-line-removed .line-numbers,
                .diff-line-modified .line-numbers {
                    font-weight: bold;
                }
            `;
            document.head.appendChild(diffStyles);
        }

        // Store for cleanup
        window._multiColumnEditors = editors;
        window._multiColumnModels = models;

        // Hide regular diff editor if open
        const regularContainer = document.getElementById('monacoCompareContainer');
        if (regularContainer) {
            regularContainer.style.display = 'none';
        }

        // Add floating toolbar with controls for multi-column
        addDiffEditorToolbar(multiContainer, null, editors);

        // Show multi-column container
        multiContainer.style.display = 'flex';

        console.log('[Compare] Multi-column diff editor opened with highlighting');
    }

    /**
     * Open diff in Monaco editor side-by-side - OPTIMIZED
     */
    function openInDiffEditor(localContent, remoteContent) {
        const data = window.AhmadIDEModules?.compareWithRelease?.currentComparison;
        if (!data) return;

        const monaco = window.monaco;
        if (!monaco) {
            showNotification('error', EXTENSION_NAME, 'Monaco editor not available');
            return;
        }

        // Check if we're comparing the same routine (fast path) or different routine (slow path)
        const container = document.getElementById('monacoCompareContainer');
        const isEditorVisible = container && container.style.display !== 'none';
        const hasValidModels = window._monacoCompareModels?.originalModel && window._monacoCompareModels?.modifiedModel;
        const isSameRoutine = window._lastComparedRoutine === data.routineName;

        if (isEditorVisible && window._monacoCompareEditor && hasValidModels && isSameRoutine) {
            // FAST PATH: Same routine, just update model content
            try {
                // Check if models are still valid (not disposed)
                window._monacoCompareModels.originalModel.getLineCount();
                window._monacoCompareModels.modifiedModel.getLineCount();

                // Models are valid, update content
                window._monacoCompareModels.originalModel.setValue(remoteContent);
                window._monacoCompareModels.modifiedModel.setValue(localContent);

                // Update headers with new environment
                updateDiffHeaders();

                console.log('[Compare] Fast path: Editor reused successfully');
                // Diff editor will auto-recalculate
                return;
            } catch (e) {
                // If fast path fails, fall through to full recreation
                console.warn('[Compare] Fast path failed, recreating editor:', e);
                // Clear invalid references
                window._monacoCompareModels = null;
            }
        }

        // Different routine or first time - full recreation
        console.log('[Compare] Slow path: Creating new diff editor for', data.routineName);
        window._lastComparedRoutine = data.routineName;

        // SLOW PATH: Full editor creation
        // Clear existing diff editor model first
        if (window._monacoCompareEditor) {
            try {
                window._monacoCompareEditor.setModel(null);
            } catch (_) {}
        }

        // Dispose existing models
        if (window._monacoCompareModels) {
            try {
                window._monacoCompareModels.originalModel?.dispose?.();
                window._monacoCompareModels.modifiedModel?.dispose?.();
                window._monacoCompareModels = null;
            } catch (_) {}
        }

        // Create unique URIs with timestamp to avoid collisions
        const timestamp = Date.now();
        const originalUri = monaco.Uri.parse(`inmemory://release/${data.routineName}-${timestamp}.m`);
        const modifiedUri = monaco.Uri.parse(`inmemory://local/${data.routineName}-${timestamp}.m`);

        // Create fresh models
        const originalModel = monaco.editor.createModel(remoteContent, 'mumps', originalUri);
        const modifiedModel = monaco.editor.createModel(localContent, 'mumps', modifiedUri);

        // Get or create diff editor container - FAST PATH
        let diffEditorContainer = document.getElementById('monacoCompareContainer');
        if (!diffEditorContainer) {
            // Create container in main editor area
            const editorArea = document.getElementById('editor');
            if (!editorArea) {
                console.error('[Compare] Editor container #editor not found in DOM');
                showNotification('error', EXTENSION_NAME, 'Editor container not found');
                return;
            }

            // Get parent to position overlay correctly
            const editorParent = editorArea.parentElement;
            if (!editorParent) {
                console.error('[Compare] Editor parent not found');
                showNotification('error', EXTENSION_NAME, 'Editor parent container not found');
                return;
            }

            diffEditorContainer = document.createElement('div');
            diffEditorContainer.id = 'monacoCompareContainer';
            diffEditorContainer.style.cssText = 'position: absolute; top: 0; left: 0; right: 0; bottom: 0; z-index: 100; background: var(--ps-bg-main, var(--bg, #282a36));';
            editorParent.appendChild(diffEditorContainer);

            // Add close button
            const closeBtn = document.createElement('button');
            closeBtn.className = 'ps-btn ps-btn-ghost';
            closeBtn.innerHTML = '✕ Close Diff';
            closeBtn.style.cssText = 'position: absolute; top: 8px; right: 8px; z-index: 101;';
            closeBtn.addEventListener('click', () => {
                closeDiffEditor();
            });
            diffEditorContainer.appendChild(closeBtn);

            // Create headers container
            const headersContainer = document.createElement('div');
            headersContainer.id = 'monacoCompareHeaders';
            headersContainer.style.cssText = 'position: absolute; top: 40px; left: 0; right: 0; height: 52px; display: flex; z-index: 102; pointer-events: none;';
            diffEditorContainer.appendChild(headersContainer);

            // Create editor wrapper
            const editorWrapper = document.createElement('div');
            editorWrapper.id = 'monacoCompareDiffEditor';
            editorWrapper.style.cssText = 'width: 100%; height: 100%; padding-top: 92px;';
            diffEditorContainer.appendChild(editorWrapper);
        }

        // Ensure headers container exists (even when reusing diff container)
        let headersContainer = document.getElementById('monacoCompareHeaders');
        if (!headersContainer) {
            headersContainer = document.createElement('div');
            headersContainer.id = 'monacoCompareHeaders';
            headersContainer.style.cssText = 'position: absolute; top: 40px; left: 0; right: 0; height: 52px; display: flex; z-index: 102; pointer-events: none;';
            diffEditorContainer.appendChild(headersContainer);
        }

        // Ensure editor wrapper exists
        let editorWrapper = document.getElementById('monacoCompareDiffEditor');
        if (!editorWrapper) {
            editorWrapper = document.createElement('div');
            editorWrapper.id = 'monacoCompareDiffEditor';
            editorWrapper.style.cssText = 'width: 100%; height: 100%; padding-top: 92px;';
            diffEditorContainer.appendChild(editorWrapper);
        }

        // Dispose existing diff editor if any
        if (window._monacoCompareEditor) {
            try {
                window._monacoCompareEditor.dispose();
            } catch (_) {}
        }

        // Create diff editor - EDITABLE (view-only, no saving)
        const diffEditor = monaco.editor.createDiffEditor(
            document.getElementById('monacoCompareDiffEditor'),
            {
                enableSplitViewResizing: true,
                renderSideBySide: true,
                readOnly: false,  // Allow editing
                originalEditable: true,  // Allow editing release code
                renderOverviewRuler: true,
                ignoreTrimWhitespace: runtime.ignoreWhitespace,
                theme: 'vs-dark',
                fontSize: 13,
                fontFamily: "'ains Mono', 'Fira Code', 'Consolas', monospace",
                lineNumbers: 'on',
                minimap: { enabled: true },
                scrollBeyondLastLine: false,
                renderWhitespace: 'selection',
                diffWordWrap: 'on',
                // Better diff colors
                diffAlgorithm: 'advanced',
                renderIndicators: true,
                originalAriaLabel: 'Release',
                modifiedAriaLabel: 'Local'
            }
        );

        diffEditor.setModel({
            original: originalModel,
            modified: modifiedModel
        });

        // Store reference for cleanup
        window._monacoCompareEditor = diffEditor;
        window._monacoCompareModels = { originalModel, modifiedModel };

        // Add escape key listener
        const escapeHandler = (e) => {
            if (e.key === 'Escape' && diffEditorContainer.style.display !== 'none') {
                closeDiffEditor();
            }
        };
        document.addEventListener('keydown', escapeHandler);
        diffEditorContainer._escapeHandler = escapeHandler;

        // Update environment headers
        updateDiffHeaders();

        // Add floating toolbar with controls
        addDiffEditorToolbar(diffEditorContainer, diffEditor);

        // Add inline transfer arrows between editors
        addInlineTransferArrows(diffEditor, originalModel, modifiedModel);

        // Show diff editor
        diffEditorContainer.style.display = 'block';
    }

    /**
     * Update diff editor headers with environment labels
     */
    function updateDiffHeaders() {
        const headersContainer = document.getElementById('monacoCompareHeaders');
        if (!headersContainer) return;

        const data = window.AhmadIDEModules?.compareWithRelease?.currentComparison;
        if (!data) return;

        const activeRemote = data.remoteContents?.[data.activeTab];
        const envLabel = activeRemote ? extractEnvironmentFromPath(activeRemote.path || '') : 'RELEASE';
        const remotePath = activeRemote?.path || '';

        headersContainer.innerHTML = `
            <div style="flex: 1; display: flex; align-items: center; padding: 8px 64px 8px 48px; background: linear-gradient(180deg, rgba(189, 147, 249, 0.18) 0%, rgba(189, 147, 249, 0.10) 100%); border-bottom: 2px solid var(--ps-accent-blue, var(--accent, #bd93f9)); border-right: 1px solid var(--ps-border, var(--border, #44475a));">
                <div style="flex: 1;">
                    <div style="font-size: 15px; font-weight: 700; color: var(--ps-accent-blue, var(--accent, #bd93f9)); margin-bottom: 3px; letter-spacing: 0.5px; text-transform: uppercase;">
                        <svg width="18" height="18" fill="var(--ps-accent-blue, var(--accent, #bd93f9))" style="vertical-align: middle; margin-right: 8px;">
                            <rect x="3" y="3" width="10" height="10" rx="1" fill="none" stroke="currentColor" stroke-width="1.5"/>
                            <path d="M5 6h6M5 8h6M5 10h4" stroke="currentColor" stroke-width="1.2"/>
                        </svg>
                        Release · ${escapeHtml(envLabel)}
                    </div>
                    <div style="font-size: 11px; color: var(--ps-text-muted, var(--muted, #6272a4)); font-family: 'Consolas', monospace; opacity: 0.9;" title="${escapeHtml(remotePath)}">${escapeHtml(truncatePath(remotePath, 60))}</div>
                </div>
            </div>
            <div style="flex: 1; display: flex; align-items: center; padding: 8px 48px 8px 64px; background: linear-gradient(180deg, rgba(80, 250, 123, 0.18) 0%, rgba(80, 250, 123, 0.10) 100%); border-bottom: 2px solid var(--ps-accent-green, var(--accent-green, #50fa7b));">
                <div style="flex: 1;">
                    <div style="font-size: 15px; font-weight: 700; color: var(--ps-accent-green, var(--accent-green, #50fa7b)); margin-bottom: 3px; letter-spacing: 0.5px; text-transform: uppercase;">
                        <svg width="18" height="18" fill="var(--ps-accent-green, var(--accent-green, #50fa7b))" style="vertical-align: middle; margin-right: 8px;">
                            <path d="M9 2.5l-6 5v8h5v-5h2v5h5v-8l-6-5z" fill="currentColor"/>
                        </svg>
                        Local Working Copy
                    </div>
                    <div style="font-size: 11px; color: var(--ps-text-muted, var(--muted, #6272a4)); opacity: 0.9;">${escapeHtml(data.routineName)}.m (modified)</div>
                </div>
            </div>
        `;
    }

    /**
     * Truncate path for display
     */
    function truncatePath(path, maxLength) {
        if (!path || path.length <= maxLength) return path;
        const parts = path.split('/');
        if (parts.length <= 2) return path;

        // Keep first and last parts, show ... in middle
        return `${parts[0]}/.../${parts[parts.length - 1]}`;
    }

    /**
     * Close diff editor - SAFE CLEANUP
     */
    function closeDiffEditor() {
        console.log('[Compare] Closing diff editor and cleaning up');

        // Close regular 2-column diff editor
        const container = document.getElementById('monacoCompareContainer');
        if (container) {
            container.style.display = 'none';

            // Remove keyboard listener if exists
            if (container._escapeHandler) {
                document.removeEventListener('keydown', container._escapeHandler);
                container._escapeHandler = null;
            }
        }

        // Close multi-column diff editor
        const multiContainer = document.getElementById('monacoMultiCompareContainer');
        if (multiContainer) {
            multiContainer.style.display = 'none';
        }

        // Dispose multi-column editors
        if (window._multiColumnEditors) {
            console.log('[Compare] Disposing multi-column editors');
            window._multiColumnEditors.forEach(ed => {
                try { ed.dispose(); } catch (_) {}
            });
            window._multiColumnEditors = null;
        }

        // Dispose multi-column models
        if (window._multiColumnModels) {
            console.log('[Compare] Disposing multi-column models');
            window._multiColumnModels.forEach(model => {
                try { model.dispose(); } catch (_) {}
            });
            window._multiColumnModels = null;
        }

        // CRITICAL: Clear diff editor model BEFORE disposing
        if (window._monacoCompareEditor) {
            try {
                window._monacoCompareEditor.setModel(null);
            } catch (_) {}
        }

        // Dispose models safely
        if (window._monacoCompareModels) {
            console.log('[Compare] Disposing 2-column models');
            try {
                window._monacoCompareModels.originalModel?.dispose?.();
                window._monacoCompareModels.modifiedModel?.dispose?.();
                window._monacoCompareModels = null;
            } catch (_) {}
        }

        // Clear routine tracking so next open will recreate everything
        window._lastComparedRoutine = null;

        // NOTE: Keep currentComparison data intact - needed for reopening and side panel
        // The data will be replaced when a new comparison starts

        console.log('[Compare] Cleanup complete');
    }

    /**
     * Render problems list
     */
    function renderProblemsList(changes) {
        const problemsList = document.getElementById('compareProblemsList');
        const problemsCount = document.getElementById('compareProblemsCount');

        if (!problemsList || !problemsCount) return;

        const mapFn = typeof utils.mapChangesToProblems === 'function'
            ? utils.mapChangesToProblems
            : (chs) => (Array.isArray(chs) ? chs : []).map((c) => ({
                lineNumber: c.lineNumber,
                type: c.type,
                side: c.type === 'added' ? 'local' : c.type === 'removed' ? 'remote' : 'both',
                isWhitespaceOnly: !!c.isWhitespaceOnly
            }));

        const problems = mapFn(changes);
        problemsCount.textContent = problems.length;

        const html = problems.map((p, idx) => {
            const active = idx === runtime.currentChangeIndex;
            return `
                <div class="ps-compare-problem-item${active ? ' active' : ''}" data-line="${p.lineNumber}" data-idx="${idx}">
                    <svg width="16" height="16" fill="${getChangeColor(p.type)}" style="flex-shrink: 0;">
                        ${getChangeIcon(p.type)}
                    </svg>
                    <span class="ps-compare-problem-line">Line ${p.lineNumber}</span>
                    <span class="ps-compare-problem-type">${escapeHtml(String(p.type || ''))}</span>
                    <span class="ps-compare-problem-side">${escapeHtml(String(p.side || ''))}</span>
                    ${p.isWhitespaceOnly ? '<span class="ps-compare-problem-badge">WS</span>' : ''}
                </div>
            `;
        }).join('');

        problemsList.innerHTML = html || '<div class="ps-compare-empty-problems">No changes</div>';
    }

    /**
     * Render minimap
     */
    function renderMinimap(changes, totalLines = 0) {
        const minimap = document.getElementById('compareMinimap');
        if (!minimap) return;

        // Simple minimap implementation
        const maxLine = Math.max(Number(totalLines || 0) || 0, 100);
        const markerHeight = 100 / maxLine;

        const html = changes.map(change => {
            const top = (change.lineNumber / maxLine) * 100;
            return `
                <div class="ps-minimap-marker ps-minimap-${change.type}"
                     style="top: ${top}%; height: ${markerHeight}%"
                     data-line="${change.lineNumber}"></div>
            `;
        }).join('');

        minimap.innerHTML = html;
    }

    /**
     * Helper functions
     */
    function getChangeIcon(type) {
        switch (type) {
            case 'added': return '<path d="M8 3v10M3 8h10" stroke="currentColor" stroke-width="2"/>';
            case 'removed': return '<path d="M3 8h10" stroke="currentColor" stroke-width="2"/>';
            case 'modified': return '<path d="M12 4l-8 8m0-8l8 8" stroke="currentColor" stroke-width="2"/>';
            default: return '';
        }
    }

    function getChangeColor(type) {
        switch (type) {
            case 'added': return 'var(--ps-accent-green)';
            case 'removed': return 'var(--ps-accent-red)';
            case 'modified': return 'var(--ps-accent-orange)';
            default: return 'var(--ps-text-secondary)';
        }
    }

    function getFileNameFromUri(uri) {
        return uri.substring(uri.lastIndexOf('/') + 1);
    }

    /**
     * Extract environment name from path
     * ./moh/localr/A1CKC5.m -> "moh"
     * ./khcc/routines/A1CKC5.m -> "khcc"
     */
    function extractEnvironmentFromPath(path) {
        const match = path.match(/^\.\/([^\/]+)\//);
        return match ? match[1].toUpperCase() : '';
    }

    function shellQuote(value) {
        return `'${String(value || '').replace(/'/g, `'\\''`)}'`;
    }

    function escapeHtml(str) {
        const div = document.createElement('div');
        div.textContent = str;
        return div.innerHTML;
    }

    function showNotification(type, title, message) {
        const kind = String(type || 'info').toLowerCase();
        const t = String(title || '').trim() || EXTENSION_NAME;
        const msg = String(message || '').trim();
        try {
            if (typeof showToast === 'function') {
                showToast(kind, t, msg);
                return;
            }
        } catch (_) { }
        try {
            console.log(`[${kind.toUpperCase()}] ${t}${msg ? `: ${msg}` : ''}`);
        } catch (_) { }
    }

    // Cache utilities
    const cache = new Map();

    function getFromCache(key) {
        const entry = cache.get(key);
        if (!entry) return null;
        if (Date.now() > entry.expires) {
            cache.delete(key);
            return null;
        }
        return entry.value;
    }

    function setCache(key, value, ttl) {
        cache.set(key, {
            value,
            expires: Date.now() + ttl
        });
    }

    // Initialize cache reference
    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.compareWithRelease = window.AhmadIDEModules.compareWithRelease || {};
        window.AhmadIDEModules.compareWithRelease.cache = cache;
    }

    // Navigation functions
    function getCurrentComparison() {
        return window.AhmadIDEModules?.compareWithRelease?.currentComparison || null;
    }

    function getCurrentChanges() {
        const cmp = getCurrentComparison();
        return Array.isArray(cmp?.changes) ? cmp.changes : [];
    }

    function highlightActiveChange() {
        const changes = getCurrentChanges();
        const change = changes[runtime.currentChangeIndex] || null;
        const lineNumber = Number(change?.lineNumber || 0) || 0;
        if (!lineNumber) return;

        const diffView = document.getElementById('compareDiffView');
        diffView?.querySelectorAll?.('.ps-diff-line.ps-diff-selected')?.forEach?.((el) => el.classList.remove('ps-diff-selected'));
        diffView?.querySelectorAll?.(`.ps-diff-line[data-line="${lineNumber}"]`)?.forEach?.((el) => el.classList.add('ps-diff-selected'));

        const problems = document.getElementById('compareProblemsList');
        problems?.querySelectorAll?.('.ps-compare-problem-item.active')?.forEach?.((el) => el.classList.remove('active'));
        problems?.querySelector?.(`.ps-compare-problem-item[data-idx="${runtime.currentChangeIndex}"]`)?.classList.add('active');
    }

    function navigateToNextChange() {
        const changes = getCurrentChanges();
        if (!changes.length) return;
        runtime.currentChangeIndex = (runtime.currentChangeIndex + 1) % changes.length;
        scrollToLine(changes[runtime.currentChangeIndex].lineNumber, { setIndex: false });
    }

    function navigateToPrevChange() {
        const changes = getCurrentChanges();
        if (!changes.length) return;
        runtime.currentChangeIndex = (runtime.currentChangeIndex - 1 + changes.length) % changes.length;
        scrollToLine(changes[runtime.currentChangeIndex].lineNumber, { setIndex: false });
    }

    function scrollToLine(lineNumber, { setIndex = false } = {}) {
        const n = Number(lineNumber || 0) || 0;
        if (!n) return;

        // Try to scroll in Monaco diff editor if open
        if (window._monacoCompareEditor) {
            try {
                window._monacoCompareEditor.revealLineInCenter(n);
            } catch (_) {}
        }

        // Fallback to HTML diff view
        const diffView = document.getElementById('compareDiffView');
        const target = diffView?.querySelector?.(`.ps-diff-line[data-line="${n}"]`);
        if (target?.scrollIntoView) {
            try { target.scrollIntoView({ behavior: 'smooth', block: 'center' }); } catch (_) { target.scrollIntoView(); }
        }

        if (setIndex) {
            const idx = getCurrentChanges().findIndex((c) => Number(c?.lineNumber || 0) === n);
            if (idx >= 0) runtime.currentChangeIndex = idx;
        }
        highlightActiveChange();
    }

    function toggleWhitespace() {
        runtime.ignoreWhitespace = !runtime.ignoreWhitespace;
        runtime.currentChangeIndex = -1;

        // Update Monaco diff editor if open
        if (window._monacoCompareEditor) {
            try {
                window._monacoCompareEditor.updateOptions({
                    ignoreTrimWhitespace: runtime.ignoreWhitespace
                });
            } catch (_) {}
        }

        const cmp = getCurrentComparison();
        if (cmp) performDiff(cmp);
    }

    function switchCompareTab(tabIndex) {
        const cmp = getCurrentComparison();
        if (!cmp) {
            console.error('[Compare] No comparison data available');
            return;
        }

        const newTabIndex = Math.max(0, Math.min(Number(tabIndex || 0) || 0, cmp.remoteContents.length - 1));
        console.log(`[Compare] Switching to tab ${newTabIndex} (${cmp.remoteContents.length} total)`);

        // Check if we have the remote content
        if (!cmp.remoteContents || !cmp.remoteContents[newTabIndex]) {
            console.error('[Compare] Remote content not found for tab', newTabIndex);
            return;
        }

        const newRemoteContent = cmp.remoteContents[newTabIndex].content || '';
        const activeRemote = cmp.remoteContents[newTabIndex];
        const envLabel = activeRemote ? extractEnvironmentFromPath(activeRemote.path || '') : '';

        console.log(`[Compare] Switching to environment: ${envLabel}`);

        // Update active tab state
        cmp.activeTab = newTabIndex;
        cmp.status = null;
        runtime.currentChangeIndex = -1;

        // Update tab UI - make sure to update both classes and inline styles
        document.querySelectorAll('.ps-compare-tab').forEach((tab, idx) => {
            const isActive = idx === newTabIndex;
            tab.classList.toggle('active', isActive);
            // Update inline styles for immediate visual feedback
            tab.style.background = isActive ? 'var(--ps-bg-selected, var(--selection-bg, #44475a))' : 'transparent';
            tab.style.color = isActive ? 'var(--ps-text-main, var(--text, #f8f8f2))' : 'var(--ps-text-muted, var(--muted, #6272a4))';
            // Update checkmark
            const checkmark = tab.querySelector('span');
            if (checkmark) {
                checkmark.textContent = isActive ? '✓' : '';
            } else if (isActive) {
                tab.innerHTML += '<span style="margin-left: 6px; color: var(--ps-accent-green, var(--accent-green, #50fa7b));">✓</span>';
            }
        });

        // Update toolbar title with environment
        const envDisplay = envLabel ? `<span style="color: var(--ps-text-muted, var(--muted, #6272a4)); font-weight: normal; margin-left: 8px;">(${envLabel})</span>` : '';
        const titleEl = document.querySelector('.ps-compare-title');
        if (titleEl) {
            titleEl.innerHTML = `${escapeHtml(cmp.routineName)}.m ${envDisplay}`;
        }

        // Recalculate changes for new comparison
        const raw = calculateDiff(cmp.localContent, newRemoteContent);
        const changes = runtime.ignoreWhitespace ? raw.filter((c) => !c.isWhitespaceOnly) : raw;
        cmp.changes = changes;

        console.log(`[Compare] Found ${changes.length} changes in new comparison`);

        // Update Monaco diff editor
        if (window._monacoCompareEditor && window.monaco) {
            const hasValidModels = window._monacoCompareModels?.originalModel && window._monacoCompareModels?.modifiedModel;

            if (hasValidModels) {
                try {
                    // Try to reuse models (fast path)
                    window._monacoCompareModels.originalModel.setValue(newRemoteContent);
                    window._monacoCompareModels.modifiedModel.setValue(cmp.localContent);
                    console.log('[Compare] Tab switch: Models updated (fast path)');
                } catch (e) {
                    console.warn('[Compare] Tab switch: Fast path failed, recreating models', e);

                    // CRITICAL: Clear diff editor model BEFORE disposing
                    window._monacoCompareEditor.setModel(null);

                    // Dispose old models
                    try {
                        window._monacoCompareModels.originalModel?.dispose?.();
                        window._monacoCompareModels.modifiedModel?.dispose?.();
                    } catch (_) {}

                    // Create new models
                    const timestamp = Date.now();
                    const originalUri = window.monaco.Uri.parse(`inmemory://release/${cmp.routineName}-${newTabIndex}-${timestamp}.m`);
                    const modifiedUri = window.monaco.Uri.parse(`inmemory://local/${cmp.routineName}-${newTabIndex}-${timestamp}.m`);

                    const originalModel = window.monaco.editor.createModel(newRemoteContent, 'mumps', originalUri);
                    const modifiedModel = window.monaco.editor.createModel(cmp.localContent, 'mumps', modifiedUri);

                    window._monacoCompareEditor.setModel({
                        original: originalModel,
                        modified: modifiedModel
                    });

                    window._monacoCompareModels = { originalModel, modifiedModel };
                    console.log('[Compare] Tab switch: Models recreated (slow path)');
                }
            } else {
                console.warn('[Compare] Tab switch: No valid models, full refresh needed');
                renderComparePanel();
                return;
            }
        }

        // Update all UI components
        renderProblemsList(changes);
        const maxLines = Math.max(
            String(cmp.localContent || '').split('\n').length,
            String(newRemoteContent || '').split('\n').length
        );
        renderMinimap(changes, maxLines);
        updateDiffHeaders();

        // Update changes list in compare panel
        const diffView = document.getElementById('compareDiffView');
        if (diffView) {
            renderChangesList(diffView, changes);
        }

        console.log('[Compare] Tab switch complete');
    }

    async function refreshComparison() {
        const cmp = getCurrentComparison();
        if (!cmp?.routineName) return;
        if (!Array.isArray(cmp.selectedPaths) || cmp.selectedPaths.length === 0) {
            showNotification('info', EXTENSION_NAME, 'No remote selection to refresh');
            return;
        }

        const releaseConnection = getReleaseConnection();
        if (!releaseConnection) {
            showNotification('error', 'Release Connection not configured', 'Please configure in File → Connections');
            focusReleaseConnection();
            return;
        }

        try { runtime.abortController?.abort?.(); } catch (_) { }
        const controller = new AbortController();
        runtime.abortController = controller;
        const { signal } = controller;

        // Update local content from current editor if it matches.
        // Update local content from current editor if it matches.
        let updatedLocal = false;
        try {
            const ed = window.activeEditor;
            const model = ed?.getModel?.();
            const uri = model?.uri?.toString?.() || '';
            const fileName = uri ? getFileNameFromUri(uri) : '';
            if (fileName && fileName.toLowerCase() === `${cmp.routineName}.m`.toLowerCase()) {
                cmp.localContent = model.getValue();
                updatedLocal = true;
            }
        } catch (_) { }

        // If not updated from editor, try to re-read from backend
        if (!updatedLocal && cmp.routineName && window.ahmadIDE?.readRoutine) {
            try {
                const res = await window.ahmadIDE.readRoutine(cmp.routineName);
                if (res?.ok) cmp.localContent = res.code || '';
            } catch (_) { }
        }

        openCompareUI({
            ...cmp,
            remoteContents: [],
            status: { phase: 'fetch', message: `Refreshing ${cmp.selectedPaths.length} file(s)…` }
        });

        try {
            const remoteContents = await fetchRemoteContents(cmp.selectedPaths, releaseConnection, { signal });
            openCompareUI({
                ...cmp,
                remoteContents,
                status: null
            });
        } catch (e) {
            if (signal?.aborted || e?.name === 'AbortError') {
                openCompareUI({ ...cmp, status: null });
                return;
            }
            showNotification('error', EXTENSION_NAME, e?.message || 'Refresh failed');
            openCompareUI({ ...cmp, status: { phase: 'error', message: e?.message || 'Refresh failed' } });
        } finally {
            if (runtime.abortController === controller) runtime.abortController = null;
        }
    }

    async function copyRemotePath() {
        const cmp = getCurrentComparison();
        const activeTab = Number(cmp?.activeTab || 0) || 0;
        const path = cmp?.remoteContents?.[activeTab]?.path || '';
        if (!path) {
            showNotification('info', EXTENSION_NAME, 'No remote path to copy');
            return;
        }
        try {
            if (navigator?.clipboard?.writeText) {
                await navigator.clipboard.writeText(String(path));
            } else {
                const ta = document.createElement('textarea');
                ta.value = String(path);
                ta.style.position = 'fixed';
                ta.style.opacity = '0';
                document.body.appendChild(ta);
                ta.focus();
                ta.select();
                document.execCommand('copy');
                ta.remove();
            }
            showNotification('success', EXTENSION_NAME, 'Remote path copied');
        } catch (_) {
            showNotification('error', EXTENSION_NAME, 'Copy failed');
        }
    }

    // Export extension
    const extension = {
        id: EXTENSION_ID,
        name: EXTENSION_NAME,
        version: EXTENSION_VERSION,
        description: 'Compare local routines with release server versions via SSH',
        defaultEnabled: true,
        contributes: {
            commands: [{
                id: 'compareWithRelease.compare',
                title: 'Compare with Release',
                category: 'Git'
            }],
            menus: {
                editor: ['compareWithRelease.compare']
            },
            keybindings: [{
                command: 'compareWithRelease.compare',
                key: 'Ctrl+Alt+R',
                mac: 'Cmd+Alt+R'
            }]
        },
        activate,
        deactivate
    };

    // Register extension
    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.extensions = window.AhmadIDEModules.extensions || {};
        window.AhmadIDEModules.extensions.bundled = window.AhmadIDEModules.extensions.bundled || [];
        window.AhmadIDEModules.extensions.bundled.push(extension);

        // Export public API for context menu
        window.AhmadIDEModules.extensions.compareWithRelease = {
            handleCompareWithRelease
        };
    }

    if (typeof module !== 'undefined' && module.exports) {
        module.exports = extension;
    }
})();
