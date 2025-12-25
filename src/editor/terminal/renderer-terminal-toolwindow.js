(() => {
    const createNoopLogger = () => ({
        debug: () => { },
        info: () => { },
        warn: () => { },
        error: () => { },
        isEnabled: () => false
    });

    function createTerminalToolWindowManager({ deps } = {}) {
        const logger = deps?.logger || createNoopLogger();
        const terminalConfig = deps?.terminalConfig || {};
        const getCurrentProject = deps?.getCurrentProject || (() => null);
        const getEnvInfoCache = deps?.getEnvInfoCache || (() => null);
        const getActiveEditor = deps?.getActiveEditor || (() => null);
        const showToast = deps?.showToast || (() => { });
        const toggleToolWindowPanel = deps?.toggleToolWindowPanel || (() => { });

        const terminalEngineSources = deps?.terminalEngineSources || [
            './node_modules/xterm/lib/xterm.js'
        ];

        let terminalFallbackMode = false;

        const createTerminalEngineLoader =
            deps?.createTerminalEngineLoader ||
            (typeof window !== 'undefined' ? window.AhmadIDEModules?.features?.terminal?.createTerminalEngineLoader : null);

        const engineLoader = createTerminalEngineLoader
            ? createTerminalEngineLoader({
                deps: {
                    sources: terminalEngineSources,
                    getTerminal: () => window.Terminal,
                    setFallbackMode: (v) => { terminalFallbackMode = !!v; }
                }
            })
            : null;

        const ensureTerminalEngine = engineLoader?.ensureTerminalEngine
            ? engineLoader.ensureTerminalEngine
            : async () => {
                terminalFallbackMode = true;
                throw new Error('Terminal engine loader missing');
            };

        const state = {
            tabs: [],
            active: null,
            counter: 0,
            sessionMap: {}, // sessionId -> { tabId, paneId }
            _wiredPty: false,
            _wiredUi: false,
            _resizeObs: null
        };

        const getTerminalCwd = () =>
            terminalConfig.startDir ||
            getCurrentProject()?.projectPath ||
            getEnvInfoCache()?.cwd ||
            '';

        const getEls = () => ({
            tabsHost: document.getElementById('terminalToolTabs'),
            viewport: document.getElementById('terminalToolViewport'),
            errorBox: document.getElementById('terminalToolError'),
            newTabBtn: document.getElementById('terminalToolNewTabBtn'),
            splitRightBtn: document.getElementById('terminalToolSplitBtn'),
            splitDownBtn: document.getElementById('terminalToolSplitDownBtn'),
            menuBtn: document.getElementById('terminalToolMenuBtn'),
            optionsBtn: document.getElementById('terminalToolOptionsBtn'),
            hideBtn: document.getElementById('terminalToolHideBtn')
        });

        const setError = (message) => {
            const { errorBox } = getEls();
            if (!errorBox) return;
            const msg = String(message || '').trim();
            if (!msg) {
                errorBox.classList.add('hidden');
                errorBox.textContent = '';
                return;
            }
            errorBox.classList.remove('hidden');
            errorBox.textContent = msg;
        };

        const buildTerminalOptions = () => {
            const styles = getComputedStyle(document.documentElement);
            const font = (styles.getPropertyValue('--font-terminal') || styles.getPropertyValue('--font-code') || '').trim() || 'monospace';
            const fontSize = parseInt((styles.getPropertyValue('--font-size-terminal') || styles.getPropertyValue('--font-size-code') || '').trim(), 10) || 13;
            const background = (styles.getPropertyValue('--terminal-bg') || styles.getPropertyValue('--editor-bg') || '#1e1e1e').trim();
            const foreground = (styles.getPropertyValue('--text') || '#ffffff').trim();
            const selection = (styles.getPropertyValue('--accent-soft') || styles.getPropertyValue('--selection-bg') || 'rgba(53,116,240,0.25)').trim();
            return {
                allowProposedApi: true,
                convertEol: true,
                fontFamily: font,
                fontSize,
                lineHeight: 1.2,
                disableStdin: false,
                cursorBlink: true,
                scrollback: 5000,
                theme: {
                    background,
                    foreground,
                    cursor: foreground,
                    selection
                }
            };
        };

        const isEditableTarget = (el) => {
            if (!el) return false;
            const tag = (el.tagName || '').toLowerCase();
            return el.isContentEditable || tag === 'input' || tag === 'textarea' || tag === 'select';
        };

        const writeToSession = (sessionId, data) => {
            if (!sessionId || !data) return;
            try {
                if (window.ahmadIDE?.terminalWriteFast) {
                    window.ahmadIDE.terminalWriteFast(sessionId, data);
                    return;
                }
                if (window.ahmadIDE?.terminalWrite) {
                    window.ahmadIDE.terminalWrite(sessionId, data).catch?.(() => { });
                }
            } catch (_) { }
        };

        const resizePtySession = (sessionId, cols, rows) => {
            if (!sessionId || !cols || !rows) return;
            try {
                if (window.ahmadIDE?.terminalResizeFast) {
                    window.ahmadIDE.terminalResizeFast(sessionId, cols, rows);
                    return;
                }
                if (window.ahmadIDE?.terminalResize) {
                    window.ahmadIDE.terminalResize(sessionId, cols, rows).catch?.(() => { });
                }
            } catch (_) { }
        };

        function ensurePtyListeners() {
            if (state._wiredPty) return;
            if (!window.ahmadIDE?.onTerminalData) return;

            window.ahmadIDE.onTerminalData((payload) => {
                const id = payload?.id;
                if (!id) return;
                const target = state.sessionMap[id];
                if (!target) return;
                const tab = state.tabs.find(t => t.id === target.tabId);
                const pane = tab?.panes?.find(p => p.paneId === target.paneId);
                if (!pane) return;
                try {
                    pane.term?.write?.(payload.data || '');
                } catch (_) { }
            });

            window.ahmadIDE.onTerminalExit((payload) => {
                const id = payload?.id;
                if (!id) return;
                const target = state.sessionMap[id];
                if (!target) return;
                const tab = state.tabs.find(t => t.id === target.tabId);
                const pane = tab?.panes?.find(p => p.paneId === target.paneId);
                if (!pane) return;
                pane.exited = payload?.code;
                pane.sessionId = null;
                delete state.sessionMap[id];
                try {
                    pane.term?.writeln?.(`\r\n[Process exited with code ${payload?.code ?? ''}]`);
                } catch (_) { }
                renderTabs();
            });

            state._wiredPty = true;
        }

        const measureTerminal = (term, container) => {
            if (!term || !container || terminalFallbackMode) return null;
            const rect = container.getBoundingClientRect();
            if (rect.width < 20 || rect.height < 20) return null;

            let cellWidth = null;
            let cellHeight = null;
            const coreDims = term?._core?._renderService?.dimensions;
            if (coreDims) {
                cellWidth = coreDims.actualCellWidth || coreDims.css?.cellWidth || null;
                cellHeight = coreDims.actualCellHeight || coreDims.css?.cellHeight || null;
            }
            if (!cellWidth || !cellHeight) {
                const rowEl = container.querySelector?.('.xterm-rows > div');
                if (rowEl) {
                    const rowRect = rowEl.getBoundingClientRect();
                    if (rowRect.width && rowRect.height) {
                        cellHeight = rowRect.height;
                        cellWidth = rowRect.width / (term.cols || 80);
                    }
                }
            }
            if (!cellWidth || !cellHeight) return null;

            const cols = Math.max(20, Math.floor(rect.width / cellWidth));
            const rows = Math.max(5, Math.floor(rect.height / cellHeight));
            return { cols, rows };
        };

        const fitPane = (pane, { resizeSession = true } = {}) => {
            if (!pane?.term || !pane?.host) return;
            const dims = measureTerminal(pane.term, pane.host);
            if (!dims) return;
            const last = pane.lastSize;
            if (last && last.cols === dims.cols && last.rows === dims.rows) return;
            try { pane.term.resize(dims.cols, dims.rows); } catch (_) { }
            pane.lastSize = dims;
            if (resizeSession && pane.sessionId) {
                resizePtySession(pane.sessionId, dims.cols, dims.rows);
            }
        };

        const fitActiveTab = () => {
            const tab = state.tabs.find(t => t.id === state.active);
            if (!tab) return;
            tab.panes.forEach((p) => fitPane(p, { resizeSession: true }));
        };

        const ensureResizeObserver = () => {
            if (state._resizeObs) return;
            const { viewport } = getEls();
            if (!viewport) return;
            state._resizeObs = new ResizeObserver(() => {
                // Debounce with rAF to avoid resize storms.
                if (ensureResizeObserver._raf) cancelAnimationFrame(ensureResizeObserver._raf);
                ensureResizeObserver._raf = requestAnimationFrame(() => {
                    ensureResizeObserver._raf = 0;
                    fitActiveTab();
                });
            });
            state._resizeObs.observe(viewport);
        };

        const createTabContainer = (tabId) => {
            const { viewport } = getEls();
            if (!viewport) return null;
            const container = document.createElement('div');
            container.className = 'terminal-instance';
            container.dataset.tabId = tabId;
            viewport.appendChild(container);
            return container;
        };

        const renderTabs = () => {
            const { tabsHost } = getEls();
            if (!tabsHost) return;
            tabsHost.innerHTML = '';

            state.tabs.forEach((t) => {
                const tabEl = document.createElement('div');
                tabEl.className = 'terminal-tab' + (t.id === state.active ? ' active' : '');
                tabEl.title = t.name || 'Terminal';

                const nameSpan = document.createElement('span');
                nameSpan.className = 'tab-name';
                nameSpan.textContent = t.name || 'Terminal';
                tabEl.appendChild(nameSpan);

                const closeBtn = document.createElement('span');
                closeBtn.className = 'tab-close';
                closeBtn.innerHTML = '×';
                closeBtn.title = 'Close';
                closeBtn.addEventListener('click', (e) => {
                    e.stopPropagation();
                    closeTab(t.id).catch(() => { });
                });
                tabEl.appendChild(closeBtn);

                tabEl.addEventListener('click', () => activateTab(t.id));
                tabEl.addEventListener('contextmenu', (e) => {
                    e.preventDefault();
                    e.stopPropagation();
                    openTabContextMenu(e.clientX, e.clientY, t.id);
                });

                tabsHost.appendChild(tabEl);
            });
        };

        const setActivePaneUi = (tab) => {
            const panes = tab?.panes || [];
            panes.forEach((p) => {
                p.paneEl?.classList?.toggle('active', p.paneId === tab.activePaneId);
            });
        };

        const focusActivePane = () => {
            const tab = state.tabs.find(t => t.id === state.active);
            if (!tab) return;
            const pane = tab.panes.find(p => p.paneId === tab.activePaneId) || tab.panes[0];
            if (!pane) return;
            tab.activePaneId = pane.paneId;
            setActivePaneUi(tab);
            try { pane.term?.focus?.(); } catch (_) { }
        };

        const activateTab = (tabId) => {
            state.active = tabId;
            renderTabs();
            const { viewport } = getEls();
            if (viewport) {
                viewport.querySelectorAll('.terminal-instance').forEach((el) => {
                    el.classList.toggle('active', el.dataset.tabId === tabId);
                });
            }
            const tab = state.tabs.find(t => t.id === tabId);
            if (tab && !tab.activePaneId && tab.panes.length) tab.activePaneId = tab.panes[0].paneId;
            setActivePaneUi(tab);
            fitActiveTab();
            setTimeout(() => focusActivePane(), 0);
        };

        const closePane = async (tab, paneId) => {
            if (!tab) return;
            const idx = tab.panes.findIndex(p => p.paneId === paneId);
            if (idx === -1) return;
            const pane = tab.panes[idx];
            const sessionId = pane.sessionId;
            if (sessionId && window.ahmadIDE?.terminalClose) {
                await window.ahmadIDE.terminalClose(sessionId).catch(() => { });
                delete state.sessionMap[sessionId];
            }
            try { pane.term?.dispose?.(); } catch (_) { }
            try { pane.paneEl?.remove?.(); } catch (_) { }
            tab.panes.splice(idx, 1);
        };

        const closeTab = async (tabId) => {
            const idx = state.tabs.findIndex(t => t.id === tabId);
            if (idx === -1) return;
            const closing = state.tabs[idx];

            for (const p of closing.panes) {
                const sessionId = p.sessionId;
                if (sessionId && window.ahmadIDE?.terminalClose) {
                    await window.ahmadIDE.terminalClose(sessionId).catch(() => { });
                    delete state.sessionMap[sessionId];
                }
                try { p.term?.dispose?.(); } catch (_) { }
            }
            try { closing.container?.remove?.(); } catch (_) { }

            state.tabs.splice(idx, 1);
            if (state.active === tabId) {
                const next = state.tabs[Math.min(idx, state.tabs.length - 1)] || state.tabs[state.tabs.length - 1] || null;
                state.active = next ? next.id : null;
            }

            renderTabs();
            if (state.active) {
                activateTab(state.active);
            }
        };

        const makeSplitContainer = (tab, mode) => {
            if (!tab?.container) return null;
            tab.container.innerHTML = '';

            const split = document.createElement('div');
            split.className = `terminal-split terminal-split--${mode || 'single'}`;
            split.dataset.splitMode = mode || 'single';
            tab.container.appendChild(split);

            tab.splitEl = split;
            return split;
        };

        const attachSplitDrag = (tab, split, divider, mode) => {
            if (!tab || !split || !divider) return;

            const onPointerDown = (e) => {
                if (e.button !== 0) return;
                e.preventDefault();
                e.stopPropagation();
                const startX = e.clientX;
                const startY = e.clientY;
                const rect = split.getBoundingClientRect();

                const startA = split.style.getPropertyValue('--split-a');
                // If unset, infer from first pane.
                const initialA = (() => {
                    if (startA && startA.includes('px')) return parseFloat(startA);
                    if (mode === 'horizontal') return rect.height / 2;
                    return rect.width / 2;
                })();

                const onMove = (ev) => {
                    const dx = ev.clientX - startX;
                    const dy = ev.clientY - startY;
                    if (mode === 'horizontal') {
                        const px = Math.max(80, Math.min(rect.height - 80, initialA + dy));
                        split.style.setProperty('--split-a', `${px}px`);
                        split.style.setProperty('--split-b', '1fr');
                    } else {
                        const px = Math.max(120, Math.min(rect.width - 120, initialA + dx));
                        split.style.setProperty('--split-a', `${px}px`);
                        split.style.setProperty('--split-b', '1fr');
                    }
                    tab.panes.forEach((p) => fitPane(p, { resizeSession: true }));
                };

                const onUp = () => {
                    window.removeEventListener('pointermove', onMove, true);
                    window.removeEventListener('pointerup', onUp, true);
                    tab.panes.forEach((p) => fitPane(p, { resizeSession: true }));
                };

                window.addEventListener('pointermove', onMove, true);
                window.addEventListener('pointerup', onUp, true);
            };

            divider.addEventListener('pointerdown', onPointerDown);
        };

        const createPane = async (tab, { focus = true } = {}) => {
            if (!tab?.splitEl) return null;
            const split = tab.splitEl;

            tab.paneCounter = (tab.paneCounter || 0) + 1;
            const paneId = `${tab.id}:p${tab.paneCounter}`;

            const paneEl = document.createElement('div');
            paneEl.className = 'terminal-pane';
            paneEl.dataset.paneId = paneId;

            const host = document.createElement('div');
            host.className = 'terminal-pane-host';
            paneEl.appendChild(host);

            const onActivatePane = () => {
                tab.activePaneId = paneId;
                setActivePaneUi(tab);
                try { pane.term?.focus?.(); } catch (_) { }
            };
            paneEl.addEventListener('mousedown', (e) => {
                if (e.button !== 0) return;
                onActivatePane();
            });

            // Context menu on the pane
            paneEl.addEventListener('contextmenu', (e) => {
                e.preventDefault();
                e.stopPropagation();
                onActivatePane();
                openPaneContextMenu(e.clientX, e.clientY, tab.id, paneId);
            });

            const pane = {
                paneId,
                paneEl,
                host,
                term: null,
                sessionId: null,
                pendingInput: '',
                lastSize: null,
                exited: undefined
            };
            tab.panes.push(pane);

            // Mount into split grid
            split.appendChild(paneEl);

            let terminalCtor = null;
            try {
                terminalCtor = await ensureTerminalEngine();
                setError('');
            } catch (err) {
                terminalCtor = null;
                terminalFallbackMode = true;
                setError(err?.message || 'Terminal engine unavailable');
            }

            if (!terminalCtor) {
                paneEl.classList.add('plain-terminal');
                host.textContent = 'Terminal engine unavailable (xterm).';
                return pane;
            }

            const term = new terminalCtor(buildTerminalOptions());
            pane.term = term;
            try {
                term.open(host);
            } catch (err) {
                setError(err?.message || 'Failed to open terminal');
            }

            // xterm -> pty
            term.onData((data) => {
                if (!pane.sessionId) {
                    pane.pendingInput = String(pane.pendingInput || '') + String(data || '');
                    return;
                }
                writeToSession(pane.sessionId, data);
            });

            term.attachCustomKeyEventHandler((ev) => {
                // Terminal-native copy/paste like JetBrains (Linux): Ctrl+Shift+C / Ctrl+Shift+V
                if (ev.ctrlKey && ev.shiftKey && !ev.altKey && !ev.metaKey) {
                    const key = String(ev.key || '').toLowerCase();
                    if (key === 'c') {
                        const text = String(term.getSelection?.() || '');
                        if (text) {
                            navigator.clipboard.writeText(text).catch(() => { });
                        }
                        return false;
                    }
                    if (key === 'v') {
                        navigator.clipboard.readText().then((text) => {
                            if (text) writeToSession(pane.sessionId, text);
                        }).catch(() => { });
                        return false;
                    }
                }
                if (
                    terminalConfig.escapeToEditor &&
                    !terminalConfig.overrideIdeShortcuts &&
                    ev.key === 'Escape' &&
                    !ev.altKey && !ev.ctrlKey && !ev.metaKey && !ev.shiftKey
                ) {
                    getActiveEditor()?.focus?.();
                    return false;
                }
                return true;
            });

            // Spawn PTY
            const dims = measureTerminal(term, host);
            if (!window.ahmadIDE?.terminalCreate) {
                term.writeln('Terminal backend unavailable.');
                setError('Terminal backend unavailable');
            } else {
                const res = await window.ahmadIDE.terminalCreate({
                    shell: terminalConfig.shellPath || undefined,
                    cwd: getTerminalCwd(),
                    cols: dims?.cols,
                    rows: dims?.rows
                }).catch((e) => ({ ok: false, error: e?.message || 'terminalCreate failed' }));

                if (res?.ok) {
                    pane.sessionId = res.id;
                    state.sessionMap[res.id] = { tabId: tab.id, paneId };
                    if (pane.pendingInput) {
                        writeToSession(pane.sessionId, pane.pendingInput);
                        pane.pendingInput = '';
                    }
                    if (res.backend && res.backend !== 'pty') {
                        const msg = `Interactive terminal backend unavailable (node-pty). Run \\\"npm run rebuild:pty\\\" to rebuild native modules for Electron.`;
                        try { term.writeln(`\x1b[33m⚠ ${msg}\x1b[0m`); } catch (_) { }
                        setError(msg);
                        showToast('error', 'Terminal', msg);
                    }
                } else {
                    term.writeln(`\x1b[31m✗ Failed to start terminal: ${res?.error || 'unknown error'}\x1b[0m`);
                    setError(res?.error || 'Failed to start terminal');
                }
            }

            // Fit initial
            fitPane(pane, { resizeSession: true });

            if (focus) {
                tab.activePaneId = paneId;
                setActivePaneUi(tab);
                setTimeout(() => {
                    try { term.focus(); } catch (_) { }
                }, 0);
            }

            return pane;
        };

        const ensureSplitLayout = async (tab, mode) => {
            if (!tab) return;
            const modeKey = mode === 'horizontal' ? 'horizontal' : (mode === 'vertical' ? 'vertical' : 'single');
            const split = makeSplitContainer(tab, modeKey);
            if (!split) return;

            tab.panes = [];
            tab.activePaneId = null;

            if (modeKey === 'single') {
                await createPane(tab, { focus: true });
                return;
            }

            // A | divider | B
            const paneA = await createPane(tab, { focus: true });

            const divider = document.createElement('div');
            divider.className = 'terminal-split-divider';
            split.appendChild(divider);

            await createPane(tab, { focus: false });

            attachSplitDrag(tab, split, divider, modeKey);
            tab.activePaneId = paneA?.paneId || tab.panes[0]?.paneId || null;
            setActivePaneUi(tab);
            fitActiveTab();
        };

        const splitActiveTab = async (mode) => {
            const tab = state.tabs.find(t => t.id === state.active);
            if (!tab) return;
            const currentMode = tab.splitEl?.dataset?.splitMode || 'single';

            // If already split, just switch mode (keep it 2 panes) by rebuilding layout.
            if (currentMode !== 'single') {
                await unsplitActiveTab({ keepSecond: true, nextMode: mode });
                return;
            }

            const existingPane = tab.panes?.[0] || null;
            if (!existingPane) {
                await ensureSplitLayout(tab, mode);
                return;
            }

            // Convert single -> split by creating a second pane and adding divider.
            const splitKey = mode === 'horizontal' ? 'horizontal' : 'vertical';
            tab.container.innerHTML = '';

            const split = document.createElement('div');
            split.className = `terminal-split terminal-split--${splitKey}`;
            split.dataset.splitMode = splitKey;
            tab.container.appendChild(split);
            tab.splitEl = split;

            // Reattach first pane element
            split.appendChild(existingPane.paneEl);

            const divider = document.createElement('div');
            divider.className = 'terminal-split-divider';
            split.appendChild(divider);

            await createPane(tab, { focus: false });
            attachSplitDrag(tab, split, divider, splitKey);

            tab.activePaneId = existingPane.paneId;
            setActivePaneUi(tab);
            fitActiveTab();
        };

        const unsplitActiveTab = async ({ keepSecond = false, nextMode = 'single' } = {}) => {
            const tab = state.tabs.find(t => t.id === state.active);
            if (!tab) return;
            const mode = tab.splitEl?.dataset?.splitMode || 'single';
            if (mode === 'single') return;

            const panes = Array.isArray(tab.panes) ? tab.panes.slice() : [];
            if (panes.length < 2) {
                tab.container.innerHTML = '';
                tab.splitEl = null;
                await ensureSplitLayout(tab, nextMode === 'single' ? 'single' : nextMode);
                return;
            }

            const keepPane = keepSecond ? panes[1] : panes[0];
            const dropPane = keepSecond ? panes[0] : panes[1];

            await closePane(tab, dropPane.paneId);

            // Rebuild single layout container and attach kept pane.
            tab.container.innerHTML = '';
            const split = document.createElement('div');
            split.className = 'terminal-split terminal-split--single';
            split.dataset.splitMode = 'single';
            tab.container.appendChild(split);
            tab.splitEl = split;
            split.appendChild(keepPane.paneEl);

            tab.panes = [keepPane];
            tab.activePaneId = keepPane.paneId;
            setActivePaneUi(tab);
            fitActiveTab();
        };

        const newTab = async () => {
            state.counter += 1;
            const id = `termtool${state.counter}`;
            const name = state.counter === 1 ? 'Local' : `Local ${state.counter}`;
            const container = createTabContainer(id);
            if (!container) {
                showToast('error', 'Terminal', 'Missing terminal container');
                return;
            }

            const tab = {
                id,
                name,
                container,
                panes: [],
                activePaneId: null,
                splitEl: null,
                paneCounter: 0
            };
            state.tabs.push(tab);
            state.active = id;
            container.classList.add('active');

            // Default layout (single pane)
            tab.splitEl = makeSplitContainer(tab, 'single');
            await createPane(tab, { focus: true });

            renderTabs();
            activateTab(id);
        };

        const openTerminalToolWindow = async (opts = {}) => {
            const featureRegistry = window.AhmadIDEModules?.app?.featureRegistry || null;
            try { featureRegistry?.ensureById?.('terminalToolPanel'); } catch (_) { }
            toggleToolWindowPanel('terminalToolPanel', 'bottom');

            // Ensure DOM wired + a default tab exists.
            wireDom();
            ensurePtyListeners();
            ensureResizeObserver();
            if (!state.tabs.length) {
                await newTab();
            } else if (state.active) {
                activateTab(state.active);
            }

            if (!opts.skipFocus) setTimeout(() => focusActivePane(), 0);
        };

        const openTabContextMenu = (x, y, tabId) => {
            const menu = window.AhmadIDEModules?.ui?.menu;
            const controller = menu?.controller || menu?.createMenuController?.({});
            if (!controller) return;
            const openContextMenu = menu?.openContextMenu;

            const tab = state.tabs.find(t => t.id === tabId);
            const ctx = { tabId, tabName: tab?.name || '' };

            const items = [
                { id: 'term.new', label: 'New Tab', icon: 'add', action: 'term:new' },
                { type: 'separator' },
                { id: 'term.splitRight', label: 'Split Right', action: 'term:split-right' },
                { id: 'term.splitDown', label: 'Split Down', action: 'term:split-down' },
                { id: 'term.unsplit', label: 'Unsplit', action: 'term:unsplit', disabled: () => (tab?.splitEl?.dataset?.splitMode || 'single') === 'single' },
                { type: 'separator' },
                { id: 'term.rename', label: 'Rename…', action: 'term:rename' },
                { id: 'term.close', label: 'Close Tab', action: 'term:close' }
            ];

            const payload = {
                controller,
                x,
                y,
                items,
                ctx,
                onAction: (action) => {
                    switch (action) {
                        case 'term:new':
                            newTab().catch(() => { });
                            return;
                        case 'term:split-right':
                            state.active = tabId;
                            activateTab(tabId);
                            splitActiveTab('vertical').catch(() => { });
                            return;
                        case 'term:split-down':
                            state.active = tabId;
                            activateTab(tabId);
                            splitActiveTab('horizontal').catch(() => { });
                            return;
                        case 'term:unsplit':
                            state.active = tabId;
                            activateTab(tabId);
                            unsplitActiveTab().catch(() => { });
                            return;
                        case 'term:rename':
                            renameTab(tabId).catch(() => { });
                            return;
                        case 'term:close':
                            closeTab(tabId).catch(() => { });
                            return;
                    }
                }
            };
            if (typeof openContextMenu === 'function') {
                openContextMenu(payload);
                return;
            }
            controller.openAtPoint(payload);
        };

        const openPaneContextMenu = (x, y, tabId, paneId) => {
            const menu = window.AhmadIDEModules?.ui?.menu;
            const controller = menu?.controller || menu?.createMenuController?.({});
            if (!controller) return;
            const openContextMenu = menu?.openContextMenu;
            const tab = state.tabs.find(t => t.id === tabId);
            const pane = tab?.panes?.find(p => p.paneId === paneId);
            if (!tab || !pane) return;

            const items = [
                { id: 'term.copy', label: 'Copy', action: 'term:copy' },
                { id: 'term.paste', label: 'Paste', action: 'term:paste' },
                { type: 'separator' },
                { id: 'term.clear', label: 'Clear', action: 'term:clear' },
                { id: 'term.ctrlc', label: 'Send Ctrl+C', action: 'term:ctrlc' },
                { type: 'separator' },
                { id: 'term.splitRight', label: 'Split Right', action: 'term:split-right' },
                { id: 'term.splitDown', label: 'Split Down', action: 'term:split-down' },
                { id: 'term.unsplit', label: 'Unsplit', action: 'term:unsplit', disabled: () => (tab?.splitEl?.dataset?.splitMode || 'single') === 'single' },
                { type: 'separator' },
                { id: 'term.close', label: 'Close Tab', action: 'term:close' }
            ];

            const payload = {
                controller,
                x,
                y,
                items,
                ctx: { tabId, paneId },
                onAction: async (action) => {
                    if (!tab || !pane) return;
                    tab.activePaneId = paneId;
                    setActivePaneUi(tab);

                    switch (action) {
                        case 'term:copy': {
                            const text = String(pane.term?.getSelection?.() || '');
                            if (!text) return;
                            try { await navigator.clipboard.writeText(text); } catch (_) { }
                            return;
                        }
                        case 'term:paste': {
                            let text = '';
                            try { text = await navigator.clipboard.readText(); } catch (_) { }
                            if (!text) return;
                            writeToSession(pane.sessionId, text);
                            return;
                        }
                        case 'term:clear': {
                            // Clear screen (Ctrl+L) and reset buffer for a clean view.
                            try { pane.term?.reset?.(); } catch (_) { }
                            writeToSession(pane.sessionId, '\u000c');
                            return;
                        }
                        case 'term:ctrlc': {
                            writeToSession(pane.sessionId, '\u0003');
                            return;
                        }
                        case 'term:split-right':
                            await splitActiveTab('vertical');
                            return;
                        case 'term:split-down':
                            await splitActiveTab('horizontal');
                            return;
                        case 'term:unsplit':
                            await unsplitActiveTab();
                            return;
                        case 'term:close':
                            await closeTab(tabId);
                            return;
                    }
                }
            };
            if (typeof openContextMenu === 'function') {
                openContextMenu(payload);
                return;
            }
            controller.openAtPoint(payload);
        };

        const renameTab = async (tabId) => {
            const tab = state.tabs.find(t => t.id === tabId);
            if (!tab) return;
            const current = tab.name || '';

            let next = null;
            try {
                const fn = window.AhmadIDEModules?.ui?.showPrompt;
                if (typeof fn === 'function') {
                    next = await fn({
                        title: 'Rename Terminal Tab',
                        message: 'Enter a new name:',
                        placeholder: 'e.g. Local',
                        defaultValue: current
                    });
                } else {
                    next = window.prompt('Rename Terminal Tab', current);
                }
            } catch (_) {
                next = null;
            }

            if (next == null) return;
            const name = String(next || '').trim();
            if (!name) return;
            tab.name = name;
            renderTabs();
        };

        const wireDom = () => {
            if (state._wiredUi) return;
            const { tabsHost, viewport, newTabBtn, splitRightBtn, splitDownBtn, menuBtn, optionsBtn, hideBtn } = getEls();
            if (!tabsHost || !viewport || !newTabBtn || !splitRightBtn || !splitDownBtn) return;
            state._wiredUi = true;

            ensurePtyListeners();
            ensureResizeObserver();

            newTabBtn.addEventListener('click', () => newTab().catch(() => { }));
            splitRightBtn.addEventListener('click', () => splitActiveTab('vertical').catch(() => { }));
            splitDownBtn.addEventListener('click', () => splitActiveTab('horizontal').catch(() => { }));

            hideBtn?.addEventListener('click', () => {
                toggleToolWindowPanel('terminalToolPanel', 'bottom');
            });

            // Basic "More" menu (context menu at button)
            const openMoreMenu = (anchorEl) => {
                if (!anchorEl) return;
                const rect = anchorEl.getBoundingClientRect();
                openTabContextMenu(rect.left, rect.bottom + 6, state.active);
            };
            menuBtn?.addEventListener('click', (e) => {
                e.preventDefault();
                e.stopPropagation();
                openMoreMenu(menuBtn);
            });
            optionsBtn?.addEventListener('click', (e) => {
                e.preventDefault();
                e.stopPropagation();
                openMoreMenu(optionsBtn);
            });

            // Panel-level context menu (empty area)
            viewport.addEventListener('contextmenu', (e) => {
                if (isEditableTarget(e.target)) return;
                const tab = state.tabs.find(t => t.id === state.active);
                // If right-clicking on the xterm itself, the pane handler will handle.
                if (e.target?.closest?.('.xterm')) return;
                openTabContextMenu(e.clientX, e.clientY, tab?.id || state.active);
            });

            // React to tool window activation (ensure fit + focus).
            window.addEventListener('ahmadIDE:toolwindow-activated', (e) => {
                const panelId = e?.detail?.panelId;
                if (panelId !== 'terminalToolPanel') return;
                wireDom();
                ensurePtyListeners();
                ensureResizeObserver();
                if (!state.tabs.length) {
                    newTab().catch(() => { });
                } else if (state.active) {
                    activateTab(state.active);
                }
                setTimeout(() => focusActivePane(), 0);
            });
        };

        // Lazy-mount support
        try {
            const featureRegistry = window.AhmadIDEModules?.app?.featureRegistry;
            featureRegistry?.onMounted?.('terminalToolPanel', () => {
                wireDom();
                if (!state.tabs.length) {
                    newTab().catch(() => { });
                }
            });
        } catch (_) { }

        return {
            openTerminalToolWindow,
            newTab,
            splitRight: () => splitActiveTab('vertical'),
            splitDown: () => splitActiveTab('horizontal'),
            unsplit: () => unsplitActiveTab(),
            focus: () => focusActivePane(),
            fit: () => fitActiveTab(),
            getState: () => state
        };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.terminalTool = window.AhmadIDEModules.terminalTool || {};
        window.AhmadIDEModules.terminalTool.createTerminalToolWindowManager = createTerminalToolWindowManager;
    }
})();
