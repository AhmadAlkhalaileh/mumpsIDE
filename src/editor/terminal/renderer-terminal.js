(() => {
    const createNoopLogger = () => ({
        debug: () => { },
        info: () => { },
        warn: () => { },
        error: () => { },
        isEnabled: () => false
    });

    function createTerminalManager({ deps } = {}) {
        const logger = deps?.logger || createNoopLogger();
        const terminalConfig = deps?.terminalConfig;
        const getCurrentProject = deps?.getCurrentProject || (() => null);
        const getEnvInfoCache = deps?.getEnvInfoCache || (() => null);
        const getGlobalTerminalState = deps?.getGlobalTerminalState || (() => null);
        const getActiveEditor = deps?.getActiveEditor || (() => null);
        const showToast = deps?.showToast || (() => { });
        const terminalEngineSources = deps?.terminalEngineSources || [
            './node_modules/xterm/lib/xterm.js',
            'https://cdn.jsdelivr.net/npm/xterm@5.3.0/lib/xterm.js'
        ];

        if (!terminalConfig) {
            throw new Error('createTerminalManager requires deps.terminalConfig');
        }

        let terminalEnginePromise = null;
        let terminalFallbackMode = false;
        let terminalResizeObserver = null;

        const getTerminalCwd = () =>
            terminalConfig.startDir ||
            getCurrentProject()?.projectPath ||
            getEnvInfoCache()?.cwd ||
            (typeof process !== 'undefined' && process.cwd ? process.cwd() : '');

        function createTerminalState() {
            return { tabs: [], active: null, counter: 0, sessionMap: {}, _wiredTerminalEvents: false };
        }

        function getActiveTerminalTab(state) {
            if (!state) return null;
            return state.tabs.find(t => t.id === state.active) || null;
        }

        const focusTerminal = () => {
            const tab = getActiveTerminalTab(getGlobalTerminalState());
            if (tab?.term) {
                tab.term.focus();
            }
        };

        const isTerminalFocused = () => {
            const active = document.activeElement;
            if (!active) return false;
            return !!active.closest?.('#terminalViewport') || !!active.closest?.('.xterm');
        };

        function loadScript(src) {
            return new Promise((resolve, reject) => {
                const existing = document.querySelector(`script[data-src="${src}"]`);
                if (existing) {
                    if (existing.dataset.loaded === 'true') {
                        resolve();
                        return;
                    }
                    existing.addEventListener('load', () => resolve());
                    existing.addEventListener('error', () => reject(new Error(`Failed to load ${src}`)));
                    return;
                }
                const s = document.createElement('script');
                s.src = src;
                s.async = true;
                s.dataset.src = src;
                s.onload = () => {
                    s.dataset.loaded = 'true';
                    resolve();
                };
                s.onerror = () => reject(new Error(`Failed to load ${src}`));
                document.head.appendChild(s);
            });
        }

        async function ensureTerminalEngine() {
            if (window.Terminal) {
                terminalFallbackMode = false;
                return window.Terminal;
            }
            if (terminalEnginePromise) return terminalEnginePromise;
            terminalEnginePromise = (async () => {
                let lastErr = null;
                for (const src of terminalEngineSources) {
                    try {
                        await loadScript(src);
                        if (window.Terminal) {
                            terminalFallbackMode = false;
                            return window.Terminal;
                        }
                    } catch (err) {
                        lastErr = err;
                    }
                }
                throw new Error(`Terminal engine unavailable. Install xterm locally or allow CDN. ${lastErr ? lastErr.message : ''}`);
            })();
            try {
                const term = await terminalEnginePromise;
                terminalFallbackMode = false;
                return term;
            } catch (err) {
                terminalEnginePromise = null;
                terminalFallbackMode = true;
                throw err;
            }
        }

        function getTerminalElements() {
            return {
                tabsHost: document.getElementById('terminalTabs'),
                viewport: document.getElementById('terminalViewport'),
                errorBox: document.getElementById('terminalError'),
                statusPill: document.getElementById('terminalStatusPill')
            };
        }

        function setTerminalError(message) {
            const { errorBox } = getTerminalElements();
            if (!errorBox) return;
            if (!message) {
                errorBox.classList.add('hidden');
                errorBox.textContent = '';
                return;
            }
            errorBox.classList.remove('hidden');
            errorBox.textContent = message;
        }

        function updateTerminalStatusPill() {
            const { statusPill: pill } = getTerminalElements();
            if (!pill) return;
            const shellLabel = terminalConfig.shellPath ? `Shell: ${terminalConfig.shellPath}` : 'Shell: Default';
            const cwdLabel = getTerminalCwd() || '';
            pill.textContent = shellLabel;
            pill.title = `${shellLabel}\nStart in: ${cwdLabel || 'Project root'}`;
        }

        function writeAndFollow(tab, text, { newline = false } = {}) {
            if (!tab?.term) return;
            const writer = newline ? tab.term.writeln : tab.term.write;
            const doScroll = () => {
                if (typeof requestAnimationFrame === 'function') {
                    requestAnimationFrame(() => autoScrollTerminal(tab));
                } else {
                    autoScrollTerminal(tab);
                }
            };
            if (typeof writer === 'function') {
                try {
                    writer.call(tab.term, text ?? '', doScroll);
                } catch (e) {
                    writer.call(tab.term, text ?? '');
                    doScroll();
                }
            } else {
                doScroll();
            }
        }

        function ensureTerminalListeners(state) {
            if (!window.ahmadIDE.onTerminalData || state._wiredTerminalEvents) return;
            window.ahmadIDE.onTerminalData((payload) => {
                if (!payload) return;
                const tabId = state.sessionMap[payload.id];
                const tab = state.tabs.find(t => t.id === tabId);
                if (!tab) return;
                if (tab.term) {
                    writeAndFollow(tab, payload.data || '');
                } else {
                    tab.buffer = tab.buffer || [];
                    tab.buffer.push(payload.data || '');
                }
            });
            window.ahmadIDE.onTerminalExit((payload) => {
                const tabId = state.sessionMap[payload.id];
                const tab = state.tabs.find(t => t.id === tabId);
                if (!tab) return;
                tab.exited = payload.code;
                tab.sessionId = null;
                delete state.sessionMap[payload.id];
                const message = `\r\n[Process exited with code ${payload.code ?? ''}]`;
                if (tab.term) writeAndFollow(tab, message);
                if (tab.container) tab.container.classList.add('exited');
                renderTerminalTabs(state);
            });
            state._wiredTerminalEvents = true;
        }

        async function closeTerminalTab(state, id) {
            if (!state) return;
            const idx = state.tabs.findIndex(t => t.id === id);
            if (idx === -1) return;
            const tab = state.tabs[idx];
            logger.info('TERMINAL_CLOSE_TAB', { id, sessionId: tab?.sessionId });
            if (tab.sessionId && window.ahmadIDE.terminalClose) {
                await window.ahmadIDE.terminalClose(tab.sessionId);
                delete state.sessionMap[tab.sessionId];
            }
            if (tab.term) {
                tab.term.dispose();
            }
            tab.container?.remove();
            state.tabs.splice(idx, 1);
            if (state.active === id) {
                state.active = state.tabs.length ? state.tabs[state.tabs.length - 1].id : null;
            }
            renderTerminalTabs(state);
            refreshTerminalLayout(state);
        }

        function renderTerminalTabs(state) {
            const { tabsHost: host } = getTerminalElements();
            if (!host) return;
            host.innerHTML = '';
            state.tabs.forEach(t => {
                const tab = document.createElement('div');
                tab.className = 'terminal-tab' + (t.id === state.active ? ' active' : '') + (t.exited !== undefined ? ' exited' : '');
                tab.textContent = t.name;
                tab.onclick = () => activateTerminalTab(state, t.id);
                const close = document.createElement('span');
                close.className = 'tab-close';
                close.textContent = '✕';
                close.title = 'Close tab';
                close.onclick = (e) => {
                    e.stopPropagation();
                    closeTerminalTab(state, t.id);
                };
                tab.appendChild(close);
                host.appendChild(tab);
            });
        }

        function measureTerminal(tab) {
            if (!tab?.term || !tab?.container) return null;
            const rect = tab.container.getBoundingClientRect();
            if (rect.width < 10 || rect.height < 10) return null;
            if (terminalFallbackMode) return null;
            let cellWidth = null;
            let cellHeight = null;
            const coreDims = tab.term._core?._renderService?.dimensions;
            if (coreDims) {
                cellWidth = coreDims.actualCellWidth || coreDims.css?.cellWidth || null;
                cellHeight = coreDims.actualCellHeight || coreDims.css?.cellHeight || null;
            }
            if (!cellWidth || !cellHeight) {
                const rowEl = tab.container.querySelector('.xterm-rows > div');
                if (rowEl) {
                    const rowRect = rowEl.getBoundingClientRect();
                    if (rowRect.width && rowRect.height) {
                        cellHeight = rowRect.height;
                        cellWidth = rowRect.width / (tab.term.cols || 80);
                    }
                }
            }
            if (!cellWidth || !cellHeight) return null;
            const cols = Math.max(20, Math.floor(rect.width / cellWidth));
            const rows = Math.max(5, Math.floor(rect.height / cellHeight));
            return { cols, rows };
        }

        function refreshTerminalLayout(state, { resizeSession = true } = {}) {
            if (!state) return;
            const tab = getActiveTerminalTab(state);
            if (!tab?.term) return;
            const dims = measureTerminal(tab);
            if (!dims) return;
            if (tab.lastSize && tab.lastSize.cols === dims.cols && tab.lastSize.rows === dims.rows) return;
            try {
                tab.term.resize(dims.cols, dims.rows);
            } catch (e) {
                console.warn('Terminal resize failed', e);
            }
            tab.lastSize = dims;
            if (resizeSession && tab.sessionId && window.ahmadIDE.terminalResize) {
                window.ahmadIDE.terminalResize(tab.sessionId, dims.cols, dims.rows);
            }
        }

        function ensureTerminalResizeObserver(state) {
            if (terminalResizeObserver) return;
            const { viewport } = getTerminalElements();
            if (!viewport) return;
            terminalResizeObserver = new ResizeObserver(() => refreshTerminalLayout(state));
            terminalResizeObserver.observe(viewport);
        }

        function autoScrollTerminal(tab) {
            if (!tab) return;
            if (tab._scrollTimer) clearTimeout(tab._scrollTimer);
            tab._scrollTimer = setTimeout(() => {
                // xterm terminals - wait a tick so the write buffer flushes before scrolling
                if (tab.term && typeof tab.term.scrollToBottom === 'function') {
                    tab.term.scrollToBottom();
                    const viewportEl = tab.container?.querySelector?.('.xterm-viewport');
                    if (viewportEl) viewportEl.scrollTop = viewportEl.scrollHeight;
                } else if (tab._plain && tab.container) {
                    // plain renderer
                    const out = tab.container.querySelector('.plain-terminal-output');
                    if (out) out.scrollTop = out.scrollHeight;
                }
                if (tab.container) {
                    tab.container.scrollTop = tab.container.scrollHeight;
                }
                tab._scrollTimer = null;
            }, 0);
        }

        function activateTerminalTab(state, tabId) {
            if (!state) return;
            state.active = tabId;
            renderTerminalTabs(state);
            const tab = state.tabs.find(t => t.id === tabId);
            const { viewport } = getTerminalElements();
            if (viewport) {
                viewport.querySelectorAll('.terminal-instance').forEach(el => {
                    el.classList.toggle('active', el.dataset.tabId === tabId);
                });
            }
            refreshTerminalLayout(state);
            autoScrollTerminal(tab);
            setTimeout(() => {
                focusTerminal();
                autoScrollTerminal(tab);
            }, 30);
        }

        function createTerminalContainer(tabId) {
            const { viewport } = getTerminalElements();
            if (!viewport) return null;
            const container = document.createElement('div');
            container.className = 'terminal-instance';
            container.dataset.tabId = tabId;
            viewport.appendChild(container);
            return container;
        }

        function buildTerminalOptions() {
            const styles = getComputedStyle(document.documentElement);
            const font = (styles.getPropertyValue('--font-code') || '').trim() || 'monospace';
            const fontSize = parseInt((styles.getPropertyValue('--font-size-code') || '').trim(), 10) || 13;
            const background = (styles.getPropertyValue('--terminal-input-bg') || styles.getPropertyValue('--editor-bg') || '#1e1e1e').trim();
            const foreground = (styles.getPropertyValue('--text') || '#ffffff').trim();
            const selection = (styles.getPropertyValue('--selection-bg') || 'rgba(53,116,240,0.25)').trim();
            return {
                allowProposedApi: true,
                convertEol: true,
                fontFamily: font,
                fontSize,
                lineHeight: 1.2,
                disableStdin: false,
                cursorBlink: true,
                theme: {
                    background,
                    foreground,
                    cursor: foreground,
                    selection
                }
            };
        }

        function flushBufferedOutput(tab) {
            if (!tab?.term || !tab?.buffer || !tab.buffer.length) return;
            const chunks = Array.isArray(tab.buffer) ? tab.buffer : [tab.buffer];
            chunks.forEach(chunk => writeAndFollow(tab, chunk));
            tab.buffer = Array.isArray(tab.buffer) ? [] : '';
        }

        async function startTerminalSession(tab, state) {
            updateTerminalStatusPill();
            const dims = measureTerminal(tab);
            const sessionOptions = {
                shell: terminalConfig.shellPath || undefined,
                cwd: getTerminalCwd(),
                cols: dims?.cols,
                rows: dims?.rows
            };
            if (!window.ahmadIDE.terminalCreate) {
                writeAndFollow(tab, 'Terminal backend unavailable.', { newline: true });
                return;
            }
            const res = await window.ahmadIDE.terminalCreate(sessionOptions);
            if (res && res.ok) {
                tab.sessionId = res.id;
                state.sessionMap[res.id] = tab.id;
                if (dims && !tab._plain) {
                    await window.ahmadIDE.terminalResize(tab.sessionId, dims.cols, dims.rows);
                }
                flushBufferedOutput(tab);
                setTerminalError(null);
                if (tab._plain) {
                    writeAndFollow(tab, 'Running with basic renderer. Install xterm for full TUI/ANSI support.', { newline: true });
                }
            } else {
                const msg = res?.error || 'Unknown error starting terminal';
                writeAndFollow(tab, `\x1b[31m✗ Failed to start terminal: ${msg}\x1b[0m`, { newline: true });
                setTerminalError(msg);
            }
        }

        async function sendCtrlC(state) {
            const tab = getActiveTerminalTab(state);
            if (!tab || !tab.sessionId || !window.ahmadIDE.terminalWrite) return;
            await window.ahmadIDE.terminalWrite(tab.sessionId, '\u0003');
            writeAndFollow(tab, '^C\r\n');
        }

        function createPlainTerminalTab(id, name, container, state) {
            container.classList.add('plain-terminal');
            const output = document.createElement('pre');
            output.className = 'plain-terminal-output';
            output.textContent = 'Starting...';
            const hiddenInput = document.createElement('textarea');
            hiddenInput.className = 'plain-terminal-input-hidden';
            hiddenInput.setAttribute('aria-label', 'Terminal input');
            container.appendChild(output);
            container.appendChild(hiddenInput);

            const tab = {
                id,
                name,
                buffer: '',
                sessionId: null,
                term: null,
                container,
                lastSize: null,
                _plain: true
            };

            const render = () => {
                output.textContent = tab.buffer || '';
                output.scrollTop = output.scrollHeight;
            };

            const send = async (data) => {
                if (tab.sessionId && window.ahmadIDE.terminalWrite) {
                    await window.ahmadIDE.terminalWrite(tab.sessionId, data);
                }
            };

            tab.term = {
                write: (data) => {
                    tab.buffer += data || '';
                    render();
                },
                writeln: (data) => {
                    tab.buffer += `${data ?? ''}\n`;
                    render();
                },
                reset: () => {
                    tab.buffer = '';
                    render();
                },
                resize: () => { },
                dispose: () => { },
                focus: () => hiddenInput.focus()
            };

            const handleKey = async (e) => {
                if (e.key === 'Escape' && terminalConfig.escapeToEditor && !terminalConfig.overrideIdeShortcuts) {
                    e.preventDefault();
                    getActiveEditor()?.focus();
                    return;
                }
                if (e.ctrlKey && e.key.toLowerCase() === 'c') {
                    e.preventDefault();
                    await sendCtrlC(state);
                    return;
                }
                if (!tab.sessionId) return;
                if (e.key === 'Enter') {
                    e.preventDefault();
                    await send('\r');
                    return;
                }
                if (e.key === 'Backspace') {
                    e.preventDefault();
                    await send('\x7f');
                    return;
                }
                if (e.key === 'Tab') {
                    e.preventDefault();
                    await send('\t');
                    return;
                }
                if (e.key.length === 1 && !e.metaKey) {
                    e.preventDefault();
                    await send(e.key);
                }
            };

            hiddenInput.addEventListener('keydown', handleKey);
            container.addEventListener('mousedown', () => hiddenInput.focus());
            hiddenInput.tabIndex = 0;

            render();
            return tab;
        }

        async function addTerminalTab(state, isDefault = false) {
            try {
                logger.info('TERMINAL_NEW_TAB', { isDefault, existingTabs: state?.tabs?.length });
                let terminalCtor = null;
                try {
                    terminalCtor = await ensureTerminalEngine();
                } catch (err) {
                    console.warn('Falling back to basic terminal renderer', err);
                    setTerminalError(err?.message || 'Terminal engine unavailable; using fallback renderer');
                }
                ensureTerminalListeners(state);
                ensureTerminalResizeObserver(state);
                state.counter += 1;
                const id = `term${state.counter}`;
                const tabName = isDefault ? 'Local' : `Local ${state.counter}`;
                const container = createTerminalContainer(id);
                if (!container) {
                    showToast('error', 'Terminal', 'Missing terminal container');
                    return;
                }
                const tab = terminalCtor && !terminalFallbackMode
                    ? {
                        id,
                        name: tabName,
                        buffer: [],
                        sessionId: null,
                        term: new terminalCtor(buildTerminalOptions()),
                        container,
                        lastSize: null
                    }
                    : createPlainTerminalTab(id, tabName, container, state);
                state.tabs.push(tab);
                state.active = id;

                if (tab.term && !tab._plain) {
                    tab.term.onData(async (data) => {
                        if (tab.sessionId && window.ahmadIDE.terminalWrite) {
                            await window.ahmadIDE.terminalWrite(tab.sessionId, data);
                        }
                    });
                    tab.term.onResize(({ cols, rows }) => {
                        tab.lastSize = { cols, rows };
                        if (tab.sessionId && window.ahmadIDE.terminalResize) {
                            window.ahmadIDE.terminalResize(tab.sessionId, cols, rows);
                        }
                    });
                    tab.term.attachCustomKeyEventHandler((ev) => {
                        if (
                            terminalConfig.escapeToEditor &&
                            !terminalConfig.overrideIdeShortcuts &&
                            ev.key === 'Escape' &&
                            !ev.altKey && !ev.ctrlKey && !ev.metaKey && !ev.shiftKey
                        ) {
                            getActiveEditor()?.focus();
                            return false;
                        }
                        return true;
                    });
                    tab.term.open(container);
                }
                renderTerminalTabs(state);
                activateTerminalTab(state, id);
                refreshTerminalLayout(state, { resizeSession: false });
                setTimeout(() => refreshTerminalLayout(state), 50);
                flushBufferedOutput(tab);
                await startTerminalSession(tab, state);
                logger.info('TERMINAL_TAB_READY', { id, sessionId: tab.sessionId });
            } catch (err) {
                console.error('Failed to create terminal tab', err);
                setTerminalError(err?.message || 'Terminal engine unavailable');
                showToast('error', 'Terminal', err?.message || 'Unable to start terminal');
                logger.error('TERMINAL_START_ERROR', { message: err?.message, stack: err?.stack });
            }
        }

        function appendOutput(text, state) {
            if (!state || !state.tabs.length) return;
            const tab = getActiveTerminalTab(state);
            if (!tab) return;
            const lines = Array.isArray(text) ? text : [text];
            if (tab.term) {
                lines.forEach(line => writeAndFollow(tab, line || '', { newline: true }));
            } else {
                tab.buffer = tab.buffer || [];
                lines.forEach(line => tab.buffer.push((line || '') + '\n'));
            }
            flushBufferedOutput(tab);
        }

        function clearOutput(state) {
            if (!state) return;
            const tab = getActiveTerminalTab(state);
            if (!tab?.term) return;
            tab.term.reset();
            tab.buffer = [];
            tab.lastSize = null;
            refreshTerminalLayout(state);
        }

        return {
            getTerminalCwd,
            focusTerminal,
            isTerminalFocused,
            createTerminalState,
            getActiveTerminalTab,
            updateTerminalStatusPill,
            ensureTerminalListeners,
            renderTerminalTabs,
            activateTerminalTab,
            refreshTerminalLayout,
            addTerminalTab,
            closeTerminalTab,
            appendOutput,
            clearOutput,
            sendCtrlC
        };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.terminal = window.AhmadIDEModules.terminal || {};
        window.AhmadIDEModules.terminal.createTerminalManager = createTerminalManager;
    }
})();
