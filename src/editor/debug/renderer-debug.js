(() => {
    const createNoopLogger = () => ({
        debug: () => { },
        info: () => { },
        warn: () => { },
        error: () => { },
        isEnabled: () => false
    });

    function createDebugManager({ state, deps } = {}) {
        if (!state?.currentDebugSessionRef) {
            throw new Error('createDebugManager requires { state.currentDebugSessionRef }');
        }

        const logger = deps?.logger || createNoopLogger();
        const dbgLog = deps?.dbgLog || (() => { });
        const showToast = deps?.showToast || (() => { });
        const highlightLine = deps?.highlightLine || (() => { });
        const ensureBottomPanel = deps?.ensureBottomPanel || (() => { });
        const getToolWindowState = deps?.getToolWindowState || (() => null);
        const getMonaco = deps?.getMonaco || (() => (typeof monaco !== 'undefined' ? monaco : null));

        const getActiveEditor = deps?.getActiveEditor || (() => null);
        const getActiveRoutineName = deps?.getActiveRoutineName || (() => null);
        const getOpenTabs = deps?.getOpenTabs || (() => []);
        const getActiveTabId = deps?.getActiveTabId || (() => null);
        const getDbgStateRef = deps?.getDbgStateRef || (() => null);
        const getRoutineStateRef = deps?.getRoutineStateRef || (() => null);
        const getRoutinesCache = deps?.getRoutinesCache || (() => []);

        const normalizeRoutineTarget = deps?.normalizeRoutineTarget;
        const findOpenTab = deps?.findOpenTab;
        const createTab = deps?.createTab;
        const switchTab = deps?.switchTab;
        const loadRoutineByName = deps?.loadRoutineByName;

        const getGlobalTerminalState = deps?.getGlobalTerminalState || (() => null);
        const appendOutput = deps?.appendOutput || (() => { });
        const clearOutput = deps?.clearOutput || (() => { });
        const ensureTerminalListeners = deps?.ensureTerminalListeners || (() => { });
        const getTerminalCwd = deps?.getTerminalCwd || (() => null);
        const terminalConfig = deps?.terminalConfig || {};

        if (!normalizeRoutineTarget || typeof normalizeRoutineTarget !== 'function') {
            throw new Error('createDebugManager requires deps.normalizeRoutineTarget');
        }
        if (!findOpenTab || typeof findOpenTab !== 'function') {
            throw new Error('createDebugManager requires deps.findOpenTab');
        }
        if (!createTab || typeof createTab !== 'function') {
            throw new Error('createDebugManager requires deps.createTab');
        }
        if (!switchTab || typeof switchTab !== 'function') {
            throw new Error('createDebugManager requires deps.switchTab');
        }
        if (!loadRoutineByName || typeof loadRoutineByName !== 'function') {
            throw new Error('createDebugManager requires deps.loadRoutineByName');
        }

        const currentDebugSessionRef = state.currentDebugSessionRef;
        const expandedArrayKeys = new Set();
        const getDebugTimelineService = () =>
            deps?.debugTimelineService || window.AhmadIDEModules?.services?.debugTimelineService || null;

        let lastDebugAction = null; // 'into' | 'over' | 'out' | 'continue' | 'start' | null
        const setGlobalDebugPaused = (paused) => {
            const next = !!paused;
            try { window.__ahmadIDE_debugPaused = next; } catch (_) { }
            try {
                window.AhmadIDEModules = window.AhmadIDEModules || {};
                window.AhmadIDEModules.debug = window.AhmadIDEModules.debug || {};
                window.AhmadIDEModules.debug.state = window.AhmadIDEModules.debug.state || {};
                window.AhmadIDEModules.debug.state.paused = next;
            } catch (_) { }
        };

        const bpKeyFor = (file, line) => `${file || 'Untitled'}::${line}`;
        const parseBpKey = (key) => {
            const parts = key.split('::');
            const line = parseInt(parts.pop(), 10);
            const file = parts.join('::') || 'Untitled';
            return { file, line, key };
        };

        const getActiveRoutine = () => {
            const activeRoutineName = getActiveRoutineName();
            const normalized = normalizeRoutineTarget(activeRoutineName);
            if (normalized.path || normalized.base) {
                return normalized.path || normalized.base;
            }
            const openTabs = getOpenTabs() || [];
            const activeTabId = getActiveTabId();
            const tab = openTabs.find(t => t.id === activeTabId);
            const tabNorm = normalizeRoutineTarget(tab?.path || tab?.name || '');
            return tabNorm.path || tabNorm.base || 'Untitled';
        };

        function toggleBreakpoint(line, dbgState, editor) {
            dbgState.breakpoints = dbgState.breakpoints || new Set();
            const file = getActiveRoutine();
            const key = bpKeyFor(file, line);
            if (dbgState.breakpoints.has(key)) {
                dbgState.breakpoints.delete(key);
                logger.info('DEBUG_BREAKPOINT_REMOVED', { file, line });
            } else {
                dbgState.breakpoints.add(key);
                logger.info('DEBUG_BREAKPOINT_SET', { file, line });
            }
            renderBreakpoints(dbgState);
            decorateBreakpoints(editor, dbgState);
        }

        async function focusDebugLocation(routineName, lineNumber, termState, dbgState) {
            const targetInfo = normalizeRoutineTarget(routineName);
            if (!targetInfo.base) return false;

            // TMPDBG (or the active buffer) should map back to the user's current tab
            const homeRoutine = normalizeRoutineTarget(dbgState?.homeRoutine || '');
            if (targetInfo.base === 'TMPDBG' || (homeRoutine.base && targetInfo.base === homeRoutine.base)) {
                if (dbgState?.homeTabId) {
                    switchTab(dbgState.homeTabId);
                }
            } else {
                const targetRoutineKey = targetInfo.path || targetInfo.base;
                let tab = findOpenTab(targetRoutineKey);
                if (!tab) {
                    const readRes = await window.ahmadIDE.readRoutine(targetRoutineKey);
                    if (!readRes.ok) {
                        appendOutput(`✗ Failed to load ${targetRoutineKey}: ${readRes.error || 'Unable to read routine'}`, termState);
                        showToast('error', 'Debug', `Cannot open ${targetRoutineKey}`);
                        return false;
                    }
                    tab = createTab(targetRoutineKey, readRes.code || '');
                }
                switchTab(tab.id);
            }

            const activeEditor = getActiveEditor();
            if (activeEditor && lineNumber) {
                activeEditor.revealLineInCenter(lineNumber);
                activeEditor.setPosition({ lineNumber, column: 1 });
            }
            return true;
        }

        function renderBreakpoints(dbgState) {
            const host = document.getElementById('breakpointsList');
            if (!host) return;
            host.innerHTML = '';
            host.classList.add('bp-list');
            const list = Array.from(dbgState.breakpoints || [])
                .map(parseBpKey)
                .filter(bp => !isNaN(bp.line))
                .sort((a, b) => a.file.localeCompare(b.file) || a.line - b.line);

            if (!list.length) {
                host.innerHTML = '<li>No breakpoints</li>';
                return;
            }
            list.forEach(bp => {
                const li = document.createElement('li');
                li.className = 'bp-item';
                const isCurrentFile = bp.file === getActiveRoutine();
                if (isCurrentFile) li.classList.add('active');
                li.style.cursor = 'pointer';
                li.onclick = async () => {
                    const dbgStateRef = getDbgStateRef();
                    await focusDebugLocation(bp.file, bp.line, getGlobalTerminalState(), dbgStateRef);
                    highlightLine(getActiveEditor(), bp.line);
                };

                const meta = document.createElement('div');
                meta.className = 'bp-meta';

                const dot = document.createElement('span');
                dot.className = 'bp-dot';

                const file = document.createElement('span');
                file.className = 'bp-file';
                file.textContent = bp.file;

                const line = document.createElement('span');
                line.className = 'bp-line';
                line.textContent = `:${bp.line}`;

                meta.appendChild(dot);
                meta.appendChild(file);
                meta.appendChild(line);

                const remove = document.createElement('button');
                remove.className = 'bp-remove';
                remove.textContent = '✕';
                remove.title = 'Remove breakpoint';
                remove.onclick = (e) => {
                    e.stopPropagation();
                    dbgState.breakpoints.delete(bp.key);
                    renderBreakpoints(dbgState);
                    decorateBreakpoints(getActiveEditor(), dbgState);
                };
                li.appendChild(meta);
                li.appendChild(remove);
                host.appendChild(li);
            });
        }

        function decorateBreakpoints(editor, dbgState) {
            if (!editor) return;
            const currentFile = getActiveRoutine();
            const monacoRef = getMonaco();
            const ranges = Array.from(dbgState.breakpoints || [])
                .map(parseBpKey)
                .filter(bp => bp.file === currentFile)
                .map(({ line }) => ({
                    range: new monacoRef.Range(line, 1, line, 1),
                    options: {
                        isWholeLine: false,
                        glyphMarginClassName: 'breakpoint-glyph'
                    }
                }));
            editor._bpDecorations = editor.deltaDecorations(editor._bpDecorations || [], ranges);
        }

        function renderLocals(locals) {
            const host = document.getElementById('localsList');
            if (!host) return;
            host.innerHTML = '';
            const entries = locals ? Object.entries(locals) : [];
            if (!entries.length) {
                host.innerHTML = '<li>None</li>';
                return;
            }
            entries.forEach(([k, v]) => {
                const li = document.createElement('li');

                // Check if it's an array
                if (typeof v === 'object' && v._isArray) {
                    const elementCount = Object.keys(v._elements || {}).length;
                    const expanded = expandedArrayKeys.has(k);

                    const toggle = document.createElement('button');
                    toggle.className = 'locals-toggle';
                    toggle.textContent = expanded ? '-' : '+';
                    toggle.title = expanded ? 'Collapse' : 'Expand';
                    toggle.onclick = (e) => {
                        e.stopPropagation();
                        if (expanded) expandedArrayKeys.delete(k);
                        else expandedArrayKeys.add(k);
                        renderLocals(locals);
                    };

                    const label = document.createElement('span');
                    label.className = 'locals-array-label';
                    label.innerHTML = `<strong>${k}</strong> (Array, ${elementCount} elements)`;
                    label.onclick = toggle.onclick;

                    li.appendChild(toggle);
                    li.appendChild(label);

                    if (expanded) {
                        const ul = document.createElement('ul');
                        ul.className = 'locals-children';
                        const sortedKeys = Object.keys(v._elements).sort();
                        const limit = 50;
                        sortedKeys.slice(0, limit).forEach(key => {
                            const subLi = document.createElement('li');
                            subLi.textContent = `${k}${key} = "${v._elements[key]}"`;
                            ul.appendChild(subLi);
                        });
                        if (sortedKeys.length > limit) {
                            const moreLi = document.createElement('li');
                            moreLi.textContent = `... and ${sortedKeys.length - limit} more (truncated)`;
                            moreLi.style.fontStyle = 'italic';
                            ul.appendChild(moreLi);
                        }
                        li.appendChild(ul);
                    }
                } else {
                    // Simple variable
                    li.innerHTML = `<span style="color: #9cdcfe">${k}</span> = <span style="color: #ce9178">"${v}"</span>`;
                }

                host.appendChild(li);
            });
        }

        function renderStack(stack) {
            const host = document.getElementById('stackList');
            if (!host) return;
            host.innerHTML = '';
            if (!stack || !stack.length) {
                host.innerHTML = '<li>Empty</li>';
                return;
            }
            stack.forEach((frame, idx) => {
                const li = document.createElement('li');
                li.textContent = `#${idx} ${frame}`;
                li.style.cursor = 'pointer';
                li.onclick = async () => {
                    const match = `${frame}`.match(/^([^:]+):(\d+)/);
                    const routine = match ? match[1] : null;
                    const line = match ? parseInt(match[2], 10) : null;
                    if (routine && line) {
                        const dbgStateRef = getDbgStateRef();
                        await focusDebugLocation(routine, line, getGlobalTerminalState(), dbgStateRef);
                        highlightLine(getActiveEditor(), line);
                    } else {
                        showToast('info', 'Debug', 'Frame navigation not available (TODO: richer frame data).');
                    }
                };
                host.appendChild(li);
            });
        }

        function renderDebugConsole(lines) {
            const host = document.getElementById('debugOutput');
            if (!host) return;
            host.textContent = (lines || []).join('\n');
            host.scrollTop = host.scrollHeight;
        }

        function appendDebugConsole(lines) {
            const host = document.getElementById('debugOutput');
            if (!host) return;
            const existing = host.textContent ? host.textContent.split('\n') : [];
            const next = existing.concat(Array.isArray(lines) ? lines : [lines]);
            host.textContent = next.join('\n');
            host.scrollTop = host.scrollHeight;
        }

        function ensureConsoleInput() {
            const pane = document.getElementById('tab-console');
            if (!pane) {
                console.warn('[DEBUG CONSOLE] tab-console element not found - retrying in 1s');
                // Retry after panel might be mounted
                setTimeout(() => {
                    const paneRetry = document.getElementById('tab-console');
                    if (!paneRetry) {
                        console.error('[DEBUG CONSOLE] tab-console still not found after retry');
                        return;
                    }
                    console.log('[DEBUG CONSOLE] Found on retry, initializing...');
                    ensureConsoleInputInternal(paneRetry);
                }, 1000);
                return;
            }
            ensureConsoleInputInternal(pane);
        }

        function ensureConsoleInputInternal(pane) {
            if (!pane) return;

            let input = document.getElementById('debugConsoleInput');
            let btn = document.getElementById('debugConsoleSend');

            console.log('[DEBUG CONSOLE] Elements found:', { input: !!input, btn: !!btn });

            // If elements don't exist, create them
            if (!input || !btn) {
                console.log('[DEBUG CONSOLE] Creating console input elements');
                const wrapper = document.createElement('div');
                wrapper.className = 'debug-console-input';
                input = document.createElement('input');
                input.id = 'debugConsoleInput';
                input.type = 'text';
                input.className = 'ui-input';
                input.placeholder = 'Enter MUMPS code...';
                btn = document.createElement('button');
                btn.id = 'debugConsoleSend';
                btn.className = 'ui-btn ui-btn--sm ui-btn--primary';
                btn.textContent = 'Send';
                btn.title = 'Execute in paused frame';
                wrapper.appendChild(input);
                wrapper.appendChild(btn);
                pane.appendChild(wrapper);
            }

            // Skip if already wired
            if (btn && btn.dataset && btn.dataset.wired) {
                console.log('[DEBUG CONSOLE] Already wired, skipping');
                return;
            }
            if (btn && btn.dataset) {
                btn.dataset.wired = 'true';
            }

            console.log('[DEBUG CONSOLE] Wiring event listeners');

            const run = async () => {
                console.log('[DEBUG CONSOLE] Run function called');
                const currentDebugSession = currentDebugSessionRef.value;
                if (!currentDebugSession || !currentDebugSession.id) {
                    console.warn('[DEBUG CONSOLE] No active debug session');
                    showToast('warn', 'Debug', 'No active debug session. Start debugging first.');
                    return;
                }
                const code = (input.value || '').trim();
                if (!code) {
                    console.warn('[DEBUG CONSOLE] No code to execute');
                    return;
                }
                console.log('[DEBUG CONSOLE] Executing:', code);
                appendDebugConsole(`> ${code}`);
                try {
                    const res = await window.ahmadIDE.debugEval(currentDebugSession.id, code);
                    console.log('[DEBUG CONSOLE] Result:', res);
                    if (res && res.ok) {
                        if (res.output) appendDebugConsole(res.output.split(/\r?\n/));
                        if (res.locals) {
                            currentDebugSession.locals = res.locals;
                            renderLocals(currentDebugSession.locals);
                        }
                    } else {
                        const msg = res?.error || 'Eval failed';
                        appendDebugConsole(`! ${msg}`);
                        if (res?.output) appendDebugConsole(res.output.split(/\r?\n/));
                        showToast('error', 'Debug', msg);
                    }
                } catch (e) {
                    console.error('[DEBUG CONSOLE] Error:', e);
                    const msg = e?.message || 'Eval error';
                    appendDebugConsole(`! ${msg}`);
                    showToast('error', 'Debug', msg);
                } finally {
                    input.value = '';
                }
            };

            if (btn) {
                btn.addEventListener('click', () => {
                    console.log('[DEBUG CONSOLE] Send button clicked');
                    run();
                });
            }
            if (input) {
                input.addEventListener('keydown', (e) => {
                    if (e.key === 'Enter') {
                        console.log('[DEBUG CONSOLE] Enter key pressed');
                        e.preventDefault();
                        run();
                    }
                });
            }
            console.log('[DEBUG CONSOLE] Event listeners attached');
        }

        function logDebug(lines, termState, replace = false) {
            const host = document.getElementById('debugOutput');
            if (!host) return;
            const existing = replace ? [] : (host.textContent ? host.textContent.split('\n') : []);
            const next = existing.concat(lines || []);
            host.textContent = next.join('\n');
            host.scrollTop = host.scrollHeight;
            if (termState) appendOutput(lines.join('\n'), termState);
        }

        async function execTerminalCommand(cmd, state) {
            ensureTerminalListeners(state);
            appendOutput(`$ ${cmd}`, state);
            if (cmd.toLowerCase() === 'clear') {
                clearOutput(state);
                return;
            }
            const active = state.tabs.find(t => t.id === state.active);
            if (!active) return;

            if (!active.sessionId && window.ahmadIDE.terminalCreate) {
                const resCreate = await window.ahmadIDE.terminalCreate({
                    shell: terminalConfig.shellPath || undefined,
                    cwd: getTerminalCwd()
                });
                if (resCreate && resCreate.ok) {
                    active.sessionId = resCreate.id;
                } else {
                    appendOutput(`✗ Unable to start terminal: ${resCreate?.error || 'unknown error'}`, state);
                    showToast('error', 'Terminal', resCreate?.error || 'Unable to start terminal');
                    return;
                }
            }

            if (active.sessionId && window.ahmadIDE.terminalWrite) {
                await window.ahmadIDE.terminalWrite(active.sessionId, `${cmd}\n`);
            } else {
                const res = await window.ahmadIDE.hostExec(cmd);
                if (res.ok) {
                    if (res.stdout) appendOutput(res.stdout.trimEnd(), state);
                    if (res.stderr) appendOutput(res.stderr.trimEnd(), state);
                } else {
                    appendOutput(`✗ ${res.error || res.stderr || 'Command failed'}`, state);
                    showToast('error', 'Terminal', res.error || res.stderr || 'Command failed');
                }
            }
        }

        // Where to stop: QUIT / Q / EOF
        function isStopLine(editor, lineNumber) {
            const model = editor.getModel();
            if (!model) return true;
            const total = model.getLineCount();
            if (lineNumber > total) return true;

            const text = model.getLineContent(lineNumber).trim().toUpperCase();
            if (text.startsWith('QUIT') || text === 'Q') return true;

            return false;
        }

        // Helper function to check if line contains WRITE or READ and extract the expression
        function parseWriteReadCommand(lineText) {
            if (!lineText) return null;

            const trimmed = lineText.trim();

            // Check for WRITE command (W or WRITE)
            const writeMatch = trimmed.match(/^\s*(?:W|WRITE)\s+(.+)$/i);
            if (writeMatch) {
                return {
                    type: 'WRITE',
                    expression: writeMatch[1].trim()
                };
            }

            // Check for READ command (R or READ)
            const readMatch = trimmed.match(/^\s*(?:R|READ)\s+(.+)$/i);
            if (readMatch) {
                return {
                    type: 'READ',
                    expression: readMatch[1].trim()
                };
            }

            return null;
        }

        // Helper function to show WRITE/READ in terminal during debug
        function logDebugIO(editor, lineNum, termState) {
            const model = editor.getModel();
            if (!model) return;

            const lineText = model.getLineContent(lineNum);
            const ioCommand = parseWriteReadCommand(lineText);

            if (ioCommand) {
                const msg = `[Line ${lineNum}] ${ioCommand.type}: ${ioCommand.expression}`;
                appendOutput(`  📝 ${msg}`, termState);
            }
        }

        const routineKey = (value, fallback = 'TMPDBG') => {
            const info = normalizeRoutineTarget(value);
            return info.path || info.base || fallback;
        };

        function normalizeCallStack(stack) {
            if (!Array.isArray(stack)) return [];
            return stack.map(frame => {
                if (typeof frame === 'string') return frame;
                const routine = routineKey(frame?.routine || frame?.returnRoutine || 'TMPDBG');
                const line = frame?.line || frame?.returnLine || '?';
                const tag = frame?.tag || frame?.returnTag;
                return tag ? `${routine}:${line} (${tag})` : `${routine}:${line}`;
            });
        }

        function showDebugError(msg) {
            console.error(msg);
            alert(msg);
        }

        function gotoEditorLine(lineNumber) {
            const activeEditor = getActiveEditor();
            if (!activeEditor || !lineNumber) return;
            activeEditor.revealLineInCenter(lineNumber);
            activeEditor.setPosition({ lineNumber: lineNumber, column: 1 });
            highlightLine(activeEditor, lineNumber);
        }

        function showDebugOutput(output) {
            if (!output || !output.trim()) return;
            const termState = getGlobalTerminalState();
            if (termState) {
                const rawLines = Array.isArray(output) ? output : output.split(/\r?\n/);
                const lines = rawLines.filter((line, idx) => line.length || idx < rawLines.length - 1);
                appendOutput(lines, termState);
            }
            appendDebugConsole(output.split(/\r?\n/));
        }

        async function startDebugSession(editorParam = null, dbgStateParam = null, terminalState = null, debugBarEl = document.getElementById('debugBar'), bpLinesOverride = null) {
            const editorInstance = editorParam || getActiveEditor();
            if (!editorInstance) return;
            const code = editorInstance.getValue();

            const dbgStateRef = getDbgStateRef();
            const dbgState = dbgStateParam || dbgStateRef;
            const termState = terminalState || getGlobalTerminalState();

            // Use real breakpoints from the current UI state (fall back to legacy global if present)
            const inferredBps = Array.isArray(bpLinesOverride)
                ? bpLinesOverride
                : [];
            const legacyBps = (window.editorBreakpoints && Array.isArray(window.editorBreakpoints))
                ? window.editorBreakpoints
                : [];
            const bpLines = Array.from(new Set([...(inferredBps || []), ...(legacyBps || [])]))
                .map(n => parseInt(n, 10))
                .filter(n => Number.isInteger(n) && n > 0);
            window.editorBreakpoints = bpLines; // keep legacy helpers in sync
            const breakpoints = bpLines.map(line => ({ line }));

            // Reset UI before starting to ensure clean state, but keep debug mode toggle untouched
            resetDebugUI(false);

            logger.debug('Starting debug session...', { breakpoints });

            showToast('info', 'Debug', 'Initializing debug session...');

            let res;
            try {
                res = await window.ahmadIDE.debugStart(code, breakpoints, null);
            } catch (e) {
                showToast('error', 'Debug', 'Start exception: ' + e.message);
                console.error(e);
                return;
            }

            if (!res || !res.ok) {
                const rawError = res?.error || 'Unknown error';
                const friendlyError = res?.friendlyError && res.friendlyError.trim();
                const outputText = (res && res.output) ? res.output.trim() : '';

                const primary = friendlyError || rawError;
                const firstLine = outputText
                    ? (outputText.split(/\r?\n/).find(Boolean) || primary)
                    : primary;
                const shortMsg = firstLine.length > 140 ? `${firstLine.slice(0, 140)}…` : firstLine;
                const toastMsg = outputText ? `${shortMsg} (see Terminal for details)` : shortMsg;

                console.error('Start failed:', rawError, friendlyError, outputText);

                if (outputText) {
                    ensureBottomPanel('terminalPanel');
                    appendOutput([
                        '--- Debug start failed ---',
                        friendlyError || rawError,
                        outputText,
                        '--- end ---'
                    ], getGlobalTerminalState());
                }

                showToast('error', 'Debug', toastMsg);
                resetDebugUI(true, true); // keep armed so user can fix and retry
                return;
            }

            logger.debug('debugStart response OK', res);
            dbgLog('[DEBUG] Full response object:', JSON.stringify(res, null, 2));

            currentDebugSessionRef.value = {
                id: res.sessionId,
                engine: res.engine || 'zstep',
                currentLine: res.currentLine,
                currentRoutine: res.currentRoutine || 'TMPDBG',
                currentTag: res.currentTag || '',
                stack: res.stack || [],
                locals: res.locals || {},
                ready: res.ready || false,
                // Remember the original user routine so we can switch back when returning from external calls
                originalRoutine: getRoutineStateRef()?.current || null
            };

            const currentDebugSession = currentDebugSessionRef.value;
            setGlobalDebugPaused(!!currentDebugSession.currentLine);
            try {
                getDebugTimelineService()?.startSession?.(currentDebugSession.id);
            } catch (_) { }
            if (dbgState) {
                dbgState.sessionId = res.sessionId;
                dbgState.locals = currentDebugSession.locals;
                dbgState.state = currentDebugSession.ready ? 'ready' : 'paused';
                dbgState.currentLine = currentDebugSession.currentLine || null;
                dbgState.currentRoutine = currentDebugSession.currentRoutine;
            }

            logger.debug('Debug Session Started:', currentDebugSession);

            // Show initial variables and stack
            renderLocals(currentDebugSession.locals);
            renderStack(currentDebugSession.stack);
            updateDebugButtonState();
            ensureConsoleInput();

            // Remember currently open bottom panel so we can restore after debug
            if (dbgState) {
                dbgState.previousBottomPanel = getToolWindowState()?.bottom?.activePanel || null;
            }

            // Show debug bar if hidden
            const bar = debugBarEl || document.getElementById('debugBar');
            dbgLog('[DEBUG UI] debugBar element:', bar);
            if (bar) {
                bar.classList.remove('hidden');
                bar.style.display = 'flex'; // Force show
                dbgLog('[DEBUG UI] debugBar shown');
            }

            // Auto-open Debug panel without hiding the terminal permanently
            ensureBottomPanel('debugPanel');

            // If the engine is ready but not executing, automatically run to first breakpoint (or stop on entry if none)
            if (currentDebugSession.ready && !currentDebugSession.currentLine) {
                try {
                    setGlobalDebugPaused(false);
                    lastDebugAction = (breakpoints && breakpoints.length) ? 'continue' : 'into';
                    const initialResult = (breakpoints && breakpoints.length)
                        ? await window.ahmadIDE.debugContinue(currentDebugSession.id)
                        : await window.ahmadIDE.debugStep(currentDebugSession.id, 'into');
                    await handleDebugResult(initialResult);
                    return;
                } catch (err) {
                    showToast('error', 'Debug', 'Failed to start execution: ' + err.message);
                    return;
                }
            }

            // If the backend already paused us somewhere, reflect that immediately
            if (currentDebugSession.currentLine) {
                dbgLog('[DEBUG] Debugger in PAUSED state - enabling all buttons');
                setGlobalDebugPaused(true);
                setDebugButtons(true);
                gotoEditorLine(currentDebugSession.currentLine);
                showToast('success', 'Debug', 'Debugger paused at line ' + currentDebugSession.currentLine);
                try {
                    getDebugTimelineService()?.captureOnStop?.({
                        sessionId: currentDebugSession.id,
                        reason: 'Start',
                        location: { routine: currentDebugSession.currentRoutine, tag: currentDebugSession.currentTag, line: currentDebugSession.currentLine },
                        stack: currentDebugSession.stack,
                        locals: currentDebugSession.locals
                    });
                } catch (_) { }
                return;
            }

            dbgLog('[DEBUG] Debugger initialized without a pause location (unexpected).');
        }

        async function debugStepInto() {
            const currentDebugSession = currentDebugSessionRef.value;
            if (!currentDebugSession || !currentDebugSession.id) {
                showToast('warn', 'Debug', 'No active debug session');
                return;
            }
            setGlobalDebugPaused(false);
            lastDebugAction = 'into';
            const result = await window.ahmadIDE.debugStep(currentDebugSession.id, 'into');
            await handleDebugResult(result);
        }

        async function debugStepOver() {
            const currentDebugSession = currentDebugSessionRef.value;
            if (!currentDebugSession || !currentDebugSession.id) {
                showToast('warn', 'Debug', 'No active debug session');
                return;
            }
            setGlobalDebugPaused(false);
            lastDebugAction = 'over';
            const result = await window.ahmadIDE.debugStep(currentDebugSession.id, 'over');
            await handleDebugResult(result);
        }

        async function debugStepOut() {
            const currentDebugSession = currentDebugSessionRef.value;
            if (!currentDebugSession || !currentDebugSession.id) {
                showToast('warn', 'Debug', 'No active debug session');
                return;
            }
            setGlobalDebugPaused(false);
            lastDebugAction = 'out';
            const result = await window.ahmadIDE.debugStep(currentDebugSession.id, 'out');
            await handleDebugResult(result);
        }

        async function debugContinue() {
            const currentDebugSession = currentDebugSessionRef.value;
            dbgLog('[DEBUG] debugContinue called, session:', currentDebugSession);
            if (!currentDebugSession || !currentDebugSession.id) {
                showToast('warn', 'Debug', 'No active debug session');
                return;
            }
            setGlobalDebugPaused(false);
            lastDebugAction = 'continue';
            const result = await window.ahmadIDE.debugContinue(currentDebugSession.id);
            await handleDebugResult(result);
        }

        async function debugStop() {
            const currentDebugSession = currentDebugSessionRef.value;
            if (!currentDebugSession || !currentDebugSession.id) {
                // Just reset UI if no session
                resetDebugUI(true);
                return;
            }
            try {
                const result = await window.ahmadIDE.debugStop(currentDebugSession.id);
                if (result && result.ok) {
                    showDebugOutput(result.output || '');
                    resetDebugUI(true, true); // keep armed so next Run still debugs
                    showToast('info', 'Debug', 'Debug session stopped');
                } else {
                    showDebugError('Stop failed: ' + (result?.error || 'Unknown'));
                }
            } catch (err) {
                showDebugError('Stop failed: ' + (err?.message || 'Unknown'));
            } finally {
                updateDebugButtonState();
            }
        }

        async function handleDebugResult(result) {
            if (!result) {
                showToast('error', 'Debug', 'No response from debugger');
                return;
            }

            showDebugOutput(result.output || '');

            if (!result.ok) {
                const msg = result?.error || 'Unknown error';
                const friendly = result?.friendlyError || '';
                const displayMsg = friendly || msg;
                if (msg === 'Program finished' || msg === 'end') {
                    resetDebugUI(true, true); // keep armed so Run will debug again
                    showToast('success', 'Debug', 'Program finished');
                } else {
                    // Non-fatal error - show error but keep debug session active if it still exists
                    showToast('error', 'Debug', displayMsg);
                    dbgLog('[DEBUG] Debug error:', msg, friendly);

                    // Only reset UI if error indicates session is dead
                    if (msg.includes('Session not found') || msg.includes('process error') ||
                        msg.includes('Failed to send command') || result.output) {
                        resetDebugUI(true, true); // keep armed for retry
                    }
                }
                return;
            }

            if (result.currentLine) {
                const dbgStateRef = getDbgStateRef();
                const routineStateRef = getRoutineStateRef();

                // Execution has started - transition from "ready" to "running"
                const wasReady = currentDebugSessionRef.value?.ready && !currentDebugSessionRef.value?.currentLine;

                if (!currentDebugSessionRef.value) {
                    currentDebugSessionRef.value = {
                        id: result.sessionId || null,
                        engine: 'zstep',
                        originalRoutine: routineStateRef?.current || null,
                        currentTag: result.currentTag || ''
                    };
                }

                const currentDebugSession = currentDebugSessionRef.value;
                currentDebugSession.currentLine = result.currentLine;
                currentDebugSession.currentRoutine = result.currentRoutine || currentDebugSession.currentRoutine;
                currentDebugSession.currentTag = result.currentTag || currentDebugSession.currentTag || '';
                currentDebugSession.stack = result.stack || currentDebugSession.stack || [];
                // CRITICAL: Always update locals from result (don't use fallback to old locals)
                currentDebugSession.locals = result.locals || {};
                currentDebugSession.ready = false; // No longer in ready state

                if (dbgStateRef) {
                    dbgStateRef.locals = currentDebugSession.locals;
                    dbgStateRef.currentLine = result.currentLine;
                    dbgStateRef.state = 'paused';
                    dbgStateRef.currentRoutine = currentDebugSession.currentRoutine;
                }

                // EXTERNAL ROUTINE STEP INTO FIX:
                const newRoutine = result.currentRoutine;
                const currentlyLoadedRoutine = routineStateRef?.current;
                let routineJustChanged = false;

                // Determine which file to load based on the routine
                let fileToLoad = newRoutine;
                if (newRoutine === 'TMPDBG') {
                    // Returning to TMPDBG means returning to the original user routine
                    fileToLoad = currentDebugSession?.originalRoutine;
                    dbgLog(`[DEBUG] Returning to TMPDBG from external routine, will load original routine: ${fileToLoad}`);
                } else if (newRoutine !== 'TMPDBG' && newRoutine !== currentlyLoadedRoutine) {
                    dbgLog(`[DEBUG] Stepping into external routine ${newRoutine}`);
                }

                if (newRoutine && fileToLoad && fileToLoad !== currentlyLoadedRoutine) {
                    dbgLog(`[DEBUG] Routine changed from ${currentlyLoadedRoutine} to ${newRoutine} (file: ${fileToLoad}) - loading source`);
                    routineJustChanged = true;
                    try {
                        await loadRoutineByName(fileToLoad, routineStateRef, getActiveEditor(), getRoutinesCache(), getGlobalTerminalState());
                        dbgLog(`[DEBUG] Successfully loaded routine ${fileToLoad}`);
                    } catch (err) {
                        dbgLog(`[DEBUG] Failed to load routine ${fileToLoad}:`, err);
                        showToast('warn', 'Debug', `Could not load routine ${fileToLoad}: ${err.message}`);
                    }
                }

                const bpLines = (window.editorBreakpoints && Array.isArray(window.editorBreakpoints))
                    ? window.editorBreakpoints
                    : [];
                const isUserBreakpoint = bpLines.includes(result.currentLine);

                // Always show the exact line from the backend (no offset adjustments)
                const displayLine = result.currentLine;

                dbgLog(`[DEBUG] Line display: currentLine=${result.currentLine}, isBreakpoint=${isUserBreakpoint}, routineChanged=${routineJustChanged}, displayLine=${displayLine}`);
                gotoEditorLine(displayLine);

                // Update Variables and Stack panels IMMEDIATELY
                dbgLog('[DEBUG] Updating variables panel with', Object.keys(currentDebugSession.locals).length, 'variables');
                renderLocals(currentDebugSession.locals);
                renderStack(currentDebugSession.stack);
                updateDebugButtonState();

                // Enable all debug buttons (debugger is paused at a line)
                setGlobalDebugPaused(true);
                setDebugButtons(true);

                if (wasReady) {
                    dbgLog('[DEBUG] Execution started (transitioned from ready to running)');
                }

                logger.debug('Debug State Updated:', currentDebugSession);
                dbgLog('[DEBUG] Current variables:', currentDebugSession.locals);

                // Time‑travel snapshots (optional; guarded inside service)
                try {
                    const reason = isUserBreakpoint
                        ? 'Breakpoint'
                        : lastDebugAction === 'into'
                            ? 'Step Into'
                            : lastDebugAction === 'over'
                                ? 'Step Over'
                                : lastDebugAction === 'out'
                                    ? 'Step Out'
                                    : lastDebugAction === 'continue'
                                        ? 'Continue'
                                        : wasReady
                                            ? 'Start'
                                            : 'Stop';

                    getDebugTimelineService()?.captureOnStop?.({
                        sessionId: currentDebugSession.id,
                        reason,
                        location: { routine: currentDebugSession.currentRoutine, tag: currentDebugSession.currentTag, line: currentDebugSession.currentLine },
                        stack: currentDebugSession.stack,
                        locals: currentDebugSession.locals
                    });
                } catch (_) { }
                lastDebugAction = null;
            }
        }

        function updateDebugButtonState() {
            const btn = document.getElementById('debugStartBtn');
            if (!btn) return;
            const dbgStateRef = getDbgStateRef();
            const armed = !!(dbgStateRef && dbgStateRef.debugModeEnabled);
            const live = !!(currentDebugSessionRef.value && currentDebugSessionRef.value.id);
            btn.classList.toggle('armed', armed);
            btn.classList.toggle('live', live);
            btn.title = live
                ? 'Debugging (session active)'
                : armed
                    ? 'Debug armed (Run to start)'
                    : 'Debug (Shift+F9)';
        }

        function resetDebugUI(clearSession = false, keepArmed = false) {
            setGlobalDebugPaused(false);
            setDebugButtons(false);
            const bar = document.getElementById('debugBar');
            if (bar) {
                bar.classList.add('hidden');
                bar.style.display = 'none';
            }

            // Clear editor decorations
            const activeEditor = getActiveEditor();
            if (activeEditor) {
                // Remove highlight line
                highlightLine(activeEditor, null);
            }

            // Clear stack and locals panels
            renderStack([]);
            renderLocals({});

            // Reset session
            if (clearSession) {
                try {
                    const sid = currentDebugSessionRef.value?.id;
                    if (sid) getDebugTimelineService()?.endSession?.(sid);
                } catch (_) { }
                currentDebugSessionRef.value = null;
                const dbgStateRef = getDbgStateRef();
                if (dbgStateRef) {
                    dbgStateRef.sessionId = null;
                    dbgStateRef.locals = {};
                    dbgStateRef.state = 'stopped';
                    dbgStateRef.currentLine = null;
                    if (dbgStateRef.previousBottomPanel) {
                        ensureBottomPanel(dbgStateRef.previousBottomPanel);
                        dbgStateRef.previousBottomPanel = null;
                    }
                }
                if (!keepArmed && dbgStateRef) {
                    dbgStateRef.debugModeEnabled = false;
                }
            }
            updateDebugButtonState();
        }

        function wireDebugTimelineEvents() {
            try {
                if (!window || window.__ahmadIDE_debugTimelineWired) return;
                window.__ahmadIDE_debugTimelineWired = true;
            } catch (_) {
                return;
            }

            window.addEventListener('ahmadIDE:debugTimelineCapture', () => {
                try {
                    const current = currentDebugSessionRef.value;
                    if (!current?.id) return;
                    getDebugTimelineService()?.captureManual?.({
                        sessionId: current.id,
                        reason: 'Manual',
                        location: { routine: current.currentRoutine, tag: current.currentTag, line: current.currentLine },
                        stack: current.stack || [],
                        locals: current.locals || {}
                    });
                } catch (_) { }
            });

            window.addEventListener('ahmadIDE:debugTimelineNavigate', async (e) => {
                try {
                    const routine = String(e?.detail?.routine || '').trim();
                    const line = Number(e?.detail?.line || 0) || null;
                    if (!routine || !line) return;
                    const dbgStateRef = getDbgStateRef();
                    await focusDebugLocation(routine, line, getGlobalTerminalState(), dbgStateRef);
                    highlightLine(getActiveEditor(), line);
                } catch (_) { }
            });
        }

        function setDebugButtons(enabled) {
            const ids = [
                'dbgStepIntoBtn', 'dbgStepOverBtn', 'dbgStepOutBtn',
                'dbgContinueBtn', 'dbgStopBtn', 'dbgRestartBtn', 'dbgPauseBtn'
            ];
            dbgLog('[DEBUG] setDebugButtons called with enabled =', enabled);
            ids.forEach(id => {
                const el = document.getElementById(id);
                if (el) {
                    if (enabled) {
                        el.removeAttribute('disabled');
                        el.disabled = false;
                    } else {
                        el.setAttribute('disabled', 'true');
                        el.disabled = true;
                    }
                    dbgLog(`[DEBUG] Button ${id}: disabled = ${el.disabled}, hasAttribute = ${el.hasAttribute('disabled')}`);
                } else {
                    dbgLog(`[DEBUG] Button ${id} not found in DOM`);
                }
            });
        }

        function scheduleDebugUiBindings() {
            setTimeout(() => {
                const bind = (id, fn) => {
                    const el = document.getElementById(id);
                    if (el) {
                        // Remove old listeners involves cloning or removing event listener if we had the ref.
                        // Simpler: clone node to wipe old listeners
                        const newEl = el.cloneNode(true);
                        el.parentNode.replaceChild(newEl, el);
                        newEl.addEventListener('click', fn);
                    }
                };

                // Session starts when RUN button is clicked (handled in runBtn click handler)
                bind('dbgStepIntoBtn', debugStepInto);
                bind('dbgStepOverBtn', debugStepOver);
                bind('dbgStepOutBtn', debugStepOut);
                bind('dbgContinueBtn', debugContinue);
                bind('dbgStopBtn', debugStop);
                ensureConsoleInput();
            }, 500);
        }

        function registerMumpsHover() {
            const monacoRef = getMonaco();
            monacoRef.languages.registerHoverProvider('mumps', {
                provideHover: function (model, position) {
                    // Only show hover during active debug session
                    const currentDebugSession = currentDebugSessionRef.value;
                    if (!currentDebugSession || !currentDebugSession.locals) {
                        logger.debug('[HOVER] No debug session or locals');
                        return null;
                    }
                    // Don't show hover in "ready" state (before execution starts)
                    if (!currentDebugSession.currentLine) {
                        return null;
                    }

                    const word = model.getWordAtPosition(position);
                    if (!word) {
                        return null;
                    }

                    let varName = word.word.toUpperCase();

                    // Handle Mumps % variables (e.g., %Z) and global variables (e.g., ^DIC)
                    // which getWordAtPosition might split
                    // Check character immediately before the word
                    if (word.startColumn > 1) {
                        const prevCharRange = new monacoRef.Range(position.lineNumber, word.startColumn - 1, position.lineNumber, word.startColumn);
                        const prevChar = model.getValueInRange(prevCharRange);
                        if (prevChar === '%') {
                            varName = '%' + varName;
                        } else if (prevChar === '^') {
                            varName = '^' + varName;
                        }
                    }

                    const subMatch = varName.match(/^(\^?[A-Z%][A-Z0-9]*)(\(.+\))$/);
                    const baseName = subMatch ? subMatch[1] : null;
                    const subscriptKey = subMatch ? subMatch[2] : null;

                    // Check locals for the variable
                    // 1. Exact match (as returned by debugger - should already be uppercase)
                    let val = currentDebugSession.locals[varName];

                    // Fallback: try to find case-insensitive match if exact fails
                    if (val === undefined) {
                        const keys = Object.keys(currentDebugSession.locals);
                        const match = keys.find(k => k.toUpperCase() === varName);
                        if (match) {
                            val = currentDebugSession.locals[match];
                        }
                    }

                    // If hovering an array element (ARR(1)), show that specific value when available
                    if (val === undefined && baseName && currentDebugSession.locals[baseName]) {
                        const arrVal = currentDebugSession.locals[baseName];
                        if (arrVal && typeof arrVal === 'object' && arrVal._isArray && subscriptKey) {
                            const elementVal = arrVal._elements ? arrVal._elements[subscriptKey] : undefined;
                            if (elementVal !== undefined) {
                                return {
                                    contents: [
                                        { value: `**${baseName}${subscriptKey}**` },
                                        { value: '`' + String(elementVal) + '`' }
                                    ]
                                };
                            }
                            val = arrVal;
                        }
                    }

                    if (val !== undefined && val !== null) {
                        let contents = [];
                        if (typeof val === 'object' && val._isArray) {
                            const len = Object.keys(val._elements || {}).length;
                            const keys = Object.keys(val._elements || {}).slice(0, 6);
                            const preview = keys.map(k => `• ${baseName || varName}${k} = \`${val._elements[k]}\``).join('\n');
                            contents = [
                                { value: `**${baseName || varName}** (Array, ${len} item${len === 1 ? '' : 's'})` },
                                { value: preview || '_No elements_' }
                            ];
                            if (len > keys.length) {
                                contents.push({ value: `_…${len - keys.length} more_` });
                            }
                        } else {
                            contents = [
                                { value: `**${varName}**` },
                                { value: '`' + String(val) + '`' }
                            ];
                        }

                        return {
                            range: new monacoRef.Range(position.lineNumber, word.startColumn - (varName.startsWith('%') ? 1 : 0), position.lineNumber, word.endColumn),
                            contents
                        };
                    }

                    return null;
                }
            });
        }

        return {
            parseBpKey,
            getActiveRoutine,
            toggleBreakpoint,
            renderBreakpoints,
            decorateBreakpoints,
            renderLocals,
            renderStack,
            renderDebugConsole,
            appendDebugConsole,
            ensureConsoleInput,
            logDebug,
            execTerminalCommand,
            isStopLine,
            parseWriteReadCommand,
            logDebugIO,
            normalizeCallStack,
            startDebugSession,
            debugStepInto,
            debugStepOver,
            debugStepOut,
            debugContinue,
            debugStop,
            resetDebugUI,
            setDebugButtons,
            updateDebugButtonState,
            scheduleDebugUiBindings,
            registerMumpsHover,
            wireDebugTimelineEvents
        };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.debug = window.AhmadIDEModules.debug || {};
        window.AhmadIDEModules.debug.createDebugManager = createDebugManager;
    }
})();
