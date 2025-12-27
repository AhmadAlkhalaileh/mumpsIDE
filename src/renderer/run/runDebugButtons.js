(() => {
    function createRunDebugButtonsWiring({ deps } = {}) {
        const loadRoutineList = deps?.loadRoutineList;
        const routinesManager = deps?.routinesManager;
        const logger = deps?.logger;
        const appendOutput = deps?.appendOutput;
        const mumpsLinter = deps?.mumpsLinter;
        const hasLintRules = deps?.hasLintRules;
        const applyLintMarkers = deps?.applyLintMarkers;
        const renderProblems = deps?.renderProblems;
        const debugContinue = deps?.debugContinue;
        const startDebugSession = deps?.startDebugSession;
        const runMumpsCode = deps?.runMumpsCode;
        const debugStop = deps?.debugStop;
        const updateDebugButtonState = deps?.updateDebugButtonState;
        const clearOutput = deps?.clearOutput;
        const addTerminalTab = deps?.addTerminalTab;
        const toggleToolWindowPanel = deps?.toggleToolWindowPanel;
        const currentDebugSessionRef = deps?.currentDebugSessionRef;

        function wireRunDebugButtons($, editor, routineState, terminalState, dbgState, debugBar, getBpLines) {
            // --- Run & Debug buttons ---
            const runConfigState = {
                active: 'run-current',
                labels: {
                    'run-current': 'Current file (Run)',
                    'debug-current': 'Current file (Debug)'
                }
            };

            const runConfigMenu = document.getElementById('runConfigMenu');
            if (runConfigMenu) runConfigMenu.remove();
            const runConfigBtn = document.getElementById('runConfigBtn');
            const runConfigTitleEl = document.getElementById('runConfigTitle');
            const runBtnEl = document.getElementById('runBtn');
            const debugStartBtnEl = document.getElementById('debugStartBtn');

            const formatRunConfigTitle = (label) => {
                const raw = String(label || '').trim();
                if (!raw) return raw;
                // "Current file (Run)" -> "Current file" (PhpStorm-style config label)
                const noParen = raw.replace(/\s*\([^)]*\)\s*$/, '').trim();
                return noParen || raw;
            };

            const setRunConfig = (id) => {
                if (!runConfigState.labels[id]) return;
                runConfigState.active = id;
                const label = runConfigState.labels[id];
                if (runConfigBtn) {
                    runConfigBtn.title = label;
                    runConfigBtn.setAttribute('aria-label', label);
                }
                if (runConfigTitleEl) {
                    runConfigTitleEl.textContent = formatRunConfigTitle(label);
                }
                if (runBtnEl) {
                    runBtnEl.title = `${label} (Ctrl+Enter)`;
                }
                if (runConfigMenu) {
                    runConfigMenu.querySelectorAll('.run-config-item').forEach((item) => {
                        const cfg = item.getAttribute('data-config');
                        item.classList.toggle('active', cfg === id);
                    });
                }
            };

            window.AhmadIDEModules = window.AhmadIDEModules || {};
            window.AhmadIDEModules.app = window.AhmadIDEModules.app || {};
            window.AhmadIDEModules.app.runConfig = window.AhmadIDEModules.app.runConfig || {};
            window.AhmadIDEModules.app.runConfig.getActive = () => runConfigState.active;
            window.AhmadIDEModules.app.runConfig.setActive = (id) => setRunConfig(id);
            window.AhmadIDEModules.app.refreshRoutines = async () => {
                try {
                    if (!routinesManager || !routineState || !editor) return false;
                    const search = document.getElementById('routineSearch')?.value || '';
                    await loadRoutineList(routineState, editor, search);
                    return true;
                } catch (_) {
                    return false;
                }
            };

            const shouldDebugForRun = () => {
                const bps = getBpLines();
                const hasBps = bps.length > 0;
                const debugArmed = dbgState.debugModeEnabled;
                const explicitDebugCfg = runConfigState.active === 'debug-current';
                // Only start debugger when it's armed (Debug button) AND a breakpoint exists,
                // or when explicitly selecting the debug run config with at least one breakpoint.
                return (debugArmed && hasBps) || (explicitDebugCfg && hasBps);
            };

            // Initialize defaults
            setRunConfig(runConfigState.active);

            // RUN: If debug mode enabled, start debugging. Otherwise, run normally.
            if ($) {
                $('#runBtn').on('click', async () => {
                    // Lint code - only errors block execution (warnings/info allowed)
                    const code = editor.getValue();
                    const linter = window._mumpsLinter || mumpsLinter;
                    if (hasLintRules(linter)) {
                        const lintResult = linter.lint(code || '', { mode: 'create' });
                        applyLintMarkers(editor.getModel(), lintResult.issues || []);
                        renderProblems((lintResult.issues || []).map(i => ({
                            message: i.message || i.description || '',
                            severity: i.severity || 'info',
                            line: i.line || null,
                            code: i.ruleId || i.code || null
                        })));
                        const summary = lintResult.summary || { errors: 0, warnings: 0, info: 0 };
                        if (summary.errors > 0) {
                            appendOutput(
                                `✗ Cannot run: ${summary.errors} error(s) found`,
                                terminalState
                            );
                            return;
                        }
                    } else {
                        // Fallback to existing marker check
                        const markers = monaco.editor.getModelMarkers({ owner: 'mumps-check' }) || [];
                        if (markers.length) {
                            const m = markers[0];
                            appendOutput(
                                `✗ Cannot run: ${m.message} (line ${m.startLineNumber})`,
                                terminalState
                            );
                            return;
                        }
                    }

                    const bpLines = getBpLines();
                    const debugActive = shouldDebugForRun();

                    // If debug mode enabled, check if session already ready
                    if (debugActive) {
                        // Check if debugger is already initialized and waiting
                        const currentDebugSession = currentDebugSessionRef.value;
                        if (currentDebugSession && currentDebugSession.ready && !currentDebugSession.currentLine) {
                            // Debugger is ready, send Continue to start execution
                            logger.debug('[RUN] Debugger already ready, sending Continue command');
                            await debugContinue();
                        } else {
                            // Start new debug session
                            debugStartBtnEl?.classList.add('active');
                            dbgState.debugModeEnabled = true;
                            await startDebugSession(editor, dbgState, terminalState, debugBar, bpLines);
                        }
                    } else {
                        // Normal execution (no debugging)
                        const res = await runMumpsCode(editor, terminalState);
                        if (!res || !res.ok) {
                            await stopDebug(editor, dbgState, terminalState, debugBar);
                        }
                    }
                });

                // DEBUG BUTTON: arm/disarm debugger (Run starts it)
                $('#debugStartBtn').on('click', () => {
                    const $btn = $('#debugStartBtn');
                    // If a session is active, stop it
                    const currentDebugSession = currentDebugSessionRef.value;
                    if (currentDebugSession && currentDebugSession.id) {
                        debugStop();
                        return;
                    }
                    dbgState.debugModeEnabled = !dbgState.debugModeEnabled;
                    if (dbgState.debugModeEnabled) {
                        const bpLines = getBpLines();
                        if (bpLines.length === 0) {
                            appendOutput('⚠️  Debug armed, but no breakpoints set. Run will execute normally.', terminalState);
                        } else {
                            appendOutput('✅ Debug armed. Click Run to start debugging.', terminalState);
                        }
                    } else {
                        appendOutput('🛑 Debug disarmed. Run will execute normally.', terminalState);
                    }
                    updateDebugButtonState();
                });
            }

            const wireRunOutputHeader = () => {
                const panelEl = document.getElementById('terminalPanel');
                if (!panelEl || panelEl.dataset.runOutputWired === '1') return;

                const clearBtn = document.getElementById('terminalClearBtn');
                const newTabBtn = document.getElementById('terminalNewTabBtn');
                const hideBtn = document.getElementById('terminalHideBtn');
                const menuBtn = document.getElementById('terminalMenuBtn');
                const dropdownBtn = document.getElementById('terminalDropdownBtn');

                // Not mounted yet.
                if (!clearBtn && !newTabBtn && !hideBtn && !menuBtn && !dropdownBtn) return;
                panelEl.dataset.runOutputWired = '1';

                clearBtn?.addEventListener('click', () => clearOutput(terminalState));
                newTabBtn?.addEventListener('click', () => addTerminalTab(terminalState).catch(() => { }));
                hideBtn?.addEventListener('click', () => toggleToolWindowPanel('terminalPanel', 'bottom'));

                const openRunMenu = (anchorEl) => {
                    const menu = window.AhmadIDEModules?.ui?.menu;
                    const controller = menu?.controller || menu?.createMenuController?.({});
                    if (!controller?.openAtElement) return;
                    controller.openAtElement({
                        anchorEl,
                        items: [
                            { id: 'run.new', label: 'New Run Tab', action: 'run:new-tab' },
                            { id: 'run.clear', label: 'Clear Output', action: 'run:clear' },
                            { type: 'separator' },
                            { id: 'run.hide', label: 'Hide', action: 'run:hide' }
                        ],
                        ctx: {},
                        onAction: async (action) => {
                            switch (action) {
                                case 'run:new-tab':
                                    await addTerminalTab(terminalState);
                                    return;
                                case 'run:clear':
                                    clearOutput(terminalState);
                                    return;
                                case 'run:hide':
                                    toggleToolWindowPanel('terminalPanel', 'bottom');
                                    return;
                            }
                        }
                    });
                };

                const bindMenuBtn = (btn) => {
                    if (!btn) return;
                    btn.addEventListener('click', (e) => {
                        e.preventDefault();
                        e.stopPropagation();
                        openRunMenu(btn);
                    });
                };

                bindMenuBtn(menuBtn);
                bindMenuBtn(dropdownBtn);
            };

            wireRunOutputHeader();
            try {
                window.AhmadIDEModules?.app?.featureRegistry?.onMounted?.('terminalPanel', () => wireRunOutputHeader());
            } catch (_) { }

            // Initialize -style terminal header
            const initTerminalHeader = window.AhmadIDEModules?.terminal?.initTerminalHeader;
            if (initTerminalHeader) {
                const headerCtrl = initTerminalHeader();
                if (window.AhmadIDEModules?.terminal) {
                    window.AhmadIDEModules.terminal.terminalHeaderCtrl = headerCtrl;
                }
            }
        }

        return { wireRunDebugButtons };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.renderer = window.AhmadIDEModules.renderer || {};
        window.AhmadIDEModules.renderer.run = window.AhmadIDEModules.renderer.run || {};
        window.AhmadIDEModules.renderer.run.createRunDebugButtonsWiring = createRunDebugButtonsWiring;
    }
})();
