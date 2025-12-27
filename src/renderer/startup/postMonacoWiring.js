(() => {
    function createPostMonacoWiring({ deps } = {}) {
        const connectionsManager = deps?.connectionsManager;
        const appendOutput = deps?.appendOutput;
        const setConnStatus = deps?.setConnStatus;
        const loadRoutineList = deps?.loadRoutineList;
        const closeShortcutsPanel = deps?.closeShortcutsPanel;
        const parseShortcutString = deps?.parseShortcutString;
        const loadShortcutPrefs = deps?.loadShortcutPrefs;
        const persistShortcutPrefs = deps?.persistShortcutPrefs;
        const registeredShortcuts = deps?.registeredShortcuts;
        const applyShortcutBinding = deps?.applyShortcutBinding;
        const renderShortcutsPanel = deps?.renderShortcutsPanel;
        const setCollapseStateAll = deps?.setCollapseStateAll;
        const getActiveEditor = deps?.getActiveEditor;
        const renderBreakpoints = deps?.renderBreakpoints;
        const decorateBreakpoints = deps?.decorateBreakpoints;
        const settingsPanelManager = deps?.settingsPanelManager;
        const projectCreateManager = deps?.projectCreateManager;
        const gitToolWindowManager = deps?.gitToolWindowManager;
        const setDebugButtons = deps?.setDebugButtons;
        const renderProjectTreeLoading = deps?.renderProjectTreeLoading;
        const renderProjectTree = deps?.renderProjectTree;
        const showToast = deps?.showToast;
        const renderLocals = deps?.renderLocals;
        const renderStack = deps?.renderStack;
        const renderDebugConsole = deps?.renderDebugConsole;
        const resetDebugUI = deps?.resetDebugUI;
        const ctrlHoverManager = deps?.ctrlHoverManager;

        function wirePostMonacoWiring(editor, routineState, terminalState, dbgState, fetchCurrentBranch, onGitToolWindowApi) {
            connectionsManager.wireConnectionsPanel({ editor, routineState, terminalState });

            // Auto-connect check (Docker default)
            setTimeout(() => {
                connectionsManager.checkForAutoConnect?.({ editor, routineState, terminalState }, {
                    appendOutput,
                    setConnStatus,
                    closeConnectionsPanel: () => {
                        const panel = document.getElementById('connectionsPanel');
                        if (panel) panel.classList.add('hidden');
                        const overlay = document.getElementById('connectionsOverlay');
                        if (overlay) overlay.classList.add('hidden');
                    },
                    loadRoutineList: async (rs, ed) => {
                        await loadRoutineList(rs, ed, document.getElementById('routineSearch')?.value || '', null);
                    }
                });
            }, 500);

            document.getElementById('closeShortcutsBtn')?.addEventListener('click', closeShortcutsPanel);
            // Legacy overlay removed - dialog handles backdrop clicks
            document.getElementById('saveShortcutBtn')?.addEventListener('click', () => {
                const input = document.getElementById('shortcutInput');
                const select = document.getElementById('shortcutSelect');
                if (!input || !select) return;
                const actionId = select.value;
                const parsed = parseShortcutString(input.value);
                if (!parsed) {
                    appendOutput('✗ Invalid shortcut. Use format like Ctrl+D or Ctrl+Shift+L', terminalState);
                    return;
                }
                const prefs = loadShortcutPrefs();
                prefs[actionId] = parsed;
                persistShortcutPrefs(prefs);
                const target = registeredShortcuts.find(s => s.actionId === actionId);
                if (target) {
                    applyShortcutBinding(editor, actionId, parsed, target.handler);
                    target.binding = parsed;
                }
                appendOutput(`✓ Shortcut updated: ${actionId} -> ${input.value}`, terminalState);
                renderShortcutsPanel();
            });
            document.getElementById('expandAllBtn')?.addEventListener('click', () => setCollapseStateAll(false));
            document.getElementById('collapseAllBtn')?.addEventListener('click', () => setCollapseStateAll(true));
            document.getElementById('bpClearAllBtn')?.addEventListener('click', () => {
                dbgState.breakpoints?.clear();
                renderBreakpoints(dbgState);
                decorateBreakpoints(getActiveEditor(), dbgState);
            });
            document.getElementById('gitClearBtn')?.addEventListener('click', () => {
                const out = document.getElementById('gitOutput');
                if (out) out.textContent = 'Git ready.';
            });
            settingsPanelManager.wireSettingsPanel();
            projectCreateManager.wireNewProjectPanel();

            const gitToolWindowApi = gitToolWindowManager.wireGitToolWindow({ fetchCurrentBranch });
            if (typeof onGitToolWindowApi === 'function') onGitToolWindowApi(gitToolWindowApi);

            // --- Initial debug / UI state ---
            setDebugButtons(false);
            renderProjectTreeLoading('Loading routines…');
            loadRoutineList(routineState, editor).catch((err) => {
                console.error('[Routines] Failed to load:', err);
                renderProjectTree([], routineState, editor);
                showToast('error', 'Routines', err?.message || 'Failed to load routines');
            });
            // Run Output (legacy terminal) is lazy: only create tabs when Run is triggered
            // or when the Run tool window is opened. Avoid early init because the panel
            // template isn't mounted while hidden, which can cause "Missing terminal container".
            renderBreakpoints(dbgState);
            renderLocals({});
            renderStack([]);
            renderDebugConsole([]);
            resetDebugUI();
            ctrlHoverManager.bindCtrlHoverAndGutter(editor, dbgState);
        }

        return { wirePostMonacoWiring };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.renderer = window.AhmadIDEModules.renderer || {};
        window.AhmadIDEModules.renderer.startup = window.AhmadIDEModules.renderer.startup || {};
        window.AhmadIDEModules.renderer.startup.createPostMonacoWiring = createPostMonacoWiring;
    }
})();
