(() => {
    function createToolWindowManager({ state, deps } = {}) {
        const toolWindowState = state?.toolWindowState;
        const logger = deps?.logger;
        const getGlobalTerminalState = deps?.getGlobalTerminalState;
        const refreshTerminalLayout = deps?.refreshTerminalLayout;
        const focusTerminal = deps?.focusTerminal;
        const getTerminalToolApi = deps?.getTerminalToolApi;
        const getOpenGitToolWindow = deps?.getOpenGitToolWindow;
        const getOpenCommitToolWindow = deps?.getOpenCommitToolWindow;

        function ensureBottomPanel(panelId) {
            const state = toolWindowState.bottom;
            if (!state) return;
            if (state.visible && state.activePanel === panelId) return;
            toggleToolWindowPanel(panelId, 'bottom');
        }

        function setActiveToolWindow(panelId) {
            // Legacy support - map to new structure
            const btn = document.querySelector(`.tool-window-stripe-btn[data-panel="${panelId}"]`);
            if (btn) {
                const position = btn.getAttribute('data-position');
                toggleToolWindowPanel(panelId, position);
            }
        }

        function toggleToolWindowPanel(panelId, position) {
            const state = toolWindowState[position];
            if (!state) return;
            logger.info('TOOLWINDOW_TOGGLE', { panelId, position, visible: state.visible, activePanel: state.activePanel });

            const contentArea = document.getElementById(`${position}ToolWindow`);
            const buttons = document.querySelectorAll(`.tool-window-stripe-btn[data-position="${position}"]`);

            // If clicking the same panel, toggle visibility (bottom keeps stripe visible)
            if (state.activePanel === panelId && state.visible) {
                state.visible = false;
                state.activePanel = null;
                if (position === 'bottom') {
                    if (contentArea) contentArea.classList.add('hidden'); // Use hidden to collapse fully
                    const bottomPanels = ['terminalToolPanel', 'terminalPanel', 'debugPanel', 'problemsPanel', 'comparePanel', 'servicesPanel', 'gitToolPanel', 'extensionsPanel', 'globalImpactPanel', 'patchTrackingPanel'];
                    bottomPanels.forEach(id => {
                        const el = document.getElementById(id);
                        if (el) el.classList.add('hidden');
                    });

                    // Collapse wrapper to reclaim space (icons-only mode)
                    const wrapper = document.getElementById('bottomBarWrapper');
                    if (wrapper) wrapper.classList.add('collapsed');
                } else {
                    if (contentArea) contentArea.classList.add('hidden');
                }
                buttons.forEach(b => b.classList.remove('active'));
                try {
                    window.dispatchEvent(new CustomEvent('ahmadIDE:toolwindow-hidden', { detail: { panelId, position } }));
                } catch (_) { }
                return;
            }

            // Lazy-mount heavy panels on first open to reduce DOM/layout/paint cost.
            try {
                window.AhmadIDEModules?.app?.featureRegistry?.ensureById?.(panelId);
            } catch (_) {
                // ignore mount failures
            }

            // Show the content area and switch panels
            state.visible = true;
            state.activePanel = panelId;
            if (contentArea) {
                contentArea.classList.remove('hidden');
                if (position === 'bottom') {
                    contentArea.classList.remove('collapsed');
                }
            }

            // Update button states
            buttons.forEach(b => {
                b.classList.toggle('active', b.getAttribute('data-panel') === panelId);
            });

            // Show/hide panels within the content area
            const panels = contentArea ? contentArea.querySelectorAll('.tool-window-panel') : [];
            panels.forEach(p => {
                p.classList.toggle('hidden', p.id !== panelId);
            });

            // Handle bottom panels specially (they're in bottom-panels-container)
            if (position === 'bottom') {
                const bottomPanels = ['terminalToolPanel', 'terminalPanel', 'debugPanel', 'problemsPanel', 'comparePanel', 'servicesPanel', 'gitToolPanel', 'extensionsPanel', 'globalImpactPanel', 'patchTrackingPanel'];
                bottomPanels.forEach(id => {
                    const el = document.getElementById(id);
                    if (el) el.classList.toggle('hidden', id !== panelId);
                });

                // Ensure wrapper is visible and expanded
                const wrapper = document.getElementById('bottomBarWrapper');
                if (wrapper) {
                    wrapper.classList.remove('hidden');
                    wrapper.classList.remove('collapsed');
                }
            }

            const globalTerminalState = getGlobalTerminalState ? getGlobalTerminalState() : null;
            if (panelId === 'terminalPanel' && state.visible && globalTerminalState) {
                refreshTerminalLayout(globalTerminalState);
                setTimeout(() => focusTerminal(), 10);
            }
            if (panelId === 'terminalToolPanel' && state.visible) {
                setTimeout(() => {
                    const terminalToolApi = getTerminalToolApi ? getTerminalToolApi() : null;
                    try { terminalToolApi?.fit?.(); } catch (_) { }
                    try { terminalToolApi?.focus?.(); } catch (_) { }
                }, 10);
            }

            try {
                window.dispatchEvent(new CustomEvent('ahmadIDE:toolwindow-activated', { detail: { panelId, position } }));
            } catch (_) { }
        }

        function bindToolWindows() {
            // Bind new -style tool window stripe buttons
            const stripeButtons = document.querySelectorAll('.tool-window-stripe-btn');
            stripeButtons.forEach(btn => {
                btn.addEventListener('click', () => {
                    const panelId = btn.getAttribute('data-panel');
                    const position = btn.getAttribute('data-position');
                    if (panelId === 'terminalToolPanel') {
                        const terminalToolApi = getTerminalToolApi ? getTerminalToolApi() : null;
                        if (terminalToolApi?.openTerminalToolWindow) {
                            terminalToolApi.openTerminalToolWindow({ source: 'stripe' });
                        } else {
                            toggleToolWindowPanel('terminalToolPanel', 'bottom');
                        }
                        return;
                    }
                    if (panelId === 'gitToolPanel') {
                        const openGitToolWindow = getOpenGitToolWindow ? getOpenGitToolWindow() : null;
                        openGitToolWindow({ source: 'stripe' });
                        return;
                    }
                    if (panelId === 'commitPanel') {
                        const openCommitToolWindow = getOpenCommitToolWindow ? getOpenCommitToolWindow() : null;
                        openCommitToolWindow({ source: 'stripe' });
                        return;
                    }
                    toggleToolWindowPanel(panelId, position);
                });
            });

            // Bind hide buttons in tool window headers
            document.querySelectorAll('.hide-panel-btn, #hideProjectBtn').forEach(btn => {
                btn.addEventListener('click', () => {
                    const panel = btn.closest('.tool-window-panel');
                    if (panel) {
                        const panelId = panel.id;
                        const stripeBtn = document.querySelector(`.tool-window-stripe-btn[data-panel="${panelId}"]`);
                        if (stripeBtn) {
                            const position = stripeBtn.getAttribute('data-position');
                            toggleToolWindowPanel(panelId, position);
                        }
                    }
                });
            });

            // Initialize: show left (project) and bottom (terminal) by default
            toolWindowState.left.visible = true;
            toolWindowState.left.activePanel = 'projectPanel';
            toolWindowState.bottom.visible = true;
            toolWindowState.bottom.activePanel = 'terminalToolPanel';
            toolWindowState.right.visible = false;

            // Set initial UI state
            document.getElementById('leftToolWindow')?.classList.remove('hidden');
            document.getElementById('rightToolWindow')?.classList.add('hidden');
            document.getElementById('bottomToolWindow')?.classList.remove('hidden');

            // Hide non-active bottom panels
            ['terminalPanel', 'debugPanel', 'problemsPanel', 'comparePanel', 'servicesPanel', 'gitToolPanel', 'extensionsPanel'].forEach(id => {
                const el = document.getElementById(id);
                if (el) el.classList.add('hidden');
            });
            document.getElementById('terminalToolPanel')?.classList.remove('hidden');

            // Ensure default bottom panel is mounted (Terminal is default).
            try { window.AhmadIDEModules?.app?.featureRegistry?.ensureById?.('terminalToolPanel'); } catch (_) { }
            try { window.dispatchEvent(new CustomEvent('ahmadIDE:toolwindow-activated', { detail: { panelId: 'terminalToolPanel', position: 'bottom' } })); } catch (_) { }

            // Legacy binding for old toolwindow-btn (if any still exist)
            const legacyButtons = document.querySelectorAll('.toolwindow-btn');
            legacyButtons.forEach(btn => {
                btn.addEventListener('click', () => {
                    const target = btn.getAttribute('data-panel');
                    setActiveToolWindow(target);
                });
            });
        }

        return {
            ensureBottomPanel,
            setActiveToolWindow,
            toggleToolWindowPanel,
            bindToolWindows
        };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.renderer = window.AhmadIDEModules.renderer || {};
        window.AhmadIDEModules.renderer.toolWindows = window.AhmadIDEModules.renderer.toolWindows || {};
        window.AhmadIDEModules.renderer.toolWindows.createToolWindowManager = createToolWindowManager;
    }
})();
