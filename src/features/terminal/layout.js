(() => {
    function createTerminalLayout({ deps } = {}) {
        const isFallbackMode = deps?.isFallbackMode || (() => false);
        const getActiveTerminalTab = deps?.getActiveTerminalTab || (() => null);
        const terminalResizeSession =
            deps?.terminalResizeSession ||
            ((sessionId, cols, rows) => window.ahmadIDE?.terminalResize?.(sessionId, cols, rows));

        function measureTerminal(tab) {
            if (!tab?.term || !tab?.container) return null;
            const rect = tab.container.getBoundingClientRect();
            if (rect.width < 10 || rect.height < 10) return null;
            if (isFallbackMode()) return null;

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
            if (resizeSession && tab.sessionId && terminalResizeSession) {
                terminalResizeSession(tab.sessionId, dims.cols, dims.rows);
            }
        }

        return { measureTerminal, refreshTerminalLayout };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.features = window.AhmadIDEModules.features || {};
        window.AhmadIDEModules.features.terminal = window.AhmadIDEModules.features.terminal || {};
        window.AhmadIDEModules.features.terminal.createTerminalLayout = createTerminalLayout;
    }
})();

