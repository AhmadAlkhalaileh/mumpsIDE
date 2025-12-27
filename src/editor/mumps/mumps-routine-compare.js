/**
 * MUMPS Routine Compare
 * Compare two routines in Monaco diff editor
 */

(() => {
    'use strict';

    class MumpsRoutineCompare {
        constructor({ monaco, ahmadIDE, showToast } = {}) {
            this.monaco = monaco;
            this.ahmadIDE = ahmadIDE;
            this.showToast = showToast || (() => { });
            this.diffEditor = null;
            this.diffModels = { original: null, modified: null };
            this.diffPanel = null;
        }

        /**
         * Show compare dialog and open diff
         */
        async compare(leftRoutineName = null, rightRoutineName = null) {
            // If no left routine provided, use current editor
            if (!leftRoutineName) {
                const currentModel = this.getCurrentModel();
                if (!currentModel) {
                    this.showToast('error', 'Compare', 'No active routine to compare');
                    return;
                }
                leftRoutineName = this.getRoutineNameFromModel(currentModel);
            }

            // Show routine selector dialog if right routine NOT provided
            if (!rightRoutineName) {
                rightRoutineName = await this.showRoutineSelector(leftRoutineName);
            }

            if (!rightRoutineName) return;

            // Load both routines
            const leftContent = await this.loadRoutine(leftRoutineName);
            const rightContent = await this.loadRoutine(rightRoutineName);

            if (leftContent === null || rightContent === null) {
                this.showToast('error', 'Compare', 'Failed to load routine(s)');
                return;
            }

            // Open diff editor
            this.openDiffEditor(leftRoutineName, leftContent, rightRoutineName, rightContent);
        }

        /**
         * Show routine selector dialog
         */
        async showRoutineSelector(currentRoutine) {
            return new Promise((resolve) => {
                // Create dialog
                const dialog = document.createElement('div');
                dialog.className = 'mumps-compare-dialog';
                dialog.innerHTML = `
                    <div class="mumps-compare-overlay"></div>
                    <div class="mumps-compare-content">
                        <div class="mumps-compare-header">
                            <h3>Compare Routines</h3>
                            <button class="close-btn">×</button>
                        </div>
                        <div class="mumps-compare-body">
                            <div class="compare-info">
                                <label>Current routine:</label>
                                <strong>${currentRoutine}</strong>
                            </div>
                            <div class="compare-input">
                                <label for="compareWithInput">Compare with:</label>
                                <input
                                    type="text"
                                    id="compareWithInput"
                                    placeholder="Enter routine name..."
                                    autocomplete="off"
                                />
                                <div class="compare-suggestions" id="compareSuggestions"></div>
                            </div>
                        </div>
                        <div class="mumps-compare-footer">
                            <button class="btn ghost cancel-btn">Cancel</button>
                            <button class="btn primary compare-btn" disabled>Compare</button>
                        </div>
                    </div>
                `;

                document.body.appendChild(dialog);

                const input = dialog.querySelector('#compareWithInput');
                const compareBtn = dialog.querySelector('.compare-btn');
                const cancelBtn = dialog.querySelector('.cancel-btn');
                const closeBtn = dialog.querySelector('.close-btn');

                const cleanup = () => {
                    dialog.remove();
                };

                const cancel = () => {
                    cleanup();
                    resolve(null);
                };

                const confirm = () => {
                    const routineName = input.value.trim();
                    if (routineName) {
                        cleanup();
                        resolve(routineName);
                    }
                };

                // Event listeners
                input.addEventListener('input', () => {
                    compareBtn.disabled = !input.value.trim();
                });

                input.addEventListener('keydown', (e) => {
                    if (e.key === 'Enter' && input.value.trim()) {
                        confirm();
                    } else if (e.key === 'Escape') {
                        cancel();
                    }
                });

                compareBtn.addEventListener('click', confirm);
                cancelBtn.addEventListener('click', cancel);
                closeBtn.addEventListener('click', cancel);
                dialog.querySelector('.mumps-compare-overlay').addEventListener('click', cancel);

                // Focus input
                setTimeout(() => input.focus(), 100);
            });
        }

        /**
         * Load routine content
         */
        async loadRoutine(routineName) {
            if (!this.ahmadIDE) {
                console.error('[Compare] ahmadIDE bridge not available');
                this.showToast('error', 'Compare', 'IDE bridge not available');
                return null;
            }

            try {
                // Preferred API (preload.js): readRoutine(name) -> { ok, code }
                if (typeof this.ahmadIDE.readRoutine === 'function') {
                    const result = await this.ahmadIDE.readRoutine(routineName);
                    console.log('[Compare] readRoutine result for', routineName, ':', result);

                    if (result && result.ok && typeof result.code === 'string') return result.code;
                    if (result && result.ok && typeof result.content === 'string') return result.content;

                    // Show error details
                    if (result && !result.ok) {
                        const errMsg = result.error || result.message || 'Unknown error';
                        console.error('[Compare] readRoutine failed:', errMsg);
                        this.showToast('warning', 'Compare', `Could not load ${routineName}: ${errMsg}`);
                    }
                    return null;
                }

                // Backwards-compat fallback
                if (typeof this.ahmadIDE.loadRoutine === 'function') {
                    const result = await this.ahmadIDE.loadRoutine(routineName);
                    if (result && typeof result.content === 'string') return result.content;
                    if (result && typeof result.code === 'string') return result.code;
                    return null;
                }

                console.error('[Compare] No compatible routine read API found on ahmadIDE');
                this.showToast('error', 'Compare', 'No routine read API available');
                return null;
            } catch (err) {
                console.error('[Compare] Failed to load routine:', err);
                this.showToast('error', 'Compare', `Error loading routine: ${err.message}`);
                return null;
            }
        }

        disposeDiffModels() {
            try {
                this.diffModels?.original?.dispose?.();
            } catch (_) { }
            try {
                this.diffModels?.modified?.dispose?.();
            } catch (_) { }
            this.diffModels = { original: null, modified: null };
        }

        /**
         * Open Monaco diff editor
         */
        openDiffEditor(leftName, leftContent, rightName, rightContent) {
            // Create or reuse diff panel
            if (!this.diffPanel) {
                this.createDiffPanel();
            }

            // Clean up previous editor/models
            if (this.diffEditor) {
                try { this.diffEditor.dispose(); } catch (_) { }
                this.diffEditor = null;
            }
            this.disposeDiffModels();

            // Create models
            const ts = Date.now();
            const leftUri = this.monaco.Uri?.parse?.(`inmemory://compare/original/${encodeURIComponent(leftName)}-${ts}.m`);
            const rightUri = this.monaco.Uri?.parse?.(`inmemory://compare/modified/${encodeURIComponent(rightName)}-${ts}.m`);

            const leftModel = this.monaco.editor.createModel(leftContent, 'mumps', leftUri);
            const rightModel = this.monaco.editor.createModel(rightContent, 'mumps', rightUri);
            this.diffModels = { original: leftModel, modified: rightModel };

            // Create diff editor
            const container = this.diffPanel.querySelector('.diff-editor-container');
            container.innerHTML = ''; // Clear previous

            this.diffEditor = this.monaco.editor.createDiffEditor(container, {
                automaticLayout: true,
                renderSideBySide: true,
                readOnly: true,
                minimap: { enabled: true },
                scrollBeyondLastLine: false,
                fontSize: 13,
                lineNumbers: 'on',
                renderOverviewRuler: true,
                overviewRulerBorder: true,
                scrollbar: {
                    verticalScrollbarSize: 12,
                    horizontalScrollbarSize: 12
                },
                diffWordWrap: 'off',
                ignoreTrimWhitespace: false
            });

            this.diffEditor.setModel({
                original: leftModel,
                modified: rightModel
            });

            // Update header
            this.diffPanel.querySelector('.left-label').textContent = leftName;
            this.diffPanel.querySelector('.right-label').textContent = rightName;

            // Show panel
            this.diffPanel.classList.add('visible');

            // Build changes list
            this.buildChangesList();

            this.showToast('success', 'Compare', `Comparing: ${leftName} ↔ ${rightName}`);
        }

        /**
         * Create diff panel UI
         */
        createDiffPanel() {
            const panel = document.createElement('div');
            panel.className = 'mumps-diff-panel';
            panel.innerHTML = `
                <div class="diff-panel-header">
                    <div class="diff-panel-title">
                        <span class="left-label"></span>
                        <span class="diff-icon">↔</span>
                        <span class="right-label"></span>
                    </div>
                    <div class="diff-panel-controls">
                        <button class="btn ghost icon-btn" id="diffSwapBtn" title="Swap sides">
                            <span data-ui-icon="swap" data-ui-icon-size="16"></span>
                        </button>
                        <button class="btn ghost icon-btn" id="diffWhitespaceBtn" title="Toggle whitespace">
                            <span data-ui-icon="whitespace" data-ui-icon-size="16"></span>
                        </button>
                        <button class="btn ghost icon-btn" id="diffExportBtn" title="Export patch">
                            <span data-ui-icon="export" data-ui-icon-size="16"></span>
                        </button>
                        <button class="btn ghost icon-btn" id="diffCloseBtn" title="Close">
                            <span data-ui-icon="close" data-ui-icon-size="16"></span>
                        </button>
                    </div>
                </div>
                <div class="diff-panel-body">
                    <div class="diff-editor-container"></div>
                    <div class="diff-changes-panel">
                        <div class="changes-header">Changes</div>
                        <div class="changes-list"></div>
                    </div>
                </div>
            `;

            document.body.appendChild(panel);
            this.diffPanel = panel;

            // Bind controls
            this.bindDiffControls();
        }

        /**
         * Bind diff panel controls
         */
        bindDiffControls() {
            const swapBtn = this.diffPanel.querySelector('#diffSwapBtn');
            const wsBtn = this.diffPanel.querySelector('#diffWhitespaceBtn');
            const exportBtn = this.diffPanel.querySelector('#diffExportBtn');
            const closeBtn = this.diffPanel.querySelector('#diffCloseBtn');

            swapBtn?.addEventListener('click', () => this.swapSides());
            wsBtn?.addEventListener('click', () => this.toggleWhitespace());
            exportBtn?.addEventListener('click', () => this.exportPatch());
            closeBtn?.addEventListener('click', () => this.closeDiff());
        }

        /**
         * Swap diff sides
         */
        swapSides() {
            if (!this.diffEditor) return;

            const model = this.diffEditor.getModel();
            const temp = model.original;
            this.diffEditor.setModel({
                original: model.modified,
                modified: temp
            });

            // Swap labels
            const leftLabel = this.diffPanel.querySelector('.left-label');
            const rightLabel = this.diffPanel.querySelector('.right-label');
            const tempText = leftLabel.textContent;
            leftLabel.textContent = rightLabel.textContent;
            rightLabel.textContent = tempText;

            this.buildChangesList();
        }

        /**
         * Toggle whitespace differences
         */
        toggleWhitespace() {
            if (!this.diffEditor) return;

            const current = this.diffEditor.getOptions().get(this.monaco.editor.EditorOption.ignoreTrimWhitespace);
            this.diffEditor.updateOptions({ ignoreTrimWhitespace: !current });

            this.showToast('info', 'Diff', `Whitespace: ${current ? 'visible' : 'ignored'}`);
        }

        /**
         * Export patch
         */
        exportPatch() {
            this.showToast('info', 'Export', 'Patch export feature coming soon');
        }

        /**
         * Close diff panel
         */
        closeDiff() {
            if (this.diffPanel) {
                this.diffPanel.classList.remove('visible');
            }

            if (this.diffEditor) {
                try { this.diffEditor.dispose(); } catch (_) { }
                this.diffEditor = null;
            }

            this.disposeDiffModels();
        }

        /**
         * Build changes list sidebar
         */
        buildChangesList(retryCount = 0) {
            if (!this.diffEditor) return;

            const changesList = this.diffPanel.querySelector('.changes-list');

            // Show loading state on first call
            if (retryCount === 0) {
                changesList.innerHTML = '<div class="no-changes" style="opacity: 0.5;">Computing differences...</div>';
            }

            const changes = this.diffEditor.getLineChanges() || [];

            // If no changes found and we haven't retried much, wait for Monaco to compute
            if (changes.length === 0 && retryCount < 5) {
                setTimeout(() => this.buildChangesList(retryCount + 1), 200);
                return;
            }

            changesList.innerHTML = '';

            if (changes.length === 0) {
                changesList.innerHTML = '<div class="no-changes">No differences found</div>';
                return;
            }

            // Add summary header
            const summary = document.createElement('div');
            summary.className = 'changes-summary';
            summary.style.cssText = 'padding: 8px 14px; margin-bottom: 8px; font-size: 11px; color: #8b9dff; text-align: center;';
            const added = changes.filter(c => c.originalEndLineNumber === 0).length;
            const deleted = changes.filter(c => c.modifiedEndLineNumber === 0).length;
            const modified = changes.length - added - deleted;
            summary.innerHTML = `<span style="color: #5cf09e;">+${added}</span> · <span style="color: #ff7b7b;">-${deleted}</span> · <span style="color: #ffcc50;">~${modified}</span>`;
            changesList.appendChild(summary);

            changes.forEach((change, index) => {
                const item = document.createElement('div');
                item.className = 'change-item';

                let icon = '~';
                let type = 'modified';
                let desc = 'Modified';

                if (change.modifiedEndLineNumber === 0) {
                    icon = '−';
                    type = 'deleted';
                    desc = 'Deleted';
                } else if (change.originalEndLineNumber === 0) {
                    icon = '+';
                    type = 'added';
                    desc = 'Added';
                }

                const startLine = change.modifiedStartLineNumber || change.originalStartLineNumber;
                const endLine = change.modifiedEndLineNumber || change.originalEndLineNumber;
                const lineRange = startLine === endLine ? `Line ${startLine}` : `Lines ${startLine}-${endLine}`;

                item.innerHTML = `
                    <span class="change-icon ${type}">${icon}</span>
                    <span class="change-desc">${desc}</span>
                    <span class="change-line">${lineRange}</span>
                `;

                item.addEventListener('click', () => {
                    const line = change.modifiedStartLineNumber || change.originalStartLineNumber || 1;
                    const modifiedEditor = this.diffEditor.getModifiedEditor?.();
                    if (modifiedEditor?.revealLineInCenter) {
                        modifiedEditor.revealLineInCenter(line);
                        modifiedEditor.setPosition({ lineNumber: line, column: 1 });
                    }
                });

                changesList.appendChild(item);
            });
        }

        /**
         * Get current model
         */
        getCurrentModel() {
            try {
                if (typeof activeEditor !== 'undefined' && activeEditor) {
                    return activeEditor.getModel();
                }
            } catch {
                return null;
            }
            return null;
        }

        /**
         * Get routine name from model
         */
        getRoutineNameFromModel(model) {
            try {
                const uri = model?.uri;
                if (!uri) return '';

                // Prefer URI path (keeps folder prefix like localr/ROU)
                const path = (typeof uri.path === 'string') ? uri.path : '';
                if (path) {
                    return path.replace(/^\/+/, '').replace(/\.(m|mumps)$/i, '');
                }

                // Fallback: parse from string
                const uriStr = uri.toString?.() || '';
                const withoutScheme = uriStr.replace(/^[a-zA-Z][a-zA-Z0-9+.-]*:/, '');
                const cleaned = withoutScheme.split('?')[0].split('#')[0];
                return cleaned.replace(/^\/+/, '').replace(/\.(m|mumps)$/i, '');
            } catch (_) {
                return '';
            }
        }

        /**
         * Register context menu action
         */
        registerContextMenu(menuRegistry) {
            // This would be registered in the context menu system
            console.log('[Routine Compare] Ready for context menu integration');
        }
    }

    // Export
    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.mumps = window.AhmadIDEModules.mumps || {};
        window.AhmadIDEModules.mumps.MumpsRoutineCompare = MumpsRoutineCompare;
        // Global fallback
        window.MumpsRoutineCompare = MumpsRoutineCompare;
    }
})();
