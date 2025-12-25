/**
 * Git Blame Integration for Monaco Editor
 * Shows author and patch information on hover
 */

(() => {
    function createGitBlameProvider({ deps } = {}) {
        const getMonaco = deps?.getMonaco || (() => (typeof monaco !== 'undefined' ? monaco : null));

        let blameCache = new Map();
        let decorationsMap = new Map(); // Map of editor -> decoration IDs
        let currentLineDecorationsMap = new Map(); // Map of editor -> decoration IDs (single current-line)
        let currentLineLastKeyMap = new Map(); // Map of editor -> last rendered key
        let currentLineTimerMap = new Map(); // Map of editor -> timer id
        let currentLineDisposablesMap = new Map(); // Map of editor -> disposables
        let currentLineWidgetMap = new Map(); // Map of editor -> content widget
        let editorCreateDisposable = null;
        let enabled = true;
        const GLOBAL_REPO_ROOT_KEY = 'ahmadIDE:gitRepoRootGlobal';
        const VISTA_REPO_ROOT_KEY = 'ahmadIDE:vistaRoutinesRepoPath';
        let repoPathApplied = false;

        async function ensureRepoPathApplied() {
            if (repoPathApplied) return;
            repoPathApplied = true;

            const api = window.ahmadIDE?.gitBlame;
            if (!api?.setRepoPath) return;

            let repoPath = '';
            try {
                repoPath = String(localStorage.getItem(GLOBAL_REPO_ROOT_KEY) || '').trim();
            } catch (_) { }
            if (!repoPath) {
                try {
                    repoPath = String(localStorage.getItem(VISTA_REPO_ROOT_KEY) || '').trim();
                } catch (_) { }
            }

            if (!repoPath) {
                try {
                    const repoManager = window.AhmadIDEModules?.git?.repoManager;
                    const st = repoManager?.getState?.() || {};
                    repoPath = String(st.repoRootOverride || st.repoRoot || '').trim();
                } catch (_) { }
            }

            if (!repoPath) return;

            try {
                await api.setRepoPath(repoPath);
                console.log('[Git Blame] Applied repo path from settings:', repoPath);

                // Clear frontend cache when repo path is set to ensure fresh blame data
                blameCache.clear();
                console.log('[Git Blame] Cleared frontend cache after setting repo path');
            } catch (e) {
                console.warn('[Git Blame] Failed to apply repo path:', e?.message || e);
            }
        }

        /**
         * Register Git blame hover provider for all languages
         * DISABLED - User only wants inline annotations
         */
        function registerBlameHoverProvider() {
            console.log('[Git Blame] Hover provider DISABLED (user preference: inline only)');
            // Hover provider disabled - user only wants inline annotations
            return;
        }

        /**
         * Show inline blame annotations (like GitLens)
         * @param {Object} editor - Monaco editor instance
         */
        async function showInlineAnnotations(editor) {
            if (!enabled) return;

            try {
                await ensureRepoPathApplied();

                const model = editor.getModel();
                if (!model) return;

                const filePath = model.uri.toString();
                if (!filePath) return;

                // Get full blame for the file
                const result = await window.ahmadIDE.gitBlame.getBlame(filePath);
                if (!result.success || !result.blameData) {
                    return;
                }

                const blameData = result.blameData;
                blameCache.set(filePath, blameData);

                // Create inline decorations
                const decorations = [];
                const monacoRef = getMonaco();

                // Group consecutive lines with same commit to avoid clutter
                let lastHash = null;
                for (const blame of blameData) {
                    // Only show annotation if commit is different from previous line
                    if (blame.hash !== lastHash) {
                        const authorSrc = blame.patchAuthor || blame.author;
                        const author = authorSrc ? String(authorSrc).split(' ')[0] : 'Unknown';
                        const patchId = blame.patchId || '';
                        const text = patchId ? `${author} • ${patchId}` : author;

                        decorations.push({
                            range: new monacoRef.Range(blame.lineNumber, 1, blame.lineNumber, 1),
                            options: {
                                isWholeLine: false,
                                after: {
                                    content: `  ${text}`,
                                    inlineClassName: 'git-blame-annotation',
                                    cursorStops: monacoRef.editor.InjectedTextCursorStops.None
                                }
                            }
                        });
                    }
                    lastHash = blame.hash;
                }

                // Apply decorations
                const oldDecorations = decorationsMap.get(editor) || [];
                const newDecorations = editor.deltaDecorations(oldDecorations, decorations);
                decorationsMap.set(editor, newDecorations);

                console.log(`[Git Blame] Applied ${decorations.length} inline annotations`);

            } catch (error) {
                console.error('[Git Blame] Inline annotations error:', error);
            }
        }

        /**
         * Clear inline annotations
         * @param {Object} editor - Monaco editor instance
         */
        function clearInlineAnnotations(editor) {
            const oldDecorations = decorationsMap.get(editor) || [];
            if (oldDecorations.length > 0) {
                editor.deltaDecorations(oldDecorations, []);
                decorationsMap.delete(editor);
                console.log('[Git Blame] Cleared inline annotations');
            }
        }

        function clearCurrentLineDecoration(editor) {
            const old = currentLineDecorationsMap.get(editor) || [];
            if (old.length > 0) {
                try {
                    editor.deltaDecorations(old, []);
                } catch (_) { }
            }
            currentLineDecorationsMap.delete(editor);
            currentLineLastKeyMap.delete(editor);

            // Also remove content widget
            const widget = currentLineWidgetMap.get(editor);
            if (widget) {
                try {
                    editor.removeContentWidget(widget);
                } catch (_) { }
                currentLineWidgetMap.delete(editor);
            }
        }

        async function updateCurrentLineDecoration(editor) {
            if (!enabled) {
                console.log('[Git Blame] Inline disabled');
                return;
            }
            if (!editor) {
                console.log('[Git Blame] No editor');
                return;
            }

            const model = editor.getModel?.();
            if (!model) {
                console.log('[Git Blame] No model');
                return;
            }

            const position = editor.getPosition?.();
            if (!position || !position.lineNumber) {
                console.log('[Git Blame] No position');
                return;
            }

            const monacoRef = getMonaco();
            if (!monacoRef) {
                console.log('[Git Blame] No monaco');
                return;
            }

            await ensureRepoPathApplied();

            const filePath = model.uri?.toString?.() || '';
            const lineNumber = position.lineNumber;
            const key = `${filePath}#${lineNumber}`;
            if (currentLineLastKeyMap.get(editor) === key) return;
            currentLineLastKeyMap.set(editor, key);

            console.log(`[Git Blame] Updating inline for line ${lineNumber}`);

            try {
                const result = await window.ahmadIDE.gitBlame.getBlameForLine(filePath, lineNumber);
                if (!result?.success || !result?.blame) {
                    console.log('[Git Blame] No blame data for inline');
                    clearCurrentLineDecoration(editor);
                    return;
                }

                const blame = result.blame;
                console.log(`[Git Blame] Blame data received for line ${lineNumber}:`, JSON.stringify({
                    author: blame.author,
                    patchAuthor: blame.patchAuthor,
                    patchId: blame.patchId,
                    hash: blame.hash?.substring(0, 8)
                }, null, 2));

                // Show git author + patch info if available
                const gitAuthor = blame.author || 'Unknown';
                const patchAuthor = blame.patchAuthor;
                const patchId = blame.patchId;
                const patchDate = blame.patchDate;

                let text;
                if (patchAuthor && patchId) {
                    // Has patch metadata: show patch creator + ID + date
                    if (patchDate) {
                        text = `${patchAuthor} • ${patchId} • ${patchDate}`;
                    } else {
                        text = `${patchAuthor} • ${patchId}`;
                    }
                } else {
                    // No patch: show only git committer
                    text = gitAuthor;
                }

                const safeText = String(text).trim().slice(0, 180);

                console.log(`[Git Blame] Creating inline widget: "${safeText}"`);

                // Remove old widget
                const oldWidget = currentLineWidgetMap.get(editor);
                if (oldWidget) {
                    try {
                        editor.removeContentWidget(oldWidget);
                    } catch (_) { }
                }

                // Create DOM node for the widget
                const widgetNode = document.createElement('div');
                widgetNode.className = 'git-blame-inline-widget';
                widgetNode.textContent = safeText;
                widgetNode.style.cssText = `
                    color: #888;
                    font-size: 0.85em;
                    font-style: italic;
                    padding-left: 2em;
                    opacity: 0.7;
                    pointer-events: none;
                    white-space: nowrap;
                `;

                // Create content widget
                const widget = {
                    getId: () => `git-blame-line-${lineNumber}`,
                    getDomNode: () => widgetNode,
                    getPosition: () => ({
                        position: {
                            lineNumber: lineNumber,
                            column: editor.getModel().getLineMaxColumn(lineNumber)
                        },
                        preference: [monacoRef.editor.ContentWidgetPositionPreference.EXACT]
                    })
                };

                // Add widget to editor
                try {
                    editor.addContentWidget(widget);
                    currentLineWidgetMap.set(editor, widget);
                    console.log(`[Git Blame] ✓ Content widget added for line ${lineNumber}`);
                    console.log(`[Git Blame] Widget ID:`, widget.getId());
                    console.log(`[Git Blame] Widget position:`, widget.getPosition());
                } catch (e) {
                    console.error(`[Git Blame] Failed to add widget:`, e);
                }
            } catch (e) {
                console.error('[Git Blame] Error updating inline:', e);
                clearCurrentLineDecoration(editor);
            }
        }

        function scheduleCurrentLineUpdate(editor) {
            if (!editor) return;
            const oldTimer = currentLineTimerMap.get(editor);
            if (oldTimer) clearTimeout(oldTimer);
            const t = setTimeout(() => updateCurrentLineDecoration(editor), 120);
            currentLineTimerMap.set(editor, t);
        }

        function attachCurrentLineBlame(editor) {
            if (!editor) return;
            if (currentLineDisposablesMap.has(editor)) return;

            const disposables = [];
            try {
                disposables.push(
                    editor.onDidChangeCursorPosition?.(() => scheduleCurrentLineUpdate(editor))
                );
                disposables.push(
                    editor.onDidChangeModel?.(() => {
                        clearCurrentLineDecoration(editor);
                        scheduleCurrentLineUpdate(editor);
                    })
                );
                disposables.push(
                    editor.onDidDispose?.(() => {
                        const timer = currentLineTimerMap.get(editor);
                        if (timer) clearTimeout(timer);
                        currentLineTimerMap.delete(editor);
                        clearCurrentLineDecoration(editor);
                        const ds = currentLineDisposablesMap.get(editor) || [];
                        currentLineDisposablesMap.delete(editor);
                        ds.forEach((d) => {
                            try { d?.dispose?.(); } catch (_) { }
                        });
                    })
                );
            } catch (_) { }

            currentLineDisposablesMap.set(editor, disposables.filter(Boolean));
            scheduleCurrentLineUpdate(editor);
        }

        function registerCurrentLineAnnotations() {
            const monacoRef = getMonaco();
            if (!monacoRef?.editor?.onDidCreateEditor) return;
            if (editorCreateDisposable) return;

            editorCreateDisposable = monacoRef.editor.onDidCreateEditor((editor) => {
                console.log('[Git Blame] Editor created - attaching current-line blame');
                attachCurrentLineBlame(editor);
            });

            console.log('[Git Blame] Current-line annotations enabled - listening for new editors');
        }

        /**
         * Toggle inline annotations
         * @param {Object} editor - Monaco editor instance
         */
        async function toggleInlineAnnotations(editor) {
            const oldDecorations = decorationsMap.get(editor) || [];
            if (oldDecorations.length > 0) {
                clearInlineAnnotations(editor);
            } else {
                await showInlineAnnotations(editor);
            }
        }

        /**
         * Enable/disable Git blame
         * @param {boolean} isEnabled - Enable or disable
         */
        function setEnabled(isEnabled) {
            enabled = isEnabled;
            console.log(`[Git Blame] ${enabled ? 'Enabled' : 'Disabled'}`);

            if (!enabled) {
                // Clear all decorations when disabled
                try {
                    for (const editor of decorationsMap.keys()) {
                        clearInlineAnnotations(editor);
                    }
                    for (const editor of currentLineDecorationsMap.keys()) {
                        clearCurrentLineDecoration(editor);
                    }
                } catch (_) { }
            }
        }

        /**
         * Clear cache for a file
         * @param {string} filePath - File path
         */
        function clearCache(filePath) {
            if (filePath) {
                blameCache.delete(filePath);
                window.ahmadIDE.gitBlame.clearCache(filePath);
            } else {
                blameCache.clear();
                window.ahmadIDE.gitBlame.clearCache();
            }

            // Also clear all current line decorations to force refresh
            console.log('[Git Blame] Clearing all decorations due to cache clear');
            for (const editor of currentLineDecorationsMap.keys()) {
                clearCurrentLineDecoration(editor);
            }
            for (const editor of decorationsMap.keys()) {
                clearInlineAnnotations(editor);
            }
        }

        return {
            registerBlameHoverProvider,
            registerCurrentLineAnnotations,
            showInlineAnnotations,
            clearInlineAnnotations,
            toggleInlineAnnotations,
            attachCurrentLineBlame,
            setEnabled,
            clearCache
        };
    }

    // Auto-register when Monaco is available
    if (typeof window !== 'undefined') {
        window.createGitBlameProvider = createGitBlameProvider;

        // Initialize immediately if Monaco is ready
        function initGitBlame() {
            if (typeof monaco !== 'undefined') {
                console.log('[Git Blame] Monaco detected, initializing...');
                const provider = createGitBlameProvider();

                // Clear cache on startup to ensure fresh data with latest blame format
                if (window.ahmadIDE?.gitBlame?.clearCache) {
                    window.ahmadIDE.gitBlame.clearCache();
                    console.log('[Git Blame] Cleared cache on initialization');
                }

                provider.registerBlameHoverProvider();
                provider.registerCurrentLineAnnotations();

                // Also attach to existing editors (if any)
                // Monaco typically reuses the same editor instance with different models
                try {
                    const activeEditor = window.AhmadIDEModules?.mumps?.getEditor?.();
                    if (activeEditor) {
                        console.log('[Git Blame] Found existing editor - attaching current-line blame');
                        provider.attachCurrentLineBlame(activeEditor);
                    } else {
                        console.log('[Git Blame] No existing editor found - will attach when created');
                    }
                } catch (e) {
                    console.log('[Git Blame] Could not attach to existing editor:', e.message);
                }

                window.gitBlameProvider = provider;
                console.log('[Git Blame] ✓ Provider initialized and ready');
                return true;
            }
            return false;
        }

        // Try immediate initialization
        if (!initGitBlame()) {
            // Wait for Monaco to load
            const checkInterval = setInterval(() => {
                if (initGitBlame()) {
                    clearInterval(checkInterval);
                }
            }, 100);

            // Stop checking after 10 seconds
            setTimeout(() => clearInterval(checkInterval), 10000);
        }
    }

    // Export for module systems
    if (typeof module !== 'undefined' && module.exports) {
        module.exports = { createGitBlameProvider };
    }
})();
