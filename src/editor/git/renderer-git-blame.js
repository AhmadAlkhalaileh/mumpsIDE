/**
 * Git Blame Integration for Monaco Editor
 * Shows author and patch information inline - OPTIMIZED FOR PERFORMANCE
 */

(() => {
    function createGitBlameProvider({ deps } = {}) {
        const getMonaco = deps?.getMonaco || (() => (typeof monaco !== 'undefined' ? monaco : null));

        let fileBlameCache = new Map(); // filePath -> Map(lineNumber -> blameData)
        let decorationsMap = new Map();
        let currentLineWidgetMap = new Map();
        let currentLineLastKeyMap = new Map();
        let currentLineTimerMap = new Map();
        let currentLineDisposablesMap = new Map();
        let pendingFetchMap = new Map(); // filePath -> Promise
        let scheduledPrefetchMap = new Map(); // filePath -> { type, id }
        let hoverDisposable = null;
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
                fileBlameCache.clear();
            } catch (_) { }
        }

        async function prefetchFileBlame(filePath) {
            if (!filePath) return null;

            // Check cache first
            if (fileBlameCache.has(filePath)) {
                return fileBlameCache.get(filePath);
            }

            // Check if already fetching
            if (pendingFetchMap.has(filePath)) {
                return await pendingFetchMap.get(filePath);
            }

            // Fetch blame data for entire file
            const fetchPromise = (async () => {
                try {
                    await ensureRepoPathApplied();

                    const result = await window.ahmadIDE.gitBlame.getBlame(filePath);
                    if (!result?.success || !Array.isArray(result?.blameData)) {
                        return null;
                    }

                    // Index by line number for O(1) lookup
                    const lineMap = new Map();
                    for (const blame of result.blameData) {
                        if (blame?.lineNumber) {
                            lineMap.set(blame.lineNumber, blame);
                        }
                    }

                    fileBlameCache.set(filePath, lineMap);
                    return lineMap;
                } catch (_) {
                    return null;
                } finally {
                    pendingFetchMap.delete(filePath);
                }
            })();

            pendingFetchMap.set(filePath, fetchPromise);
            return await fetchPromise;
        }

        function cancelScheduledPrefetch(filePath) {
            if (!filePath) return;
            const entry = scheduledPrefetchMap.get(filePath);
            if (!entry) return;
            scheduledPrefetchMap.delete(filePath);
            try {
                if (entry.type === 'idle' && typeof window !== 'undefined' && typeof window.cancelIdleCallback === 'function') {
                    window.cancelIdleCallback(entry.id);
                } else {
                    clearTimeout(entry.id);
                }
            } catch (_) { }
        }

        function schedulePrefetchFileBlame(filePath) {
            if (!filePath) return;
            if (fileBlameCache.has(filePath) || pendingFetchMap.has(filePath)) return;
            if (scheduledPrefetchMap.has(filePath)) return;

            const run = () => {
                scheduledPrefetchMap.delete(filePath);
                prefetchFileBlame(filePath).then(() => {
                    try {
                        for (const editor of currentLineDisposablesMap.keys()) {
                            const model = editor?.getModel?.();
                            const fp = model?.uri?.toString?.() || '';
                            if (fp === filePath) scheduleCurrentLineUpdate(editor);
                        }
                    } catch (_) { }
                });
            };

            try {
                if (typeof window !== 'undefined' && typeof window.requestIdleCallback === 'function') {
                    const id = window.requestIdleCallback(run, { timeout: 2000 });
                    scheduledPrefetchMap.set(filePath, { type: 'idle', id });
                    return;
                }
            } catch (_) { }

            const id = setTimeout(run, 1500);
            scheduledPrefetchMap.set(filePath, { type: 'timeout', id });
        }

        function registerBlameHoverProvider() {
            const monacoRef = getMonaco();
            if (!monacoRef?.languages?.registerHoverProvider) return;
            if (hoverDisposable) return;

            const escapeMd = (s) => String(s || '').replace(/([\\\\`*_{}\\[\\]()#+\\-.!])/g, '\\\\$1');
            const fmtDate = (val) => {
                if (!val) return '';
                try {
                    const d = (val instanceof Date) ? val : new Date(val);
                    if (Number.isNaN(d.getTime())) return '';
                    return d.toLocaleString();
                } catch (_) {
                    return '';
                }
            };

            try {
                hoverDisposable = monacoRef.languages.registerHoverProvider('mumps', {
                    provideHover: async (model, position) => {
                        if (!enabled) return null;
                        if (!model || !position?.lineNumber) return null;

                        const filePath = model.uri?.toString?.() || '';
                        if (!filePath) return null;

                        const lineMap = fileBlameCache.get(filePath) || await prefetchFileBlame(filePath);
                        if (!lineMap) return null;

                        const blame = lineMap.get(position.lineNumber);
                        if (!blame) return null;

                        const patchId = blame.patchId;
                        const patchAuthor = blame.patchAuthor;
                        const patchDate = blame.patchDate;
                        const author = blame.author || 'Unknown';
                        const commit = blame.hashShort || (blame.hash ? String(blame.hash).slice(0, 8) : '');
                        const summary = blame.summary;
                        const isNewFile = blame.isNewFile || false;

                        const contents = [];

                        // Show NEW FILE banner for uncommitted files
                        if (isNewFile) {
                            contents.push({ value: `🆕 **NEW FILE** - Not yet committed to Git` });
                            contents.push({ value: `---` });
                        }

                        if (patchId) contents.push({ value: `**Patch**: \`${escapeMd(patchId)}\`` });
                        if (patchAuthor) contents.push({ value: `**Patch Author**: ${escapeMd(patchAuthor)}` });
                        if (!patchAuthor && !isNewFile) contents.push({ value: `**Author**: ${escapeMd(author)}` });

                        const when = patchDate || fmtDate(blame.date);
                        if (when && !isNewFile) contents.push({ value: `**Date**: ${escapeMd(when)}` });

                        if (commit && commit !== '0000000') contents.push({ value: `**Commit**: \`${escapeMd(commit)}\`` });
                        if (summary && !isNewFile) contents.push({ value: `**Summary**: ${escapeMd(summary)}` });

                        // If new file and has patch metadata, show helpful message
                        if (isNewFile && patchId) {
                            contents.push({ value: `---` });
                            contents.push({ value: `_This routine was created by patch **${escapeMd(patchId)}**. Commit it to preserve attribution._` });
                        }

                        if (!contents.length) return null;

                        return {
                            range: new monacoRef.Range(position.lineNumber, 1, position.lineNumber, 1),
                            contents
                        };
                    }
                });
            } catch (_) { }
        }

        async function showInlineAnnotations(editor) {
            if (!enabled) return;

            try {
                const model = editor.getModel();
                if (!model) return;

                const filePath = model.uri.toString();
                if (!filePath) return;

                const lineMap = await prefetchFileBlame(filePath);
                if (!lineMap) return;

                const monacoRef = getMonaco();
                if (!monacoRef) return;

                const decorations = [];
                let lastHash = null;

                // Create decorations from cached data
                for (const [lineNumber, blame] of lineMap.entries()) {
                    if (blame.hash !== lastHash) {
                        const authorSrc = blame.patchAuthor || blame.author;
                        const author = authorSrc ? String(authorSrc).split(' ')[0] : 'Unknown';
                        const patchId = blame.patchId || '';
                        const text = patchId ? `${author} • ${patchId}` : author;

                        decorations.push({
                            range: new monacoRef.Range(lineNumber, 1, lineNumber, 1),
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

                const oldDecorations = decorationsMap.get(editor) || [];
                const newDecorations = editor.deltaDecorations(oldDecorations, decorations);
                decorationsMap.set(editor, newDecorations);
            } catch (_) { }
        }

        function clearInlineAnnotations(editor) {
            const oldDecorations = decorationsMap.get(editor) || [];
            if (oldDecorations.length > 0) {
                editor.deltaDecorations(oldDecorations, []);
                decorationsMap.delete(editor);
            }
        }

        function clearCurrentLineWidget(editor) {
            const widget = currentLineWidgetMap.get(editor);
            if (widget) {
                try {
                    editor.removeContentWidget(widget);
                } catch (_) { }
                currentLineWidgetMap.delete(editor);
            }
        }

        async function updateCurrentLineWidget(editor) {
            if (!enabled || !editor) return;

            const model = editor.getModel?.();
            if (!model) return;

            const position = editor.getPosition?.();
            if (!position || !position.lineNumber) return;

            const monacoRef = getMonaco();
            if (!monacoRef) return;

            const filePath = model.uri?.toString?.() || '';
            const lineNumber = position.lineNumber;
            const key = `${filePath}#${lineNumber}`;

            if (currentLineLastKeyMap.get(editor) === key) return;
            currentLineLastKeyMap.set(editor, key);

            // Get from cache (already prefetched when file opened)
            const lineMap = fileBlameCache.get(filePath);
            if (!lineMap) {
                // Trigger background fetch
                schedulePrefetchFileBlame(filePath);
                clearCurrentLineWidget(editor);
                return;
            }

            const blame = lineMap.get(lineNumber);
            if (!blame) {
                clearCurrentLineWidget(editor);
                return;
            }

            const gitAuthor = blame.author || 'Unknown';
            const patchAuthor = blame.patchAuthor;
            const patchId = blame.patchId;
            const patchDate = blame.patchDate;

            let text;
            if (patchAuthor && patchId) {
                text = patchDate ? `${patchAuthor} • ${patchId} • ${patchDate}` : `${patchAuthor} • ${patchId}`;
            } else {
                text = gitAuthor;
            }

            const safeText = String(text).trim().slice(0, 180);

            const oldWidget = currentLineWidgetMap.get(editor);
            if (oldWidget) {
                try {
                    editor.removeContentWidget(oldWidget);
                } catch (_) { }
            }

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

            try {
                editor.addContentWidget(widget);
                currentLineWidgetMap.set(editor, widget);
            } catch (_) { }
        }

        function scheduleCurrentLineUpdate(editor) {
            if (!editor) return;
            const oldTimer = currentLineTimerMap.get(editor);
            if (oldTimer) clearTimeout(oldTimer);
            const t = setTimeout(() => updateCurrentLineWidget(editor), 150);
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
                        clearCurrentLineWidget(editor);
                        currentLineLastKeyMap.delete(editor);

                        // Prefetch blame for new file
                        const model = editor.getModel?.();
                        if (model) {
                            const filePath = model.uri?.toString?.();
                            if (filePath) {
                                schedulePrefetchFileBlame(filePath);
                            }
                        }

                        scheduleCurrentLineUpdate(editor);
                    })
                );
                disposables.push(
                    editor.onDidDispose?.(() => {
                        const timer = currentLineTimerMap.get(editor);
                        if (timer) clearTimeout(timer);
                        currentLineTimerMap.delete(editor);
                        clearCurrentLineWidget(editor);
                        currentLineLastKeyMap.delete(editor);
                        const ds = currentLineDisposablesMap.get(editor) || [];
                        currentLineDisposablesMap.delete(editor);
                        ds.forEach((d) => {
                            try { d?.dispose?.(); } catch (_) { }
                        });
                    })
                );
            } catch (_) { }

            currentLineDisposablesMap.set(editor, disposables.filter(Boolean));

            // Prefetch blame data when attaching
            const model = editor.getModel?.();
            if (model) {
                const filePath = model.uri?.toString?.();
                if (filePath) {
                    schedulePrefetchFileBlame(filePath);
                }
            }

            scheduleCurrentLineUpdate(editor);
        }

        function registerCurrentLineAnnotations() {
            const monacoRef = getMonaco();
            if (!monacoRef?.editor?.onDidCreateEditor) return;
            if (editorCreateDisposable) return;

            editorCreateDisposable = monacoRef.editor.onDidCreateEditor((editor) => {
                attachCurrentLineBlame(editor);
            });
        }

        async function toggleInlineAnnotations(editor) {
            const oldDecorations = decorationsMap.get(editor) || [];
            if (oldDecorations.length > 0) {
                clearInlineAnnotations(editor);
            } else {
                await showInlineAnnotations(editor);
            }
        }

        function setEnabled(isEnabled) {
            enabled = isEnabled;

            if (!enabled) {
                try {
                    for (const editor of decorationsMap.keys()) {
                        clearInlineAnnotations(editor);
                    }
                    for (const editor of currentLineWidgetMap.keys()) {
                        clearCurrentLineWidget(editor);
                    }
                } catch (_) { }
            }
        }

        function clearCache(filePath) {
            if (filePath) {
                cancelScheduledPrefetch(filePath);
                fileBlameCache.delete(filePath);
                try {
                    window.ahmadIDE?.gitBlame?.clearCache?.(filePath);
                } catch (_) { }
            } else {
                try {
                    for (const fp of scheduledPrefetchMap.keys()) cancelScheduledPrefetch(fp);
                } catch (_) { }
                fileBlameCache.clear();
                try {
                    window.ahmadIDE?.gitBlame?.clearCache?.();
                } catch (_) { }
            }

            for (const editor of currentLineWidgetMap.keys()) {
                clearCurrentLineWidget(editor);
                currentLineLastKeyMap.delete(editor);
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

    if (typeof window !== 'undefined') {
        window.createGitBlameProvider = createGitBlameProvider;

        function initGitBlame() {
            if (typeof monaco !== 'undefined') {
                const provider = createGitBlameProvider();

                if (window.ahmadIDE?.gitBlame?.clearCache) {
                    window.ahmadIDE.gitBlame.clearCache();
                }

                provider.registerBlameHoverProvider();
                provider.registerCurrentLineAnnotations();

                try {
                    const activeEditor = window.AhmadIDEModules?.mumps?.getEditor?.();
                    if (activeEditor) {
                        provider.attachCurrentLineBlame(activeEditor);
                    }
                } catch (_) { }

                window.gitBlameProvider = provider;
                return true;
            }
            return false;
        }

        if (!initGitBlame()) {
            const checkInterval = setInterval(() => {
                if (initGitBlame()) {
                    clearInterval(checkInterval);
                }
            }, 100);

            setTimeout(() => clearInterval(checkInterval), 10000);
        }
    }

    if (typeof module !== 'undefined' && module.exports) {
        module.exports = { createGitBlameProvider };
    }
})();
