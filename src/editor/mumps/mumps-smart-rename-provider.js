/**
 * MUMPS Smart Rename Provider
 * Integrates cross-routine rename with Monaco editor
 */

(() => {
    function createMumpsSmartRenameProvider({ deps } = {}) {
        const getMonaco = deps?.getMonaco || (() => (typeof monaco !== 'undefined' ? monaco : null));
        const getCallIndexer = deps?.getCallIndexer || (() => window.globalCallIndexer || null);
        const getActiveEditor = deps?.getActiveEditor || (() => window.activeEditor || null);
        const showToast = deps?.showToast || ((type, title, msg) => console.log(`[${type}] ${title}: ${msg}`));

        // Module references
        let symbolIndexer = null;
        let smartRename = null;
        let smartRenameDialog = null;
        let transaction = null;
        let fallbackMode = null; // For Docker/connection mode (no CallIndexer)

        /**
         * Initialize Smart Rename system
         */
        function initialize() {
            try {
                // Load modules
                const MumpsSymbolIndexer = window.AhmadIDEModules?.mumps?.MumpsSymbolIndexer;
                const createMumpsSmartRename = window.AhmadIDEModules?.mumps?.createMumpsSmartRename;
                const createMumpsSmartRenameDialog = window.AhmadIDEModules?.mumps?.createMumpsSmartRenameDialog;
                const createMumpsRenameTransaction = window.AhmadIDEModules?.mumps?.createMumpsRenameTransaction;
                const createSmartRenameFallback = window.AhmadIDEModules?.mumps?.createSmartRenameFallback;

                // Initialize fallback mode (for Docker/connection workflows)
                if (createSmartRenameFallback) {
                    fallbackMode = createSmartRenameFallback({
                        deps: {
                            getMonaco,
                            getAllOpenModels: () => {
                                const monacoRef = getMonaco();
                                return monacoRef?.editor.getModels() || [];
                            }
                        }
                    });
                }

                if (!MumpsSymbolIndexer || !createMumpsSmartRename) {
                    console.warn('[Smart Rename] Full indexing modules not loaded, using fallback mode only');
                    // Fallback mode still works!
                    if (!fallbackMode) return false;
                    console.log('[Smart Rename] Fallback mode initialized (Docker/connection mode)');
                    return true; // Success with fallback only
                }

                // Create instances (full mode)
                symbolIndexer = new MumpsSymbolIndexer();
                smartRename = createMumpsSmartRename({
                    deps: {
                        getMonaco,
                        getSymbolIndexer: () => symbolIndexer,
                        readFile: async (path) => {
                            if (window.ahmadIDE?.readFile) {
                                const result = await window.ahmadIDE.readFile(path);
                                return result?.content || '';
                            }
                            return '';
                        }
                    }
                });

                if (createMumpsSmartRenameDialog) {
                    smartRenameDialog = createMumpsSmartRenameDialog({
                        deps: {
                            createDialog: window.AhmadIDEModules?.ui?.createDialog,
                            primitives: window.AhmadIDEModules?.ui?.primitives
                        }
                    });
                }

                if (createMumpsRenameTransaction) {
                    transaction = createMumpsRenameTransaction({
                        deps: {
                            writeFile: async (path, content) => {
                                if (window.ahmadIDE?.writeFile) {
                                    await window.ahmadIDE.writeFile(path, content);
                                }
                            },
                            readFile: async (path) => {
                                if (window.ahmadIDE?.readFile) {
                                    const result = await window.ahmadIDE.readFile(path);
                                    return result?.content || '';
                                }
                                return '';
                            }
                        }
                    });
                }

                console.log('[Smart Rename] Initialized successfully');
                return true;
            } catch (err) {
                console.error('[Smart Rename] Initialization error:', err);
                return false;
            }
        }

        /**
         * Build symbol index from CallIndexer
         */
        function buildIndex() {
            if (!symbolIndexer) {
                console.error('[Smart Rename] Symbol indexer not initialized');
                showToast('error', 'Smart Rename', 'Symbol indexer not initialized');
                return false;
            }

            const callIndexer = getCallIndexer();
            if (!callIndexer) {
                console.error('[Smart Rename] CallIndexer not available - please open a project with MUMPS files');
                showToast('warning', 'Smart Rename', 'Please open a project first. CallIndexer service is not available.');
                return false;
            }

            // Check if CallIndexer has data
            const callStats = callIndexer.getStats?.();
            if (!callStats || callStats.totalRoutines === 0) {
                console.warn('[Smart Rename] CallIndexer has no data - triggering index');
                showToast('info', 'Smart Rename', 'Building workspace index, please wait...');

                // Try to trigger indexing if possible
                if (typeof callIndexer.indexDirectory === 'function' && window.ahmadIDE?.getProjectDir) {
                    window.ahmadIDE.getProjectDir().then(dir => {
                        if (dir) {
                            callIndexer.indexDirectory(dir).then(() => {
                                showToast('success', 'Smart Rename', 'Index built successfully');
                                buildIndex(); // Retry
                            }).catch(err => {
                                showToast('error', 'Smart Rename', 'Failed to build index: ' + err.message);
                            });
                        }
                    });
                }
                return false;
            }

            try {
                symbolIndexer.buildFromCallIndexer(callIndexer);
                const stats = symbolIndexer.getStats();
                console.log('[Smart Rename] Index built:', stats);

                if (stats.totalTags === 0) {
                    showToast('warning', 'Smart Rename', 'No tags found in workspace');
                    return false;
                }

                showToast('success', 'Smart Rename', `Index ready: ${stats.totalTags} tags in ${stats.totalRoutines} routines`);
                return true;
            } catch (err) {
                console.error('[Smart Rename] Index build error:', err);
                showToast('error', 'Smart Rename', 'Failed to build index: ' + err.message);
                return false;
            }
        }

        /**
         * Trigger Smart Rename Tag action
         */
        async function triggerSmartRenameTag() {
            const editor = getActiveEditor();
            if (!editor) {
                showToast('warning', 'Smart Rename', 'No active editor');
                return;
            }

            const model = editor.getModel?.();
            const position = editor.getPosition?.();
            if (!model || !position) {
                showToast('warning', 'Smart Rename', 'No valid position');
                return;
            }

            // Check if we should use fallback mode (Docker/connection mode)
            const callIndexer = getCallIndexer();
            const useFallback = !callIndexer || !symbolIndexer;

            if (useFallback && !fallbackMode) {
                showToast('error', 'Smart Rename', 'Neither CallIndexer nor fallback mode available');
                return;
            }

            // Ensure index is built (if not using fallback)
            if (!useFallback && !symbolIndexer?.lastIndexTime) {
                showToast('info', 'Smart Rename', 'Building workspace index...');
                const success = buildIndex();
                if (!success) {
                    // Fall back to simple mode
                    if (fallbackMode) {
                        showToast('info', 'Smart Rename', 'Using simplified rename (current file only)');
                        await triggerFallbackRename(model, position);
                        return;
                    }
                    showToast('error', 'Smart Rename', 'Failed to build workspace index');
                    return;
                }
            }

            // Use fallback if no full indexer
            if (useFallback) {
                showToast('info', 'Smart Rename', 'Docker mode: Renaming in current file only');
                await triggerFallbackRename(model, position);
                return;
            }

            try {
                // Get word at cursor
                const word = model.getWordAtPosition(position);
                if (!word?.word) {
                    showToast('warning', 'Smart Rename', 'No symbol found at cursor');
                    return;
                }

                const oldTagName = word.word;
                const line = model.getLineContent(position.lineNumber);

                // Check if this is a label definition (starts at column 1)
                const isLabelLine = /^[A-Za-z%][A-Za-z0-9]*/.test(line) && position.column <= word.word.length + 1;

                if (!isLabelLine) {
                    showToast('info', 'Smart Rename', 'Smart Rename is for tag/label definitions. Use F2 for local renames.');
                    return;
                }

                // Prompt for new name
                const newTagName = await promptForNewName(oldTagName);
                if (!newTagName) return;

                // Validate new name
                if (!/^[A-Za-z%][A-Za-z0-9]*$/.test(newTagName)) {
                    showToast('error', 'Smart Rename', 'Invalid tag name');
                    return;
                }

                if (newTagName.toUpperCase() === oldTagName.toUpperCase()) {
                    showToast('info', 'Smart Rename', 'Name unchanged');
                    return;
                }

                // Compute rename plan
                showToast('info', 'Smart Rename', 'Computing rename plan...');

                const renamePlan = await smartRename.computeSmartRename({
                    model,
                    oldTagName,
                    newTagName,
                    includeLocalChanges: true,
                    includeCrossRoutineChanges: true,
                    includeUnsafeChanges: false
                });

                if (renamePlan.hasCollision) {
                    showToast('error', 'Smart Rename', `Tag "${newTagName}" already exists in this routine`);
                    return;
                }

                if (renamePlan.totalChanges === 0) {
                    showToast('info', 'Smart Rename', 'No references found');
                    return;
                }

                // Show preview dialog
                if (!smartRenameDialog) {
                    showToast('error', 'Smart Rename', 'Preview dialog not available');
                    return;
                }

                const userChoice = await smartRenameDialog.open({
                    title: 'Smart Rename Tag',
                    oldName: oldTagName,
                    newName: newTagName,
                    fileEdits: renamePlan.fileEdits,
                    riskScore: renamePlan.riskScore,
                    riskLevel: renamePlan.riskLevel,
                    riskFactors: renamePlan.riskFactors,
                    hasCollision: renamePlan.hasCollision,
                    totalChanges: renamePlan.totalChanges,
                    filesAffected: renamePlan.filesAffected,
                    localChanges: renamePlan.localChanges,
                    crossRoutineChanges: renamePlan.crossRoutineChanges,
                    unsafeChanges: renamePlan.unsafeChanges,
                    computeRename: async (opts) => {
                        return await smartRename.computeSmartRename({
                            model,
                            oldTagName,
                            newTagName,
                            includeLocalChanges: opts.includeLocalChanges !== false,
                            includeCrossRoutineChanges: opts.includeCrossRoutineChanges !== false,
                            includeUnsafeChanges: opts.includeUnsafeChanges === true
                        });
                    },
                    readFile: async (path) => {
                        if (window.ahmadIDE?.readFile) {
                            const result = await window.ahmadIDE.readFile(path);
                            return result?.content || '';
                        }
                        return '';
                    }
                });

                if (!userChoice?.apply) {
                    showToast('info', 'Smart Rename', 'Cancelled');
                    return;
                }

                // Recompute with user's choices
                const finalPlan = await smartRename.computeSmartRename({
                    model,
                    oldTagName,
                    newTagName,
                    includeLocalChanges: userChoice.includeLocal,
                    includeCrossRoutineChanges: userChoice.includeCrossRoutine,
                    includeUnsafeChanges: userChoice.includeUnsafe
                });

                // Apply transaction
                showToast('info', 'Smart Rename', 'Applying changes...');

                if (!transaction) {
                    showToast('error', 'Smart Rename', 'Transaction system not available');
                    return;
                }

                const result = await transaction.applyTransaction(finalPlan.fileEdits, {
                    onProgress: (progress) => {
                        console.log('[Smart Rename] Progress:', progress);
                    }
                });

                if (result.success) {
                    showToast('success', 'Smart Rename', `Renamed "${oldTagName}" to "${newTagName}" in ${result.filesAffected} file(s)`);

                    // Rebuild index
                    setTimeout(() => buildIndex(), 100);
                } else {
                    showToast('error', 'Smart Rename', `Failed: ${result.error || 'Unknown error'}`);
                }

            } catch (err) {
                console.error('[Smart Rename] Error:', err);
                showToast('error', 'Smart Rename', err.message || 'Unknown error');
            }
        }

        /**
         * Fallback rename (scans ALL open Monaco models for cross-routine rename)
         */
        async function triggerFallbackRename(model, position) {
            if (!fallbackMode) {
                showToast('error', 'Smart Rename', 'Fallback mode not available');
                return;
            }

            const monacoRef = getMonaco();
            if (!monacoRef) return;

            try {
                // Get word at cursor
                const word = model.getWordAtPosition(position);
                if (!word?.word) {
                    showToast('warning', 'Smart Rename', 'No symbol found at cursor');
                    return;
                }

                const oldTagName = word.word;
                const line = model.getLineContent(position.lineNumber);

                // Check if this is a label definition
                const isLabelLine = /^[A-Za-z%][A-Za-z0-9]*/.test(line) && position.column <= word.word.length + 1;

                if (!isLabelLine) {
                    showToast('info', 'Smart Rename', 'Place cursor on a tag/label at the start of a line');
                    return;
                }

                // Prompt for new name
                const newTagName = await promptForNewName(oldTagName);
                if (!newTagName) return;

                // Validate
                if (!/^[A-Za-z%][A-Za-z0-9]*$/.test(newTagName)) {
                    showToast('error', 'Smart Rename', 'Invalid tag name');
                    return;
                }

                if (newTagName.toUpperCase() === oldTagName.toUpperCase()) {
                    showToast('info', 'Smart Rename', 'Name unchanged');
                    return;
                }

                // Compute edits across ALL open models (Docker/SSH cross-routine rename)
                showToast('info', 'Smart Rename', 'Scanning open routines...');
                const renamePlan = fallbackMode.renameAcrossOpenModels(oldTagName, newTagName, model, monacoRef);

                if (renamePlan.tagNotFound) {
                    showToast('warning', 'Smart Rename', 'Tag definition not found in current file');
                    return;
                }

                if (renamePlan.totalEdits === 0) {
                    showToast('info', 'Smart Rename', 'No references found');
                    return;
                }

                // Build details for preview dialog
                const routineList = Object.entries(renamePlan.byRoutine).map(([routine, edits]) => {
                    const defCount = edits.filter(e => e.type === 'definition').length;
                    const refCount = edits.filter(e => e.type !== 'definition').length;
                    return `  • ${routine}: ${defCount > 0 ? '1 definition, ' : ''}${refCount} reference(s)`;
                });

                const details = [
                    `Found ${renamePlan.totalEdits} occurrence(s) in ${renamePlan.routinesAffected} routine(s):`,
                    '',
                    ...routineList,
                    ''
                ];

                if (renamePlan.crossRoutineCount > 0) {
                    details.push(`Cross-routine calls: ${renamePlan.crossRoutineCount}`);
                }

                // Show preview dialog with all affected routines
                const confirmed = await showConfirmDialog({
                    title: 'Smart Rename Tag (Cross-Routine)',
                    message: `Rename "${oldTagName}" to "${newTagName}"?`,
                    details,
                    okLabel: 'Apply All',
                    cancelLabel: 'Cancel'
                });

                if (!confirmed) {
                    showToast('info', 'Smart Rename', 'Cancelled');
                    return;
                }

                // Apply edits to all affected Monaco models
                const editsByModel = new Map();
                renamePlan.edits.forEach(edit => {
                    const uri = edit.resource.toString();
                    if (!editsByModel.has(uri)) {
                        editsByModel.set(uri, { model: null, edits: [] });
                    }
                    editsByModel.get(uri).edits.push({
                        range: edit.textEdit.range,
                        text: edit.textEdit.text
                    });
                });

                // Find models and apply edits
                const allModels = monacoRef.editor.getModels();
                let appliedCount = 0;
                const affectedRoutines = new Set();

                editsByModel.forEach((data, uri) => {
                    const targetModel = allModels.find(m => m.uri.toString() === uri);
                    if (targetModel) {
                        targetModel.pushEditOperations([], data.edits, () => null);
                        appliedCount += data.edits.length;
                        // Extract routine name from URI for save+zlink
                        const path = targetModel.uri.path || targetModel.uri.fsPath || '';
                        const routineName = path.split(/[\\/]/).pop().replace(/\.m$/i, '');
                        if (routineName) affectedRoutines.add(routineName);
                    }
                });

                showToast('success', 'Smart Rename',
                    `Renamed "${oldTagName}" to "${newTagName}" (${appliedCount} changes in ${renamePlan.routinesAffected} routine(s))`
                );

                // Save and ZLINK all affected routines
                if (window.ahmadIDE?.saveRoutine && window.ahmadIDE?.zlinkRoutine) {
                    showToast('info', 'Smart Rename', 'Saving affected routines...');

                    // Get tab management functions
                    const tabsApi = window.AhmadIDEModules?.tabs;
                    const getOpenTabs = tabsApi?.getOpenTabs || (() => []);
                    const markTabDirty = tabsApi?.markTabDirty || (() => { });
                    const renderTabs = tabsApi?.renderTabs || (() => { });

                    for (const routineName of affectedRoutines) {
                        try {
                            // Find model and get its content
                            const modelForRoutine = allModels.find(m => {
                                const p = m.uri.path || m.uri.fsPath || '';
                                return p.split(/[\\/]/).pop().replace(/\.m$/i, '').toUpperCase() === routineName.toUpperCase();
                            });
                            if (modelForRoutine) {
                                const code = modelForRoutine.getValue();
                                const saveRes = await window.ahmadIDE.saveRoutine(routineName, code);
                                if (saveRes.ok) {
                                    await window.ahmadIDE.zlinkRoutine(routineName);
                                    console.log(`[Smart Rename] Saved and zlinked: ${routineName}`);

                                    // Mark tab as clean (not dirty)
                                    const openTabs = getOpenTabs();
                                    const matchingTab = openTabs.find(t => {
                                        const tabName = (t.name || t.path || '').toUpperCase().replace(/\.M$/i, '');
                                        return tabName === routineName.toUpperCase() ||
                                            tabName.endsWith('/' + routineName.toUpperCase()) ||
                                            tabName.endsWith('\\' + routineName.toUpperCase());
                                    });
                                    if (matchingTab) {
                                        markTabDirty(matchingTab.id, false);
                                        console.log(`[Smart Rename] Marked tab clean: ${matchingTab.id}`);
                                    }
                                }
                            }
                        } catch (err) {
                            console.error(`[Smart Rename] Failed to save ${routineName}:`, err);
                        }
                    }

                    // Re-render tabs to clear dirty indicators
                    try { renderTabs(); } catch (_) { }

                    showToast('success', 'Smart Rename', `Saved and zlinked ${affectedRoutines.size} routine(s)`);
                }

            } catch (err) {
                console.error('[Smart Rename Fallback] Error:', err);
                showToast('error', 'Smart Rename', err.message || 'Unknown error');
            }
        }

        /**
         * Show confirmation dialog
         */
        async function showConfirmDialog({ title, message, details, okLabel = 'OK', cancelLabel = 'Cancel' }) {
            return new Promise((resolve) => {
                try {
                    const overlay = document.createElement('div');
                    overlay.style.cssText = 'position:fixed;inset:0;background:rgba(0,0,0,0.7);z-index:99999;display:flex;align-items:center;justify-content:center;';

                    const dialog = document.createElement('div');
                    dialog.style.cssText = 'background:#363948;border:1px solid #44475a;border-radius:8px;padding:24px;min-width:450px;max-width:600px;box-shadow:0 8px 32px rgba(0,0,0,0.4);';

                    const detailsHtml = Array.isArray(details)
                        ? details.map(d => `<div style="font-size:12px;color:rgba(255,255,255,0.6);margin-bottom:4px;">${d}</div>`).join('')
                        : '';

                    dialog.innerHTML = `
                        <div style="font-size:16px;font-weight:600;color:#f8f8f2;margin-bottom:16px;">${title}</div>
                        <div style="font-size:13px;color:rgba(255,255,255,0.85);margin-bottom:16px;">${message}</div>
                        ${detailsHtml}
                        <div style="display:flex;gap:8px;justify-content:flex-end;margin-top:20px;">
                            <button id="confirmCancel" style="padding:8px 16px;background:transparent;border:1px solid #44475a;border-radius:4px;color:#f8f8f2;cursor:pointer;">${cancelLabel}</button>
                            <button id="confirmOk" style="padding:8px 16px;background:#bd93f9;border:1px solid #bd93f9;border-radius:4px;color:#f8f8f2;cursor:pointer;font-weight:600;">${okLabel}</button>
                        </div>
                    `;

                    overlay.appendChild(dialog);
                    document.body.appendChild(overlay);

                    const okBtn = document.getElementById('confirmOk');
                    const cancelBtn = document.getElementById('confirmCancel');

                    const cleanup = () => {
                        document.body.removeChild(overlay);
                    };

                    okBtn.onclick = () => {
                        cleanup();
                        resolve(true);
                    };

                    cancelBtn.onclick = () => {
                        cleanup();
                        resolve(false);
                    };

                    overlay.onclick = (e) => {
                        if (e.target === overlay) {
                            cleanup();
                            resolve(false);
                        }
                    };

                    // Focus OK button
                    okBtn.focus();
                } catch (err) {
                    console.error('[Smart Rename] Confirm dialog error:', err);
                    resolve(false);
                }
            });
        }

        /**
         * Prompt user for new tag name using UI dialog
         */
        async function promptForNewName(oldName) {
            // Use UI dialog system instead of prompt()
            const dialogPrompt = window.AhmadIDEModules?.ui?.dialog?.prompt;

            if (dialogPrompt) {
                try {
                    const result = await dialogPrompt({
                        title: 'Smart Rename Tag',
                        message: `Rename tag "${oldName}" to:`,
                        defaultValue: oldName,
                        placeholder: 'Enter new tag name',
                        okLabel: 'Rename',
                        cancelLabel: 'Cancel'
                    });
                    return result ? result.trim() : null;
                } catch (err) {
                    console.error('[Smart Rename] Dialog error:', err);
                    return null;
                }
            }

            // Fallback: try to use a custom input
            return new Promise((resolve) => {
                try {
                    // Create simple modal input
                    const overlay = document.createElement('div');
                    overlay.style.cssText = 'position:fixed;inset:0;background:rgba(0,0,0,0.7);z-index:99999;display:flex;align-items:center;justify-content:center;';

                    const dialog = document.createElement('div');
                    dialog.style.cssText = 'background:#363948;border:1px solid #44475a;border-radius:8px;padding:24px;min-width:400px;box-shadow:0 8px 32px rgba(0,0,0,0.4);';
                    dialog.innerHTML = `
                        <div style="font-size:16px;font-weight:600;color:#f8f8f2;margin-bottom:16px;">Smart Rename Tag</div>
                        <div style="font-size:13px;color:rgba(255,255,255,0.7);margin-bottom:12px;">Rename tag "${oldName}" to:</div>
                        <input type="text" id="smartRenameInput" value="${oldName}" style="width:100%;padding:8px 12px;font-size:13px;background:#282a36;border:1px solid #44475a;border-radius:4px;color:#f8f8f2;margin-bottom:16px;" />
                        <div style="display:flex;gap:8px;justify-content:flex-end;">
                            <button id="smartRenameCancel" style="padding:8px 16px;background:transparent;border:1px solid #44475a;border-radius:4px;color:#f8f8f2;cursor:pointer;">Cancel</button>
                            <button id="smartRenameOk" style="padding:8px 16px;background:#bd93f9;border:1px solid #bd93f9;border-radius:4px;color:#f8f8f2;cursor:pointer;font-weight:600;">Rename</button>
                        </div>
                    `;

                    overlay.appendChild(dialog);
                    document.body.appendChild(overlay);

                    const input = document.getElementById('smartRenameInput');
                    const okBtn = document.getElementById('smartRenameOk');
                    const cancelBtn = document.getElementById('smartRenameCancel');

                    input.focus();
                    input.select();

                    const cleanup = () => {
                        document.body.removeChild(overlay);
                    };

                    const submit = () => {
                        const value = input.value.trim();
                        cleanup();
                        resolve(value || null);
                    };

                    okBtn.onclick = submit;
                    cancelBtn.onclick = () => {
                        cleanup();
                        resolve(null);
                    };

                    input.onkeydown = (e) => {
                        if (e.key === 'Enter') submit();
                        if (e.key === 'Escape') {
                            cleanup();
                            resolve(null);
                        }
                    };

                    overlay.onclick = (e) => {
                        if (e.target === overlay) {
                            cleanup();
                            resolve(null);
                        }
                    };
                } catch (err) {
                    console.error('[Smart Rename] Failed to create input dialog:', err);
                    resolve(null);
                }
            });
        }

        /**
         * Register Monaco action
         */
        function registerMonacoAction(editor) {
            if (!editor?.addAction) return;

            try {
                editor.addAction({
                    id: 'mumps.smartRenameTag',
                    label: 'Smart Rename Tag (Cross-Routine)',
                    keybindings: [
                        monaco.KeyMod.Shift | monaco.KeyMod.Alt | monaco.KeyCode.F2
                    ],
                    contextMenuGroupId: '1_modification',
                    contextMenuOrder: 1.6,
                    run: async (ed) => {
                        await triggerSmartRenameTag();
                    }
                });

                console.log('[Smart Rename] Monaco action registered');
            } catch (err) {
                console.error('[Smart Rename] Action registration error:', err);
            }
        }

        return {
            initialize,
            buildIndex,
            triggerSmartRenameTag,
            registerMonacoAction,
            getSymbolIndexer: () => symbolIndexer
        };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.mumps = window.AhmadIDEModules.mumps || {};
        window.AhmadIDEModules.mumps.createMumpsSmartRenameProvider = createMumpsSmartRenameProvider;
    }

    if (typeof module !== 'undefined' && module.exports) {
        module.exports = { createMumpsSmartRenameProvider };
    }
})();
