(() => {
    // Parse routine/tag reference at cursor position (supports TAG^RTN, ^RTN, DO TAG)
    function parseRoutineReferenceAtPosition(model, position) {
        if (!model || !position) return null;
        const lineContent = model.getLineContent(position.lineNumber) || '';
        const column = position.column;

        try {
            // Pattern 1: TAG^ROUTINE or $$TAG^ROUTINE
            const tagRoutineRx = /(\$\$)?([A-Z%][A-Z0-9]*)\^([A-Z%][A-Z0-9]+)/gi;
            let m = null;
            while ((m = tagRoutineRx.exec(lineContent))) {
                const full = m[0];
                const idx = m.index;
                const endIdx = idx + full.length;
                if (column >= idx + 1 && column <= endIdx + 1) {
                    const tag = m[2] || '';
                    const routine = m[3] || '';
                    return { type: 'external', routine, tag };
                }
            }

            // Pattern 2: ^ROUTINE (standalone)
            const routineRx = /\^([A-Z%][A-Z0-9]+)/gi;
            while ((m = routineRx.exec(lineContent))) {
                const full = m[0];
                const idx = m.index;
                const endIdx = idx + full.length;
                if (column >= idx + 1 && column <= endIdx + 1) {
                    const routine = m[1] || '';
                    return { type: 'external', routine, tag: '' };
                }
            }

            // Pattern 3: $$TAG (local extrinsic) without ^ROUTINE
            const localExtrinsicRx = /\$\$([A-Z%][A-Z0-9]*)(?!\^)/gi;
            while ((m = localExtrinsicRx.exec(lineContent))) {
                const full = m[0];
                const idx = m.index;
                const endIdx = idx + full.length;
                if (column >= idx + 1 && column <= endIdx + 1) {
                    return { type: 'local', tag: m[1] || '' };
                }
            }

            // Pattern 4: D TAG, DO TAG (local tag call)
            const localDoRx = /(?:^|\s)(?:D(?:O)?)\s+([A-Z%][A-Z0-9]*)(?=$|\s|,|\()/gi;
            while ((m = localDoRx.exec(lineContent))) {
                const full = m[0];
                const tagName = m[1] || '';
                const rel = full.lastIndexOf(tagName);
                const idx = m.index + Math.max(0, rel);
                const endIdx = idx + tagName.length;
                if (column >= idx + 1 && column <= endIdx + 1) {
                    return { type: 'local', tag: tagName };
                }
            }
        } catch (err) {
            // logger not defined this early? use console
            console.warn('GOTO_DECLARATION_PARSE_ERROR', { line: position.lineNumber, column, message: err?.message });
        }

        return null;
    }

    function createGoToDeclaration({ deps } = {}) {
        const dbgLog = deps?.dbgLog;
        const logger = deps?.logger;
        const showToast = deps?.showToast;
        const findOpenTab = deps?.findOpenTab;
        const switchTab = deps?.switchTab;
        const createTab = deps?.createTab;
        const mumpsLocalTagResolver = deps?.mumpsLocalTagResolver;
        const getActiveEditor = deps?.getActiveEditor;
        const getDbgStateRef = deps?.getDbgStateRef;
        const getActiveRoutine = deps?.getActiveRoutine;

        return async function goToDeclaration(editor, position = null, options = {}) {
            const { silentIfMissing = false } = options;
            const activeEditor = (typeof getActiveEditor === 'function') ? getActiveEditor() : null;
            const ed = editor || activeEditor;
            if (!ed) return false;

            const model = ed.getModel();
            const pos = position || ed.getPosition();
            if (!model || !pos) return false;

            // CRITICAL: DO NOT modify debug state during navigation
            // This function is for editor-only code navigation
            // If a debug session is active, it should NOT be paused/resumed/altered
            // by user clicking on routine references
            const dbgStateRef = (typeof getDbgStateRef === 'function') ? getDbgStateRef() : null;
            dbgLog('[editor] goToDeclaration (NAV ONLY)', { line: pos.lineNumber, column: pos.column, hasActiveDebug: !!dbgStateRef?.sessionId });
            logger.debug('GOTO_DECLARATION_NAV_ONLY', { line: pos.lineNumber, hasActiveDebug: !!dbgStateRef?.sessionId });

            const ref = parseRoutineReferenceAtPosition(model, pos);
            if (!ref) {
                logger.warn('GOTO_DECLARATION_NOT_FOUND', { line: pos.lineNumber, column: pos.column });
                if (!silentIfMissing) {
                    showToast('info', 'Go to Declaration', 'No symbol under cursor');
                }
                return false;
            }

            const revealTagInEditor = (targetEditor, tag) => {
                if (!targetEditor || !tag) return false;
                const targetModel = targetEditor.getModel();
                if (!targetModel) return false;
                const viaCache = mumpsLocalTagResolver?.getTagLine?.(targetModel, tag);
                const line = viaCache || null;
                if (line) {
                    targetEditor.revealLineInCenter(line);
                    targetEditor.setPosition({ lineNumber: line, column: 1 });
                    return true;
                }
                // Fallback scan (should be rare)
                const lineCount = targetModel.getLineCount();
                for (let i = 1; i <= lineCount; i++) {
                    const lineContent = targetModel.getLineContent(i).trim();
                    if (new RegExp(`^${tag}(?:\\s|;|\\()`, 'i').test(lineContent)) {
                        targetEditor.revealLineInCenter(i);
                        targetEditor.setPosition({ lineNumber: i, column: 1 });
                        return true;
                    }
                }
                return false;
            };

            if (ref.type === 'external') {
                const routine = ref.routine;
                const tag = ref.tag || '';
                try {
                    const existingTab = findOpenTab(routine);
                    if (existingTab) {
                        switchTab(existingTab.id);
                    } else {
                        const readRes = await window.ahmadIDE.readRoutine(routine);
                        if (!readRes?.ok) {
                            logger.warn('GOTO_DECLARATION_LOAD_FAIL', { routine, error: readRes?.error });
                            showToast('error', 'Go to Declaration', `Could not load ${routine}: ${readRes?.error || 'Unknown error'}`);
                            return false;
                        }
                        createTab(routine, readRes.code || '');
                    }
                } catch (err) {
                    logger.warn('GOTO_DECLARATION_OPEN_FAIL', { routine: ref.routine, error: err?.message });
                    showToast('error', 'Go to Declaration', err.message || `Failed to open ${routine}`);
                    return false;
                }

                setTimeout(() => {
                    const activeEditor = (typeof getActiveEditor === 'function') ? getActiveEditor() : null;
                    const targetEditor = activeEditor || ed;
                    if (tag && targetEditor) {
                        const found = revealTagInEditor(targetEditor, tag);
                        if (!found && !silentIfMissing) {
                            logger.warn('GOTO_DECLARATION_TAG_NOT_FOUND', { routine, tag });
                            showToast('info', 'Go to Declaration', `Tag ${tag} not found in ${routine}`);
                        }
                    }
                }, 50);

                const jumpLabel = tag ? `${tag}^${routine}` : routine;
                showToast('success', 'Navigated', jumpLabel);
                return true;
            }

            if (ref.type === 'local') {
                const found = revealTagInEditor(ed, ref.tag);
                if (found) {
                    showToast('success', 'Navigated', `Tag: ${ref.tag}`);
                } else if (!silentIfMissing) {
                    logger.warn('GOTO_DECLARATION_TAG_NOT_FOUND', { routine: getActiveRoutine(), tag: ref.tag });
                    showToast('info', 'Go to Declaration', `Tag ${ref.tag} not found in this routine`);
                }
                return found;
            }

            return false;
        };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.renderer = window.AhmadIDEModules.renderer || {};
        window.AhmadIDEModules.renderer.editor = window.AhmadIDEModules.renderer.editor || {};
        window.AhmadIDEModules.renderer.editor.declarationNavigation = window.AhmadIDEModules.renderer.editor.declarationNavigation || {};
        window.AhmadIDEModules.renderer.editor.declarationNavigation.parseRoutineReferenceAtPosition = parseRoutineReferenceAtPosition;
        window.AhmadIDEModules.renderer.editor.declarationNavigation.createGoToDeclaration = createGoToDeclaration;
    }
})();
