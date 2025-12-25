(() => {
    const createNoopLogger = () => ({
        debug: () => { },
        info: () => { },
        warn: () => { },
        error: () => { },
        isEnabled: () => false
    });

    function createRoutinesManager({ deps } = {}) {
        const logger = deps?.logger || createNoopLogger();
        const showToast = deps?.showToast || (() => { });
        const appendOutput = deps?.appendOutput || (() => { });
        const showCustomPrompt = deps?.showCustomPrompt || (() => { });
        const normalizeRoutineTarget = deps?.normalizeRoutineTarget || (() => ({ base: '', folder: null, path: '' }));
        const setActiveRoutineName = deps?.setActiveRoutineName || (() => { });
        const getRoutinesCache = deps?.getRoutinesCache || (() => []);
        const setRoutinesCache = deps?.setRoutinesCache || (() => { });
        const renderProjectTree = deps?.renderProjectTree || (() => { });
        const getActiveEditor = deps?.getActiveEditor || (() => null);

        const findOpenTab = deps?.findOpenTab || (() => null);
        const switchTab = deps?.switchTab || (() => { });
        const createTab = deps?.createTab || (() => { });
        const getActiveTabId = deps?.getActiveTabId || (() => null);
        const getOpenTabs = deps?.getOpenTabs || (() => []);
        const markTabDirty = deps?.markTabDirty || (() => { });
        const renderTabs = deps?.renderTabs || (() => { });

        const validateMumps = deps?.validateMumps || (() => { });
        const hasLintRules = deps?.hasLintRules || (() => false);
        const applyLintMarkers = deps?.applyLintMarkers || (() => { });
        const renderProblems = deps?.renderProblems || (() => { });

        const mumpsValidator = deps?.mumpsValidator || (typeof window !== 'undefined' ? window._mumpsValidator : null);
        const mumpsLinter = deps?.mumpsLinter || (typeof window !== 'undefined' ? window._mumpsLinter : null);

        function setCurrentRoutine(name) {
            const normalized = normalizeRoutineTarget(name);
            setActiveRoutineName(normalized.path || normalized.base || null);
            const displayName = normalized.base || (name || 'None');
            const displayFolder = normalized.folder || 'routines';
            const label = document.getElementById('currentRoutineLabel');
            if (label) label.textContent = normalized.base || 'None';


            const breadcrumbsContainer = document.getElementById('breadcrumbs');
            if (breadcrumbsContainer) {
                breadcrumbsContainer.innerHTML = '';

                // Project level
                const projectItem = document.createElement('span');
                projectItem.className = 'breadcrumb-item';
                projectItem.textContent = 'Project';
                projectItem.onclick = () => {
                    showToast('info', 'Navigation', 'Project root');
                };
                breadcrumbsContainer.appendChild(projectItem);

                if (normalized.base) {
                    // Separator
                    const sep1 = document.createElement('span');
                    sep1.className = 'breadcrumb-separator';
                    sep1.textContent = '›';
                    breadcrumbsContainer.appendChild(sep1);

                    // Routines folder
                    const routinesItem = document.createElement('span');
                    routinesItem.className = 'breadcrumb-item';
                    routinesItem.textContent = displayFolder;
                    routinesItem.onclick = () => {
                        showToast('info', 'Navigation', 'Routines folder');
                    };
                    breadcrumbsContainer.appendChild(routinesItem);

                    // Separator
                    const sep2 = document.createElement('span');
                    sep2.className = 'breadcrumb-separator';
                    sep2.textContent = '›';
                    breadcrumbsContainer.appendChild(sep2);

                    // Current file
                    const fileItem = document.createElement('span');
                    fileItem.className = 'breadcrumb-item active';
                    fileItem.textContent = `${displayName}.m`;
                    breadcrumbsContainer.appendChild(fileItem);
                }
            }
        }

        async function loadRoutineList(state, editor, search = '') {
            const res = await window.ahmadIDE.listRoutines('');
            if (res.ok) {
                state._cacheFull = res.routines;
                setRoutinesCache(res.routines || getRoutinesCache());
                setTimeout(() => {
                    renderProjectTree(state._cacheFull || state._lastRoutines || [], state, editor);
                }, 0);
            } else {
                setTimeout(() => renderProjectTree([], state, editor), 0);
            }
            state._lastRoutines = res.ok ? res.routines : [];
        }

        async function loadRoutineByName(name, state, editor, routinesCache = [], termState) {
            const targetInfo = normalizeRoutineTarget(name);
            const targetRoutineKey = targetInfo.path || targetInfo.base;
            if (!targetRoutineKey) return;
            logger.info('FILE_OPEN', { routine: targetRoutineKey });

            // Check if tab already exists for this routine
            const existingTab = findOpenTab(targetRoutineKey);
            if (existingTab) {
                // Just switch to existing tab
                switchTab(existingTab.id);
                return;
            }

            // Load routine from backend
            const readRes = await window.ahmadIDE.readRoutine(targetRoutineKey);
            if (!readRes.ok) {
                logger.warn('FILE_OPEN_FAIL', { routine: targetRoutineKey, error: readRes.error || readRes.stderr });
                appendOutput(`✗ Failed to load ${targetRoutineKey}: ${readRes.error || readRes.stderr}`, termState);
                showToast('error', 'Search', `Could not open ${targetRoutineKey}`);
                return false;
            }

            // Create new tab with loaded content
            createTab(targetRoutineKey, readRes.code || '', state);

            // Validate the freshly loaded model
            const modelToValidate = editor?.getModel ? editor.getModel() : getActiveEditor()?.getModel?.();
            if (modelToValidate) {
                const delayed = () => validateMumps(modelToValidate);
                if (window.requestIdleCallback) window.requestIdleCallback(delayed, { timeout: 120 });
                else setTimeout(delayed, 50);
            }

            logger.info('FILE_OPEN_SUCCESS', { routine: targetRoutineKey });
            appendOutput(`✓ Loaded routine ${targetRoutineKey}`, termState);
            return true;
        }

        async function saveRoutineFlow(editor, state, termState) {
            let name = state.current;
            if (!name) {
                showCustomPrompt('Save Routine', 'Routine name (e.g., TEST1)', async (promptName) => {
                    name = promptName;
                    await performSave(name, editor, state, termState);
                });
                return;
            }
            logger.info('FILE_SAVE_REQUEST', { routine: name });
            await performSave(name, editor, state, termState);
        }

        async function performSave(name, editor, state, termState) {
            name = name.trim();
            if (!name) {
                appendOutput('✗ Save cancelled (no name).', termState);
                showToast('info', 'Save', 'Cancelled (no name).');
                return;
            }
            if (mumpsValidator) {
                // Extract just the routine name (remove folder path if present)
                const routineNameOnly = name.includes('/') ? name.split('/').pop() : name;
                const check = mumpsValidator.validateRoutineName(routineNameOnly.toUpperCase());
                if (!check.valid) {
                    const msg = check.errors.join('; ') || 'Invalid routine name';
                    appendOutput(`✗ Invalid routine: ${msg}`, termState);
                    showToast('error', 'Save', msg);
                    return;
                }
                // Convert full path to uppercase
                name = name.toUpperCase();
            }
            const code = editor.getValue();

            // Lint with mode: 'edit' - block only on errors (warnings/info are allowed)
            const linter = window._mumpsLinter || mumpsLinter;
            if (hasLintRules(linter)) {
                const lintResult = linter.lint(code || '', { mode: 'edit' });
                applyLintMarkers(editor.getModel(), lintResult.issues || []);
                renderProblems((lintResult.issues || []).map(i => ({
                    message: i.message || i.description || '',
                    severity: i.severity || 'info',
                    line: i.line || null,
                    code: i.ruleId || i.code || null
                })));
                const summary = lintResult.summary || { errors: 0, warnings: 0, info: 0 };
                // Save should not be blocked by lint errors; keep markers/problems visible and proceed.
            }

            logger.debug('FILE_SAVE', { routine: name });
            let res = null;
            try {
                res = await window.ahmadIDE.saveRoutine(name, code);
            } catch (err) {
                const msg = String(err?.message || err || 'Save failed').trim();
                appendOutput(`✗ Save failed: ${msg}`, termState);
                showToast('error', 'Save', msg);
                logger.error('FILE_SAVE_FAIL', { routine: name, error: msg });
                return;
            }
            if (res.ok) {
                const savedPath = res.folder ? `${res.folder}/${res.routine}` : (res.routine || name);
                state.current = savedPath;
                setCurrentRoutine(state.current);
                appendOutput(`✓ Saved routine ${state.current}`, termState);
                showToast('success', 'Save', `Saved ${state.current}`);
                await window.ahmadIDE.zlinkRoutine(state.current);
                appendOutput(`✓ ZLINK ${state.current}`, termState);
                await loadRoutineList(state, editor, '', termState);
                logger.info('FILE_SAVE_SUCCESS', { routine: savedPath });

                // Clear dirty state for current tab
                const activeTabId = getActiveTabId();
                if (activeTabId) {
                    markTabDirty(activeTabId, false);
                    // Update tab name if it changed
                    const openTabs = getOpenTabs();
                    const currentTab = openTabs.find(t => t.id === activeTabId);
                    if (currentTab) {
                        currentTab.name = res.routine || currentTab.name;
                        currentTab.path = savedPath || currentTab.path;
                        currentTab.folder = res.folder || currentTab.folder;
                        if (currentTab.state) currentTab.state.current = savedPath;
                        renderTabs();
                    }
                }
            } else {
                const msg = String(res.error || res.stderr || 'Save failed').trim();
                appendOutput(`✗ Save failed: ${msg}`, termState);
                showToast('error', 'Save', msg);
                logger.error('FILE_SAVE_FAIL', { routine: name, error: msg });
            }
        }

        async function newRoutineFlow(editor, state, termState) {
            showCustomPrompt('New Routine', 'Routine name (e.g., NEWRTN)', async (name) => {
                const trimmed = name.trim();
                if (!trimmed) {
                    appendOutput('✗ New routine cancelled (no name).', termState);
                    return;
                }

                // Validate length first
                if (trimmed.length > 16) {
                    appendOutput(`✗ Invalid routine name: Name must be 16 characters or less (got ${trimmed.length})`, termState);
                    showToast('error', 'New Routine', `Name must be 16 characters or less (got ${trimmed.length})`);
                    return;
                }

                if (mumpsValidator) {
                    const check = mumpsValidator.validateRoutineName(trimmed.toUpperCase());
                    if (!check.valid) {
                        appendOutput(`✗ Invalid routine: ${check.errors.join('; ')}`, termState);
                        showToast('error', 'New Routine', check.errors.join('; '));
                        return;
                    }
                }

                const routineName = trimmed.toUpperCase();

                // Check if routine already exists
                const existingRoutine = await window.ahmadIDE.readRoutine(routineName);
                if (existingRoutine.ok && existingRoutine.code) {
                    // Routine exists - open it instead of creating new one
                    logger.info('FILE_OPEN_EXISTING', { routine: routineName, codeLength: existingRoutine.code.length });
                    appendOutput(`ℹ Routine ${routineName} already exists - opening it`, termState);
                    showToast('info', 'New Routine', `Routine ${routineName} already exists - opened for editing`);

                    // Check if tab already exists
                    normalizeRoutineTarget(routineName);
                    const routineKey = existingRoutine.folder ? `${existingRoutine.folder}/${routineName}` : routineName;
                    const existingTab = findOpenTab(routineKey);

                    if (existingTab) {
                        switchTab(existingTab.id);
                    } else {
                        // Create tab with existing code
                        createTab(routineName, existingRoutine.code, state, { folder: existingRoutine.folder });
                        state.current = existingRoutine.folder ? `${existingRoutine.folder}/${routineName}` : routineName;
                        setCurrentRoutine(state.current);
                    }
                    return;
                }

                logger.info('FILE_CREATE', { routine: routineName });
                const code = `MAIN ; ${routineName} routine\n    WRITE "Hello from ${routineName}!", !\n    QUIT\n`;

                // Create a new tab for the routine
                createTab(routineName, code, state);
                state.current = routineName;

                if (editor) {
                    editor.setValue(code);
                }
                setCurrentRoutine(routineName);
                await saveRoutineFlow(editor, state, termState);
            });

        }

        return {
            setCurrentRoutine,
            loadRoutineList,
            loadRoutineByName,
            saveRoutineFlow,
            performSave,
            newRoutineFlow
        };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.routines = window.AhmadIDEModules.routines || {};
        window.AhmadIDEModules.routines.createRoutinesManager = createRoutinesManager;
    }
})();
