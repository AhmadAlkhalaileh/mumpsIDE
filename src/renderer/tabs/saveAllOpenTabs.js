(() => {
    function createSaveAllOpenTabs({ deps } = {}) {
        const showToast = deps?.showToast;
        const appendOutput = deps?.appendOutput;
        const getGlobalTerminalState = deps?.getGlobalTerminalState;
        const getOpenTabs = deps?.getOpenTabs;
        const getTabModels = deps?.getTabModels;
        const mumpsValidator = deps?.mumpsValidator;
        const markTabDirty = deps?.markTabDirty;
        const renderTabs = deps?.renderTabs;
        const loadRoutineList = deps?.loadRoutineList;
        const getGlobalRoutineState = deps?.getGlobalRoutineState;
        const getActiveEditor = deps?.getActiveEditor;

        async function saveAllOpenTabs({ terminalState } = {}) {
            const globalTerminalState = (typeof getGlobalTerminalState === 'function') ? getGlobalTerminalState() : null;
            const openTabs = (typeof getOpenTabs === 'function') ? getOpenTabs() : null;
            const tabModels = (typeof getTabModels === 'function') ? getTabModels() : null;

            const termState = terminalState || globalTerminalState || null;
            const tabs = Array.isArray(openTabs) ? openTabs : [];

            const dirty = tabs.filter((t) => {
                if (!t) return false;
                if (!t.isDirty) return false;
                if (String(t.kind || 'routine') !== 'routine') return false;
                if (t.readOnly) return false;
                return true;
            });

            if (!dirty.length) {
                try { showToast('info', 'Save All', 'No unsaved changes'); } catch (_) { }
                return;
            }

            try { appendOutput(`💾 Save All: ${dirty.length} file(s)…`, termState); } catch (_) { }

            const validator = window._mumpsValidator || mumpsValidator || null;
            let savedCount = 0;
            let failedCount = 0;

            for (const tab of dirty) {
                const tabId = tab.id;
                const rawName = String(tab.path || tab.name || '').trim();
                if (!rawName) {
                    failedCount += 1;
                    continue;
                }

                const model = tabModels?.get?.(tabId) || null;
                const code = model?.getValue ? model.getValue() : String(tab.content || '');

                let nameToSave = rawName;
                if (validator) {
                    const routineNameOnly = nameToSave.includes('/') ? nameToSave.split('/').pop() : nameToSave;
                    const check = validator.validateRoutineName(String(routineNameOnly || '').toUpperCase());
                    if (!check?.valid) {
                        const msg = check?.errors?.join?.('; ') || 'Invalid routine name';
                        try { appendOutput(`✗ Skip ${rawName}: ${msg}`, termState); } catch (_) { }
                        try { showToast('error', 'Save All', `${rawName}: ${msg}`); } catch (_) { }
                        failedCount += 1;
                        continue;
                    }
                    nameToSave = nameToSave.toUpperCase();
                }

                let res = null;
                try {
                    res = await window.ahmadIDE.saveRoutine(nameToSave, code);
                } catch (err) {
                    const msg = String(err?.message || err || 'Save failed').trim();
                    try { appendOutput(`✗ Save failed ${rawName}: ${msg}`, termState); } catch (_) { }
                    failedCount += 1;
                    continue;
                }

                if (!res?.ok) {
                    const msg = String(res?.error || res?.stderr || 'Save failed').trim();
                    try { appendOutput(`✗ Save failed ${rawName}: ${msg}`, termState); } catch (_) { }
                    failedCount += 1;
                    continue;
                }

                const savedPath = res.folder ? `${res.folder}/${res.routine}` : (res.routine || nameToSave);
                tab.name = res.routine || tab.name;
                tab.path = savedPath || tab.path;
                tab.folder = res.folder || tab.folder;
                if (tab.state) tab.state.current = savedPath || tab.state.current;

                try { markTabDirty(tabId, false, { deferRender: true }); } catch (_) { tab.isDirty = false; }

                try {
                    await window.ahmadIDE.zlinkRoutine(savedPath);
                    appendOutput(`✓ Saved ${savedPath} (ZLINK)`, termState);
                } catch (_) {
                    try { appendOutput(`✓ Saved ${savedPath}`, termState); } catch (_) { }
                }

                savedCount += 1;
            }

            try { renderTabs(); } catch (_) { }

            if (savedCount > 0) {
                const msg = failedCount ? `Saved ${savedCount} · ${failedCount} failed` : `Saved ${savedCount}`;
                try { showToast('success', 'Save All', msg); } catch (_) { }
                try {
                    const globalRoutineState = (typeof getGlobalRoutineState === 'function') ? getGlobalRoutineState() : null;
                    const activeEditor = (typeof getActiveEditor === 'function') ? getActiveEditor() : null;
                    if (globalRoutineState && activeEditor) {
                        // Refresh project tree once (not per file).
                        loadRoutineList(globalRoutineState, activeEditor, '');
                    }
                } catch (_) { }
            } else {
                try { showToast('error', 'Save All', 'Nothing was saved'); } catch (_) { }
            }
        }

        return saveAllOpenTabs;
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.renderer = window.AhmadIDEModules.renderer || {};
        window.AhmadIDEModules.renderer.tabs = window.AhmadIDEModules.renderer.tabs || {};
        window.AhmadIDEModules.renderer.tabs.createSaveAllOpenTabs = createSaveAllOpenTabs;
    }
})();
