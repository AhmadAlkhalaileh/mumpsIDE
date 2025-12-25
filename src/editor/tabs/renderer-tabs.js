(() => {
    const createNoopLogger = () => ({
        debug: () => { },
        info: () => { },
        warn: () => { },
        error: () => { },
        isEnabled: () => false
    });

    function createTabManager({ state, deps } = {}) {
        const $ = deps?.$ || (typeof window !== 'undefined' ? (window.$ || window.jQuery || null) : null);
        const logger = deps?.logger || createNoopLogger();
        const tabMumpsIcon = deps?.tabMumpsIcon || '';
        const getMonaco = deps?.getMonaco || (() => (typeof monaco !== 'undefined' ? monaco : null));
        const getActiveEditor = deps?.getActiveEditor || (() => null);
        const setLastValidatedVersionIdNull = deps?.setLastValidatedVersionIdNull || (() => { });
        const getRoutineStateRef = deps?.getRoutineStateRef || (() => null);
        const getDbgStateRef = deps?.getDbgStateRef || (() => null);
        const decorateBreakpoints = deps?.decorateBreakpoints || (() => { });
        const renderBreakpoints = deps?.renderBreakpoints || (() => { });
        const setCurrentRoutine = deps?.setCurrentRoutine || (() => { });
        const showConfirmDialog = deps?.showConfirmDialog || (() => { });
        const showCustomPrompt = deps?.showCustomPrompt || (() => { });
        const appendOutput = deps?.appendOutput || (() => { });
        const getGlobalTerminalState = deps?.getGlobalTerminalState || (() => null);
        const onTabActivated = deps?.onTabActivated || (() => { });
        const onTabClosed = deps?.onTabClosed || (() => { });

        if (!state || !state.openTabs || !state.tabModels) {
            throw new Error('createTabManager requires { state.openTabs, state.tabModels }');
        }

        function normalizeRoutineTarget(name, folderHint = null) {
            const trimmed = (name || '').trim();
            let folder = folderHint || null;
            if (!trimmed) return { base: '', folder, path: '' };
            let base = trimmed;
            if (trimmed.includes('/')) {
                const parts = trimmed.split('/');
                folder = parts[0] || folder;
                base = parts.slice(1).join('/') || parts[1] || '';
            }
            const baseNoExt = base.replace(/\.m$/i, '');
            const normalizedBase = baseNoExt.toUpperCase();
            const path = folder ? `${folder}/${normalizedBase}` : normalizedBase;
            return { base: normalizedBase, folder, path };
        }

        const getTabKind = (tab) => String(tab?.kind || 'routine');
        const isRoutineTab = (tab) => getTabKind(tab) === 'routine';

        function findOpenTab(target, { exact = false } = {}) {
            const normTarget = normalizeRoutineTarget(target);
            const exactMatch = state.openTabs.find(t => {
                if (!isRoutineTab(t)) return false;
                const normTab = normalizeRoutineTarget(t.path || t.name);
                return normTab.path === normTarget.path;
            });
            if (exact || exactMatch) return exactMatch || null;

            const looseMatches = state.openTabs.filter(t => {
                if (!isRoutineTab(t)) return false;
                const normTab = normalizeRoutineTarget(t.path || t.name);
                return normTab.base && normTab.base === normTarget.base;
            });
            return looseMatches.length === 1 ? looseMatches[0] : null;
        }

        function scrollActiveTabIntoView() {
            const tabBar = document.getElementById('tabBar');
            const activeTab = tabBar?.querySelector('.tab.active');
            if (activeTab) {
                activeTab.scrollIntoView({ behavior: 'instant', block: 'nearest', inline: 'nearest' });
            }
        }

        // Debounced version of renderTabs to prevent excessive re-renders
        let renderTabsTimer = null;
        const debouncedRenderTabs = () => {
            if (renderTabsTimer) clearTimeout(renderTabsTimer);
            renderTabsTimer = setTimeout(() => renderTabs(), 16); // ~60fps
        };

        function markTabDirty(tabId, isDirty = true, opts = {}) {
            const tab = state.openTabs.find(t => t.id === tabId);
            if (tab && tab.isDirty !== isDirty) {
                tab.isDirty = isDirty;
                if (!opts.deferRender) {
                    debouncedRenderTabs(); // avoid reflow on every keystroke
                }
            }
        }

        function markCurrentTabClean() {
            if (state.activeTabId) {
                markTabDirty(state.activeTabId, false);
            }
        }

        function switchTab(tabId) {
            const tab = state.openTabs.find(t => t.id === tabId);
            if (!tab) return;
            if (tabId === state.activeTabId) return; // Already active

            const activeEditor = getActiveEditor();

            // Save current tab content
            if (state.activeTabId && activeEditor) {
                const currentTab = state.openTabs.find(t => t.id === state.activeTabId);
                if (currentTab) {
                    currentTab.content = activeEditor.getValue();
                }
            }

            state.activeTabId = tabId;
            const kind = getTabKind(tab);

            // Switch Monaco model (instant, no setValue)
            if (activeEditor) {
                setLastValidatedVersionIdNull(); // New model should always retrigger validation
                const model = state.tabModels.get(tabId);
                if (model) {
                    activeEditor.setModel(model);
                } else {
                    // Fallback: create model if missing
                    const monacoRef = getMonaco();
                    const lang = kind === 'routine' ? 'mumps' : (tab.language || 'plaintext');
                    const newModel = monacoRef.editor.createModel(tab.content || '', lang);
                    state.tabModels.set(tabId, newModel);
                    activeEditor.setModel(newModel);
                }
                try {
                    if (typeof activeEditor.updateOptions === 'function') {
                        activeEditor.updateOptions({ readOnly: kind === 'routine' ? false : !!tab.readOnly });
                    }
                } catch (_) { }
            }

            if (kind === 'routine') {
                const normalizedTarget = normalizeRoutineTarget(tab.path || tab.name);
                if (tab.state) {
                    tab.state.current = normalizedTarget.path || tab.state.current;
                }
                const routineStateRef = getRoutineStateRef();
                if (routineStateRef) {
                    routineStateRef.current = normalizedTarget.path || normalizedTarget.base;
                }
                setCurrentRoutine(normalizedTarget.path || normalizedTarget.base);
                logger.info('TAB_SWITCH', { tabId, path: normalizedTarget.path || normalizedTarget.base });

                const dbgStateRef = getDbgStateRef();
                if (dbgStateRef) {
                    decorateBreakpoints(activeEditor, dbgStateRef);
                    renderBreakpoints(dbgStateRef);
                }
            } else {
                logger.info('TAB_SWITCH_CUSTOM', { tabId, key: tab.key || tab.path || tab.name || '' });
            }

            renderTabs();
            scrollActiveTabIntoView();
            try { onTabActivated(tab); } catch (_) { }
        }

        function createTab(name, content = '', routineState = null, options = {}) {
            const normalized = normalizeRoutineTarget(name, options.folder || routineState?.current?.split('/')?.[0]);
            const tabPath = normalized.path || normalized.base || `UNTITLED_${(state.tabIdCounter || 0) + 1}`;

            // Check if tab already exists
            const existingTab = findOpenTab(tabPath, { exact: true });
            if (existingTab) {
                switchTab(existingTab.id);
                return existingTab;
            }

            state.tabIdCounter = (state.tabIdCounter || 0) + 1;
            const id = `tab_${state.tabIdCounter}`;
            const tabState = routineState || { current: tabPath };
            if (tabState && tabPath) {
                tabState.current = tabPath;
            }

            const tab = {
                id,
                name: normalized.base || tabPath,
                path: tabPath,
                folder: normalized.folder,
                content,
                isDirty: false,
                state: tabState,
                icon: tabMumpsIcon,
                kind: 'routine',
                language: 'mumps',
                readOnly: false,
                disposables: []  // Store disposables for cleanup
            };
            state.openTabs.push(tab);

            // Create Monaco model for this tab (for instant switching)
            const monacoRef = getMonaco();
            if (monacoRef) {
                // Create URI for the model (enables Git blame mapping)
                // Use Uri.from() to ensure proper formatting
                const uri = monacoRef.Uri.from({
                    scheme: 'docker',
                    path: `/${tabPath}` // Ensure leading slash
                });
                const model = monacoRef.editor.createModel(content, 'mumps', uri);
                state.tabModels.set(id, model);

                // Track changes for dirty state - STORE the disposable!
                const changeDisposable = model.onDidChangeContent(() => {
                    const t = state.openTabs.find(x => x.id === id);
                    if (t && !t.isDirty) {
                        t.isDirty = true;
                        debouncedRenderTabs();  // Debounced!
                    }
                });
                tab.disposables.push(changeDisposable);
            }

            renderTabs();
            switchTab(id);
            logger.info('TAB_OPEN', { id, path: tabPath, name: normalized.base, folder: normalized.folder });
            return tab;
        }

        function createCustomTab(options = {}) {
            const key = String(options.key || options.path || '').trim();
            if (!key) return null;
            const title = String(options.title || options.name || options.label || 'Untitled');
            const language = String(options.language || 'plaintext');
            const kind = String(options.kind || 'custom');
            const readOnly = options.readOnly !== false;
            const icon = options.icon != null ? String(options.icon) : '';
            const content = String(options.content ?? '');
            const meta = options.meta;

            const existing = state.openTabs.find(t => String(t.key || '') === key);
            if (existing) {
                existing.name = title;
                existing.path = String(existing.path || key);
                existing.key = key;
                existing.kind = kind;
                existing.language = language;
                existing.readOnly = !!readOnly;
                existing.icon = icon || existing.icon || '';
                existing.isDirty = false;
                existing.content = content;
                if (meta !== undefined) existing.meta = meta;

                const model = state.tabModels.get(existing.id);
                const monacoRef = getMonaco();
                if (model && typeof model.setValue === 'function') {
                    try {
                        if (model.getValue() !== content) model.setValue(content);
                    } catch (_) {
                        model.setValue(content);
                    }
                }
                try {
                    if (monacoRef?.editor?.setModelLanguage && model) {
                        monacoRef.editor.setModelLanguage(model, language);
                    }
                } catch (_) { }
                switchTab(existing.id);
                return existing;
            }

            state.tabIdCounter = (state.tabIdCounter || 0) + 1;
            const id = `tab_${state.tabIdCounter}`;

            const tab = {
                id,
                name: title,
                path: key,
                folder: null,
                content,
                isDirty: false,
                state: null,
                icon,
                kind,
                language,
                readOnly,
                key,
                meta,
                disposables: []
            };
            state.openTabs.push(tab);

            const monacoRef = getMonaco();
            if (monacoRef) {
                const model = monacoRef.editor.createModel(content, language);
                state.tabModels.set(id, model);
            }

            renderTabs();
            switchTab(id);
            logger.info('TAB_OPEN_CUSTOM', { id, key, kind });
            return tab;
        }

        function performCloseTab(tabId) {
            const index = state.openTabs.findIndex(t => t.id === tabId);
            if (index === -1) return;
            const wasActive = tabId === state.activeTabId;
            const closing = state.openTabs[index];

            // Dispose all event listeners for this tab
            if (closing.disposables) {
                closing.disposables.forEach(d => d.dispose());
                closing.disposables = [];
            }

            // Dispose Monaco model
            const model = state.tabModels.get(tabId);
            if (model) {
                model.dispose();
                state.tabModels.delete(tabId);
            }

            state.openTabs.splice(index, 1);

            const activeEditor = getActiveEditor();

            // Only change the active tab if we closed the active tab.
            if (state.openTabs.length > 0) {
                if (wasActive) {
                    // Switch to adjacent tab (: activate tab to the left, or right if none)
                    const newIndex = Math.min(index, state.openTabs.length - 1);
                    switchTab(state.openTabs[newIndex].id);
                } else {
                    // Keep current active tab; just re-render tab strip.
                    renderTabs();
                }
            } else {
                state.activeTabId = null;
                if (activeEditor) {
                    setLastValidatedVersionIdNull();
                    const monacoRef = getMonaco();
                    const emptyModel = monacoRef.editor.createModel('', 'mumps');
                    activeEditor.setModel(emptyModel);
                }
                setCurrentRoutine('');
                renderTabs();
            }
            logger.info('TAB_CLOSED', { tabId, path: closing?.path });
            try { onTabClosed(closing); } catch (_) { }
        }

        function closeTab(tabId, force = false) {
            const tab = state.openTabs.find(t => t.id === tabId);
            if (!tab) return;
            logger.info('TAB_CLOSE_REQUEST', { tabId, path: tab?.path });

            if (!force && tab.isDirty) {
                showConfirmDialog(
                    'Unsaved Changes',
                    `"${tab.name}" has unsaved changes. Do you want to discard them?`,
                    () => performCloseTab(tabId)
                );
            } else {
                performCloseTab(tabId);
            }
        }

        // Cycle through tabs (Ctrl+Tab / Ctrl+Shift+Tab)
        function cycleTab(direction = 1) {
            if (state.openTabs.length <= 1) return;
            const currentIndex = state.openTabs.findIndex(t => t.id === state.activeTabId);
            if (currentIndex === -1) return;

            const newIndex = (currentIndex + direction + state.openTabs.length) % state.openTabs.length;
            switchTab(state.openTabs[newIndex].id);
        }

        function bindTabKeyboardShortcuts() {
            if (state.tabShortcutsBound) return;
            const handler = (e) => {
                const key = (e.key || '').toLowerCase();
                if (key !== 'tab') return;
                if (!(e.ctrlKey || e.metaKey)) return;
                e.preventDefault();
                e.stopPropagation();
                cycleTab(e.shiftKey ? -1 : 1);
            };
            window.addEventListener('keydown', handler, true);
            state.tabShortcutsBound = true;
        }

        function buildTabElement(tab, isActive) {
            if (!$) return null;
            const el = $('<div class="tab"></div>');
            if (isActive) el.addClass('active');
            if (tab.isDirty) el.addClass('modified');
            el.attr('title', tab.path || tab.name || 'Untitled');

            const iconSpan = $('<span class="tab-icon"></span>').html(tab.icon || tabMumpsIcon);
            const nameSpan = $('<span class="tab-name"></span>').text(tab.name || 'Untitled');
            const closeBtn = $('<span class="tab-close">×</span>');

            closeBtn.on('click', (e) => {
                e.stopPropagation();
                closeTab(tab.id);
            });

            el.on('click', (e) => {
                if (!$(e.target).hasClass('tab-close')) {
                    switchTab(tab.id);
                }
            });

            el.on('mousedown', (e) => {
                if (e.which === 2) { // Middle button closes
                    e.preventDefault();
                    closeTab(tab.id);
                }
            });

            el.on('contextmenu', (e) => {
                e.preventDefault();
                showTabContextMenu(e.clientX, e.clientY, tab.id);
            });

            el.append(iconSpan, nameSpan, closeBtn);
            return el;
        }

        function renderTabs() {
            if (!$) return;
            const tabBar = $('#tabBar');
            if (!tabBar.length) return;

            tabBar.empty();

            state.openTabs.forEach(tab => {
                const el = buildTabElement(tab, tab.id === state.activeTabId);
                if (el) tabBar.append(el);
            });

            // Add "+" button for new tab
            const newBtn = $('<div class="tab ghost">+</div>');
            newBtn.attr('title', 'New Routine');
            newBtn.on('click', () => {
                showCustomPrompt('New Routine', 'Routine name (e.g., NEWRTN)', (name) => {
                    if (name) {
                        createTab(name.toUpperCase(), `${name.toUpperCase()}\t; New routine\n\tQUIT\n`);
                        const terminalState = getGlobalTerminalState();
                        if (terminalState) {
                            appendOutput(`✓ Created new routine: ${name.toUpperCase()}`, terminalState);
                        }
                    }
                });
            });
            tabBar.append(newBtn);
        }

        function showTabContextMenu(x, y, tabId) {
            const tab = state.openTabs.find(t => t.id === tabId);
            if (!tab) return;
            const registry = window.AhmadIDEModules?.app?.menuRegistry;
            const controller = window.AhmadIDEModules?.ui?.menu?.controller
                || window.AhmadIDEModules?.ui?.menu?.createMenuController?.({});
            const openContextMenu = window.AhmadIDEModules?.ui?.menu?.openContextMenu;
            if (!registry || (!controller && !openContextMenu)) return;

            const ctx = { tabId };
            const items = registry.get('context.tabs', ctx);
            const payload = {
                controller,
                x,
                y,
                items,
                ctx,
                onAction: (action) => {
                    if (action === 'tab:close') return closeTab(tabId);
                    if (action === 'tab:close-others') return closeOtherTabs(tabId);
                    if (action === 'tab:close-all') return closeAllTabs();
                    if (action === 'tab:close-left') return closeTabsToSide(tabId, 'left');
                    if (action === 'tab:close-right') return closeTabsToSide(tabId, 'right');
                }
            };
            if (typeof openContextMenu === 'function') {
                openContextMenu(payload);
                return;
            }
            controller.openAtPoint(payload);
        }

        function closeOtherTabs(keepTabId) {
            const tabsToClose = state.openTabs.filter(t => t.id !== keepTabId);
            tabsToClose.forEach(t => performCloseTab(t.id));
        }

        function closeAllTabs() {
            const allTabs = [...state.openTabs];
            allTabs.forEach(t => performCloseTab(t.id));
        }

        function closeTabsToSide(tabId, side) {
            const index = state.openTabs.findIndex(t => t.id === tabId);
            if (index === -1) return;

            const tabsToClose = side === 'left'
                ? state.openTabs.slice(0, index)
                : state.openTabs.slice(index + 1);

            tabsToClose.forEach(t => performCloseTab(t.id));
        }

        return {
            normalizeRoutineTarget,
            findOpenTab,
            createTab,
            createCustomTab,
            switchTab,
            closeTab,
            performCloseTab,
            markTabDirty,
            markCurrentTabClean,
            cycleTab,
            bindTabKeyboardShortcuts,
            scrollActiveTabIntoView,
            renderTabs,
            debouncedRenderTabs,
            showTabContextMenu,
            closeOtherTabs,
            closeAllTabs,
            closeTabsToSide
        };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.tabs = window.AhmadIDEModules.tabs || {};
        window.AhmadIDEModules.tabs.createTabManager = createTabManager;
    }
})();
