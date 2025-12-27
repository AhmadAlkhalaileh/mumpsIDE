(() => {
    // jQuery helper
    const $ = window.$ || window.jQuery || null;
    const logger = (typeof window !== 'undefined' && window.MIDELogger) ? window.MIDELogger : {
        debug: () => { },
        info: () => { },
        warn: () => { },
        error: () => { },
        isEnabled: () => false
    };

    // Debug logging utility for tracing debugger operations
    const dbgLog = (typeof window !== 'undefined' && window.MIDEDebugLog) ? window.MIDEDebugLog : (...args) => {
        // Fallback if debug-log.js not loaded
        if (typeof process !== 'undefined' && process.env?.MIDE_DEBUG_TRACE === '1') {
        }
    };

    if (typeof window !== 'undefined') {
        window.onerror = (msg, src, line, col, err) => {
            logger.error('GLOBAL_WINDOW_ERROR', { message: msg, src, line, col, stack: err?.stack });
        };
        window.onunhandledrejection = (event) => {
            logger.error('GLOBAL_UNHANDLED_REJECTION', { reason: event?.reason?.message || event?.reason, stack: event?.reason?.stack });
        };
    }

    // Theme presets (IDE shell + Monaco code themes)
    const createThemePresets = window.AhmadIDEModules?.renderer?.themes?.createThemePresets;
    const themePresets = createThemePresets();
    const ideThemes = themePresets.ideThemes;
    const defaultIdeTheme = themePresets.defaultIdeTheme;
    const defaultCodeTheme = themePresets.defaultCodeTheme;
    const codeThemes = themePresets.codeThemes;
    let currentCodeTheme = themePresets.currentCodeTheme;
    const collapsedTreeNodes = new Set();
    let activeEditor = null;
    // Terminal configuration (TODO: expose via Settings UI)
    const terminalConfig = {
        shellPath: null, // Default: system shell
        startDir: null, // Default: project root
        // Match JetBrains Terminal: terminal input wins while focused (except a few toolwindow shortcuts).
        overrideIdeShortcuts: true,
        escapeToEditor: true // If true and shortcuts are not overridden, Esc returns focus to editor
    };
    let envInfoCache = null;
    const MUMPSValidatorCtor = (typeof window !== 'undefined' && window.MUMPSValidator)
        ? window.MUMPSValidator
        : (typeof MUMPSValidator !== 'undefined' ? MUMPSValidator : null);
    const MUMPSLinterCtor = (typeof window !== 'undefined' && window.MUMPSLinter)
        ? window.MUMPSLinter
        : (typeof MUMPSLinter !== 'undefined' ? MUMPSLinter : null);

    const mumpsValidator = MUMPSValidatorCtor ? new MUMPSValidatorCtor() : null;
    const mumpsLinter = MUMPSLinterCtor ? new MUMPSLinterCtor() : null;
    const MUMPSLexerClass = typeof MUMPSLexer !== 'undefined' ? MUMPSLexer : null;
    const MUMPSParserClass = typeof MUMPSParser !== 'undefined' ? MUMPSParser : null;
    const registeredShortcuts = [];
    let shortcutPrefsApi = null;
    const expandedArrayKeys = new Set(); // track expanded arrays in Locals panel
    let activeRoutineName = null; // for breakpoint grouping and labels
    let routineState = null; // shared routine state for search/navigation helpers
    let routineStateRef = null; // shared ref for project search
    let dbgStateRef = null; // shared debug state reference for helpers outside init scope
    let currentDebugSession = null; // current debug session (shared with debug module)
    let debugManager = null; // debug module instance (src/editor/debug/renderer-debug.js)
    let problemsManager = null; // problems UI module instance (src/editor/problems/renderer-problems.js)
    let diagnosticsManager = null; // lint/diagnostics module instance (src/editor/diagnostics/renderer-diagnostics.js)
    let mumpsMonacoManager = null; // monaco+mumps bootstrap module instance (src/editor/mumps/renderer-mumps-monaco.js)
    let gitRepoManager = null; // git repo detection/state module instance (src/editor/git/renderer-git-repo-manager.js)
    let gitToolWindowManager = null; // git tool window module instance (src/editor/git/renderer-git-toolwindow.js)
    let gitSettingsManager = null; // git settings panel module instance (src/editor/git/renderer-git-settings.js)
    let connectionsManager = null; // connections panel module instance (src/editor/connections/renderer-connections.js)
    let extensionsManager = null; // extensions view module instance (src/editor/extensions/renderer-extensions.js)
    let projectCreateManager = null; // new project creation module instance (src/editor/project/renderer-project-create.js)
    let projectTreeManager = null; // project tree module instance (src/editor/project/renderer-project-tree.js)
    let projectContextMenuManager = null; // project tree context menu module instance (src/editor/project/renderer-project-context-menu.js)
    let editorContextMenuManager = null; // editor context menu module instance (src/editor/ui/renderer-editor-context-menu.js)
    let settingsPanelManager = null; // settings panel UI module instance (src/editor/ui/renderer-settings-panel.js)
    let ctrlHoverManager = null; // ctrl+hover and gutter module instance (src/editor/ui/renderer-ctrl-hover.js)
    let routinesManager = null; // routines module instance (src/editor/routines/renderer-routines.js)
    const mumpsLocalTagResolver = (() => {
        try {
            const create = window.AhmadIDEModules?.mumps?.createMumpsLocalTagResolver;
            return typeof create === 'function' ? create() : null;
        } catch (_) {
            return null;
        }
    })();

    // Declaration navigation moved to src/renderer/editor/declarationNavigation.js
    const declarationNavigation = window.AhmadIDEModules?.renderer?.editor?.declarationNavigation;
    if (!declarationNavigation) {
        logger.error('DECLARATION_NAV_MODULE_MISSING', { path: './src/renderer/editor/declarationNavigation.js' });
        throw new Error('Declaration navigation module missing: ./src/renderer/editor/declarationNavigation.js');
    }
    const parseRoutineReferenceAtPosition = declarationNavigation.parseRoutineReferenceAtPosition;
    // Phase 3A: menus are now rendered by the unified ui.menu controller + MenuBar,
    // with menu definitions sourced from src/app/registries/menuRegistry.js.
    const clickEl = (id) => {
        const elId = String(id || '').trim();
        if (!elId) return;
        try {
            if ($) {
                const $el = $('#' + elId);
                if ($el && $el.length) $el.trigger('click');
                return;
            }
        } catch (_) { }
        document.getElementById(elId)?.click?.();
    };

    const notImplemented = (label) => {
        const msg = `${label || 'This action'} is not implemented yet.`;
        try {
            if (typeof showToast === 'function') {
                showToast('info', 'Not implemented', msg);
                return;
            }
        } catch (_) { }
        console.info('[Not implemented]', msg);
    };

    async function runMenuAction(action, ctx = {}) {
        const editor = ctx?.editor || activeEditor || null;
        const terminalState = ctx?.terminalState || globalTerminalState || null;
        const dialogRegistry = window.AhmadIDEModules?.app?.dialogRegistry || null;

        logger.info('MENU_ACTION', { action });
        switch (action) {
            case 'save':
                clickEl('saveRoutineBtn');
                return;
            case 'save-all':
                await saveAllOpenTabs({ terminalState });
                return;
            case 'undo':
                editor?.trigger('keyboard', 'undo', null);
                return;
            case 'redo':
                editor?.trigger('keyboard', 'redo', null);
                return;
            case 'cut':
                document.execCommand('cut');
                return;
            case 'copy':
                document.execCommand('copy');
                return;
            case 'paste':
                document.execCommand('paste');
                return;
            case 'find':
                if (dialogRegistry?.open?.('find')) return;
                editor?.trigger('keyboard', 'actions.find', null);
                return;
            case 'replace':
                if (dialogRegistry?.open?.('replace')) return;
                editor?.trigger('keyboard', 'editor.action.startFindReplaceAction', null);
                return;
            case 'find-in-folder':
                openFindReplaceDialog('find', getSelectedText());
                return;
            case 'replace-in-folder':
                openFindReplaceDialog('replace', getSelectedText());
                return;
            case 'search-everywhere':
                if (dialogRegistry?.open?.('search-everywhere')) return;
                openSearchEverywhere('');
                return;
            case 'comment':
                editor?.trigger('keyboard', 'editor.action.commentLine', null);
                return;
            case 'duplicate-line':
                duplicateLine(editor);
                return;
            case 'select-all':
                try {
                    const actionObj = editor?.getAction?.('editor.action.selectAll');
                    if (actionObj?.isSupported?.()) {
                        await actionObj.run();
                        return;
                    }
                } catch (_) { }
                try { document.execCommand('selectAll'); } catch (_) { }
                return;
            case 'goto-line':
                editor?.trigger('keyboard', 'editor.action.gotoLine', null);
                return;
            case 'goto-file':
                if (dialogRegistry?.open?.('goto-file')) return;
                openSearchEverywhere('');
                return;
            case 'goto-declaration':
                try { await goToDeclaration(editor, null, { silentIfMissing: false }); } catch (_) { }
                return;
            case 'show-goto-map': {
                if (typeof window.showMumpsGotoMap === 'function') {
                    try { window.showMumpsGotoMap(); } catch (_) { }
                    return;
                }

                // Fallback: attempt late init if the integration module loaded after the editor.
                const initMumpsLinterEnhancements = window.AhmadIDEModules?.mumps?.initMumpsLinterEnhancements;
                if (typeof initMumpsLinterEnhancements === 'function' && typeof window.monaco !== 'undefined') {
                    try {
                        initMumpsLinterEnhancements({
                            monacoRef: window.monaco,
                            getActiveEditor: () => editor || activeEditor || null,
                            showToast: (typeof showToast === 'function') ? showToast : (() => { })
                        });
                    } catch (_) { }
                }

                if (typeof window.showMumpsGotoMap === 'function') {
                    try { window.showMumpsGotoMap(); } catch (_) { }
                    return;
                }

                notImplemented('GOTO Flow Map');
                return;
            }
            case 'mumps:generate-tag-header': {
                const ed = editor || activeEditor || null;
                if (!ed) {
                    try { showToast?.('info', 'Tag Header', 'No active editor'); } catch (_) { }
                    return;
                }

                try {
                    const generator = window.mumpsTagHeaderGenerator;
                    if (generator?.generateTagHeader) {
                        generator.generateTagHeader(ed);
                        return;
                    }

                    const Gen = window.AhmadIDEModules?.mumps?.MumpsTagHeaderGenerator || window.MumpsTagHeaderGenerator;
                    if (!Gen) {
                        notImplemented('Generate Tag Header');
                        return;
                    }

                    const settingsService = window.AhmadIDEModules?.services?.settingsService;
                    const showToastFn = (typeof showToast === 'function') ? showToast : (() => { });
                    new Gen({ showToast: showToastFn, settingsService }).generateTagHeader(ed);
                } catch (err) {
                    try { showToast?.('error', 'Tag Header', err?.message || 'Failed to generate tag header'); } catch (_) { }
                }
                return;
            }
            case 'mumps:find-global-references':
            case 'mumps:show-global-impact':
                {
                    const api = window.AhmadIDEModules?.globalImpact?.manager || null;
                    if (api?.openFromEditor) {
                        try { await api.openFromEditor(editor); } catch (_) { }
                        return;
                    }
                    if (api?.openPanel) {
                        try { await api.openPanel({ focusSearch: true }); } catch (_) { }
                        return;
                    }
                    try { showToast?.('error', 'Global Impact', 'Global Impact is not available'); } catch (_) { }
                }
                return;
            case 'expand-selection':
                editor?.trigger('keyboard', 'editor.action.smartSelect.expand', null);
                return;
            case 'shrink-selection':
                editor?.trigger('keyboard', 'editor.action.smartSelect.shrink', null);
                return;
            case 'tab-next':
                cycleTab(1);
                return;
            case 'tab-prev':
                cycleTab(-1);
                return;
            case 'tab:close':
                try { if (activeTabId) tabManager?.closeTab?.(activeTabId); } catch (_) { }
                return;
            case 'tab:close-all':
                try { tabManager?.closeAllTabs?.(); } catch (_) { }
                return;
            case 'tab:close-others':
                try { if (activeTabId) tabManager?.closeOtherTabs?.(activeTabId); } catch (_) { }
                return;
            case 'tab:close-left':
                try { if (activeTabId) tabManager?.closeTabsToSide?.(activeTabId, 'left'); } catch (_) { }
                return;
            case 'tab:close-right':
                try { if (activeTabId) tabManager?.closeTabsToSide?.(activeTabId, 'right'); } catch (_) { }
                return;
            case 'reformat':
                {
                    const selections = editor?.getSelections?.() || [];
                    const hasSelection = selections.some((s) => {
                        if (!s) return false;
                        return s.startLineNumber !== s.endLineNumber || s.startColumn !== s.endColumn;
                    });

                    const preferredActionId = hasSelection ? 'editor.action.formatSelection' : 'editor.action.formatDocument';
                    const fallbackActionId = hasSelection ? 'editor.action.formatDocument' : null;

                    try {
                        const actionObj = editor?.getAction?.(preferredActionId);
                        if (actionObj?.isSupported?.()) {
                            await actionObj.run();
                            return;
                        }
                    } catch (_) { }

                    try {
                        editor?.trigger('keyboard', preferredActionId, null);
                        return;
                    } catch (_) { }

                    if (fallbackActionId) {
                        try {
                            const actionObj = editor?.getAction?.(fallbackActionId);
                            if (actionObj?.isSupported?.()) {
                                await actionObj.run();
                                return;
                            }
                        } catch (_) { }
                        try {
                            editor?.trigger('keyboard', fallbackActionId, null);
                            return;
                        } catch (_) { }
                    }

                    try { showToast?.('info', 'Format Code', 'Formatting is not available for this file'); } catch (_) { }
                }
                return;
            case 'rename':
                editor?.trigger('keyboard', 'editor.action.rename', null);
                return;
            case 'lint':
                clickEl('lintBtn');
                return;
            case 'tool-project':
                toggleToolWindowPanel('projectPanel', 'left');
                return;
            case 'tool-structure':
                toggleToolWindowPanel('structurePanel', 'left');
                return;
            case 'tool-commit':
                openCommitToolWindow({ source: 'menubar' });
                return;
            case 'tool-terminal':
                toggleTerminal();
                return;
            case 'tool-run':
                toggleToolWindowPanel('terminalPanel', 'bottom');
                return;
            case 'tool-debug':
                toggleToolWindowPanel('debugPanel', 'bottom');
                return;
            case 'tool-todo':
                toggleToolWindowPanel('problemsPanel', 'bottom');
                return;
            case 'tool-services':
                toggleToolWindowPanel('servicesPanel', 'bottom');
                return;
            case 'tool-git':
                openGitToolWindow({ source: 'menubar' });
                return;
            case 'tool-patch-tracking':
                toggleToolWindowPanel('patchTrackingPanel', 'bottom');
                return;
            case 'vcs:commit':
                openCommitToolWindow({ source: 'menubar:vcs' });
                return;
            case 'vcs:history':
                openGitToolWindow({ source: 'menubar:vcs' });
                clickEl('gitLogBtn');
                return;
            case 'vcs:push':
                openGitToolWindow({ source: 'menubar:vcs' });
                await runGitQuickCmd('git push', { toastLabel: 'Git Push' });
                return;
            case 'vcs:pull':
                openGitToolWindow({ source: 'menubar:vcs' });
                await runGitQuickCmd('git pull', { toastLabel: 'Git Pull' });
                return;
            case 'vcs:open-git':
                openGitToolWindow({ source: 'menubar:vcs' });
                return;
            case 'menu-self-test':
                if (typeof window.runMenuAction === 'function') {
                    try { await window.runMenuAction('menu-self-test'); return; } catch (_) { }
                }
                notImplemented(action);
                return;
            case 'toggle-sidebar':
                toggleSidebar();
                return;
            case 'toggle-terminal':
                toggleTerminal();
                return;
            case 'terminal-new-tab':
                if (terminalToolApi?.openTerminalToolWindow) {
                    terminalToolApi.openTerminalToolWindow({ source: 'shortcut:terminal-new-tab' });
                    await terminalToolApi.newTab?.();
                    return;
                }
                // Fallback: open legacy Run Output panel and add a new tab.
                if (terminalState && !terminalState.tabs.length) await addTerminalTab(terminalState, true);
                await addTerminalTab(terminalState);
                toggleToolWindowPanel('terminalPanel', 'bottom');
                setTimeout(() => {
                    if (terminalState) refreshTerminalLayout(terminalState);
                    focusTerminal();
                }, 50);
                return;
            case 'terminal':
                if (terminalToolApi?.openTerminalToolWindow) {
                    terminalToolApi.openTerminalToolWindow({ source: 'menu:terminal' });
                    return;
                }
                // Fallback: open legacy Run Output panel.
                if (terminalState && !terminalState.tabs.length) await addTerminalTab(terminalState, true);
                toggleToolWindowPanel('terminalPanel', 'bottom');
                setTimeout(() => {
                    if (terminalState) refreshTerminalLayout(terminalState);
                    focusTerminal();
                }, 50);
                return;
            case 'connections':
                {
                    const btn = document.getElementById('toggleConnections');
                    if (btn) {
                        btn.dispatchEvent(new MouseEvent('click', { bubbles: true, cancelable: true, shiftKey: true }));
                        return;
                    }
                }
                clickEl('toggleConnections');
                return;
            case 'extensions':
                toggleToolWindowPanel('extensionsPanel', 'bottom');
                return;
            case 'toggle-devtools':
                try { await window.ahmadIDE?.toggleDevTools?.(); } catch (_) { }
                return;
            case 'run':
                clickEl('runBtn');
                return;
            case 'debug':
                clickEl('debugStartBtn');
                return;
            case 'stop-debug':
                clickEl('dbgStopBtn');
                return;
            case 'shortcuts':
                window.AhmadIDEModules?.app?.dialogRegistry?.open('shortcuts');
                return;
            case 'settings':
                window.AhmadIDEModules?.app?.dialogRegistry?.open('settings') || openSettingsPanel();
                return;
            case 'new-project':
                window.AhmadIDEModules?.app?.dialogRegistry?.open('new-project') || openNewProjectPanel();
                return;
            case 'open-project':
                window.AhmadIDEModules?.app?.dialogRegistry?.open('open-project') || await openProjectDialog();
                return;
            case 'close-project':
                closeCurrentProject();
                return;
            case 'new-file':
                await createNewFile();
                return;
            case 'git':
                openGitToolWindow();
                return;
            case 'git-status':
                openGitToolWindow();
                clickEl('gitStatusBtn');
                return;
            case 'git-diff':
                openGitToolWindow();
                clickEl('gitDiffBtn');
                return;
            case 'git-history':
                openGitToolWindow();
                clickEl('gitLogBtn');
                return;
            case 'about':
                window.AhmadIDEModules?.app?.dialogRegistry?.open('about') || notImplemented(action);
                return;

            case 'appearance':
            case 'recent-files':
            case 'refactor-extract':
            case 'window-store':
            case 'docs':
                notImplemented(action);
                return;
            case 'exit-app':
                try {
                    if (window.ahmadIDE?.exitApp) {
                        await window.ahmadIDE.exitApp();
                    } else {
                        window.close();
                    }
                } catch (err) {
                    showToast('error', 'Exit', err?.message || 'Exit failed');
                }
                return;
            default:
                // Delegated to legacy/global handlers?
                if (window.AhmadIDEModules?.actions?.runAction) {
                    if (await window.AhmadIDEModules.actions.runAction(action, ctx)) return;
                }

                // Delegate to global menuActions.js (which renderer.js shadows locally)
                if (typeof window.runMenuAction === 'function' && window.runMenuAction !== runMenuAction) {
                    try {
                        await window.runMenuAction(action, ctx);
                        return;
                    } catch (e) {
                        console.error('[Delegated Action Failed]', action, e);
                    }
                }

                notImplemented(action);
        }
    }

    async function saveAllOpenTabs({ terminalState } = {}) {
        return saveAllOpenTabsImpl({ terminalState });
    }
    const findReplaceState = {
        mode: 'find',
        options: { matchCase: false, wholeWords: false, regex: false },
        token: 0,
        scopeFolder: null
    };
    const searchEverywhereState = {
        index: [],
        selectedIndex: 0,
        open: false
    };
    let routinesCache = [];
    let shortcutDefaults = {};
    let routineFilterTerm = '';
    let activeDebugTab = 'tab-breakpoints';
    const maxLintTextLength = 200000;  // Skip linting for files > 200KB
    const maxProblemItems = 100;  // Limit problems panel for performance
    const RE_DQUOTE = /\"/g;
    const RE_PAREN_OPEN = /\(/g;
    const RE_PAREN_CLOSE = /\)/g;
    const RE_LINE_START = /^[^A-Za-z%;\s]/;
    const RE_SUSPICIOUS = /[{}\[\]\\]/;
    let lastValidatedVersionId = null;
    let lintSkipNotified = false;
    if (typeof window !== 'undefined') {
        window._mumpsLinter = mumpsLinter;
        window._mumpsValidator = mumpsValidator;
        window._mumpsLexerClass = MUMPSLexerClass;
        window._mumpsParserClass = MUMPSParserClass;
    }

    function applyIdeTheme(key) {
        const selected = ideThemes[key] ? key : defaultIdeTheme;
        const payload = ideThemes[selected];
        Object.entries(payload.vars || {}).forEach(([cssVar, val]) => {
            // Fonts are controlled by Settings (Phase 3).
            if (cssVar.startsWith('--font-') || cssVar.startsWith('--font-size-') || cssVar === '--line-height-code' || cssVar === '--font-weight-code' || cssVar === '--font-ligatures-code') {
                return;
            }
            document.documentElement.style.setProperty(cssVar, val);
        });
        document.body.dataset.ideTheme = selected;
        try {
            localStorage.setItem('ahmadIDE:theme:ide', selected);
        } catch (e) {
            // ignore storage errors
        }
        return selected;
    }

    const preferredIdeTheme = (() => {
        try {
            return localStorage.getItem('ahmadIDE:theme:ide') || defaultIdeTheme;
        } catch (e) {
            return defaultIdeTheme;
        }
    })();
    applyIdeTheme(preferredIdeTheme);

    function applyCodeTheme(name) {
        const theme = (name && typeof name === 'string') ? name : defaultCodeTheme;
        const finalName = codeThemes.includes(theme) ? theme : defaultCodeTheme;
        currentCodeTheme = finalName;
        if (typeof monaco !== 'undefined' && monaco.editor) {
            monaco.editor.setTheme(finalName);
        }
        try {
            localStorage.setItem('ahmadIDE:theme:code', finalName);
        } catch (e) {
            // ignore storage errors
        }
        return finalName;
    }

    function bindThemeSelectors(editor) {
        const ideSelect = $ ? $('#ideThemeSelect') : null;
        const codeSelect = $ ? $('#codeThemeSelect') : null;
        const safeIde = ideThemes[preferredIdeTheme] ? preferredIdeTheme : defaultIdeTheme;
        const safeCode = codeThemes.includes(currentCodeTheme) ? currentCodeTheme : defaultCodeTheme;
        if (ideSelect && ideSelect.length) {
            ideSelect.val(safeIde || defaultIdeTheme);
            ideSelect.on('change', () => applyIdeTheme(ideSelect.val()));
        } else {
            const el = document.getElementById('ideThemeSelect');
            if (el) {
                el.value = safeIde || defaultIdeTheme;
                el.addEventListener('change', () => applyIdeTheme(el.value));
            }
        }

        if (codeSelect && codeSelect.length) {
            codeSelect.val(safeCode || defaultCodeTheme);
            codeSelect.on('change', () => applyCodeTheme(codeSelect.val()));
        } else {
            const el = document.getElementById('codeThemeSelect');
            if (el) {
                el.value = safeCode || defaultCodeTheme;
                el.addEventListener('change', () => applyCodeTheme(el.value));
            }
        }

        applyIdeTheme(safeIde);
        applyCodeTheme(safeCode);
    }

    function bindSettingsPanelThemes() {
        const ideSel = document.getElementById('settingsIdeTheme');
        const codeSel = document.getElementById('settingsCodeTheme');
        if (!ideSel && !codeSel) {
            // Settings panel is lazy-mounted; bind when it becomes available.
            if (!bindSettingsPanelThemes.__lazyHooked) {
                const fr = window.AhmadIDEModules?.app?.featureRegistry;
                fr?.onMounted?.('settingsPanel', () => bindSettingsPanelThemes());
                bindSettingsPanelThemes.__lazyHooked = true;
            }
            return;
        }
        if (ideSel) {
            ideSel.value = preferredIdeTheme || defaultIdeTheme;
            if (!ideSel.dataset.bound) {
                ideSel.addEventListener('change', () => applyIdeTheme(ideSel.value));
                ideSel.dataset.bound = '1';
            }
        }
        if (codeSel) {
            codeSel.value = currentCodeTheme || defaultCodeTheme;
            if (!codeSel.dataset.bound) {
                codeSel.addEventListener('change', () => applyCodeTheme(codeSel.value));
                codeSel.dataset.bound = '1';
            }
        }
    }

    function bindGitSettingsPanel() {
        return gitSettingsManager.bindGitSettingsPanel();
    }

    function openSettingsPanel() {
        if (settingsPanelManager && typeof settingsPanelManager.openSettingsPanel === 'function') {
            return settingsPanelManager.openSettingsPanel();
        }
        // Legacy fallback removed - use dialog registry
        const dialogRegistry = window.AhmadIDEModules?.app?.dialogRegistry;
        if (dialogRegistry) {
            dialogRegistry.show('settings');
        }
    }

    let currentProject = null;

    const createProjectTreeWiring = window.AhmadIDEModules?.renderer?.project?.createProjectTreeWiring;
    const projectTreeWiring = createProjectTreeWiring({
        deps: {
            $,
            showToast,
            getActiveEditor: () => activeEditor
        }
    });

    function loadProjectIntoTree(projectData) {
        currentProject = projectData;
        window.currentProject = projectData;
        try {
            const root = String(projectData?.projectPath || '').trim();
            if (gitRepoManager && typeof gitRepoManager.setProject === 'function') {
                gitRepoManager.setProject(root).catch(() => { });
            }
        } catch (_) { }
        projectTreeWiring.renderProjectIntoTree(projectData);
    }

    // Expose for dialogs/external modules
    window.loadProjectIntoTree = loadProjectIntoTree;

    // Auto-fetch routines after connection (if project is open)
    window.autoFetchRoutinesAfterConnection = async function(options = {}) {
        const { showMessage = true, discovered } = options;

        console.log('[Auto-Fetch] ========================================');
        console.log('[Auto-Fetch] Checking if routines should be auto-fetched');
        console.log('[Auto-Fetch] Current project:', currentProject?.projectPath);
        console.log('[Auto-Fetch] Discovered paths:', discovered);

        if (!currentProject || !currentProject.projectPath) {
            console.log('[Auto-Fetch] No project open, skipping auto-fetch');
            console.log('[Auto-Fetch] ========================================');
            return { ok: false, reason: 'no_project' };
        }

        if (!discovered || !discovered.ok) {
            console.warn('[Auto-Fetch] No paths discovered from vista-profile, attempting fetch anyway...');
            console.log('[Auto-Fetch] This may work if paths are configured manually');
        }

        const routinesPath = `${currentProject.projectPath}/routines`;
        console.log('[Auto-Fetch] Target routines path:', routinesPath);

        try {
            if (showMessage && typeof showToast === 'function') {
                showToast('info', 'Auto-Fetch', 'Retrieving routines from connection...');
            }

            console.log('[Auto-Fetch] Calling fetchRoutineDirectoriesToLocal...');
            const result = await window.ahmadIDE.fetchRoutineDirectoriesToLocal(routinesPath);

            console.log('[Auto-Fetch] Fetch result:', result);

            if (result.ok) {
                console.log('[Auto-Fetch] ✓ Routines fetched successfully');

                // Refresh project tree to show new routines
                console.log('[Auto-Fetch] Refreshing project tree...');
                try {
                    if (typeof window.ahmadIDE.openProject === 'function') {
                        const refreshResult = await window.ahmadIDE.openProject(currentProject.projectPath);
                        console.log('[Auto-Fetch] Project refresh result:', refreshResult);
                        if (refreshResult?.ok && refreshResult?.data) {
                            loadProjectIntoTree(refreshResult.data);
                            console.log('[Auto-Fetch] ✓ Project tree refreshed');
                        }
                    }
                } catch (refreshError) {
                    console.warn('[Auto-Fetch] Failed to refresh project tree:', refreshError);
                }

                if (showMessage && typeof showToast === 'function') {
                    showToast('success', 'Auto-Fetch', 'Routines retrieved successfully');
                }
                console.log('[Auto-Fetch] ========================================');
                return { ok: true };
            } else {
                console.error('[Auto-Fetch] ✗ Fetch failed:', result.error);
                if (showMessage && typeof showToast === 'function') {
                    showToast('error', 'Auto-Fetch', result.error || 'Failed to fetch routines');
                }
                console.log('[Auto-Fetch] ========================================');
                return { ok: false, error: result.error };
            }
        } catch (error) {
            console.error('[Auto-Fetch] Exception:', error);
            console.error('[Auto-Fetch] Stack trace:', error.stack);
            if (showMessage && typeof showToast === 'function') {
                showToast('error', 'Auto-Fetch', error.message || 'Failed to fetch routines');
            }
            console.log('[Auto-Fetch] ========================================');
            return { ok: false, error: error.message };
        }
    };

    let diffActionsApi = null;

    function toGitPathspec(inputPath) {
        return diffActionsApi.toGitPathspec(inputPath);
    }

    async function openDiffTab(opts = {}) {
        return diffActionsApi.openDiffTab(opts);
    }

    let globalTerminalState = null;
    let globalRoutineState = null;

    const createTerminalManager = window.AhmadIDEModules?.terminal?.createTerminalManager;
    if (!createTerminalManager) {
        logger.error('TERMINAL_MODULE_MISSING', { path: './src/editor/terminal/renderer-terminal.js' });
        throw new Error('Terminal module missing: ./src/editor/terminal/renderer-terminal.js');
    }

    const terminalManager = createTerminalManager({
        deps: {
            logger,
            terminalConfig,
            mode: 'run-output',
            getCurrentProject: () => currentProject,
            getEnvInfoCache: () => envInfoCache,
            getGlobalTerminalState: () => globalTerminalState,
            getActiveEditor: () => activeEditor,
            showToast
        }
    });

    const {
        getTerminalCwd,
        focusTerminal,
        isTerminalFocused,
        createTerminalState,
        getActiveTerminalTab,
        updateTerminalStatusPill,
        ensureTerminalListeners,
        renderTerminalTabs,
        activateTerminalTab,
        refreshTerminalLayout,
        addTerminalTab,
        closeTerminalTab,
        appendOutput,
        clearOutput,
        sendCtrlC,
        applyFontSettings
    } = terminalManager;

    const createDockerUi = window.AhmadIDEModules?.renderer?.docker?.createDockerUi;
    if (!createDockerUi) {
        logger.error('DOCKER_UI_MODULE_MISSING', { path: './src/renderer/docker/dockerUi.js' });
        throw new Error('Docker UI module missing: ./src/renderer/docker/dockerUi.js');
    }
    const dockerUi = createDockerUi({
        deps: {
            appendOutput,
            loadRoutineList: (...args) => loadRoutineList(...args)
        }
    });

    // New PhpStorm-like Terminal tool window (real shell via node-pty in main).
    let terminalToolApi = null;
    try {
        const createTerminalToolWindowManager = window.AhmadIDEModules?.terminalTool?.createTerminalToolWindowManager;
        if (typeof createTerminalToolWindowManager === 'function') {
            terminalToolApi = createTerminalToolWindowManager({
                deps: {
                    logger,
                    terminalConfig,
                    getCurrentProject: () => currentProject,
                    getEnvInfoCache: () => envInfoCache,
                    getActiveEditor: () => activeEditor,
                    showToast,
                    toggleToolWindowPanel
                }
            });
        } else {
            logger.warn('TERMINAL_TOOL_MODULE_MISSING', { path: './src/editor/terminal/renderer-terminal-toolwindow.js' });
        }
    } catch (err) {
        logger.error('TERMINAL_TOOL_INIT_ERROR', { message: err?.message, stack: err?.stack });
    }

    let __lastFontSettingsKey = '';
    function applyFontSettingsToRuntime(settings) {
        if (!settings) return;
        const editorFont = settings?.fonts?.editor || {};
        const terminalFont = settings?.fonts?.terminal || {};

        const editorFamily = String(editorFont.family || '').trim();
        const editorSize = Number(editorFont.sizePx ?? 13) || 13;
        const editorLineHeight = Number(editorFont.lineHeight ?? 1.55) || 1.55;
        const editorWeight = String(editorFont.weight ?? '400');
        const editorLigatures = !!editorFont.ligatures;

        const terminalFamily = String(terminalFont.family || editorFamily || '').trim();
        const terminalSize = Number(terminalFont.sizePx ?? editorSize) || editorSize;

        const key = `${editorFamily}|${editorSize}|${editorLineHeight}|${editorWeight}|${editorLigatures ? '1' : '0'}|${terminalFamily}|${terminalSize}`;
        if (key === __lastFontSettingsKey) return;
        __lastFontSettingsKey = key;

        try {
            if (activeEditor && typeof activeEditor.updateOptions === 'function') {
                activeEditor.updateOptions({
                    fontFamily: editorFamily || undefined,
                    fontSize: editorSize,
                    lineHeight: Math.max(12, Math.round(editorSize * editorLineHeight)),
                    fontLigatures: editorLigatures,
                    fontWeight: editorWeight
                });
                try {
                    if (typeof monaco !== 'undefined' && monaco?.editor?.remeasureFonts) {
                        monaco.editor.remeasureFonts();
                    }
                } catch (_) { }
            }
        } catch (_) { }

        try {
            applyFontSettings({ fontFamily: terminalFamily, fontSize: terminalSize });
        } catch (_) { }
    }

    // Apply runtime font updates on Settings → Apply/OK.
    window.addEventListener('ahmadIDE:settings-changed', (e) => {
        const settings = e?.detail;
        applyFontSettingsToRuntime(settings);
    });

    const mumpsFileIconSvg = `<svg width="16" height="16" viewBox="0 0 16 16"><defs><linearGradient id="mg" x1="0" y1="0" x2="1" y2="1"><stop offset="0%" stop-color="#f0a35c"/><stop offset="100%" stop-color="#d67f3c"/></linearGradient></defs><rect width="16" height="16" rx="3" fill="url(#mg)"/><text x="8" y="11.5" text-anchor="middle" font-size="8" font-weight="bold" fill="#19100c" font-family="monospace">MS</text></svg>`;

    // ============================================
    // -style Tab Management
    // ============================================
    // Tab state: { id, name, path, folder, content, isDirty, state, model }
    // - path: normalized unique routine key (folder/name)
    // - model: Monaco editor model for instant switching
    let openTabs = [];
    let activeTabId = null;
    let tabIdCounter = 0;
    let tabModels = new Map(); // Map of tab id -> Monaco model
    let tabShortcutsBound = false;

    // Tab icon (same as project tree)
    const tabMumpsIcon = mumpsFileIconSvg;

    const tabsState = {
        openTabs,
        tabModels,
        get activeTabId() { return activeTabId; },
        set activeTabId(v) { activeTabId = v; },
        get tabIdCounter() { return tabIdCounter; },
        set tabIdCounter(v) { tabIdCounter = v; },
        get tabShortcutsBound() { return tabShortcutsBound; },
        set tabShortcutsBound(v) { tabShortcutsBound = v; }
    };

    const createTabManager = window.AhmadIDEModules?.tabs?.createTabManager;
    if (!createTabManager) {
        logger.error('TABS_MODULE_MISSING', { path: './src/editor/tabs/renderer-tabs.js' });
        throw new Error('Tabs module missing: ./src/editor/tabs/renderer-tabs.js');
    }

    const tabManager = createTabManager({
        state: tabsState,
        deps: {
            $,
            logger,
            tabMumpsIcon,
            getMonaco: () => (typeof monaco !== 'undefined' ? monaco : null),
            getActiveEditor: () => activeEditor,
            setLastValidatedVersionIdNull: () => { lastValidatedVersionId = null; },
            getRoutineStateRef: () => routineStateRef,
            getDbgStateRef: () => dbgStateRef,
            decorateBreakpoints,
            renderBreakpoints,
            setCurrentRoutine,
            showConfirmDialog,
            showCustomPrompt,
            appendOutput,
            getGlobalTerminalState: () => globalTerminalState,
            onTabActivated: (tab) => handleEditorTabActivated(tab),
            onTabClosed: (tab) => handleEditorTabClosed(tab)
        }
    });

    const normalizeRoutineTarget = tabManager.normalizeRoutineTarget;
    const findOpenTab = tabManager.findOpenTab;
    const createTab = tabManager.createTab;
    const createCustomTab = tabManager.createCustomTab;
    const switchTab = tabManager.switchTab;
    const markTabDirty = tabManager.markTabDirty;
    const cycleTab = tabManager.cycleTab;
    const bindTabKeyboardShortcuts = tabManager.bindTabKeyboardShortcuts;
    const renderTabs = tabManager.renderTabs;

    // Expose tab manager globally for other modules (e.g., Smart Rename)
    window.tabManager = tabManager;
    window.AhmadIDEModules.tabs = window.AhmadIDEModules.tabs || {};
    window.AhmadIDEModules.tabs.getOpenTabs = () => openTabs;
    window.AhmadIDEModules.tabs.markTabDirty = markTabDirty;
    window.AhmadIDEModules.tabs.renderTabs = renderTabs;

    const createSaveAllOpenTabs = window.AhmadIDEModules?.renderer?.tabs?.createSaveAllOpenTabs;
    if (!createSaveAllOpenTabs) {
        logger.error('SAVE_ALL_TABS_MODULE_MISSING', { path: './src/renderer/tabs/saveAllOpenTabs.js' });
        throw new Error('Save All tabs module missing: ./src/renderer/tabs/saveAllOpenTabs.js');
    }
    const saveAllOpenTabsImpl = createSaveAllOpenTabs({
        deps: {
            showToast,
            appendOutput,
            getGlobalTerminalState: () => globalTerminalState,
            getOpenTabs: () => openTabs,
            getTabModels: () => tabModels,
            mumpsValidator,
            markTabDirty,
            renderTabs,
            loadRoutineList,
            getGlobalRoutineState: () => globalRoutineState,
            getActiveEditor: () => activeEditor
        }
    });

    const goToDeclaration = declarationNavigation.createGoToDeclaration({
        deps: {
            dbgLog,
            logger,
            showToast,
            findOpenTab,
            switchTab,
            createTab,
            mumpsLocalTagResolver,
            getActiveEditor: () => activeEditor,
            getDbgStateRef: () => dbgStateRef,
            getActiveRoutine: () => getActiveRoutine()
        }
    });

    function getActiveTab() {
        try {
            return openTabs.find(t => t.id === activeTabId) || null;
        } catch (_) {
            return null;
        }
    }

    function isDiffTab(tab) {
        return String(tab?.kind || '') === 'diff';
    }

    function canSaveActiveTab() {
        const tab = getActiveTab();
        if (!tab) return true;
        return !isDiffTab(tab);
    }

    // Diff toolbar in the main editor action bar (shown only for diff tabs).
    // Split diff viewer overlay (replaces plain-text diff tabs rendering).
    const createDiffTabUi = window.AhmadIDEModules?.features?.git?.diffTabUi?.createDiffTabUi;
    const {
        computeDiffAnchors,
        syncDiffToolbar,
        syncDiffTabViewer,
        handleEditorTabActivated,
        handleEditorTabClosed
    } = createDiffTabUi({
        getActiveTab,
        isDiffTab,
        openDiffTab,
        getActiveEditor: () => activeEditor
    });

    const createDiffActions = window.AhmadIDEModules?.renderer?.git?.createDiffActions;
    diffActionsApi = createDiffActions({
        deps: {
            getGitRepoManager: () => gitRepoManager,
            getCurrentProject: () => currentProject,
            showToast,
            normalizeGitError: (...args) => normalizeGitError(...args),
            createCustomTab,
            computeDiffAnchors,
            syncDiffToolbar,
            syncDiffTabViewer
        }
    });

    // Breakpoints / Debugger moved to src/editor/debug/renderer-debug.js
    const createDebugManager = window.AhmadIDEModules?.debug?.createDebugManager;
    const createDebugStub = () => ({
        parseBpKey: (_key) => ({ routine: '', line: null }),
        getActiveRoutine: () => '',
        toggleBreakpoint: () => { },
        renderBreakpoints: () => { },
        decorateBreakpoints: () => { },
        renderLocals: () => { },
        renderStack: () => { },
        renderDebugConsole: () => { },
        updateDebugButtonState: () => { },
        resetDebugUI: () => { },
        setDebugButtons: () => { },
        scheduleDebugUiBindings: () => { },
        registerMumpsHover: () => { },
        startDebugSession: async () => ({ ok: false, error: 'Debug unavailable' }),
        debugContinue: async () => ({ ok: false, error: 'Debug unavailable' }),
        debugStop: async () => ({ ok: false, error: 'Debug unavailable' })
    });

    const currentDebugSessionRef = {
        get value() { return currentDebugSession; },
        set value(v) { currentDebugSession = v; }
    };

    if (!createDebugManager) {
        logger.error('DEBUG_MODULE_MISSING', { path: './src/editor/debug/renderer-debug.js' });
        debugManager = createDebugStub();
    } else {
        try {
            debugManager = createDebugManager({
                state: { currentDebugSessionRef },
                deps: {
                    logger,
                    dbgLog,
                    showToast,
                    highlightLine,
                    ensureBottomPanel,
                    getToolWindowState: () => toolWindowState,
                    getMonaco: () => (typeof monaco !== 'undefined' ? monaco : null),
                    getActiveEditor: () => activeEditor,
                    getActiveRoutineName: () => activeRoutineName,
                    getOpenTabs: () => openTabs,
                    getActiveTabId: () => activeTabId,
                    getDbgStateRef: () => dbgStateRef,
                    getRoutineStateRef: () => routineStateRef,
                    getRoutinesCache: () => routinesCache,
                    normalizeRoutineTarget,
                    findOpenTab,
                    createTab,
                    switchTab,
                    loadRoutineByName,
                    getGlobalTerminalState: () => globalTerminalState,
                    appendOutput,
                    clearOutput,
                    ensureTerminalListeners,
                    getTerminalCwd,
                    terminalConfig
                }
            });
        } catch (err) {
            logger.error('DEBUG_MODULE_INIT_ERROR', { message: err?.message, stack: err?.stack });
            debugManager = createDebugStub();
        }
    }

    // Time‑Travel Debugger (Timeline tab) wiring (safe/no-op when disabled).
    try { debugManager?.wireDebugTimelineEvents?.(); } catch (_) { }
    try {
        const createDebugTimelineUiManager = window.AhmadIDEModules?.debugTimeline?.createDebugTimelineUiManager;
        if (typeof createDebugTimelineUiManager === 'function') {
            const timelineUi = createDebugTimelineUiManager({ deps: { logger } });
            timelineUi?.wire?.();
            window.AhmadIDEModules.debugTimeline = window.AhmadIDEModules.debugTimeline || {};
            window.AhmadIDEModules.debugTimeline.manager = timelineUi;
        }
    } catch (err) {
        logger.warn('DEBUG_TIMELINE_UI_INIT_FAIL', { message: err?.message, stack: err?.stack });
    }

    // Problems UI moved to src/editor/problems/renderer-problems.js
    const createProblemsManager = window.AhmadIDEModules?.problems?.createProblemsManager;
    if (!createProblemsManager) {
        logger.error('PROBLEMS_MODULE_MISSING', { path: './src/editor/problems/renderer-problems.js' });
        throw new Error('Problems module missing: ./src/editor/problems/renderer-problems.js');
    }

    // Diagnostics (lint/markers) moved to src/editor/diagnostics/renderer-diagnostics.js
    const createDiagnosticsManager = window.AhmadIDEModules?.diagnostics?.createDiagnosticsManager;
    if (!createDiagnosticsManager) {
        logger.error('DIAGNOSTICS_MODULE_MISSING', { path: './src/editor/diagnostics/renderer-diagnostics.js' });
        throw new Error('Diagnostics module missing: ./src/editor/diagnostics/renderer-diagnostics.js');
    }

    const lastValidatedVersionIdRef = {
        get value() { return lastValidatedVersionId; },
        set value(v) { lastValidatedVersionId = v; }
    };

    const lintSkipNotifiedRef = {
        get value() { return lintSkipNotified; },
        set value(v) { lintSkipNotified = v; }
    };

    diagnosticsManager = createDiagnosticsManager({
        state: {
            maxLintTextLength,
            maxProblemItems,
            lastValidatedVersionIdRef,
            lintSkipNotifiedRef,
            regex: {
                RE_DQUOTE,
                RE_PAREN_OPEN,
                RE_PAREN_CLOSE,
                RE_LINE_START,
                RE_SUSPICIOUS
            }
        },
        deps: {
            showToast,
            renderProblems: (...args) => renderProblems(...args),
            getMonaco: () => (typeof monaco !== 'undefined' ? monaco : null),
            mumpsLinter,
            MUMPSLexerClass,
            MUMPSParserClass
        }
    });

    problemsManager = createProblemsManager({
        state: { maxProblemItems },
        deps: {
            revealLine,
            normalizeSeverity: (...args) => diagnosticsManager.normalizeSeverity(...args),
            setActiveDebugTab,
            getActiveDebugTab: () => activeDebugTab
        }
    });

    // MUMPS Monaco bootstrap moved to src/editor/mumps/renderer-mumps-monaco.js
    const createMumpsMonacoManager = window.AhmadIDEModules?.mumpsMonaco?.createMumpsMonacoManager;
    if (!createMumpsMonacoManager) {
        logger.error('MUMPS_MONACO_MODULE_MISSING', { path: './src/editor/mumps/renderer-mumps-monaco.js' });
        throw new Error('MUMPS Monaco module missing: ./src/editor/mumps/renderer-mumps-monaco.js');
    }
    mumpsMonacoManager = createMumpsMonacoManager({
        deps: {
            $,
            getMonaco: () => (typeof monaco !== 'undefined' ? monaco : null)
        }
    });

    // Git repo detection/state moved to src/editor/git/renderer-git-repo-manager.js
    const createGitRepoManager = window.AhmadIDEModules?.git?.createGitRepoManager;
    if (!createGitRepoManager) {
        logger.error('GIT_REPO_MANAGER_MODULE_MISSING', { path: './src/editor/git/renderer-git-repo-manager.js' });
        throw new Error('Git Repo Manager module missing: ./src/editor/git/renderer-git-repo-manager.js');
    }
    gitRepoManager = createGitRepoManager({
        deps: {
            showToast,
            logger,
            getCurrentProject: () => currentProject,
            fs: null,
            path: null
        }
    });
    try {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.git = window.AhmadIDEModules.git || {};
        window.AhmadIDEModules.git.repoManager = gitRepoManager;
        window.AhmadIDE = window.AhmadIDE || {};
        window.AhmadIDE.gitRepoManager = gitRepoManager;
    } catch (_) { }

    // Git tool window moved to src/editor/git/renderer-git-toolwindow.js
    const createGitToolWindowManager = window.AhmadIDEModules?.git?.createGitToolWindowManager;
    if (!createGitToolWindowManager) {
        logger.error('GIT_TOOL_WINDOW_MODULE_MISSING', { path: './src/editor/git/renderer-git-toolwindow.js' });
        throw new Error('Git Tool Window module missing: ./src/editor/git/renderer-git-toolwindow.js');
    }
    gitToolWindowManager = createGitToolWindowManager({
        deps: {
            showToast,
            normalizeGitError: (...args) => normalizeGitError(...args),
            toggleToolWindowPanel,
            runGit: (cmd, opts) => gitRepoManager?.runGit
                ? gitRepoManager.runGit(cmd, opts)
                : window.ahmadIDE.git(cmd),
            gitActions: gitRepoManager?.actions,
            getGitRepoState: () => (gitRepoManager?.getState ? gitRepoManager.getState() : null),
            subscribeGitRepoState: (fn) => (gitRepoManager?.subscribe ? gitRepoManager.subscribe(fn) : (() => { })),
            openDiffTab
        }
    });

    // Global Impact (Globals Where-Used)
    const createGlobalImpactManager = window.AhmadIDEModules?.globalImpact?.createGlobalImpactManager;
    if (typeof createGlobalImpactManager === 'function') {
        try {
            const globalImpactManager = createGlobalImpactManager({
                deps: {
                    logger,
                    showToast,
                    ensureBottomPanel,
                    toggleToolWindowPanel,
                    getMonaco: () => (typeof monaco !== 'undefined' ? monaco : null),
                    getActiveEditor: () => activeEditor,
                    getProjectRoot: () => String(currentProject?.projectPath || '').trim(),
                    openRoutine: async (routinePath) => {
                        try {
                            return await loadRoutineByName(routinePath, routineState, activeEditor, routinesCache, globalTerminalState);
                        } catch (err) {
                            return { ok: false, error: String(err?.message || err || 'Open failed') };
                        }
                    },
                    runDirectCommand: async (code, { title } = {}) => {
                        try { ensureBottomPanel('terminalPanel'); } catch (_) { }
                        try {
                            if (globalTerminalState && !globalTerminalState.tabs?.length) {
                                await addTerminalTab(globalTerminalState, true);
                            }
                        } catch (_) { }

                        try {
                            const label = String(title || '').trim();
                            if (label) appendOutput(`🔎 ${label}`, globalTerminalState);
                        } catch (_) { }

                        const res = await window.ahmadIDE.execute(String(code || ''));
                        if (res?.ok) {
                            try { appendOutput(res.output || '(no output)', globalTerminalState); } catch (_) { }
                        } else {
                            const msg = res?.error || res?.stderr || 'Execution failed';
                            try { appendOutput(`✗ Execute error: ${msg}`, globalTerminalState); } catch (_) { }
                        }
                        return res;
                    }
                }
            });
            const api = globalImpactManager.wireGlobalImpactPanel();
            window.AhmadIDEModules = window.AhmadIDEModules || {};
            window.AhmadIDEModules.globalImpact = window.AhmadIDEModules.globalImpact || {};
            window.AhmadIDEModules.globalImpact.manager = api;
            logger.info('GLOBAL_IMPACT_INIT', {});
        } catch (err) {
            logger.warn('GLOBAL_IMPACT_INIT_FAIL', { message: err?.message, stack: err?.stack });
        }
    }

    // Git settings panel moved to src/editor/git/renderer-git-settings.js
    const createGitSettingsManager = window.AhmadIDEModules?.git?.createGitSettingsManager;
    if (!createGitSettingsManager) {
        logger.error('GIT_SETTINGS_MODULE_MISSING', { path: './src/editor/git/renderer-git-settings.js' });
        throw new Error('Git Settings module missing: ./src/editor/git/renderer-git-settings.js');
    }
    gitSettingsManager = createGitSettingsManager({
        deps: {
            $,
            showToast,
            getCurrentProject: () => currentProject,
            runGit: (cmd, opts) => gitRepoManager?.runGit
                ? gitRepoManager.runGit(cmd, opts)
                : window.ahmadIDE.git(cmd),
            getGitRepoState: () => (gitRepoManager?.getState ? gitRepoManager.getState() : null)
        }
    });

    // Connections panel moved to src/editor/connections/renderer-connections.js
    const createConnectionsManager = window.AhmadIDEModules?.connections?.createConnectionsManager;
    if (!createConnectionsManager) {
        logger.error('CONNECTIONS_MODULE_MISSING', { path: './src/editor/connections/renderer-connections.js' });
        throw new Error('Connections module missing: ./src/editor/connections/renderer-connections.js');
    }
    connectionsManager = createConnectionsManager({
        deps: {
            appendOutput,
            loadRoutineList: (...args) => loadRoutineList(...args),
            renderDocker: (...args) => renderDocker(...args),
            setConnStatus: (...args) => setConnStatus(...args),
            getRoutineSearchValue: () => {
                try {
                    if (typeof routineSearch !== 'undefined' && routineSearch && routineSearch.value != null) {
                        return routineSearch.value || '';
                    }
                } catch (e) {
                    // ignore global lookup errors
                }
                return document.getElementById('routineSearch')?.value || '';
            }
        }
    });

    // Extensions view moved to src/editor/extensions/renderer-extensions.js
    const createExtensionsManager = window.AhmadIDEModules?.extensions?.createExtensionsManager;
    if (!createExtensionsManager) {
        logger.error('EXTENSIONS_MODULE_MISSING', { path: './src/editor/extensions/renderer-extensions.js' });
        throw new Error('Extensions module missing: ./src/editor/extensions/renderer-extensions.js');
    }
    extensionsManager = createExtensionsManager({
        deps: {
            updateDebugButtonState: (...args) => updateDebugButtonState(...args)
        }
    });

    // New Project panel moved to src/editor/project/renderer-project-create.js
    const createProjectCreateManager = window.AhmadIDEModules?.projectCreate?.createProjectCreateManager;
    if (!createProjectCreateManager) {
        logger.error('PROJECT_CREATE_MODULE_MISSING', { path: './src/editor/project/renderer-project-create.js' });
        throw new Error('Project Create module missing: ./src/editor/project/renderer-project-create.js');
    }
    projectCreateManager = createProjectCreateManager({
        deps: {
            $,
            showToast,
            loadProjectIntoTree: (...args) => loadProjectIntoTree(...args)
        }
    });

    // Add scanFontsDir to window.AhmadIDE
    window.AhmadIDE = window.AhmadIDE || {};
    Object.assign(window.AhmadIDE, {
        scanFontsDir: async () => {
            try {
                if (typeof require === 'undefined') return [];
                const fs = require('fs');
                const path = require('path');
                const fontsDir = path.join(__dirname, 'assets', 'fonts');
                if (!fs.existsSync(fontsDir)) return [];

                const files = await fs.promises.readdir(fontsDir);
                const fontFiles = files.filter(f => /\.(ttf|otf|woff|woff2)$/i.test(f));
                return fontFiles.map(f => ({
                    fileName: f,
                    url: `file://${path.join(fontsDir, f)}`
                }));
            } catch (err) {
                console.error('Failed to scan fonts dir:', err);
                return [];
            }
        }
    });

    // Project tree context menu moved to src/editor/project/renderer-project-context-menu.js
    const createProjectContextMenuManager = window.AhmadIDEModules?.projectContextMenu?.createProjectContextMenuManager;
    if (!createProjectContextMenuManager) {
        logger.error('PROJECT_CONTEXT_MENU_MODULE_MISSING', { path: './src/editor/project/renderer-project-context-menu.js' });
        throw new Error('Project Context Menu module missing: ./src/editor/project/renderer-project-context-menu.js');
    }
    projectContextMenuManager = createProjectContextMenuManager({
        deps: {
            $,
            showToast,
            showCustomPrompt,
            createTab,
            setCurrentRoutine: (...args) => setCurrentRoutine(...args),
            loadRoutineByName: (...args) => loadRoutineByName(...args),
            loadRoutineList: (...args) => loadRoutineList(...args),
            getCurrentProject: () => currentProject,
            openGitToolWindow: (...args) => openGitToolWindow(...args),
            runGitQuickCmd: (...args) => runGitQuickCmd(...args),
            toGitPathspec: (...args) => toGitPathspec(...args)
        }
    });

    // Editor context menu moved to src/editor/ui/renderer-editor-context-menu.js
    const createEditorContextMenuManager = window.AhmadIDEModules?.ui?.createEditorContextMenuManager;
    if (!createEditorContextMenuManager) {
        logger.error('EDITOR_CONTEXT_MENU_MODULE_MISSING', { path: './src/editor/ui/renderer-editor-context-menu.js' });
        throw new Error('Editor Context Menu module missing: ./src/editor/ui/renderer-editor-context-menu.js');
    }
    editorContextMenuManager = createEditorContextMenuManager({
        deps: {
            $,
            showToast,
            getActiveEditor: () => activeEditor,
            parseRoutineReferenceAtPosition,
            getActiveRoutine,
            // Do NOT pass runMenuAction for smart-rename-tag; let local handler execute
            runMenuAction: (action, ctx) => {
                if (action === 'smart-rename-tag') {
                    // Handle locally
                    if (window.smartRenameProvider) {
                        return window.smartRenameProvider.triggerSmartRenameTag();
                    } else {
                        showToast('error', 'Smart Rename', 'Provider not initialized. Reload IDE.');
                        return;
                    }
                }
                return runMenuAction(action, ctx);
            },
            runGitContextAction: (...args) => runGitContextAction(...args),
            goToDeclaration: (...args) => goToDeclaration(...args)
        }
    });

    // Settings panel UI moved to src/editor/ui/renderer-settings-panel.js
    const createSettingsPanelManager = window.AhmadIDEModules?.ui?.createSettingsPanelManager;
    if (!createSettingsPanelManager) {
        logger.error('SETTINGS_PANEL_MODULE_MISSING', { path: './src/editor/ui/renderer-settings-panel.js' });
        throw new Error('Settings Panel module missing: ./src/editor/ui/renderer-settings-panel.js');
    }
    settingsPanelManager = createSettingsPanelManager();

    // Ctrl+hover + gutter click moved to src/editor/ui/renderer-ctrl-hover.js
    const createCtrlHoverManager = window.AhmadIDEModules?.ui?.createCtrlHoverManager;
    if (!createCtrlHoverManager) {
        logger.error('CTRL_HOVER_MODULE_MISSING', { path: './src/editor/ui/renderer-ctrl-hover.js' });
        throw new Error('Ctrl Hover module missing: ./src/editor/ui/renderer-ctrl-hover.js');
    }
    ctrlHoverManager = createCtrlHoverManager({
        deps: {
            getMonaco: () => (typeof monaco !== 'undefined' ? monaco : null),
            parseRoutineReferenceAtPosition,
            resolveLocalTagLine: (model, tag) => mumpsLocalTagResolver?.getTagLine?.(model, tag) || null,
            goToDeclaration: (...args) => goToDeclaration(...args),
            toggleBreakpoint: (...args) => toggleBreakpoint(...args)
        }
    });

    // Project tree moved to src/editor/project/renderer-project-tree.js
    const createProjectTreeManager = window.AhmadIDEModules?.projectTree?.createProjectTreeManager;
    if (!createProjectTreeManager) {
        logger.error('PROJECT_TREE_MODULE_MISSING', { path: './src/editor/project/renderer-project-tree.js' });
        throw new Error('Project Tree module missing: ./src/editor/project/renderer-project-tree.js');
    }
    projectTreeManager = createProjectTreeManager({
        state: {
            collapsedTreeNodes,
            mumpsFileIconSvg
        },
        deps: {
            $,
            normalizeRoutineTarget,
            loadRoutineByName: (...args) => loadRoutineByName(...args),
            showProjectContextMenu: (...args) => showProjectContextMenu(...args),
            getCurrentProject: () => currentProject,
            getActiveEditor: () => activeEditor,
            getRoutineFilterTerm: () => routineFilterTerm,
            getRoutineState: () => routineState
        }
    });

    // Routines moved to src/editor/routines/renderer-routines.js
    const createRoutinesManager = window.AhmadIDEModules?.routines?.createRoutinesManager;
    if (!createRoutinesManager) {
        logger.error('ROUTINES_MODULE_MISSING', { path: './src/editor/routines/renderer-routines.js' });
        throw new Error('Routines module missing: ./src/editor/routines/renderer-routines.js');
    }
    routinesManager = createRoutinesManager({
        deps: {
            logger,
            showToast,
            appendOutput,
            showCustomPrompt,
            normalizeRoutineTarget,
            setActiveRoutineName: (v) => { activeRoutineName = v; },
            getRoutinesCache: () => routinesCache,
            setRoutinesCache: (v) => { routinesCache = v; },
            renderProjectTree: (...args) => renderProjectTree(...args),
            getActiveEditor: () => activeEditor,
            findOpenTab,
            switchTab,
            createTab,
            getActiveTabId: () => activeTabId,
            getOpenTabs: () => openTabs,
            markTabDirty,
            renderTabs,
            validateMumps: (...args) => validateMumps(...args),
            hasLintRules: (...args) => hasLintRules(...args),
            applyLintMarkers: (...args) => applyLintMarkers(...args),
            renderProblems: (...args) => renderProblems(...args),
            mumpsValidator,
            mumpsLinter
        }
    });

    async function showConfirmDialog(title, message, onConfirm) {
        const showConfirm = window.AhmadIDEModules?.ui?.showConfirm;
        if (showConfirm) {
            const confirmed = await showConfirm({ title, message, confirmLabel: 'Discard', cancelLabel: 'Cancel' });
            if (confirmed && onConfirm) onConfirm();
        }
    }

    async function showCustomPrompt(title, placeholder, callback) {
        const showPrompt = window.AhmadIDEModules?.ui?.showPrompt;
        if (showPrompt) {
            const value = await showPrompt({ title, placeholder });
            if (value && callback) callback(value);
        } else {
            // Fallback to built-in prompt
            const value = prompt(title, '');
            if (value && callback) callback(value);
        }
    }


    // ============================================
    // -style Project Tree Context Menu
    // ============================================

    function showProjectContextMenu(x, y, options = {}) {
        return projectContextMenuManager.showProjectContextMenu(x, y, options);
    }

    function showRoutinesFolderContextMenu(x, y, routineStateRef, editorRef) {
        return projectContextMenuManager.showRoutinesFolderContextMenu(x, y, routineStateRef, editorRef);
    }

    function getFileIcon(filename) {
        const ext = filename.split('.').pop().toLowerCase();
        const name = filename.toLowerCase();

        // Special files ( style)
        if (name === '.gitignore') return { icon: '◉', cls: 'special' };
        if (name === '.env') return { icon: '⚙', cls: 'special' };
        if (name === 'package.json') return { icon: '📦', cls: 'special' };
        if (name === 'composer.json') return { icon: '🎼', cls: 'special' };
        if (name === 'dockerfile') return { icon: '🐳', cls: 'special' };
        if (name === '.gitkeep') return { icon: '◉', cls: 'special' };
        if (name === 'readme.md' || name === 'readme.txt') return { icon: '📖', cls: 'special' };

        // File type icons ( style)
        const iconMap = {
            // Code files
            'php': { icon: 'Φ', cls: 'php' },
            'js': { icon: 'JS', cls: 'js' },
            'jsx': { icon: 'JSX', cls: 'js' },
            'ts': { icon: 'TS', cls: 'ts' },
            'tsx': { icon: 'TSX', cls: 'ts' },
            'vue': { icon: 'V', cls: 'js' },
            'py': { icon: 'Py', cls: 'py' },
            'java': { icon: 'J', cls: 'java' },
            'cpp': { icon: 'C++', cls: 'cpp' },
            'c': { icon: 'C', cls: 'c' },
            'h': { icon: 'H', cls: 'cpp' },
            'go': { icon: 'Go', cls: 'go' },
            'rs': { icon: 'Rs', cls: 'rs' },
            'rb': { icon: 'Rb', cls: 'default' },

            // Web files
            'html': { icon: 'H', cls: 'html' },
            'htm': { icon: 'H', cls: 'html' },
            'css': { icon: 'S', cls: 'css' },
            'scss': { icon: 'S', cls: 'scss' },
            'sass': { icon: 'S', cls: 'scss' },
            'less': { icon: 'L', cls: 'less' },

            // Data/Config files
            'json': { icon: '{}', cls: 'json' },
            'xml': { icon: '<>', cls: 'xml' },
            'yaml': { icon: 'Y', cls: 'yaml' },
            'yml': { icon: 'Y', cls: 'yaml' },
            'toml': { icon: 'T', cls: 'default' },
            'ini': { icon: 'I', cls: 'default' },
            'conf': { icon: 'C', cls: 'default' },
            'config': { icon: 'C', cls: 'default' },

            // MUMPS
            'm': { icon: 'M', cls: 'mumps' },
            'int': { icon: 'M', cls: 'mumps' },

            // Documentation
            'md': { icon: 'MD', cls: 'md' },
            'txt': { icon: 'T', cls: 'default' },
            'rst': { icon: 'R', cls: 'md' },

            // Images
            'png': { icon: '🖼', cls: 'special' },
            'jpg': { icon: '🖼', cls: 'special' },
            'jpeg': { icon: '🖼', cls: 'special' },
            'gif': { icon: '🖼', cls: 'special' },
            'svg': { icon: '🎨', cls: 'special' },
            'ico': { icon: '◈', cls: 'special' },

            // Other
            'sql': { icon: 'DB', cls: 'sql' },
            'sh': { icon: 'SH', cls: 'sh' },
            'bat': { icon: 'BAT', cls: 'default' },
            'ps1': { icon: 'PS', cls: 'default' },
            'lock': { icon: '🔒', cls: 'special' }
        };

        return iconMap[ext] || { icon: '📄', cls: 'default' };
    }

    // Project tree moved to src/editor/project/renderer-project-tree.js
    function renderProjectTreeLoading(message = 'Loading routines…') {
        return projectTreeManager.renderProjectTreeLoading(message);
    }

    function renderProjectTree(routines = [], routineStateRef = null, editorRef = null) {
        return projectTreeManager.renderProjectTree(routines, routineStateRef, editorRef);
    }

    function defaultShortcut(actionId) {
        return shortcutDefaults[actionId] || null;
    }

    function setCollapseStateAll(collapsed) {
        return projectTreeManager.setCollapseStateAll(collapsed);
    }

    function registerKeybinding(editor, label, actionId, handler, binding) {
        if (!editor || !handler) return;
        const prefs = loadShortcutPrefs();
        const effectiveBinding = prefs[actionId] || binding || defaultShortcut(actionId);
        if (effectiveBinding) {
            applyShortcutBinding(editor, actionId, effectiveBinding, handler);
        }
        registeredShortcuts.push({
            label,
            actionId,
            handler,
            binding: effectiveBinding,
            defaultBinding: binding || defaultShortcut(actionId)
        });
        try {
            registeredShortcuts.sort((a, b) => a.label.localeCompare(b.label));
        } catch (e) {
            // ignore sort errors
        }
    }

    // ========== MUMPS Reference Parser (shared utility) ==========
    // Parse routine/tag reference at cursor position (supports TAG^RTN, ^RTN, DO TAG)
    // MUST be defined BEFORE any code that references it (hoisting doesn't work for nested scopes)


    function bindEditorContextMenu(editor) {
        return editorContextMenuManager.bindEditorContextMenu(editor);
    }

    const createShortcutPrefs = window.AhmadIDEModules?.renderer?.shortcuts?.createShortcutPrefs;
    if (!createShortcutPrefs) {
        logger.error('SHORTCUT_PREFS_MODULE_MISSING', { path: './src/renderer/shortcuts/shortcutPrefs.js' });
        throw new Error('Shortcuts prefs module missing: ./src/renderer/shortcuts/shortcutPrefs.js');
    }
    shortcutPrefsApi = createShortcutPrefs({ state: { registeredShortcuts } });

    function loadShortcutPrefs() { return shortcutPrefsApi.loadShortcutPrefs(); }

    function persistShortcutPrefs(map) { return shortcutPrefsApi.persistShortcutPrefs(map); }

    function keyCodeFromToken(tok) { return shortcutPrefsApi.keyCodeFromToken(tok); }

    function parseShortcutString(str) { return shortcutPrefsApi.parseShortcutString(str); }

    function applyShortcutBinding(editor, actionId, binding, handler) { return shortcutPrefsApi.applyShortcutBinding(editor, actionId, binding, handler); }

    // Search (Find/Replace in Path + Search Everywhere) moved to src/editor/search/renderer-search.js
    const createSearchManager = window.AhmadIDEModules?.search?.createSearchManager;
    if (!createSearchManager) {
        logger.error('SEARCH_MODULE_MISSING', { path: './src/editor/search/renderer-search.js' });
        throw new Error('Search module missing: ./src/editor/search/renderer-search.js');
    }

    const routinesCacheRef = {
        get value() { return routinesCache; },
        set value(v) { routinesCache = v; }
    };

    const searchManager = createSearchManager({
        state: {
            findReplaceState,
            searchEverywhereState,
            routinesCacheRef
        },
        deps: {
            logger,
            showToast,
            getCurrentProject: () => currentProject,
            getActiveEditor: () => activeEditor,
            getRoutineState: () => routineState,
            getGlobalTerminalState: () => globalTerminalState,
            normalizeRoutineTarget,
            getActiveRoutine: () => getActiveRoutine(),
            loadRoutineByName,
            revealLine,
            findOpenTab,
            tabModels,
            getActiveTabId: () => activeTabId,
            renderTabs
        }
    });

    const {
        getSelectedText,
        searchDebounce,
        openFindReplaceDialog,
        closeFindReplaceDialog,
        executeFindReplacePreview,
        updateFindScopeLabels,
        toggleFindMode,
        confirmAndReplaceAll,
        openSearchEverywhereResult,
        openSearchEverywhere,
        closeSearchEverywhere,
        renderSearchEverywhereResults
    } = searchManager;

    // Expose search functions globally for menu actions
    window.searchManager = searchManager;
    window.openSearchEverywhere = openSearchEverywhere;

    function describeBinding(binding) { return shortcutPrefsApi.describeBinding(binding); }

    function renderShortcutsPanel() { return shortcutPrefsApi.renderShortcutsPanel(); }

    function openShortcutsPanel() {
        // Legacy implementation replaced by dialog registry
        const dialogRegistry = window.AhmadIDEModules?.app?.dialogRegistry;
        if (dialogRegistry) {
            dialogRegistry.show('shortcuts');
        }
    }

    function closeShortcutsPanel() {
        // No-op - handled by dialog close
    }

    function openAboutDialog() {
        const dialogRegistry = window.AhmadIDEModules?.app?.dialogRegistry;
        if (dialogRegistry) {
            dialogRegistry.show('about');
        }
    }

    function openNewProjectPanel() {
        return projectCreateManager.openNewProjectPanel();
    }

    function closeNewProjectPanel() {
        return projectCreateManager.closeNewProjectPanel();
    }

    async function createNewFile() {
        showToast('info', 'New File', 'Feature coming soon');
    }

    function closeCurrentProject() {
        showToast('info', 'Close Project', 'Feature coming soon');
    }

    async function openProjectDialog() {
        // Legacy implementation replaced by dialog registry
        logger.info('PROJECT_OPEN_DIALOG', {});
        const dialogRegistry = window.AhmadIDEModules?.app?.dialogRegistry;
        if (dialogRegistry) {
            dialogRegistry.show('open-project');
        }
    }

    const createGitQuickActions = window.AhmadIDEModules?.renderer?.git?.createGitQuickActions;
    if (!createGitQuickActions) {
        logger.error('GIT_QUICK_ACTIONS_MODULE_MISSING', { path: './src/renderer/git/gitQuickActions.js' });
        throw new Error('Git Quick Actions module missing: ./src/renderer/git/gitQuickActions.js');
    }
    const gitQuickActionsApi = createGitQuickActions({
        deps: {
            logger,
            showToast,
            gitRepoManager,
            toGitPathspec,
            openGitToolWindow: (...args) => openGitToolWindow(...args),
            openCommitToolWindow: (...args) => openCommitToolWindow(...args)
        }
    });

    const normalizeGitError = (...args) => gitQuickActionsApi.normalizeGitError(...args);

    let openGitToolWindow = (opts = {}) => {
        // Use new tool window system - Git is on the bottom bar
        toggleToolWindowPanel('gitToolPanel', 'bottom');
    };

    let openCommitToolWindow = (opts = {}) => {
        // Open commit panel in left sidebar (IntelliJ-style)
        toggleToolWindowPanel('commitPanel', 'left');
        // Focus commit message input after panel is shown
        setTimeout(() => {
            const msg = document.getElementById('commitMessageInput');
            msg?.focus();
        }, 50);
    };

    function openGitPanel() {
        openGitToolWindow();
    }

    function closeGitPanel() {
        // Switch back to terminal in bottom panel
        toggleToolWindowPanel('terminalToolPanel', 'bottom');
    }

    const gitOutputGlobal = (...args) => gitQuickActionsApi.gitOutputGlobal(...args);

    async function runGitQuickCmd(cmd, { toastLabel = 'Git', silent = false } = {}) { return gitQuickActionsApi.runGitQuickCmd(cmd, { toastLabel, silent }); }

    async function runGitContextAction(action, path) { return gitQuickActionsApi.runGitContextAction(action, path); }

    function toggleSidebar() {
        // Toggle the left tool window (Project panel) using new -style layout
        toggleToolWindowPanel('projectPanel', 'left');
    }

    function toggleTerminal() {
        if (terminalToolApi?.openTerminalToolWindow) {
            terminalToolApi.openTerminalToolWindow({ source: 'toggle-terminal' });
            return;
        }
        // Fallback: legacy terminal panel (now Run Output)
        if (globalTerminalState && !globalTerminalState.tabs.length) addTerminalTab(globalTerminalState, true);
        toggleToolWindowPanel('terminalPanel', 'bottom');
        setTimeout(() => {
            if (globalTerminalState) refreshTerminalLayout(globalTerminalState);
            focusTerminal();
        }, 80);
    }

    function revealLine(lineNumber) {
        if (!activeEditor || !lineNumber) return;
        const model = activeEditor.getModel();
        if (!model) return;
        const line = Math.max(1, Math.min(model.getLineCount(), lineNumber));
        activeEditor.revealLineInCenter(line);
        activeEditor.setPosition({ lineNumber: line, column: 1 });
        activeEditor.focus();
        highlightLine(activeEditor, line);
    }

    function duplicateLine(editor) {
        if (!editor || typeof monaco === 'undefined') return;
        const model = editor.getModel();
        const selection = editor.getSelection();
        if (!model || !selection) return;

        const start = selection.startLineNumber;
        const end = selection.endLineNumber;
        const lines = [];
        for (let line = start; line <= end; line += 1) {
            lines.push(model.getLineContent(line));
        }
        const insertLine = end + 1;
        const text = `${lines.join('\n')}\n`;
        const range = new monaco.Range(insertLine, 1, insertLine, 1);
        editor.executeEdits('duplicate-line', [{ range, text }]);
        editor.setSelection(new monaco.Selection(insertLine, 1, insertLine + lines.length - 1, lines[lines.length - 1].length + 1));
    }

    async function loadAutocompleteData() {
        return mumpsMonacoManager.loadAutocompleteData();
    }

    function wireMenuBar(editor, routineState, terminalState) {
        const createMenuBarWiring = window.AhmadIDEModules?.renderer?.menu?.createMenuBarWiring;
        const menuBarWiring = createMenuBarWiring({ deps: { logger, runMenuAction } });
        return menuBarWiring.wireMenuBar(editor, routineState, terminalState);
    }

    const createGlobalShortcutsBinder = window.AhmadIDEModules?.renderer?.shortcuts?.createGlobalShortcutsBinder;
    const globalShortcutsBinder = createGlobalShortcutsBinder({
        deps: {
            closeFindReplaceDialog,
            closeSearchEverywhere,
            runMenuAction,
            terminalConfig,
            getGlobalRoutineState: () => globalRoutineState,
            getGlobalTerminalState: () => globalTerminalState,
            getActiveEditor: () => activeEditor,
            canSaveActiveTab,
            showToast,
            saveAllOpenTabs,
            saveRoutineFlow
        }
    });

    function bindGlobalShortcuts() {
        return globalShortcutsBinder.bindGlobalShortcuts();
    }

    // --- Shared debug controls (hoisted) ---


    function setDebugBarVisibility(show, active) {
        const bar = document.getElementById('debugBar');
        if (!bar) return;
        if (!show) {
            bar.classList.add('hidden');
            bar.classList.remove('inactive');
            bar.style.display = 'none';
            bar.style.pointerEvents = 'none';
            bar.querySelectorAll('button').forEach(btn => { btn.disabled = true; });
            return;
        }
        bar.classList.remove('hidden');
        bar.style.display = 'flex';
        bar.style.pointerEvents = 'auto';
        if (active) {
            bar.classList.remove('inactive');
            bar.querySelectorAll('button').forEach(btn => { btn.disabled = false; });
        } else {
            bar.classList.add('inactive');
            bar.querySelectorAll('button').forEach(btn => { btn.disabled = true; });
        }
    }

    // -style tool window management
    const toolWindowState = {
        left: { visible: true, activePanel: 'projectPanel' },
        right: { visible: false, activePanel: null },
        bottom: { visible: true, activePanel: 'terminalToolPanel' }
    };

    const createToolWindowManager = window.AhmadIDEModules?.renderer?.toolWindows?.createToolWindowManager;
    const toolWindowManager = createToolWindowManager({
        state: { toolWindowState },
        deps: {
            logger,
            getGlobalTerminalState: () => globalTerminalState,
            refreshTerminalLayout,
            focusTerminal,
            getTerminalToolApi: () => terminalToolApi,
            getOpenGitToolWindow: () => openGitToolWindow,
            getOpenCommitToolWindow: () => openCommitToolWindow
        }
    });

    function ensureBottomPanel(panelId) {
        return toolWindowManager.ensureBottomPanel(panelId);
    }

    function setActiveToolWindow(panelId) {
        return toolWindowManager.setActiveToolWindow(panelId);
    }

    function toggleToolWindowPanel(panelId, position) {
        return toolWindowManager.toggleToolWindowPanel(panelId, position);
    }

    function bindToolWindows() {
        return toolWindowManager.bindToolWindows();
    }

    // Statusbar: clicking Problems opens the Problems tool window.
    try {
        const problemsSummaryEl = document.getElementById('problemsSummary');
        if (problemsSummaryEl && !problemsSummaryEl.dataset.mideOpenProblemsWired) {
            problemsSummaryEl.dataset.mideOpenProblemsWired = '1';
            problemsSummaryEl.setAttribute('title', 'Open Problems (Alt+6)');
            problemsSummaryEl.setAttribute('aria-label', 'Open Problems (Alt+6)');
            problemsSummaryEl.addEventListener('click', (e) => {
                e.preventDefault();
                e.stopPropagation();
                try { ensureBottomPanel('problemsPanel'); } catch (_) { }
            });
        }
    } catch (_) { }

    function setActiveDebugTab(tabId) {
        activeDebugTab = tabId || activeDebugTab;
        const tabs = document.querySelectorAll('.debug-tab');
        const panes = document.querySelectorAll('.debug-tabpane');
        tabs.forEach(t => {
            t.classList.toggle('active', t.getAttribute('data-tab') === activeDebugTab);
        });
        panes.forEach(p => {
            p.classList.toggle('active', p.id === activeDebugTab);
        });
    }

    function bindDebugTabs() {
        const tabs = document.querySelectorAll('.debug-tab');
        if (!tabs.length) {
            // Debug panel is lazy-mounted; bind when it becomes available.
            if (!bindDebugTabs.__lazyHooked) {
                const fr = window.AhmadIDEModules?.app?.featureRegistry;
                fr?.onMounted?.('debugPanel', () => {
                    bindDebugTabs();
                    // Wire console input after panel is mounted
                    setTimeout(() => {
                        try {
                            if (debugManager && debugManager.ensureConsoleInput) {
                                console.log('[DEBUG] Wiring console input after debugPanel mount');
                                debugManager.ensureConsoleInput();
                            }
                        } catch (e) {
                            console.error('[DEBUG] Failed to wire console on mount:', e);
                        }
                    }, 100);
                });
                bindDebugTabs.__lazyHooked = true;
            }
            return;
        }
        if (bindDebugTabs.__bound) {
            setActiveDebugTab(activeDebugTab);
            return;
        }
        bindDebugTabs.__bound = true;
        tabs.forEach(tab => {
            tab.addEventListener('click', () => {
                const target = tab.getAttribute('data-tab');
                setActiveDebugTab(target);
                // Ensure console input is wired when console tab is activated
                if (target === 'tab-console') {
                    try {
                        if (debugManager && debugManager.ensureConsoleInput) {
                            console.log('[DEBUG] Wiring console input on tab click');
                            debugManager.ensureConsoleInput();
                        }
                    } catch (e) {
                        console.error('[DEBUG] Failed to ensure console input:', e);
                    }
                }
            });
        });
        setActiveDebugTab(activeDebugTab);
    }

    function showToast(sev, title, message) {
        const container = document.getElementById('toastContainer');
        if (!container) return;
        const toast = document.createElement('div');
        toast.className = `toast ${sev || 'info'}`;
        const icon = document.createElement('span');
        icon.className = 'toast-icon';
        icon.textContent = sev === 'error' ? '⛔' : 'ℹ';
        const body = document.createElement('div');
        body.className = 'toast-body';
        const t = document.createElement('div');
        t.className = 'toast-title';
        t.textContent = title || (sev === 'error' ? 'Error' : 'Info');
        const msg = document.createElement('div');
        msg.textContent = message || '';
        body.appendChild(t);
        body.appendChild(msg);
        const close = document.createElement('span');
        close.className = 'toast-close';
        close.textContent = '✕';
        close.onclick = () => container.removeChild(toast);
        toast.appendChild(icon);
        toast.appendChild(body);
        toast.appendChild(close);
        container.appendChild(toast);
        setTimeout(() => {
            if (toast.parentElement === container) container.removeChild(toast);
        }, 5000);
    }

    // Monaco loader is now loaded in HTML head via monaco-loader-init.js
    // Wait for it to be ready, then bootstrap
    function waitForMonacoLoader(callback) {
        if (typeof require !== 'undefined' && require.config) {
            callback();
        } else {
            setTimeout(() => waitForMonacoLoader(callback), 50);
        }
    }

    waitForMonacoLoader(bootstrapMonaco);

    function bootstrapMonaco() {
        window.MonacoEnvironment = {
            getWorkerUrl: function () {
                return './node_modules/monaco-editor/min/vs/base/worker/workerMain.js';
            }
        };

        require.config({ paths: { vs: './node_modules/monaco-editor/min/vs' } });

        require(['vs/editor/editor.main'], async () => {
            const editorHost = document.getElementById('editor');

            registerMumpsLanguage();
            registerMumpsThemes();
            const mumpsAutoData = await loadAutocompleteData();
            registerMumpsCompletion(mumpsAutoData);
            registerMumpsHover();
            try {
                const fileman = window.AhmadIDEModules?.mumps?.fileman;
                const settingsService = window.AhmadIDEModules?.services?.settingsService;
                fileman?.registerHoverProvider?.({ monaco, settingsService });
            } catch (err) {
                console.error('[MUMPS] FileMan hover docs error:', err);
            }

            // Ensure settings + custom fonts are applied before Monaco measures text.
            try {
                const settingsService = window.AhmadIDEModules?.services?.settingsService;
                const fontService = window.AhmadIDEModules?.services?.fontService;
                const settings = settingsService?.get?.();
                if (settings && fontService) {
                    await fontService.ensureCustomFontsLoaded(settings);
                    fontService.applyFontsToDocument(settings);
                }
            } catch (_) { }

            const rootStyles = getComputedStyle(document.documentElement);
            const codeFont = (rootStyles.getPropertyValue('--font-code') || '').trim()
                || 'ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, "Liberation Mono", "Courier New", monospace';
            const codeFontSizeValue = (rootStyles.getPropertyValue('--font-size-code') || '').trim();
            const codeFontSize = parseInt(codeFontSizeValue, 10) || 13;
            const codeLineHeight = parseFloat((rootStyles.getPropertyValue('--line-height-code') || '').trim()) || 1.55;
            const codeWeight = (rootStyles.getPropertyValue('--font-weight-code') || '').trim() || '400';
            const ligatures = (rootStyles.getPropertyValue('--font-ligatures-code') || '').trim();
            const codeLigatures = ligatures === '0' ? false : true;

            const editor = monaco.editor.create(editorHost, {
                value: sampleMumps(),
                language: 'mumps',
                theme: currentCodeTheme || defaultCodeTheme,
                tabCompletion: 'onlySnippets',
                fontSize: codeFontSize,
                fontFamily: codeFont,
                fontLigatures: codeLigatures,
                fontWeight: codeWeight,
                lineHeight: Math.max(12, Math.round(codeFontSize * codeLineHeight)),
                tabSize: 2,
                insertSpaces: true,
                detectIndentation: false,
                lineNumbers: 'on',
                renderLineHighlight: 'line',
                renderLineHighlightOnlyWhenFocus: false,
                cursorBlinking: 'blink',
                smoothScrolling: false,
                minimap: { enabled: false },
                automaticLayout: true,
                glyphMargin: true,
                contextmenu: false,
                // FORCE EAGER TOKENIZATION - disable lazy rendering
                'semanticHighlighting.enabled': true,
                // Performance optimizations
                renderValidationDecorations: 'on',
                quickSuggestions: { other: true, comments: false, strings: false },
                suggestOnTriggerCharacters: true,
                suggest: {
                    snippetsPreventQuickSuggestions: false,  // Allow snippets in quick suggestions
                    showSnippets: true
                },
                folding: true,
                foldingStrategy: 'auto',
                foldingHighlight: false,
                showFoldingControls: 'always',
                occurrencesHighlight: 'singleFile',  // Highlight all occurrences of selected word
                renderWhitespace: 'none',  // Don't render whitespace
                overviewRulerBorder: false,
                scrollBeyondLastLine: false,
                wordBasedSuggestions: 'off',
                parameterHints: { enabled: false },
                // Additional performance optimizations
                matchBrackets: 'never',  // Disable bracket matching
                selectionHighlight: true,  // Enable selection highlighting
                links: false,  // Disable link detection
                colorDecorators: false,  // Disable color decorators
                codeLens: false,  // Disable code lens
                lightbulb: { enabled: 'off' },  // Disable lightbulb suggestions
                hover: { enabled: true },  // Enable hover so debugger tooltips work
                inlayHints: { enabled: 'off' },  // Disable inlay hints
                stickyScroll: { enabled: false },  // Disable sticky scroll
                guides: { indentation: true, highlightActiveIndentation: true, bracketPairs: false },  // Visual guides (lightweight)

                // Fix UI Layout Issues
                fixedOverflowWidgets: false,  // Revert: positioning issue (appearing in center). Handle clipping via CSS.
                find: {
                    addExtraSpaceOnTop: false,  // Overlay search widget instead of shifting content down
                    autoFindInSelection: 'multiline',
                    seedSearchStringFromSelection: 'selection'
                },

                accessibilitySupport: 'off',  // Reduce accessibility overhead
                cursorSmoothCaretAnimation: 'off',  // Disable cursor animation
                rulers: [
                    { column: 80, color: 'rgba(189,147,249,0.12)' },
                    { column: 120, color: 'rgba(98,114,164,0.10)' }
                ],
                // SCROLL PERFORMANCE - critical for lag fix
                scrollbar: {
                    vertical: 'visible',
                    horizontal: 'visible',
                    useShadows: false,  // Disable scrollbar shadows
                    verticalScrollbarSize: 10,
                    horizontalScrollbarSize: 10,
                    scrollByPage: false
                },
                overviewRulerLanes: 0,  // Disable overview ruler lanes
                hideCursorInOverviewRuler: true,
                renderLineHighlightOnlyWhenFocus: true,  // Only highlight when focused
                fastScrollSensitivity: 7,  // Faster scroll response
                mouseWheelScrollSensitivity: 1.5,  // Better scroll feel
                cursorWidth: 2,  // Simpler cursor
                renderFinalNewline: 'off',  // Skip rendering trailing newline
                lineDecorationsWidth: 22,  // Extra gutter spacing + block guides
                lineNumbersMinChars: 4  // Comfortable gutter width
            });
            activeEditor = editor;
            try { mumpsMonacoManager?.attachDotIndentGuides?.(editor); } catch (_) { }

            // FORCE IMMEDIATE TOKENIZATION on scroll to fix color delay
            let scrollTokenizeTimer = null;
            editor.onDidScrollChange(() => {
                if (scrollTokenizeTimer) clearTimeout(scrollTokenizeTimer);
                scrollTokenizeTimer = setTimeout(() => {
                    const model = editor.getModel();
                    if (model && model.tokenization) {
                        const visibleRanges = editor.getVisibleRanges();
                        for (const range of visibleRanges) {
                            for (let line = range.startLineNumber; line <= range.endLineNumber; line++) {
                                try {
                                    model.tokenization.forceTokenization(line);
                                } catch (e) { }
                            }
                        }
                        editor.render(true);
                    }
                }, 10);
            });

            // Initialize states BEFORE adding editor actions (so Ctrl+S can access them)
            routineState = { current: null };
            routineStateRef = routineState;
            globalRoutineState = routineState;
            const terminalState = createTerminalState();
            globalTerminalState = terminalState;
            updateTerminalStatusPill();

            // Editor -style context menu actions moved to src/renderer/editor/editorMonacoActions.js
            const createEditorMonacoActions = window.AhmadIDEModules?.renderer?.editor?.createEditorMonacoActions;
            if (!createEditorMonacoActions) {
                logger.error('EDITOR_MONACO_ACTIONS_MODULE_MISSING', { path: './src/renderer/editor/editorMonacoActions.js' });
                throw new Error('Editor Monaco Actions module missing: ./src/renderer/editor/editorMonacoActions.js');
            }
            const editorMonacoActionsApi = createEditorMonacoActions();
            editorMonacoActionsApi.registerEditorActions(editor);

            // Initialize Smart Rename Provider
            const createMumpsSmartRenameProvider = window.AhmadIDEModules?.mumps?.createMumpsSmartRenameProvider;

            if (createMumpsSmartRenameProvider) {
                try {
                    console.log('[Smart Rename] Initializing provider...');
                    const smartRenameProvider = createMumpsSmartRenameProvider({
                        deps: {
                            getMonaco: () => monaco,
                            getCallIndexer: () => window.globalCallIndexer || null,
                            getActiveEditor: () => activeEditor,
                            showToast
                        }
                    });

                    const initialized = smartRenameProvider.initialize ? smartRenameProvider.initialize() : true;

                    // Always set the provider (works in fallback mode even if full init fails)
                    window.smartRenameProvider = smartRenameProvider;

                    // Always register Monaco action (works in both modes)
                    if (smartRenameProvider.registerMonacoAction) {
                        smartRenameProvider.registerMonacoAction(editor);
                    }

                    if (initialized) {
                        logger.info('SMART_RENAME_INITIALIZED', { initialized: true, mode: 'full+fallback' });
                        console.log('[Smart Rename] Provider initialized and registered globally.');
                    } else {
                        logger.warn('SMART_RENAME_FALLBACK_ONLY', { initialized: false, mode: 'fallback-only' });
                        console.warn('[Smart Rename] Provider initialized in fallback mode.');
                    }
                } catch (err) {
                    logger.error('SMART_RENAME_INIT_ERROR', { error: err.message, stack: err.stack });
                    console.error('[Smart Rename] Critical initialization error:', err);
                    // Ensure partial object exists to prevent UI crashes in context menu
                    window.smartRenameProvider = {
                        triggerSmartRenameTag: async () => {
                            showToast('error', 'Smart Rename', 'Feature failed to initialize. Please reload.');
                        }
                    };
                }
            } else {
                logger.error('SMART_RENAME_MODULE_MISSING', { hint: 'Check script includes in index.html' });
                console.error('[Smart Rename] Module factory not found. Scripts may be missing.');
            }

            // Initialize Enhanced Linter Features
            const initMumpsLinterEnhancements = window.AhmadIDEModules?.mumps?.initMumpsLinterEnhancements;

            if (initMumpsLinterEnhancements) {
                try {
                    console.log('[Linter] Initializing enhanced linter features...');
                    initMumpsLinterEnhancements({
                        monacoRef: monaco,
                        getActiveEditor: () => activeEditor,
                        showToast
                    });
                    console.log('[Linter] ✓ Enhanced linter features initialized');
                } catch (err) {
                    console.error('[Linter] Failed to initialize enhancements:', err);
                }
            } else {
                console.warn('[Linter] Enhanced linter module not found');
            }

            // Initialize MUMPS Features (Snippets, NEW autocomplete, Compare, Quick Fix)
            const MumpsSnippetsService = window.AhmadIDEModules?.mumps?.MumpsSnippetsService;
            const MumpsNewAutocompleteProvider = window.AhmadIDEModules?.mumps?.MumpsNewAutocompleteProvider;
            const MumpsRoutineCompare = window.AhmadIDEModules?.mumps?.MumpsRoutineCompare;
            const MumpsQuickFixProvider = window.AhmadIDEModules?.mumps?.MumpsQuickFixProvider;

            // Quick Fix Provider for linter autofixes
            if (MumpsQuickFixProvider) {
                try {
                    const quickFixProvider = new MumpsQuickFixProvider(monaco);
                    quickFixProvider.register();
                    window.mumpsQuickFix = quickFixProvider;
                    console.log('[MUMPS] ✓ Quick Fix provider initialized');
                } catch (err) {
                    console.error('[MUMPS] Quick Fix error:', err);
                }
            }


            if (MumpsSnippetsService) {
                try {
                    const snippetsService = new MumpsSnippetsService({
                        settingsService: window.AhmadIDEModules?.services?.settingsService,
                        showToast
                    });
                    snippetsService.registerCompletionProvider(monaco);
                    window.mumpsSnippets = snippetsService;
                    console.log('[MUMPS] ✓ Snippets service initialized');
                } catch (err) {
                    console.error('[MUMPS] Snippets error:', err);
                }
            }

            if (MumpsNewAutocompleteProvider) {
                try {
                    const newAutocomplete = new MumpsNewAutocompleteProvider({
                        monaco,
                        settingsService: window.AhmadIDEModules?.services?.settingsService,
                        showToast
                    });
                    newAutocomplete.register();
                    window.mumpsNewAutocomplete = newAutocomplete;
                    console.log('[MUMPS] ✓ NEW autocomplete initialized');
                } catch (err) {
                    console.error('[MUMPS] NEW autocomplete error:', err);
                }
            }

            // Tag Header Generator - generates individual tag documentation headers
            const MumpsTagHeaderGenerator = window.AhmadIDEModules?.mumps?.MumpsTagHeaderGenerator;
            if (MumpsTagHeaderGenerator) {
                try {
                    const tagHeaderGenerator = new MumpsTagHeaderGenerator({
                        showToast,
                        settingsService: window.AhmadIDEModules?.services?.settingsService
                    });
                    tagHeaderGenerator.registerCommand(editor, monaco);
                    window.mumpsTagHeaderGenerator = tagHeaderGenerator;
                    console.log('[MUMPS] ✓ Tag header generator initialized');
                } catch (err) {
                    console.error('[MUMPS] Tag header generator error:', err);
                }
            }

            if (MumpsRoutineCompare) {
                try {
                    const routineCompare = new MumpsRoutineCompare({
                        monaco,
                        ahmadIDE: window.ahmadIDE,
                        showToast
                    });
                    window.mumpsCompare = routineCompare;
                    window.compareRoutines = (leftRoutine, rightRoutine) => routineCompare.compare(leftRoutine, rightRoutine);
                    console.log('[MUMPS] ✓ Routine compare initialized');
                } catch (err) {
                    console.error('[MUMPS] Routine compare error:', err);
                }
            }

            const dbgState = {
                sessionId: null,
                breakpoints: new Set(),
                stack: [],
                engine: 'legacy',
                locals: {},
                state: 'stopped',
                currentLine: null,
                currentRoutine: null,
                homeTabId: null,
                homeRoutine: null,
                debugModeEnabled: false // Track if debug button was clicked
            };
            const debugBar = document.getElementById('debugBar');
            dbgStateRef = dbgState;

            applyCodeTheme(currentCodeTheme || defaultCodeTheme);
            bindThemeSelectors(editor);
            const wireEditorKeybindings = window.AhmadIDEModules?.renderer?.shortcuts?.wireEditorKeybindings;
            if (!wireEditorKeybindings) {
                logger.error('EDITOR_KEYBINDINGS_MODULE_MISSING', { path: './src/renderer/shortcuts/editorKeybindings.js' });
                throw new Error('Editor Keybindings module missing: ./src/renderer/shortcuts/editorKeybindings.js');
            }
            const terminalToolApiRef = {
                get value() { return terminalToolApi; }
            };
            wireEditorKeybindings({
                deps: {
                    editor,
                    loadShortcutPrefs,
                    setShortcutDefaults: (v) => { shortcutDefaults = v; },
                    duplicateLine,
                    registerKeybinding,
                    clickEl,
                    toggleSidebar,
                    toggleTerminal,
                    terminalToolApiRef,
                    openSearchEverywhere,
                    showToast,
                    openFindReplaceDialog,
                    getSelectedText,
                    toggleToolWindowPanel,
                    openGitToolWindow: (...args) => openGitToolWindow(...args),
                    openCommitToolWindow: (...args) => openCommitToolWindow(...args)
                }
            });

            renderProjectTree([], routineState, editor);
            // Initialize empty tab bar
            renderTabs();
            bindTabKeyboardShortcuts();
            // -like editor context menu
            bindEditorContextMenu(editor);
            bindSettingsPanelThemes();
            bindGitSettingsPanel();
            bindDebugTabs();
            bindToolWindows();
            bindGlobalShortcuts();
            // Debounced terminal resize for performance
            let terminalResizeTimer = null;
            window.addEventListener('resize', () => {
                if (terminalResizeTimer) clearTimeout(terminalResizeTimer);
                terminalResizeTimer = setTimeout(() => {
                    if (globalTerminalState) {
                        refreshTerminalLayout(globalTerminalState, { resizeSession: true });
                    }
                }, 200); // Debounce 200ms - increased to reduce lag
            });
            try {
                window.AhmadIDEModules?.services?.extensionsService?.start?.();
            } catch (_) { }
            initExtensionsView();

            // -style search bindings
            const wireSearchBindings = window.AhmadIDEModules?.renderer?.search?.wireSearchBindings;
            wireSearchBindings({
                updateFindScopeLabels,
                findReplaceState,
                toggleFindMode,
                executeFindReplacePreview,
                closeFindReplaceDialog,
                confirmAndReplaceAll,
                searchDebounce,
                renderSearchEverywhereResults,
                searchEverywhereState,
                openSearchEverywhereResult,
                closeSearchEverywhere
            });

            // Global shortcuts are handled centrally in bindGlobalShortcuts().
            // Services tool window is lazy-mounted and wired via src/features/services/panel-bindings.js

            const getBpLines = () =>
                Array.from(dbgState.breakpoints || [])
                    .map(parseBpKey)
                    .filter(bp => bp.file === getActiveRoutine())
                    .map(bp => bp.line)
                    .filter(n => !isNaN(n));

            const getBpEntries = () =>
                Array.from(dbgState.breakpoints || [])
                    .map(parseBpKey)
                    .filter(bp => !isNaN(bp.line));

            // Monaco uses automaticLayout; extra layout() calls cause layout storms.

            // --- Validation wiring ---
            // Run lint on idle to keep typing responsive; same rules, less contention
            let lintHandle = null;
            const scheduleValidate = (model) => {
                if (lintHandle) {
                    if (window.cancelIdleCallback) cancelIdleCallback(lintHandle);
                    else clearTimeout(lintHandle);
                }
                const run = () => { lintHandle = null; validateMumps(model); };
                lintHandle = window.requestIdleCallback
                    ? requestIdleCallback(run, { timeout: 800 })
                    : setTimeout(run, 800);
            };
            editor.onDidChangeModelContent(() => {
                scheduleValidate(editor.getModel());
                if (activeTabId) {
                    markTabDirty(activeTabId, true, { deferRender: true });
                }
            });
            validateMumps(editor.getModel());
            // Removed duplicate validation - already called on line 3902

            const createStatusBarWiring = window.AhmadIDEModules?.renderer?.status?.createStatusBarWiring;
            const statusBarWiring = createStatusBarWiring({
                deps: {
                    getGitRepoManager: () => gitRepoManager,
                    getCurrentProject: () => currentProject
                }
            });
            const statusBarApi = statusBarWiring.wireStatusBar(editor);
            const fetchCurrentBranch = statusBarApi.fetchCurrentBranch;

            // --- Env info ---
            const envInfo = await window.ahmadIDE.getEnv();
            envInfoCache = envInfo;
            document.getElementById('envInfo').textContent =
                `${envInfo.platform} | electron ${envInfo.versions.electron}`;
            // Default to "Ready" until the user connects (or auto-connect succeeds).
            // Avoid showing Docker as "connected" when no container is selected.
            setConnStatus('Ready', 'subtle');
            wireMenuBar(editor, routineState, terminalState);

            // --- Run & Debug buttons ---
            const createRunDebugButtonsWiring = window.AhmadIDEModules?.renderer?.run?.createRunDebugButtonsWiring;
            const runDebugButtonsWiring = createRunDebugButtonsWiring({
                deps: {
                    loadRoutineList,
                    routinesManager,
                    logger,
                    appendOutput,
                    mumpsLinter,
                    hasLintRules,
                    applyLintMarkers,
                    renderProblems,
                    debugContinue,
                    startDebugSession,
                    runMumpsCode,
                    debugStop,
                    updateDebugButtonState,
                    clearOutput,
                    addTerminalTab,
                    toggleToolWindowPanel,
                    currentDebugSessionRef
                }
            });

            runDebugButtonsWiring.wireRunDebugButtons($, editor, routineState, terminalState, dbgState, debugBar, getBpLines);

            const createEditorToolbarWiring = window.AhmadIDEModules?.renderer?.editor?.createEditorToolbarWiring;
            const editorToolbarWiring = createEditorToolbarWiring({
                deps: {
                    canSaveActiveTab,
                    showToast,
                    saveRoutineFlow,
                    newRoutineFlow,
                    appendOutput,
                    mumpsLinter,
                    hasLintRules,
                    applyLintMarkers,
                    renderProblems
                }
            });
            editorToolbarWiring.wireEditorToolbar($, editor, routineState, terminalState);

            const createPostMonacoWiring = window.AhmadIDEModules?.renderer?.startup?.createPostMonacoWiring;
            const postMonacoWiring = createPostMonacoWiring({
                deps: {
                    connectionsManager,
                    appendOutput,
                    setConnStatus,
                    loadRoutineList: (...args) => loadRoutineList(...args),
                    closeShortcutsPanel,
                    parseShortcutString,
                    loadShortcutPrefs,
                    persistShortcutPrefs,
                    registeredShortcuts,
                    applyShortcutBinding,
                    renderShortcutsPanel,
                    setCollapseStateAll,
                    getActiveEditor: () => activeEditor,
                    renderBreakpoints,
                    decorateBreakpoints,
                    settingsPanelManager,
                    projectCreateManager,
                    gitToolWindowManager,
                    setDebugButtons,
                    renderProjectTreeLoading,
                    renderProjectTree,
                    showToast,
                    renderLocals,
                    renderStack,
                    renderDebugConsole,
                    resetDebugUI,
                    ctrlHoverManager
                }
            });
            postMonacoWiring.wirePostMonacoWiring(editor, routineState, terminalState, dbgState, fetchCurrentBranch, (gitToolWindowApi) => {
                openGitToolWindow = gitToolWindowApi.openGitToolWindow;
                openCommitToolWindow = gitToolWindowApi.openCommitToolWindow;
                openGitPanel = gitToolWindowApi.openGitPanel;
            });
        });
    }

    // ---------- Terminal & Output ----------
    // Moved to src/editor/terminal/renderer-terminal.js

    // ---------- Breakpoints & Debug UI ----------
    // Moved to src/editor/debug/renderer-debug.js
    function parseBpKey(key) { return debugManager.parseBpKey(key); }
    function getActiveRoutine() { return debugManager.getActiveRoutine(); }
    function toggleBreakpoint(line, dbgState, editor) { return debugManager.toggleBreakpoint(line, dbgState, editor); }
    function renderBreakpoints(dbgState) { return debugManager.renderBreakpoints(dbgState); }
    function decorateBreakpoints(editor, dbgState) { return debugManager.decorateBreakpoints(editor, dbgState); }
    function renderLocals(locals) { return debugManager.renderLocals(locals); }
    function renderStack(stack) { return debugManager.renderStack(stack); }
    function renderDebugConsole(lines) { return debugManager.renderDebugConsole(lines); }
    function updateDebugButtonState() { return debugManager.updateDebugButtonState(); }
    function resetDebugUI(clearSession = false, keepArmed = false) { return debugManager.resetDebugUI(clearSession, keepArmed); }
    function setDebugButtons(enabled) { return debugManager.setDebugButtons(enabled); }
    async function startDebugSession(editorParam = activeEditor, dbgStateParam = dbgStateRef, terminalState = globalTerminalState, debugBarEl = document.getElementById('debugBar'), bpLinesOverride = null) {
        const inferredBps = Array.isArray(bpLinesOverride)
            ? bpLinesOverride
            : (typeof getBpLines === 'function' ? getBpLines() : []);
        return debugManager.startDebugSession(editorParam, dbgStateParam, terminalState, debugBarEl, inferredBps);
    }
    async function debugContinue() { return debugManager.debugContinue(); }
    async function debugStop() { return debugManager.debugStop(); }

    // Bind debug bar buttons (timing preserved vs. previous inline block)
    debugManager.scheduleDebugUiBindings();


    // ---------- Problems / Docker / Misc ----------

    function renderProblems(items) {
        return problemsManager.renderProblems(items);
    }

    function renderDocker(containers, routineState, editor, opts) {
        return dockerUi.renderDocker(containers, routineState, editor, opts);
    }

    function setConnStatus(text, severity) {
        return dockerUi.setConnStatus(text, severity);
    }

    function initExtensionsView() {
        return extensionsManager.initExtensionsView();
    }

    function highlightLine(editor, lineNumber) {
        if (!editor) return;
        if (editor._debugDecorations) {
            editor._debugDecorations = editor.deltaDecorations(editor._debugDecorations, []);
        }
        if (!lineNumber) return;

        // Trust backend - it already skips comment lines
        editor._debugDecorations = editor.deltaDecorations([], [
            {
                range: new monaco.Range(lineNumber, 1, lineNumber, 1),
                options: {
                    isWholeLine: true,
                    className: 'debug-line',
                    glyphMarginClassName: 'debug-glyph'
                }
            }
        ]);
    }

    // ---------- MUMPS Language Registration ----------

    function sampleMumps() {
        return mumpsMonacoManager.sampleMumps();
    }

    function registerMumpsLanguage() {
        return mumpsMonacoManager.registerMumpsLanguage();
    }

    function registerMumpsThemes() {
        return mumpsMonacoManager.registerMumpsThemes();
    }

    function registerMumpsCompletion(data) {
        return mumpsMonacoManager.registerMumpsCompletion(data);
    }

    function markerSeverity(sev) {
        return diagnosticsManager.markerSeverity(sev);
    }

    function normalizeSeverity(sev) {
        return diagnosticsManager.normalizeSeverity(sev);
    }

    function updateProblemSummary(items) {
        return problemsManager.updateProblemSummary(items);
    }

    function hasLintRules(linter) {
        return diagnosticsManager.hasLintRules(linter);
    }

    function applyLintMarkers(model, issues) {
        return diagnosticsManager.applyLintMarkers(model, issues);
    }

    function validateMumps(model) {
        return diagnosticsManager.validateMumps(model);
    }

    async function runMumpsCode(editor, terminalState) {
        // Run Output: auto-open only when running code.
        try { ensureBottomPanel('terminalPanel'); } catch (_) { }
        try {
            if (terminalState && !terminalState.tabs?.length) {
                await addTerminalTab(terminalState, true);
            }
        } catch (_) { }
        const code = editor.getValue();
        appendOutput('🚀 Executing...', terminalState);
        const res = await window.ahmadIDE.execute(code);
        if (res.ok) {
            appendOutput(res.output || '(no output)', terminalState);
        } else {
            const msg = res.error || res.stderr;
            appendOutput(`✗ Execute error: ${msg}`, terminalState);
            showToast('error', 'Execute', msg);
        }
        // Let caller decide whether to start debug or stop
        return res;
    }

    // ---------- Routines ----------

    function setCurrentRoutine(name) {
        return routinesManager.setCurrentRoutine(name);
    }

    async function loadRoutineList(state, editor, search = '') {
        return routinesManager.loadRoutineList(state, editor, search);
    }

    async function loadRoutineByName(name, state, editor, routinesCache = [], termState) {
        return routinesManager.loadRoutineByName(name, state, editor, routinesCache, termState);
    }

    async function saveRoutineFlow(editor, state, termState) {
        return routinesManager.saveRoutineFlow(editor, state, termState);
    }

    async function performSave(name, editor, state, termState) {
        return routinesManager.performSave(name, editor, state, termState);
    }

    async function newRoutineFlow(editor, state, termState) {
        return routinesManager.newRoutineFlow(editor, state, termState);
    }

    function registerMumpsHover() {
        return debugManager.registerMumpsHover();
    }

    // Expose the internal menu action runner so toolbar/drawers can dispatch actions
    // without relying on legacy global free-variables.
    try {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.app = window.AhmadIDEModules.app || {};
        if (typeof window.AhmadIDEModules.app.runMenuAction !== 'function') {
            window.AhmadIDEModules.app.runMenuAction = (action, ctx) => runMenuAction(action, ctx);
        }
    } catch (_) { }

    // Dispose all tab models on unload to release memory and markers
    window.addEventListener('beforeunload', () => {
        tabModels.forEach((model) => {
            try {
                monaco.editor.setModelMarkers(model, 'mumps-check', []);
                model.dispose();
            } catch (_) {
                // ignore cleanup errors
            }
        });
        tabModels.clear();
    });

})();
