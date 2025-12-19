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
    const ideThemes = {
        'jb-light': {
            name: 'PhpStorm Light',
            vars: {
                '--bg': '#f5f7fb',
                '--glass': 'rgba(255, 255, 255, 0.7)',
                '--panel': '#ffffff',
                '--panel-2': '#f4f6fb',
                '--panel-strong': '#ffffff',
                '--panel-soft': '#f5f7fb',
                '--sidebar': '#f5f7fb',
                '--rail-bg': '#eef1f7',
                '--editor-bg': '#ffffff',
                '--gutter-bg': '#f3f5f9',
                '--terminal-bg': '#f8f9fb',
                '--terminal-input-bg': '#edf0f7',
                '--accent': '#3574f0',
                '--accent-2': '#5b8def',
                '--accent-blue': '#3574f0',
                '--accent-green': '#3fb87f',
                '--accent-orange': '#ed8b00',
                '--accent-soft': 'rgba(53, 116, 240, 0.12)',
                '--accent-soft-border': 'rgba(53, 116, 240, 0.24)',
                '--text': '#1f232a',
                '--text-bright': '#0f4c81',
                '--muted': '#6a7280',
                '--border': '#dfe3eb',
                '--tree-selected': '#e6f0ff',
                '--tree-selected-text': '#0f1b2d',
                '--selection-bg': '#d9e4ff',
                '--hover-bg': '#ecf0f7',
                '--glow-1': 'rgba(53, 116, 240, 0.08)',
                '--glow-2': 'rgba(91, 141, 239, 0.06)',
                '--font-ui': 'Inter, \"Segoe UI\", \"SF Pro Display\", system-ui, sans-serif',
                '--font-code': 'ui-monospace, \"SFMono-Regular\", \"Menlo\", \"Monaco\", \"Consolas\", \"Liberation Mono\", \"Courier New\", monospace',
                '--font-size-ui': '13px',
                '--font-size-code': '13px'
            }
        },
        earth: {
            name: 'Earth Dark',
            vars: {
                '--bg': '#1b120e',
                '--glass': 'rgba(255,255,255,0.04)',
                '--panel': 'rgba(37, 25, 19, 0.9)',
                '--panel-2': 'rgba(45, 31, 23, 0.92)',
                '--panel-strong': '#1b120e',
                '--panel-soft': '#2a1b14',
                '--sidebar': '#3c3f41',
                '--rail-bg': '#313335',
                '--editor-bg': '#2b2b2b',
                '--gutter-bg': '#23160f',
                '--terminal-bg': '#2b2b2b',
                '--terminal-input-bg': '#313335',
                '--accent': '#d67f3c',
                '--accent-2': '#f0c27b',
                '--accent-blue': '#4a9fe8',
                '--accent-green': '#5fb865',
                '--accent-orange': '#cc7832',
                '--accent-soft': 'rgba(214,127,60,0.20)',
                '--accent-soft-border': 'rgba(214,127,60,0.32)',
                '--text': '#f5ede4',
                '--text-bright': '#ffc66d',
                '--muted': '#b8a99a',
                '--border': 'rgba(255,255,255,0.08)',
                '--tree-selected': '#2f4554',
                '--tree-selected-text': '#ffffff',
                '--selection-bg': '#214283',
                '--hover-bg': '#393b3d',
                '--glow-1': 'rgba(214,127,60,0.08)',
                '--glow-2': 'rgba(240,194,123,0.06)',
                '--font-ui': 'Inter, \"Segoe UI\", \"SF Pro Display\", system-ui, sans-serif',
                '--font-code': 'ui-monospace, \"SFMono-Regular\", \"Menlo\", \"Monaco\", \"Consolas\", \"Liberation Mono\", \"Courier New\", monospace',
                '--font-size-ui': '13px',
                '--font-size-code': '13px'
            }
        },
        desert: {
            name: 'Desert Contrast',
            vars: {
                '--bg': '#21160f',
                '--glass': 'rgba(255,255,255,0.05)',
                '--panel': 'rgba(42, 28, 20, 0.9)',
                '--panel-2': 'rgba(52, 35, 24, 0.94)',
                '--panel-strong': '#21160f',
                '--panel-soft': '#2b1c14',
                '--sidebar': '#2b1c14',
                '--rail-bg': '#2f2017',
                '--editor-bg': '#1f140e',
                '--gutter-bg': '#1a100b',
                '--terminal-bg': '#23160f',
                '--terminal-input-bg': '#2e1e15',
                '--accent': '#f0a35c',
                '--accent-2': '#ffd59a',
                '--accent-blue': '#4a9fe8',
                '--accent-green': '#5fb865',
                '--accent-orange': '#f0a35c',
                '--accent-soft': 'rgba(240,163,92,0.20)',
                '--accent-soft-border': 'rgba(240,163,92,0.32)',
                '--text': '#f7ecde',
                '--text-bright': '#ffd59a',
                '--muted': '#c7b8a4',
                '--border': 'rgba(255,255,255,0.1)',
                '--tree-selected': '#2f4554',
                '--tree-selected-text': '#ffffff',
                '--selection-bg': '#214283',
                '--hover-bg': '#3a2b22',
                '--glow-1': 'rgba(240,163,92,0.10)',
                '--glow-2': 'rgba(255,213,154,0.06)',
                '--font-ui': 'Inter, \"Segoe UI\", \"SF Pro Display\", system-ui, sans-serif',
                '--font-code': 'ui-monospace, \"SFMono-Regular\", \"Menlo\", \"Monaco\", \"Consolas\", \"Liberation Mono\", \"Courier New\", monospace',
                '--font-size-ui': '13px',
                '--font-size-code': '13px'
            }
        }
    };
    const defaultIdeTheme = 'earth';
    const defaultCodeTheme = 'mumps-earth';
    const codeThemes = ['mumps-light', 'mumps-earth', 'mumps-dark'];
    let currentCodeTheme = (() => {
        try {
            const stored = localStorage.getItem('ahmadIDE:theme:code');
            if (stored && codeThemes.includes(stored)) return stored;
            return defaultCodeTheme;
        } catch (e) {
            return defaultCodeTheme;
        }
    })();
    const collapsedTreeNodes = new Set();
    let activeEditor = null;
    // Terminal configuration (TODO: expose via Settings UI)
    const terminalConfig = {
        shellPath: null, // Default: system shell
        startDir: null, // Default: project root
        overrideIdeShortcuts: false, // If true, terminal keeps IDE shortcuts from firing while focused
        escapeToEditor: true // If true and shortcuts are not overridden, Esc returns focus to editor
    };
    let envInfoCache = null;
    const mumpsValidator = typeof MUMPSValidator !== 'undefined' ? new MUMPSValidator() : null;
    const mumpsLinter = typeof MUMPSLinter !== 'undefined' ? new MUMPSLinter() : null;
    const MUMPSLexerClass = typeof MUMPSLexer !== 'undefined' ? MUMPSLexer : null;
    const MUMPSParserClass = typeof MUMPSParser !== 'undefined' ? MUMPSParser : null;
    const registeredShortcuts = [];
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

    // ========== MUMPS Reference Parser (shared utility) ==========
    // Parse routine/tag reference at cursor position (supports TAG^RTN, ^RTN, DO TAG)
    // MOVED TOP for visibility
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

        logger.info('MENU_ACTION', { action });
        switch (action) {
            case 'save':
            case 'save-all':
                clickEl('saveRoutineBtn');
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
                editor?.trigger('keyboard', 'actions.find', null);
                return;
            case 'replace':
                editor?.trigger('keyboard', 'editor.action.startFindReplaceAction', null);
                return;
            case 'find-in-folder':
                openFindReplaceDialog('find', getSelectedText());
                return;
            case 'replace-in-folder':
                openFindReplaceDialog('replace', getSelectedText());
                return;
            case 'search-everywhere':
                openSearchEverywhere('');
                return;
            case 'comment':
                editor?.trigger('keyboard', 'editor.action.commentLine', null);
                return;
            case 'duplicate-line':
                duplicateLine(editor);
                return;
            case 'goto-line':
                editor?.trigger('keyboard', 'editor.action.gotoLine', null);
                return;
            case 'goto-file':
                openSearchEverywhere('');
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
            case 'reformat':
                editor?.trigger('keyboard', 'editor.action.formatDocument', null);
                return;
            case 'rename':
                editor?.trigger('keyboard', 'editor.action.rename', null);
                return;
            case 'lint':
                clickEl('lintBtn');
                return;
            case 'toggle-sidebar':
                toggleSidebar();
                return;
            case 'toggle-terminal':
                toggleTerminal();
                return;
            case 'terminal':
                if (terminalState && !terminalState.tabs.length) {
                    await addTerminalTab(terminalState, true);
                }
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
            case 'find':
                window.AhmadIDEModules?.app?.dialogRegistry?.open('find') || notImplemented(action);
                return;
            case 'replace':
                window.AhmadIDEModules?.app?.dialogRegistry?.open('replace') || notImplemented(action);
                return;
            case 'goto-file':
                window.AhmadIDEModules?.app?.dialogRegistry?.open('goto-file') || notImplemented(action);
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
                notImplemented(action);
        }
    }
    const coreShortcutMap = {
        'ctrl+n': { label: 'Go to File', action: 'goto-file' },
        'ctrl+shift+n': { label: 'Go to File (Alt)', action: 'goto-file' },
        'ctrl+f': { label: 'Find in File', action: 'find' },
        'ctrl+shift+f': { label: 'Find in Path (Current Folder)', action: 'find-in-folder' },
        'ctrl+shift+r': { label: 'Replace in Path (Current Folder)', action: 'replace-in-folder' },
        'ctrl+s': { label: 'Save', action: 'save' },
        'ctrl+shift+s': { label: 'Save All', action: 'save-all' },
        'ctrl+w': { label: 'Expand Selection', action: 'expand-selection' },
        'ctrl+shift+w': { label: 'Shrink Selection', action: 'shrink-selection' },
        'ctrl+tab': { label: 'Next Tab', action: 'tab-next' },
        'alt+f12': { label: 'Toggle Terminal', action: 'toggle-terminal' }
    };
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
    let lastShiftTap = 0;
    let shortcutDefaults = {};
    let routineFilterTerm = '';
    let activeDebugTab = 'tab-breakpoints';
    const maxLintTextLength = 20000;  // Skip linting for files > 20KB
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

    function loadProjectIntoTree(projectData) {
        currentProject = projectData;
        const host = $('#projectTree');
        if (!host.length) {
            return;
        }
        host.empty();

        if (!projectData || !projectData.projectPath) {
            host.append('<div class="tree-item">Error: Invalid project data</div>');
            return;
        }

        // Add project root (always expanded)
        const projectName = projectData.projectPath.split('/').pop();
        const projectRoot = $('<div class="tree-item folder"></div>')
            .css({
                'padding-left': '4px',
                'font-weight': 'bold',
                'margin-bottom': '4px'
            })
            .html('<span class="folder-icon">[-]</span> ' + projectName);
        host.append(projectRoot);

        // Helper function to create a collapsible routine folder
        function createRoutineFolder(folderName, routines, paddingLeft = 16) {
            const folderContainer = $('<div></div>');
            const folder = $('<div class="tree-item folder"></div>')
                .css({
                    'padding-left': `${paddingLeft}px`,
                    'cursor': 'pointer',
                    'user-select': 'none'
                })
                .html(`<span class="folder-icon">[+]</span> ${folderName}/`)
                .data('expanded', false);

            const folderContent = $('<div class="folder-content"></div>')
                .css('display', 'none');

            if (routines && routines.length > 0) {
                routines.forEach(routine => {
                    // Remove .m extension if present
                    const routineName = routine.replace('.m', '');
                    const routineItem = $('<div class="tree-item file"></div>')
                        .css({
                            'padding-left': `${paddingLeft + 16}px`,
                            'cursor': 'pointer'
                        })
                        .text('  [M] ' + routineName)
                        .on('click', async function () {
                            const fullPath = `${folderName}/${routineName}`;
                            try {
                                const result = await window.ahmadIDE.readRoutine(fullPath);
                                if (result.ok) {
                                    if (activeEditor) {
                                        activeEditor.setValue(result.code);
                                        showToast('success', 'Loaded', fullPath);
                                    } else {
                                        showToast('error', 'Error', 'No editor available');
                                    }
                                } else {
                                    showToast('error', 'Failed', result.error || 'Load failed');
                                }
                            } catch (err) {
                                showToast('error', 'Error', err.message);
                            }
                        });

                    folderContent.append(routineItem);
                });
            } else {
                folderContent.append(
                    $('<div class="tree-item"></div>')
                        .css({
                            'padding-left': `${paddingLeft + 16}px`,
                            'opacity': '0.5',
                            'font-style': 'italic'
                        })
                        .text('(empty)')
                );
            }

            folder.on('click', function () {
                const expanded = $(this).data('expanded');
                if (expanded) {
                    $(this).find('.folder-icon').text('[+]');
                    folderContent.slideUp(200);
                    $(this).data('expanded', false);
                } else {
                    $(this).find('.folder-icon').text('[-]');
                    folderContent.slideDown(200);
                    $(this).data('expanded', true);
                }
            });

            folderContainer.append(folder).append(folderContent);
            return folderContainer;
        }

        // localr folder
        const localrFolder = createRoutineFolder('localr', projectData.structure.localr || []);
        host.append(localrFolder);

        // routines folder
        const routinesFolder = createRoutineFolder('routines', projectData.structure.routines || []);
        host.append(routinesFolder);
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

    const mumpsFileIconSvg = `<svg width="16" height="16" viewBox="0 0 16 16"><defs><linearGradient id="mg" x1="0" y1="0" x2="1" y2="1"><stop offset="0%" stop-color="#f0a35c"/><stop offset="100%" stop-color="#d67f3c"/></linearGradient></defs><rect width="16" height="16" rx="3" fill="url(#mg)"/><text x="4" y="12" font-size="10" font-weight="bold" fill="#19100c" font-family="monospace">M</text></svg>`;

    // ============================================
    // PhpStorm-style Tab Management
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
            getGlobalTerminalState: () => globalTerminalState
        }
    });

    const normalizeRoutineTarget = tabManager.normalizeRoutineTarget;
    const findOpenTab = tabManager.findOpenTab;
    const createTab = tabManager.createTab;
    const switchTab = tabManager.switchTab;
    const markTabDirty = tabManager.markTabDirty;
    const cycleTab = tabManager.cycleTab;
    const bindTabKeyboardShortcuts = tabManager.bindTabKeyboardShortcuts;
    const renderTabs = tabManager.renderTabs;

    // Breakpoints / Debugger moved to src/editor/debug/renderer-debug.js
    const createDebugManager = window.AhmadIDEModules?.debug?.createDebugManager;
    if (!createDebugManager) {
        logger.error('DEBUG_MODULE_MISSING', { path: './src/editor/debug/renderer-debug.js' });
        throw new Error('Debug module missing: ./src/editor/debug/renderer-debug.js');
    }

    const currentDebugSessionRef = {
        get value() { return currentDebugSession; },
        set value(v) { currentDebugSession = v; }
    };

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
            toggleToolWindowPanel
        }
    });

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
            getCurrentProject: () => currentProject
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
                const fs = require('fs');
                const path = require('path');
                const fontsDir = path.join(__dirname, 'assets', 'fonts');
                if (!fs.existsSync(fontsDir)) return [];

                const files = await fs.promises.readdir(fontsDir);
                // Filter likely font extensions
                const fontFiles = files.filter(f => /\.(ttf|otf|woff|woff2)$/i.test(f));

                // Return objects with name and full path or relative URL
                return fontFiles.map(f => ({
                    fileName: f,
                    // Create a file:// URL for the frontend to use
                    url: `file://${path.join(fontsDir, f)}`
                }));
            } catch (e) {
                console.error('Failed to scan fonts dir:', e);
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
            runGitQuickCmd: (...args) => runGitQuickCmd(...args)
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
    // PhpStorm-style Project Tree Context Menu
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

        // Special files (PhpStorm style)
        if (name === '.gitignore') return { icon: '◉', cls: 'special' };
        if (name === '.env') return { icon: '⚙', cls: 'special' };
        if (name === 'package.json') return { icon: '📦', cls: 'special' };
        if (name === 'composer.json') return { icon: '🎼', cls: 'special' };
        if (name === 'dockerfile') return { icon: '🐳', cls: 'special' };
        if (name === '.gitkeep') return { icon: '◉', cls: 'special' };
        if (name === 'readme.md' || name === 'readme.txt') return { icon: '📖', cls: 'special' };

        // File type icons (PhpStorm style)
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

    async function goToDeclaration(editor, position = null, options = {}) {
        const { silentIfMissing = false } = options;
        const ed = editor || activeEditor;
        if (!ed) return false;

        const model = ed.getModel();
        const pos = position || ed.getPosition();
        if (!model || !pos) return false;

        // CRITICAL: DO NOT modify debug state during navigation
        // This function is for editor-only code navigation
        // If a debug session is active, it should NOT be paused/resumed/altered
        // by user clicking on routine references
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
    }

    function loadShortcutPrefs() {
        try {
            const raw = localStorage.getItem('ahmadIDE:shortcuts');
            return raw ? JSON.parse(raw) : {};
        } catch (e) {
            return {};
        }
    }

    function persistShortcutPrefs(map) {
        try {
            localStorage.setItem('ahmadIDE:shortcuts', JSON.stringify(map || {}));
        } catch (e) {
            // ignore storage failures
        }
    }

    function keyCodeFromToken(tok) {
        if (!tok) return null;
        const upper = tok.toUpperCase();
        if (upper.length === 1 && upper >= 'A' && upper <= 'Z') {
            return monaco.KeyCode['Key' + upper];
        }
        const funcMatch = upper.match(/^F([1-9]|1[0-2])$/);
        if (funcMatch) {
            return monaco.KeyCode['F' + funcMatch[1]];
        }
        if (upper === 'ENTER' || upper === 'RETURN') return monaco.KeyCode.Enter;
        return null;
    }

    function parseShortcutString(str) {
        if (!str || typeof str !== 'string') return null;
        const tokens = str.split('+').map(t => t.trim()).filter(Boolean);
        let binding = 0;
        let keyToken = null;
        tokens.forEach(tok => {
            const upper = tok.toUpperCase();
            if (upper === 'CTRL' || upper === 'CMD' || upper === 'CONTROL') {
                binding |= monaco.KeyMod.CtrlCmd;
            } else if (upper === 'SHIFT') {
                binding |= monaco.KeyMod.Shift;
            } else if (upper === 'ALT' || upper === 'OPTION') {
                binding |= monaco.KeyMod.Alt;
            } else if (upper === 'WIN' || upper === 'META') {
                binding |= monaco.KeyMod.WinCtrl;
            } else {
                keyToken = upper;
            }
        });
        const keyCode = keyCodeFromToken(keyToken);
        if (!keyCode) return null;
        return binding | keyCode;
    }

    function applyShortcutBinding(editor, actionId, binding, handler) {
        if (!editor || !handler || !binding) return;
        editor.addCommand(binding, handler);
    }

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

    function describeBinding(binding) {
        if (!binding && binding !== 0) return 'Unbound';
        const parts = [];
        if (binding & monaco.KeyMod.CtrlCmd) parts.push('Ctrl');
        if (binding & monaco.KeyMod.Shift) parts.push('Shift');
        if (binding & monaco.KeyMod.Alt) parts.push('Alt');
        if (binding & monaco.KeyMod.WinCtrl) parts.push('Meta');

        const keyPart = binding & 0xff;
        const keyNames = Object.keys(monaco.KeyCode).filter(k => monaco.KeyCode[k] === keyPart);
        const keyName = keyNames.length ? keyNames[0].replace(/^Key/, '') : '';
        if (keyName) parts.push(keyName);
        return parts.join('+') || 'Unbound';
    }

    function renderShortcutsPanel() {
        const list = document.getElementById('shortcutsList');
        if (!list) return;
        list.innerHTML = '';
        if (!registeredShortcuts.length) {
            const li = document.createElement('li');
            li.textContent = 'No shortcuts registered.';
            list.appendChild(li);
            return;
        }
        registeredShortcuts.forEach(sc => {
            const li = document.createElement('li');
            li.className = 'problem-item info shortcuts-list';
            const icon = document.createElement('span');
            icon.className = 'problem-icon';
            icon.textContent = '⌨';
            const text = document.createElement('span');
            text.className = 'problem-text';
            text.textContent = `${sc.label} — ${describeBinding(sc.binding)}`;
            li.appendChild(icon);
            li.appendChild(text);
            li.onclick = () => {
                navigator.clipboard?.writeText(`${sc.label} :: ${describeBinding(sc.binding)}`).catch(() => { });
            };
            list.appendChild(li);
        });
    }

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

    const normalizeGitError = (text, fallback = 'Git command failed') => {
        if (/not a git repository/i.test(text || '')) {
            return 'Git is not configured for this project';
        }
        return text || fallback;
    };

    let openGitToolWindow = (opts = {}) => {
        // Use new tool window system - Git is on the bottom bar
        toggleToolWindowPanel('gitToolPanel', 'bottom');
    };

    let openCommitToolWindow = () => {
        openGitToolWindow();
        const msg = document.getElementById('gitCommitMessage');
        msg?.focus();
    };

    function openGitPanel() {
        openGitToolWindow();
    }

    function closeGitPanel() {
        // Switch back to terminal in bottom panel
        toggleToolWindowPanel('terminalPanel', 'bottom');
    }

    const gitOutputGlobal = (text) => {
        const out = document.getElementById('gitOutput');
        if (out) {
            out.textContent += `${text}\n`;
            out.scrollTop = out.scrollHeight;
        }
    };

    async function runGitQuickCmd(cmd, { toastLabel = 'Git', silent = false } = {}) {
        logger.info('GIT_COMMAND', { cmd, toastLabel });
        if (!silent) gitOutputGlobal(`$ ${cmd}`);
        const res = await window.ahmadIDE.git(cmd);
        if (res.ok) {
            if (!silent) {
                if (res.stdout) gitOutputGlobal(res.stdout);
                if (res.stderr) gitOutputGlobal(res.stderr);
            }
            logger.info('GIT_COMMAND_SUCCESS', { cmd, stdout: res.stdout?.slice(0, 200), stderr: res.stderr?.slice(0, 200) });
        } else {
            const message = normalizeGitError(res.error || res.stderr);
            if (!silent) gitOutputGlobal(`✗ ${message}`);
            showToast('error', toastLabel, message);
            logger.error('GIT_COMMAND_FAIL', { cmd, error: message, stderr: res.stderr });
        }
        return res;
    }

    async function runGitContextAction(action, path) {
        const target = path || '.';
        const safe = target.replace(/"/g, '\\"');
        const setPath = (val) => {
            const input = document.getElementById('gitDiffPath');
            if (input) input.value = val;
        };
        const focusCommit = () => {
            const msg = document.getElementById('gitCommitMessage');
            msg?.focus();
        };
        const refresh = () => document.getElementById('gitStatusBtn')?.click();
        logger.info('GIT_CONTEXT_ACTION', { action, target });

        switch (action) {
            case 'add':
                openGitToolWindow();
                await runGitQuickCmd(`git add -- "${safe}"`, { toastLabel: 'Git Add' });
                refresh();
                return;
            case 'commit':
                openCommitToolWindow();
                setPath(target);
                await runGitQuickCmd(`git add -- "${safe}"`, { toastLabel: 'Commit File' });
                showToast('info', 'Commit File', 'File staged. Enter a commit message in Git tool window.');
                focusCommit();
                refresh();
                return;
            case 'history':
                openGitToolWindow();
                setPath(target);
                await runGitQuickCmd(`git log --oneline -- "${safe}"`, { toastLabel: 'Git History' });
                document.getElementById('gitLogBtn')?.click();
                return;
            case 'compare':
                openGitToolWindow();
                setPath(target);
                document.getElementById('gitDiffFileBtn')?.click();
                return;
            case 'rollback':
                openGitToolWindow();
                await runGitQuickCmd(`git checkout -- "${safe}"`, { toastLabel: 'Rollback' });
                refresh();
                return;
            default:
                showToast('info', 'UNKNOWN – NEED DESIGN DECISION', `Git action ${action} not wired`);
        }
    }

    function toggleSidebar() {
        // Toggle the left tool window (Project panel) using new PhpStorm-style layout
        toggleToolWindowPanel('projectPanel', 'left');
    }

    function toggleTerminal() {
        if (globalTerminalState && !globalTerminalState.tabs.length) {
            addTerminalTab(globalTerminalState, true);
        }
        // Toggle the bottom tool window (Terminal panel) using new PhpStorm-style layout
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
        const clickEl = (id) => {
            if ($) {
                const $el = $('#' + id);
                if ($el && $el.length) $el.trigger('click');
            } else {
                const el = document.getElementById(id);
                el?.click();
            }
        };

        const notImplemented = (label) => {
            showToast('info', 'Not implemented', `${label || 'This action'} is not implemented yet.`);
        };

        const runMenuAction = async (action) => {
            logger.info('MENU_ACTION', { action });
            switch (action) {
                case 'save':
                case 'save-all':
                    clickEl('saveRoutineBtn');
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
                    editor?.trigger('keyboard', 'actions.find', null);
                    return;
                case 'replace':
                    editor?.trigger('keyboard', 'editor.action.startFindReplaceAction', null);
                    return;
                case 'find-in-folder':
                    openFindReplaceDialog('find', getSelectedText());
                    return;
                case 'replace-in-folder':
                    openFindReplaceDialog('replace', getSelectedText());
                    return;
                case 'search-everywhere':
                    openSearchEverywhere('');
                    return;
                case 'comment':
                    editor?.trigger('keyboard', 'editor.action.commentLine', null);
                    return;
                case 'duplicate-line':
                    duplicateLine(editor);
                    return;
                case 'goto-line':
                    editor?.trigger('keyboard', 'editor.action.gotoLine', null);
                    return;
                case 'goto-file':
                    openSearchEverywhere('');
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
                case 'reformat':
                    editor?.trigger('keyboard', 'editor.action.formatDocument', null);
                    return;
                case 'rename':
                    editor?.trigger('keyboard', 'editor.action.rename', null);
                    return;
                case 'lint':
                    clickEl('lintBtn');
                    return;
                case 'toggle-sidebar':
                    toggleSidebar();
                    return;
                case 'toggle-terminal':
                    toggleTerminal();
                    return;
                case 'terminal':
                    if (!terminalState.tabs.length) {
                        await addTerminalTab(terminalState, true);
                    }
                    toggleToolWindowPanel('terminalPanel', 'bottom');
                    setTimeout(() => {
                        refreshTerminalLayout(terminalState);
                        focusTerminal();
                    }, 50);
                    return;
                case 'connections':
                    if (window.AhmadIDEModules?.app?.dialogRegistry?.open('connections')) {
                        return;
                    }
                    clickEl('toggleConnections');
                    return;
                case 'extensions':
                    toggleToolWindowPanel('extensionsPanel', 'bottom');
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
                    window.AhmadIDEModules?.app?.dialogRegistry?.open('new-project');
                    return;
                case 'open-project':
                    window.AhmadIDEModules?.app?.dialogRegistry?.open('open-project');
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
                    $('#gitStatusBtn').trigger('click');
                    return;
                case 'git-diff':
                    openGitToolWindow();
                    $('#gitDiffBtn').trigger('click');
                    return;
                case 'git-history':
                    openGitToolWindow();
                    $('#gitLogBtn').trigger('click');
                    return;
                case 'about':
                    window.AhmadIDEModules?.app?.dialogRegistry?.open('about') || notImplemented('About');
                    return;
                case 'find':
                    window.AhmadIDEModules?.app?.dialogRegistry?.open('find') || notImplemented('Find');
                    return;
                case 'replace':
                    window.AhmadIDEModules?.app?.dialogRegistry?.open('replace') || notImplemented('Replace');
                    return;
                case 'goto-file':
                    window.AhmadIDEModules?.app?.dialogRegistry?.open('goto-file') || notImplemented('Go to File');
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
                    notImplemented(action);
            }
        };

        const buildMenuBar = () => {
            const host = document.getElementById('mainMenu');
            if (!host) {
                console.error('Menu host element #mainMenu not found');
                return;
            }

            const menuRegistry = window.AhmadIDEModules?.app?.menuRegistry;
            const createMenuController = window.AhmadIDEModules?.ui?.menu?.createMenuController;
            const createMenuBar = window.AhmadIDEModules?.ui?.menu?.createMenuBar;

            if (!menuRegistry || !createMenuController || !createMenuBar) {
                console.error('Menu system not loaded. Missing:', {
                    menuRegistry: !!menuRegistry,
                    createMenuController: !!createMenuController,
                    createMenuBar: !!createMenuBar
                });
                return;
            }

            const menus = menuRegistry.get('menubar');
            if (!menus || menus.length === 0) {
                console.error('No menubar menus found in registry');
                return;
            }

            const controller = createMenuController({});
            const menuBar = createMenuBar({
                host,
                menus,
                controller,
                onAction: async (action) => {
                    await runMenuAction(action);
                },
                getContext: () => ({
                    toolWindows: {
                        leftVisible: !document.getElementById('leftToolWindow')?.classList?.contains('hidden'),
                        bottomVisible: !document.getElementById('bottomToolWindow')?.classList?.contains('hidden')
                    }
                })
            });

            menuBar.mount();
            console.log('[MenuBar] Mounted with', menus.length, 'menus');
        };

        buildMenuBar();
    }

    function bindGlobalShortcuts() {
        const normalizeCombo = (e) => {
            const parts = [];
            if (e.ctrlKey || e.metaKey) parts.push('ctrl');
            if (e.shiftKey) parts.push('shift');
            if (e.altKey) parts.push('alt');
            const key = (e.key || '').toLowerCase();
            if (key === ' ') parts.push('space');
            else parts.push(key);
            return parts.join('+');
        };

        const isEditableTarget = (el) => {
            if (!el) return false;
            const inTerminal = !!el.closest?.('#terminalViewport') || !!el.closest?.('.xterm');
            if (inTerminal) return terminalConfig.overrideIdeShortcuts;
            const tag = (el.tagName || '').toLowerCase();
            const editable = el.isContentEditable;
            return editable || tag === 'input' || tag === 'textarea' || tag === 'select';
        };

        const handler = async (e) => {
            const combo = normalizeCombo(e);
            const match = coreShortcutMap[combo];
            if (!match) return;
            if (isEditableTarget(e.target) && combo !== 'ctrl+s' && combo !== 'ctrl+shift+s') return;

            // Handle Ctrl+S directly to avoid conflicts with Monaco
            if (combo === 'ctrl+s' || combo === 'ctrl+shift+s') {
                e.preventDefault();
                e.stopPropagation();
                if (globalRoutineState && globalTerminalState && activeEditor) {
                    await saveRoutineFlow(activeEditor, globalRoutineState, globalTerminalState);
                }
                return;
            }

            e.preventDefault();
            e.stopImmediatePropagation();
            await runMenuAction(match.action);
        };

        window.addEventListener('keydown', handler, true);
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

    // PhpStorm-style tool window management
    const toolWindowState = {
        left: { visible: true, activePanel: 'projectPanel' },
        right: { visible: false, activePanel: null },
        bottom: { visible: true, activePanel: 'terminalPanel' }
    };

    function ensureBottomPanel(panelId) {
        const state = toolWindowState.bottom;
        if (!state) return;
        if (state.visible && state.activePanel === panelId) return;
        toggleToolWindowPanel(panelId, 'bottom');
    }

    function setActiveToolWindow(panelId) {
        // Legacy support - map to new structure
        const btn = document.querySelector(`.tool-window-stripe-btn[data-panel="${panelId}"]`);
        if (btn) {
            const position = btn.getAttribute('data-position');
            toggleToolWindowPanel(panelId, position);
        }
    }

    function toggleToolWindowPanel(panelId, position) {
        const state = toolWindowState[position];
        if (!state) return;
        logger.info('TOOLWINDOW_TOGGLE', { panelId, position, visible: state.visible, activePanel: state.activePanel });

        const contentArea = document.getElementById(`${position}ToolWindow`);
        const buttons = document.querySelectorAll(`.tool-window-stripe-btn[data-position="${position}"]`);

        // If clicking the same panel, toggle visibility (bottom keeps stripe visible)
        if (state.activePanel === panelId && state.visible) {
            state.visible = false;
            state.activePanel = null;
            if (position === 'bottom') {
                if (contentArea) contentArea.classList.add('hidden'); // Use hidden to collapse fully
                const bottomPanels = ['terminalPanel', 'debugPanel', 'problemsPanel', 'servicesPanel', 'gitToolPanel', 'extensionsPanel'];
                bottomPanels.forEach(id => {
                    const el = document.getElementById(id);
                    if (el) el.classList.add('hidden');
                });
            } else {
                if (contentArea) contentArea.classList.add('hidden');
            }
            buttons.forEach(b => b.classList.remove('active'));
            return;
        }

        // Lazy-mount heavy panels on first open to reduce DOM/layout/paint cost.
        try {
            window.AhmadIDEModules?.app?.featureRegistry?.ensureById?.(panelId);
        } catch (_) {
            // ignore mount failures
        }

        // Show the content area and switch panels
        state.visible = true;
        state.activePanel = panelId;
        if (contentArea) {
            contentArea.classList.remove('hidden');
            if (position === 'bottom') {
                contentArea.classList.remove('collapsed');
            }
        }

        // Update button states
        buttons.forEach(b => {
            b.classList.toggle('active', b.getAttribute('data-panel') === panelId);
        });

        // Show/hide panels within the content area
        const panels = contentArea ? contentArea.querySelectorAll('.tool-window-panel') : [];
        panels.forEach(p => {
            p.classList.toggle('hidden', p.id !== panelId);
        });

        // Handle bottom panels specially (they're in bottom-panels-container)
        if (position === 'bottom') {
            const bottomPanels = ['terminalPanel', 'debugPanel', 'problemsPanel', 'servicesPanel', 'gitToolPanel', 'extensionsPanel'];
            bottomPanels.forEach(id => {
                const el = document.getElementById(id);
                if (el) el.classList.toggle('hidden', id !== panelId);
            });
        }

        // Special handling for problems panel
        if (panelId === 'problemsPanel') {
            const src = document.getElementById('problemsList');
            const dst = document.getElementById('problemsListStandalone');
            if (src && dst) dst.innerHTML = src.innerHTML;
        }

        if (panelId === 'terminalPanel' && state.visible && globalTerminalState) {
            refreshTerminalLayout(globalTerminalState);
            setTimeout(() => focusTerminal(), 10);
        }
    }

    function bindToolWindows() {
        // Bind new PhpStorm-style tool window stripe buttons
        const stripeButtons = document.querySelectorAll('.tool-window-stripe-btn');
        stripeButtons.forEach(btn => {
            btn.addEventListener('click', () => {
                const panelId = btn.getAttribute('data-panel');
                const position = btn.getAttribute('data-position');
                toggleToolWindowPanel(panelId, position);
            });
        });

        // Bind hide buttons in tool window headers
        document.querySelectorAll('.hide-panel-btn, #hideProjectBtn').forEach(btn => {
            btn.addEventListener('click', () => {
                const panel = btn.closest('.tool-window-panel');
                if (panel) {
                    const panelId = panel.id;
                    const stripeBtn = document.querySelector(`.tool-window-stripe-btn[data-panel="${panelId}"]`);
                    if (stripeBtn) {
                        const position = stripeBtn.getAttribute('data-position');
                        toggleToolWindowPanel(panelId, position);
                    }
                }
            });
        });

        // Initialize: show left (project) and bottom (terminal) by default
        toolWindowState.left.visible = true;
        toolWindowState.left.activePanel = 'projectPanel';
        toolWindowState.bottom.visible = true;
        toolWindowState.bottom.activePanel = 'terminalPanel';
        toolWindowState.right.visible = false;

        // Set initial UI state
        document.getElementById('leftToolWindow')?.classList.remove('hidden');
        document.getElementById('rightToolWindow')?.classList.add('hidden');
        document.getElementById('bottomToolWindow')?.classList.remove('hidden');

        // Hide non-active bottom panels
        ['debugPanel', 'problemsPanel', 'servicesPanel', 'gitToolPanel'].forEach(id => {
            const el = document.getElementById(id);
            if (el) el.classList.add('hidden');
        });

        // Legacy binding for old toolwindow-btn (if any still exist)
        const legacyButtons = document.querySelectorAll('.toolwindow-btn');
        legacyButtons.forEach(btn => {
            btn.addEventListener('click', () => {
                const target = btn.getAttribute('data-panel');
                setActiveToolWindow(target);
            });
        });
    }

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
                fr?.onMounted?.('debugPanel', () => bindDebugTabs());
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
                // Performance optimizations
                renderValidationDecorations: 'on',
                quickSuggestions: { other: true, comments: false, strings: false },
                suggestOnTriggerCharacters: true,
                folding: false,  // Disable code folding for performance
                foldingHighlight: false,
                showFoldingControls: 'never',
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
                guides: { indentation: false, bracketPairs: false },  // Disable guides
                accessibilitySupport: 'off',  // Reduce accessibility overhead
                cursorSmoothCaretAnimation: 'off',  // Disable cursor animation
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
                lineDecorationsWidth: 0,  // Reduce line decoration overhead
                lineNumbersMinChars: 3  // Reduce gutter width
            });
            activeEditor = editor;

            // Initialize states BEFORE adding editor actions (so Ctrl+S can access them)
            routineState = { current: null };
            routineStateRef = routineState;
            globalRoutineState = routineState;
            const terminalState = createTerminalState();
            globalTerminalState = terminalState;
            updateTerminalStatusPill();

            // Add PhpStorm-style context menu actions
            editor.addAction({
                id: 'cut',
                label: 'Cut',
                keybindings: [monaco.KeyMod.CtrlCmd | monaco.KeyCode.KeyX],
                run: (ed) => {
                    document.execCommand('cut');
                }
            });

            editor.addAction({
                id: 'copy',
                label: 'Copy',
                keybindings: [monaco.KeyMod.CtrlCmd | monaco.KeyCode.KeyC],
                run: (ed) => {
                    document.execCommand('copy');
                }
            });

            editor.addAction({
                id: 'paste',
                label: 'Paste',
                keybindings: [monaco.KeyMod.CtrlCmd | monaco.KeyCode.KeyV],
                run: (ed) => {
                    document.execCommand('paste');
                }
            });

            editor.addAction({
                id: 'selectAll',
                label: 'Select All',
                keybindings: [monaco.KeyMod.CtrlCmd | monaco.KeyCode.KeyA],
                run: (ed) => {
                    ed.setSelection(ed.getModel().getFullModelRange());
                }
            });

            // Note: Ctrl+S is handled by global keydown handler to avoid conflicts

            editor.addAction({
                id: 'commentLine',
                label: 'Comment Line',
                keybindings: [monaco.KeyMod.CtrlCmd | monaco.KeyCode.Slash],
                run: (ed) => {
                    ed.trigger('keyboard', 'editor.action.commentLine', {});
                }
            });

            editor.addAction({
                id: 'formatDocument',
                label: 'Reformat Code',
                keybindings: [monaco.KeyMod.CtrlCmd | monaco.KeyMod.Alt | monaco.KeyCode.KeyL],
                run: (ed) => {
                    ed.trigger('keyboard', 'editor.action.formatDocument', {});
                }
            });

            editor.addAction({
                id: 'gotoLine',
                label: 'Go to Line...',
                keybindings: [monaco.KeyMod.CtrlCmd | monaco.KeyCode.KeyG],
                run: (ed) => {
                    ed.trigger('keyboard', 'editor.action.gotoLine', {});
                }
            });

            editor.addAction({
                id: 'findReplace',
                label: 'Find and Replace...',
                keybindings: [monaco.KeyMod.CtrlCmd | monaco.KeyCode.KeyR],
                run: (ed) => {
                    ed.trigger('keyboard', 'editor.action.startFindReplaceAction', {});
                }
            });

            editor.addAction({
                id: 'deleteLineAction',
                label: 'Delete Line',
                keybindings: [monaco.KeyMod.CtrlCmd | monaco.KeyCode.KeyY],
                run: (ed) => {
                    ed.trigger('keyboard', 'editor.action.deleteLines', {});
                }
            });

            editor.addAction({
                id: 'duplicateLine',
                label: 'Duplicate Line',
                keybindings: [monaco.KeyMod.CtrlCmd | monaco.KeyCode.KeyD],
                run: (ed) => {
                    const selection = ed.getSelection();
                    const lineNumber = selection.startLineNumber;
                    const lineContent = ed.getModel().getLineContent(lineNumber);
                    const position = { lineNumber: lineNumber, column: 1 };
                    ed.executeEdits('duplicate-line', [{
                        range: new monaco.Range(lineNumber, 1, lineNumber, 1),
                        text: lineContent + '\n'
                    }]);
                }
            });

            editor.addAction({
                id: 'moveLineUp',
                label: 'Move Line Up',
                keybindings: [monaco.KeyMod.Alt | monaco.KeyMod.Shift | monaco.KeyCode.UpArrow],
                run: (ed) => {
                    ed.trigger('keyboard', 'editor.action.moveLinesUpAction', {});
                }
            });

            editor.addAction({
                id: 'moveLineDown',
                label: 'Move Line Down',
                keybindings: [monaco.KeyMod.Alt | monaco.KeyMod.Shift | monaco.KeyCode.DownArrow],
                run: (ed) => {
                    ed.trigger('keyboard', 'editor.action.moveLinesDownAction', {});
                }
            });

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
            const shortcutPrefs = loadShortcutPrefs();
            shortcutDefaults = {
                // Existing shortcuts
                'duplicate-line': monaco.KeyMod.CtrlCmd | monaco.KeyCode.KeyD,
                'run-code': monaco.KeyMod.CtrlCmd | monaco.KeyCode.Enter,
                'lint-code': monaco.KeyMod.CtrlCmd | monaco.KeyCode.KeyL,
                'toggle-sidebar': monaco.KeyMod.CtrlCmd | monaco.KeyCode.KeyB,
                'toggle-terminal': monaco.KeyMod.CtrlCmd | monaco.KeyCode.KeyJ,
                'new-terminal': monaco.KeyMod.CtrlCmd | monaco.KeyMod.Shift | monaco.KeyCode.KeyT,

                // PhpStorm Navigation shortcuts
                'goto-file': monaco.KeyMod.CtrlCmd | monaco.KeyMod.Shift | monaco.KeyCode.KeyN,
                'goto-line': monaco.KeyMod.CtrlCmd | monaco.KeyCode.KeyG,
                'recent-files': monaco.KeyMod.CtrlCmd | monaco.KeyCode.KeyE,

                // PhpStorm Editing shortcuts
                'delete-line': monaco.KeyMod.CtrlCmd | monaco.KeyCode.KeyY,
                'comment-line': monaco.KeyMod.CtrlCmd | monaco.KeyCode.Slash,
                'extend-selection': monaco.KeyMod.CtrlCmd | monaco.KeyCode.KeyW,

                // PhpStorm Search shortcuts
                'find': monaco.KeyMod.CtrlCmd | monaco.KeyCode.KeyF,
                'replace': monaco.KeyMod.CtrlCmd | monaco.KeyCode.KeyR,
                'find-in-folder': monaco.KeyMod.CtrlCmd | monaco.KeyMod.Shift | monaco.KeyCode.KeyF,
                'replace-in-folder': monaco.KeyMod.CtrlCmd | monaco.KeyMod.Shift | monaco.KeyCode.KeyR,

                // PhpStorm Code shortcuts
                'format-code': monaco.KeyMod.CtrlCmd | monaco.KeyMod.Alt | monaco.KeyCode.KeyL,
                'optimize-imports': monaco.KeyMod.CtrlCmd | monaco.KeyMod.Alt | monaco.KeyCode.KeyO,

                // PhpStorm Refactoring
                'rename': monaco.KeyCode.F2,

                // PhpStorm Tool Windows (Alt+Number)
                'tool-project': monaco.KeyMod.Alt | monaco.KeyCode.Digit1,
                'tool-favorites': monaco.KeyMod.Alt | monaco.KeyCode.Digit2,
                'tool-find': monaco.KeyMod.Alt | monaco.KeyCode.Digit3,
                'tool-run': monaco.KeyMod.Alt | monaco.KeyCode.Digit4,
                'tool-debug': monaco.KeyMod.Alt | monaco.KeyCode.Digit5,
                'tool-todo': monaco.KeyMod.Alt | monaco.KeyCode.Digit6, // Problems
                'tool-structure': monaco.KeyMod.Alt | monaco.KeyCode.Digit7,
                'tool-services': monaco.KeyMod.Alt | monaco.KeyCode.Digit8,
                'tool-git': monaco.KeyMod.Alt | monaco.KeyCode.Digit9,
                'tool-terminal': monaco.KeyMod.Alt | monaco.KeyCode.F12
            };
            const dupBinding = shortcutPrefs['duplicate-line'] || shortcutDefaults['duplicate-line'];
            const dupHandler = () => duplicateLine(editor);
            editor.addCommand(dupBinding, dupHandler);
            registerKeybinding(editor, 'Duplicate Line', 'duplicate-line', dupHandler, shortcutDefaults['duplicate-line']);
            registerKeybinding(editor, 'Run', 'run-code', () => clickEl('runBtn'), shortcutDefaults['run-code']);
            registerKeybinding(editor, 'Lint', 'lint-code', () => clickEl('lintBtn'), shortcutDefaults['lint-code']);
            registerKeybinding(editor, 'Toggle Sidebar', 'toggle-sidebar', () => toggleSidebar(), shortcutDefaults['toggle-sidebar']);
            registerKeybinding(editor, 'Toggle Terminal', 'toggle-terminal', () => toggleTerminal(), shortcutDefaults['toggle-terminal']);
            registerKeybinding(editor, 'New Terminal', 'new-terminal', () => document.getElementById('terminalNewTabBtn')?.click(), shortcutDefaults['new-terminal']);

            // PhpStorm Navigation shortcuts
            registerKeybinding(editor, 'Go to File', 'goto-file', () => openSearchEverywhere(''), shortcutDefaults['goto-file']);
            registerKeybinding(editor, 'Go to Line', 'goto-line', () => editor.trigger('keyboard', 'editor.action.gotoLine', null), shortcutDefaults['goto-line']);
            registerKeybinding(editor, 'Recent Files', 'recent-files', () => showToast('info', 'Recent Files', 'Feature coming soon'), shortcutDefaults['recent-files']);

            // PhpStorm Editing shortcuts
            registerKeybinding(editor, 'Delete Line', 'delete-line', () => editor.trigger('keyboard', 'editor.action.deleteLines', null), shortcutDefaults['delete-line']);
            registerKeybinding(editor, 'Comment Line', 'comment-line', () => editor.trigger('keyboard', 'editor.action.commentLine', null), shortcutDefaults['comment-line']);
            registerKeybinding(editor, 'Extend Selection', 'extend-selection', () => editor.trigger('keyboard', 'editor.action.smartSelect.expand', null), shortcutDefaults['extend-selection']);

            // PhpStorm Search shortcuts
            registerKeybinding(editor, 'Find', 'find', () => editor.trigger('keyboard', 'actions.find', null), shortcutDefaults['find']);
            registerKeybinding(editor, 'Replace', 'replace', () => editor.trigger('keyboard', 'editor.action.startFindReplaceAction', null), shortcutDefaults['replace']);
            registerKeybinding(editor, 'Find in Files', 'find-in-folder', () => openFindReplaceDialog('find', getSelectedText()), shortcutDefaults['find-in-folder']);
            registerKeybinding(editor, 'Replace in Files', 'replace-in-folder', () => openFindReplaceDialog('replace', getSelectedText()), shortcutDefaults['replace-in-folder']);

            // PhpStorm Code shortcuts
            registerKeybinding(editor, 'Format Code', 'format-code', () => editor.trigger('keyboard', 'editor.action.formatDocument', null), shortcutDefaults['format-code']);
            registerKeybinding(editor, 'Optimize Imports', 'optimize-imports', () => showToast('info', 'Optimize Imports', 'Feature coming soon'), shortcutDefaults['optimize-imports']);

            // PhpStorm Refactoring
            registerKeybinding(editor, 'Rename', 'rename', () => editor.trigger('keyboard', 'editor.action.rename', null), shortcutDefaults['rename']);

            // PhpStorm Tool Windows (Alt+Number to toggle panels)
            registerKeybinding(editor, 'Tool: Project', 'tool-project', () => toggleToolWindowPanel('projectPanel', 'left'), shortcutDefaults['tool-project']);
            registerKeybinding(editor, 'Tool: Find', 'tool-find', () => openFindReplaceDialog('find', ''), shortcutDefaults['tool-find']);
            registerKeybinding(editor, 'Tool: Run', 'tool-run', () => toggleToolWindowPanel('terminalPanel', 'bottom'), shortcutDefaults['tool-run']);
            registerKeybinding(editor, 'Tool: Debug', 'tool-debug', () => toggleToolWindowPanel('debugPanel', 'bottom'), shortcutDefaults['tool-debug']);
            registerKeybinding(editor, 'Tool: Problems', 'tool-todo', () => toggleToolWindowPanel('problemsPanel', 'bottom'), shortcutDefaults['tool-todo']);
            registerKeybinding(editor, 'Tool: Structure', 'tool-structure', () => toggleToolWindowPanel('structurePanel', 'left'), shortcutDefaults['tool-structure']);
            registerKeybinding(editor, 'Tool: Services', 'tool-services', () => toggleToolWindowPanel('servicesPanel', 'bottom'), shortcutDefaults['tool-services']);
            registerKeybinding(editor, 'Tool: Git', 'tool-git', () => toggleToolWindowPanel('gitToolPanel', 'bottom'), shortcutDefaults['tool-git']);
            registerKeybinding(editor, 'Tool: Terminal', 'tool-terminal', () => toggleToolWindowPanel('terminalPanel', 'bottom'), shortcutDefaults['tool-terminal']);

            renderProjectTree([], routineState, editor);
            // Initialize empty tab bar
            renderTabs();
            bindTabKeyboardShortcuts();
            // PhpStorm-like editor context menu
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
                }, 100); // Debounce 100ms
            });
            try {
                window.AhmadIDEModules?.services?.extensionsService?.start?.();
            } catch (_) { }
            initExtensionsView();

            // PhpStorm-style search bindings
            updateFindScopeLabels();
            document.getElementById('findReplaceToggleBtn')?.addEventListener('click', () => {
                const nextMode = findReplaceState.mode === 'replace' ? 'find' : 'replace';
                toggleFindMode(nextMode);
                executeFindReplacePreview(false);
            });
            document.getElementById('closeFindDialog')?.addEventListener('click', closeFindReplaceDialog);
            // Legacy overlay removed - dialog handles backdrop clicks
            document.getElementById('replaceAllBtn')?.addEventListener('click', confirmAndReplaceAll);
            document.getElementById('findQueryInput')?.addEventListener('input', () => searchDebounce(() => executeFindReplacePreview(false), 200));
            document.getElementById('findQueryInput')?.addEventListener('keydown', (e) => {
                if (e.key === 'Enter') executeFindReplacePreview(false);
                if (e.key === 'Escape') closeFindReplaceDialog();
            });
            document.getElementById('replaceQueryInput')?.addEventListener('input', () => {
                if (findReplaceState.mode === 'replace') {
                    searchDebounce(() => executeFindReplacePreview(false), 280);
                }
            });
            document.getElementById('replaceQueryInput')?.addEventListener('keydown', (e) => {
                if (e.key === 'Enter') executeFindReplacePreview(e.ctrlKey || e.metaKey);
                if (e.key === 'Escape') closeFindReplaceDialog();
            });
            ['findCaseOption', 'findWholeOption', 'findRegexOption'].forEach(id => {
                document.getElementById(id)?.addEventListener('change', () => executeFindReplacePreview(false));
            });
            document.getElementById('findDialog')?.addEventListener('keydown', (e) => {
                if (e.key === 'Enter') {
                    if (findReplaceState.mode === 'replace' && e.ctrlKey) {
                        confirmAndReplaceAll();
                    } else {
                        executeFindReplacePreview(false);
                    }
                }
                if (e.key === 'Escape') closeFindReplaceDialog();
            });

            // Legacy overlay removed - dialog handles backdrop clicks
            const searchEverywhereInput = document.getElementById('searchEverywhereInput');
            searchEverywhereInput?.addEventListener('input', () => renderSearchEverywhereResults(searchEverywhereInput.value));
            searchEverywhereInput?.addEventListener('keydown', async (e) => {
                const resultsHost = document.getElementById('searchEverywhereResults');
                const items = resultsHost?.querySelectorAll('.search-everywhere-item') || [];
                if (e.key === 'ArrowDown') {
                    e.preventDefault();
                    searchEverywhereState.selectedIndex = Math.min(items.length - 1, searchEverywhereState.selectedIndex + 1);
                    renderSearchEverywhereResults(searchEverywhereInput.value);
                } else if (e.key === 'ArrowUp') {
                    e.preventDefault();
                    searchEverywhereState.selectedIndex = Math.max(0, searchEverywhereState.selectedIndex - 1);
                    renderSearchEverywhereResults(searchEverywhereInput.value);
                } else if (e.key === 'Enter') {
                    e.preventDefault();
                    const refreshedItems = document.getElementById('searchEverywhereResults')?.querySelectorAll('.search-everywhere-item') || [];
                    const active = refreshedItems[searchEverywhereState.selectedIndex];
                    const path = active?.dataset?.path;
                    if (path) openSearchEverywhereResult(path);
                } else if (e.key === 'Escape') {
                    closeSearchEverywhere();
                }
            });

            let nativeSearchShortcutHandler = null;
            const bindSearchShortcuts = () => {
                const handler = (e) => {
                    const key = (e.key || '').toLowerCase();
                    const targetTag = (e.target && e.target.tagName) ? e.target.tagName.toLowerCase() : '';
                    const inTextField = targetTag === 'input' || targetTag === 'textarea' || (e.target && e.target.isContentEditable);
                    if (key !== 'shift') lastShiftTap = 0;

                    if ((e.ctrlKey || e.metaKey) && e.shiftKey && !e.altKey) {
                        if (key === 'f') {
                            e.preventDefault();
                            openFindReplaceDialog('find', getSelectedText());
                            return;
                        }
                        if (key === 'r') {
                            e.preventDefault();
                            openFindReplaceDialog('replace', getSelectedText());
                            return;
                        }
                    }

                    if (!e.ctrlKey && !e.metaKey && !e.altKey && key === 'shift') {
                        if (e.repeat) return;
                        const now = Date.now();
                        if (now - lastShiftTap <= 420) {
                            lastShiftTap = 0;
                            openSearchEverywhere('');
                            return;
                        }
                        lastShiftTap = now;
                        return;
                    }

                    if (key === 'escape') {
                        closeFindReplaceDialog();
                        closeSearchEverywhere();
                    }
                };

                if ($) {
                    $(window).off('keydown.search-shortcuts');
                    $(window).on('keydown.search-shortcuts', handler);
                } else {
                    if (nativeSearchShortcutHandler) {
                        window.removeEventListener('keydown', nativeSearchShortcutHandler, true);
                    }
                    nativeSearchShortcutHandler = handler;
                    window.addEventListener('keydown', nativeSearchShortcutHandler, true);
                }
            };

            bindSearchShortcuts();
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

            // --- Status Bar Updates ---
            const updateStatusBar = () => {
                const position = editor.getPosition();
                if (position) {
                    const lineCol = document.getElementById('lineColInfo');
                    if (lineCol) {
                        lineCol.textContent = `Ln ${position.lineNumber}, Col ${position.column}`;
                    }
                }
            };

            // Update status bar on cursor position change
            editor.onDidChangeCursorPosition(updateStatusBar);
            updateStatusBar(); // Initial update

            const setBranchDisplay = (branchName) => {
                const label = (branchName && branchName.trim()) ? branchName.trim() : 'Git';
                const branchEl = document.getElementById('gitBranch');
                if (branchEl) {
                    branchEl.innerHTML = `<span class="icon">⎇</span> ${label}`;
                }
                const vcsToggle = document.getElementById('vcsWidgetBtn');
                if (vcsToggle) {
                    const hint = label === 'Git' ? 'Git' : `Git (${label})`;
                    vcsToggle.title = hint;
                    vcsToggle.setAttribute('aria-label', hint);
                }
            };

            const fetchCurrentBranch = async () => {
                if (currentProject && currentProject.projectPath) {
                    const branchRes = await window.ahmadIDE.git('git branch --show-current');
                    if (branchRes?.ok && branchRes.stdout) {
                        const name = branchRes.stdout.trim();
                        setBranchDisplay(name || 'Git');
                        return name || 'Git';
                    }
                }
                return null;
            };

            // Update Git branch if in a git repo
            fetchCurrentBranch();

            // --- Env info ---
            const envInfo = await window.ahmadIDE.getEnv();
            envInfoCache = envInfo;
            document.getElementById('envInfo').textContent =
                `${envInfo.platform} | electron ${envInfo.versions.electron}`;
            setConnStatus('Docker (local)', 'info');
            wireMenuBar(editor, routineState, terminalState);

            // --- Run & Debug buttons ---
            const runConfigState = {
                active: 'run-current',
                labels: {
                    'run-current': 'Current file (Run)',
                    'debug-current': 'Current file (Debug)'
                }
            };

            const runConfigMenu = document.getElementById('runConfigMenu');
            if (runConfigMenu) runConfigMenu.remove();
            const runConfigBtn = document.getElementById('runConfigBtn');
            const runBtnEl = document.getElementById('runBtn');
            const debugStartBtnEl = document.getElementById('debugStartBtn');

            const setRunConfig = (id) => {
                if (!runConfigState.labels[id]) return;
                runConfigState.active = id;
                const label = runConfigState.labels[id];
                if (runConfigBtn) {
                    runConfigBtn.title = label;
                    runConfigBtn.setAttribute('aria-label', label);
                }
                if (runBtnEl) {
                    runBtnEl.title = `${label} (Ctrl+Enter)`;
                }
                if (runConfigMenu) {
                    runConfigMenu.querySelectorAll('.run-config-item').forEach((item) => {
                        const cfg = item.getAttribute('data-config');
                        item.classList.toggle('active', cfg === id);
                    });
                }
            };

            window.AhmadIDEModules = window.AhmadIDEModules || {};
            window.AhmadIDEModules.app = window.AhmadIDEModules.app || {};
            window.AhmadIDEModules.app.runConfig = window.AhmadIDEModules.app.runConfig || {};
            window.AhmadIDEModules.app.runConfig.getActive = () => runConfigState.active;
            window.AhmadIDEModules.app.runConfig.setActive = (id) => setRunConfig(id);
            window.AhmadIDEModules.app.refreshRoutines = async () => {
                try {
                    if (!routinesManager || !routineState || !editor) return false;
                    const search = document.getElementById('routineSearch')?.value || '';
                    await loadRoutineList(routineState, editor, search);
                    return true;
                } catch (_) {
                    return false;
                }
            };

            const shouldDebugForRun = () => {
                const bps = getBpLines();
                const hasBps = bps.length > 0;
                const debugArmed = dbgState.debugModeEnabled;
                const explicitDebugCfg = runConfigState.active === 'debug-current';
                // Only start debugger when it's armed (Debug button) AND a breakpoint exists,
                // or when explicitly selecting the debug run config with at least one breakpoint.
                return (debugArmed && hasBps) || (explicitDebugCfg && hasBps);
            };

            // Initialize defaults
            setRunConfig(runConfigState.active);

            // RUN: If debug mode enabled, start debugging. Otherwise, run normally.
            if ($) {
                $('#runBtn').on('click', async () => {
                    // Lint code - only errors block execution (warnings/info allowed)
                    const code = editor.getValue();
                    const linter = window._mumpsLinter || mumpsLinter;
                    if (hasLintRules(linter)) {
                        const lintResult = linter.lint(code || '', { mode: 'create' });
                        applyLintMarkers(editor.getModel(), lintResult.issues || []);
                        renderProblems((lintResult.issues || []).map(i => ({
                            message: i.message || i.description || '',
                            severity: i.severity || 'info',
                            line: i.line || null,
                            code: i.ruleId || i.code || null
                        })));
                        const summary = lintResult.summary || { errors: 0, warnings: 0, info: 0 };
                        if (summary.errors > 0) {
                            appendOutput(
                                `✗ Cannot run: ${summary.errors} error(s) found`,
                                terminalState
                            );
                            return;
                        }
                    } else {
                        // Fallback to existing marker check
                        const markers = monaco.editor.getModelMarkers({ owner: 'mumps-check' }) || [];
                        if (markers.length) {
                            const m = markers[0];
                            appendOutput(
                                `✗ Cannot run: ${m.message} (line ${m.startLineNumber})`,
                                terminalState
                            );
                            return;
                        }
                    }

                    const bpLines = getBpLines();
                    const debugActive = shouldDebugForRun();

                    // If debug mode enabled, check if session already ready
                    if (debugActive) {
                        // Check if debugger is already initialized and waiting
                        if (currentDebugSession && currentDebugSession.ready && !currentDebugSession.currentLine) {
                            // Debugger is ready, send Continue to start execution
                            logger.debug('[RUN] Debugger already ready, sending Continue command');
                            await debugContinue();
                        } else {
                            // Start new debug session
                            debugStartBtnEl?.classList.add('active');
                            dbgState.debugModeEnabled = true;
                            await startDebugSession(editor, dbgState, terminalState, debugBar, bpLines);
                        }
                    } else {
                        // Normal execution (no debugging)
                        const res = await runMumpsCode(editor, terminalState);
                        if (!res || !res.ok) {
                            await stopDebug(editor, dbgState, terminalState, debugBar);
                        }
                    }
                });

                // DEBUG BUTTON: arm/disarm debugger (Run starts it)
                $('#debugStartBtn').on('click', () => {
                    const $btn = $('#debugStartBtn');
                    // If a session is active, stop it
                    if (currentDebugSession && currentDebugSession.id) {
                        debugStop();
                        return;
                    }
                    dbgState.debugModeEnabled = !dbgState.debugModeEnabled;
                    if (dbgState.debugModeEnabled) {
                        const bpLines = getBpLines();
                        if (bpLines.length === 0) {
                            appendOutput('⚠️  Debug armed, but no breakpoints set. Run will execute normally.', terminalState);
                        } else {
                            appendOutput('✅ Debug armed. Click Run to start debugging.', terminalState);
                        }
                    } else {
                        appendOutput('🛑 Debug disarmed. Run will execute normally.', terminalState);
                    }
                    updateDebugButtonState();
                });
            }

            const clearBtn = document.getElementById('terminalClearBtn');
            const newTabBtn = document.getElementById('terminalNewTabBtn');
            const hideBtn = document.getElementById('terminalHideBtn');
            clearBtn?.addEventListener('click', () => clearOutput(terminalState));
            newTabBtn?.addEventListener('click', async () => await addTerminalTab(terminalState));
            hideBtn?.addEventListener('click', () => toggleToolWindowPanel('terminalPanel', 'bottom'));

            $('#saveRoutineBtn').on('click', async () => {
                await saveRoutineFlow(editor, routineState, terminalState);
            });
            $('#undoBtn').on('click', () => {
                editor.trigger('keyboard', 'undo', {});
            });
            $('#redoBtn').on('click', () => {
                editor.trigger('keyboard', 'redo', {});
            });
            $('#newRoutineBtn').on('click', async () => {
                await newRoutineFlow(editor, routineState, terminalState);
            });

            $('#lintBtn').on('click', async () => {
                const code = editor.getValue();
                appendOutput('🧹 Linting...', terminalState);
                const linter = window._mumpsLinter || mumpsLinter;
                if (hasLintRules(linter)) {
                    const res = linter.lint(code || '', { mode: 'edit' });
                    applyLintMarkers(editor.getModel(), res.issues || []);
                    renderProblems((res.issues || []).map(i => ({
                        message: i.message || i.description || '',
                        severity: i.severity || 'info',
                        line: i.line || null,
                        code: i.ruleId || i.code || null
                    })));
                    const summary = res.summary || { errors: 0, warnings: 0, info: 0 };
                    appendOutput(`✓ Lint: ${summary.errors} errors, ${summary.warnings} warnings, ${summary.info} info`, terminalState);
                } else {
                    const res = await window.ahmadIDE.lint(code);
                    if (res.ok) {
                        appendOutput(`✓ ${res.summary}`, terminalState);
                        renderProblems([{ message: res.summary, severity: 'info' }]);
                    } else {
                        appendOutput(`✗ Lint error: ${res.error || res.stderr}`, terminalState);
                        renderProblems([{ message: res.error || 'Lint failed', severity: 'error' }]);
                    }
                }
            });

            // --- SSH / Docker handling ---
            connectionsManager.wireConnectionsPanel({ editor, routineState, terminalState });

            document.getElementById('closeShortcutsBtn')?.addEventListener('click', closeShortcutsPanel);
            // Legacy overlay removed - dialog handles backdrop clicks
            document.getElementById('saveShortcutBtn')?.addEventListener('click', () => {
                const input = document.getElementById('shortcutInput');
                const select = document.getElementById('shortcutSelect');
                if (!input || !select) return;
                const actionId = select.value;
                const parsed = parseShortcutString(input.value);
                if (!parsed) {
                    appendOutput('✗ Invalid shortcut. Use format like Ctrl+D or Ctrl+Shift+L', terminalState);
                    return;
                }
                const prefs = loadShortcutPrefs();
                prefs[actionId] = parsed;
                persistShortcutPrefs(prefs);
                const target = registeredShortcuts.find(s => s.actionId === actionId);
                if (target) {
                    applyShortcutBinding(editor, actionId, parsed, target.handler);
                    target.binding = parsed;
                }
                appendOutput(`✓ Shortcut updated: ${actionId} -> ${input.value}`, terminalState);
                renderShortcutsPanel();
            });
            document.getElementById('expandAllBtn')?.addEventListener('click', () => setCollapseStateAll(false));
            document.getElementById('collapseAllBtn')?.addEventListener('click', () => setCollapseStateAll(true));
            document.getElementById('bpClearAllBtn')?.addEventListener('click', () => {
                dbgState.breakpoints?.clear();
                renderBreakpoints(dbgState);
                decorateBreakpoints(activeEditor, dbgState);
            });
            document.getElementById('gitClearBtn')?.addEventListener('click', () => {
                const out = document.getElementById('gitOutput');
                if (out) out.textContent = 'Git ready.';
            });
            settingsPanelManager.wireSettingsPanel();
            projectCreateManager.wireNewProjectPanel();

            const gitToolWindowApi = gitToolWindowManager.wireGitToolWindow({ fetchCurrentBranch });
            openGitToolWindow = gitToolWindowApi.openGitToolWindow;
            openCommitToolWindow = gitToolWindowApi.openCommitToolWindow;
            openGitPanel = gitToolWindowApi.openGitPanel;

            // --- Initial debug / UI state ---
            setDebugButtons(false);
            renderProjectTreeLoading('Loading routines…');
            loadRoutineList(routineState, editor).catch((err) => {
                console.error('ROUTINE_LIST_INIT_FAIL', err);
                renderProjectTree([], routineState, editor);
                showToast('error', 'Routines', err?.message || 'Failed to load routines');
            });
            // Make terminal init non-blocking to prevent freeze if xterm fails
            addTerminalTab(terminalState, true).catch(err => {
                console.warn('Terminal init failed (non-fatal):', err);
            });
            renderBreakpoints(dbgState);
            renderLocals({});
            renderStack([]);
            renderDebugConsole([]);
            resetDebugUI();
            ctrlHoverManager.bindCtrlHoverAndGutter(editor, dbgState);
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
        const host = document.getElementById('dockerList');
        if (!host) return;
        const collapseEl = opts instanceof Element
            ? opts
            : (opts && opts.collapseEl instanceof Element ? opts.collapseEl : null);
        const onSelect = opts && typeof opts.onSelect === 'function' ? opts.onSelect : null;
        host.innerHTML = '';
        if (!containers || !containers.length) {
            host.textContent = 'No running containers.';
            return;
        }
        containers.forEach(c => {
            const div = document.createElement('div');
            div.className = 'docker-item';
            div.textContent = `${c.name} (${c.id}) :: ${c.status}`;
            div.onclick = async () => {
                appendOutput(`🐳 Using container ${c.name} (${c.id})`);
                // Save this container ID for later use
                try {
                    localStorage.setItem('ahmadIDE:lastContainerId', c.id);
                } catch (e) {
                    // ignore
                }
                // Load saved Docker config if available
                let dockerConfig = {};
                try {
                    const raw = localStorage.getItem('ahmadIDE:dockerConfig');
                    dockerConfig = raw ? JSON.parse(raw) : {};
                } catch (e) {
                    dockerConfig = {};
                }
                await window.ahmadIDE.setConnection('docker', { docker: { containerId: c.id, ...dockerConfig } });
                const modeLabel = dockerConfig.ydbPath ? 'configured' : 'universal';
                setConnStatus(`Docker: ${c.name} (${modeLabel})`, 'success');
                await loadRoutineList(
                    routineState,
                    editor,
                    document.getElementById('routineSearch')?.value || '',
                    null
                );
                if (onSelect) onSelect();
                if (collapseEl) collapseEl.classList.add('collapsed');
                if (window.MIDE?.scheduleEditorLayout) window.MIDE.scheduleEditorLayout('docker-select');
            };
            host.appendChild(div);
        });
        setConnStatus('Docker (listed)', 'success');
    }

    function setConnStatus(text, severity) {
        const pill = document.getElementById('connStatus');
        if (!pill) return;
        pill.textContent = text;
        pill.style.background = severity === 'error'
            ? 'rgba(248,113,113,0.18)'
            : 'rgba(14,165,233,0.12)';
        pill.style.color = severity === 'error'
            ? '#fecdd3'
            : '#38bdf8';
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
