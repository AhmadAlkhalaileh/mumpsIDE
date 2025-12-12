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
            console.debug('[DBG]', ...args);
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
                '--font-code': '\"JetBrains Mono\", \"SFMono-Regular\", \"Menlo\", \"Consolas\", \"Liberation Mono\", monospace',
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
                '--font-code': '\"JetBrains Mono\", \"SFMono-Regular\", \"Menlo\", \"Consolas\", \"Liberation Mono\", monospace',
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
                '--font-code': '\"JetBrains Mono\", \"SFMono-Regular\", \"Menlo\", \"Consolas\", \"Liberation Mono\", monospace',
                '--font-size-ui': '13px',
                '--font-size-code': '13px'
            }
        }
    };
    const defaultIdeTheme = 'jb-light';
    const defaultCodeTheme = 'mumps-light';
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

    // ========== MUMPS Reference Parser (shared utility) ==========
    // Parse routine/tag reference at cursor position (supports TAG^RTN, ^RTN, DO TAG)
    // MOVED TOP for visibility
    function parseRoutineReferenceAtPosition(model, position) {
        if (!model || !position) return null;
        const lineContent = model.getLineContent(position.lineNumber) || '';
        const column = position.column;

        try {
            // Pattern 1: TAG^ROUTINE (e.g., MAIN^ROUTINE, $$FUNC^ROUTINE)
            const tagRoutineMatch = lineContent.match(/([A-Z%][A-Z0-9]*)\^([A-Z%][A-Z0-9]+)/gi);
            if (tagRoutineMatch) {
                for (const match of tagRoutineMatch) {
                    const idx = lineContent.indexOf(match);
                    const endIdx = idx + match.length;
                    if (column >= idx + 1 && column <= endIdx + 1) {
                        const [tag, routine] = match.split('^');
                        return { type: 'external', routine, tag };
                    }
                }
            }

            // Pattern 2: ^ROUTINE (standalone)
            const routineMatch = lineContent.match(/\^([A-Z%][A-Z0-9]+)/gi);
            if (routineMatch) {
                for (const match of routineMatch) {
                    const idx = lineContent.indexOf(match);
                    const endIdx = idx + match.length;
                    if (column >= idx + 1 && column <= endIdx + 1) {
                        const routine = match.substring(1); // Remove ^
                        return { type: 'external', routine, tag: '' };
                    }
                }
            }

            // Pattern 3: D TAG, DO TAG (local tag call)
            const localTagMatch = lineContent.match(/(?:^|\s)(?:D(?:O)?)\s+([A-Z%][A-Z0-9]+)(?:\s|$|,|\()/i);
            if (localTagMatch) {
                const tagName = localTagMatch[1];
                const idx = lineContent.indexOf(tagName);
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
    const menuConfig = [
        {
            id: 'file',
            label: 'File',
            items: [
                { label: 'New', action: 'new-file' },
                { label: 'Open...', action: 'open-project' },
                { label: 'Close Project', action: 'close-project' },
                { separator: true },
                { label: 'Save All', action: 'save-all' },
                { separator: true },
                { label: 'Settings', action: 'settings' },
                { label: 'Exit', action: 'exit-app' }
            ]
        },
        {
            id: 'edit',
            label: 'Edit',
            items: [
                { label: 'Undo', action: 'undo' },
                { label: 'Redo', action: 'redo' },
                { separator: true },
                { label: 'Cut', action: 'cut' },
                { label: 'Copy', action: 'copy' },
                { label: 'Paste', action: 'paste' },
                { separator: true },
                { label: 'Find', action: 'find' },
                { label: 'Replace', action: 'replace' },
                { label: 'Duplicate Line', action: 'duplicate-line' },
                { label: 'Toggle Comment', action: 'comment' }
            ]
        },
        {
            id: 'view',
            label: 'View',
            items: [
                { label: 'Toggle Sidebar', action: 'toggle-sidebar' },
                { label: 'Terminal (Tool Window)', action: 'toggle-terminal' },
                { label: 'Appearance', action: 'appearance', implemented: false }
            ]
        },
        {
            id: 'navigate',
            label: 'Navigate',
            items: [
                { label: 'Go to File...', action: 'goto-file' },
                { label: 'Go to Line...', action: 'goto-line' },
                { label: 'Recent Files', action: 'recent-files', implemented: false }
            ]
        },
        {
            id: 'code',
            label: 'Code',
            items: [
                { label: 'Format Code', action: 'reformat' },
                { label: 'Comment/Uncomment', action: 'comment' },
                { label: 'Rename', action: 'rename' }
            ]
        },
        {
            id: 'refactor',
            label: 'Refactor',
            items: [
                { label: 'Rename', action: 'rename' },
                { label: 'Extract Method', action: 'refactor-extract', implemented: false }
            ]
        },
        {
            id: 'run',
            label: 'Run',
            items: [
                { label: 'Run', action: 'run' },
                { label: 'Debug', action: 'debug' },
                { label: 'Stop', action: 'stop-debug' }
            ]
        },
        {
            id: 'tools',
            label: 'Tools',
            items: [
                { label: 'Terminal', action: 'terminal' },
                { label: 'Connections', action: 'connections' },
                { label: 'Lint', action: 'lint' },
                { label: 'Shortcuts', action: 'shortcuts' },
                { label: 'Extensions', action: 'extensions' }
            ]
        },
        {
            id: 'vcs',
            label: 'Git',
            items: [
                { label: 'Git', action: 'git' },
                { label: 'Git Status', action: 'git-status' },
                { label: 'Git Diff', action: 'git-diff' },
                { label: 'Git History', action: 'git-history' }
            ]
        },
        {
            id: 'window',
            label: 'Window',
            items: [
                { label: 'Toggle Sidebar', action: 'toggle-sidebar' },
                { label: 'Toggle Terminal', action: 'toggle-terminal' },
                { label: 'Store Layout', action: 'window-store', implemented: false }
            ]
        },
        {
            id: 'help',
            label: 'Help',
            items: [
                { label: 'Docs', action: 'docs', implemented: false },
                { label: 'About', action: 'about', implemented: false }
            ]
        }
    ];
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
    let lintSkipNotified = false;
    const extensionsState = {
        installed: [],
        selectedId: null,
        enabled: {}
    };
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
        if (ideSel) {
            ideSel.value = preferredIdeTheme || defaultIdeTheme;
            ideSel.addEventListener('change', () => applyIdeTheme(ideSel.value));
        }
        if (codeSel) {
            codeSel.value = currentCodeTheme || defaultCodeTheme;
            codeSel.addEventListener('change', () => applyCodeTheme(codeSel.value));
        }
    }

    function bindGitSettingsPanel() {
        const saveBtn = $('#gitSettingsSave');
        const testBtn = $('#gitSettingsTest');
        const statusEl = $('#gitSettingsStatus');
        const nameInput = $('#gitUserName');
        const emailInput = $('#gitUserEmail');
        const remoteInput = $('#gitRemoteUrl');
        if (!saveBtn.length || !statusEl.length) return;

        // Prefill from project .git/config or global/home git config
        (async () => {
            // Try project .git/config first if a project is open
            if (currentProject && currentProject.projectPath) {
                const gitConfig = await window.ahmadIDE.getGitConfig(currentProject.projectPath);
                if (gitConfig?.ok) {
                    if (gitConfig.user?.name) nameInput.val(gitConfig.user.name);
                    if (gitConfig.user?.email) emailInput.val(gitConfig.user.email);
                    if (gitConfig.remote?.origin) remoteInput.val(gitConfig.remote.origin);
                    return; // Found project config, don't load global
                }
            }

            // Fallback to global git config
            const resUser = await window.ahmadIDE.git('git config --global user.name');
            if (resUser?.ok && resUser.stdout) nameInput.val(resUser.stdout.trim());
            const resEmail = await window.ahmadIDE.git('git config --global user.email');
            if (resEmail?.ok && resEmail.stdout) emailInput.val(resEmail.stdout.trim());
        })();

        const runGit = async (cmd) => {
            try {
                const res = await window.ahmadIDE.git(cmd);
                if (!res.ok) {
                    showToast('error', 'Git', res.error || res.stderr || 'Git command failed');
                    statusEl.text(`Error: ${res.error || res.stderr || 'Git command failed'}`);
                    return false;
                }
                return res;
            } catch (e) {
                showToast('error', 'Git', e.message || 'Git command failed');
                statusEl.text(`Error: ${e.message || 'Git command failed'}`);
                return false;
            }
        };

        saveBtn.on('click', async () => {
            const name = (nameInput.val() || '').trim();
            const email = (emailInput.val() || '').trim();
            const remote = (remoteInput.val() || '').trim();
            const cmds = [];
            if (name) cmds.push(`git config user.name "${name.replace(/"/g, '\\"')}"`);
            if (email) cmds.push(`git config user.email "${email.replace(/"/g, '\\"')}"`);
            if (remote) {
                const escRemote = remote.replace(/"/g, '\\"');
                cmds.push(`git remote set-url origin "${escRemote}" || git remote add origin "${escRemote}"`);
            }
            if (!cmds.length) {
                statusEl.text('No changes to apply');
                return;
            }
            statusEl.text('Applying...');
            for (const cmd of cmds) {
                const ok = await runGit(cmd);
                if (!ok) return;
            }
            statusEl.text('Git settings applied');
            showToast('info', 'Git', 'Git settings applied');
        });

        testBtn.on('click', async () => {
            statusEl.text('Testing...');
            const res = await runGit('git status --short');
            if (res && res.ok) {
                statusEl.text(res.stdout ? res.stdout.trim() : 'Clean status');
                showToast('info', 'Git', 'Status ok');
            }
        });
    }

    function openSettingsPanel() {
        const panel = document.getElementById('settingsPanel');
        const overlay = document.getElementById('settingsOverlay');
        panel?.classList.remove('hidden');
        overlay?.classList.remove('hidden');
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

    function findOpenTab(target, { exact = false } = {}) {
        const normTarget = normalizeRoutineTarget(target);
        const exactMatch = openTabs.find(t => {
            const normTab = normalizeRoutineTarget(t.path || t.name);
            return normTab.path === normTarget.path;
        });
        if (exact || exactMatch) return exactMatch || null;

        const looseMatches = openTabs.filter(t => {
            const normTab = normalizeRoutineTarget(t.path || t.name);
            return normTab.base && normTab.base === normTarget.base;
        });
        return looseMatches.length === 1 ? looseMatches[0] : null;
    }

    function createTab(name, content = '', state = null, options = {}) {
        const normalized = normalizeRoutineTarget(name, options.folder || state?.current?.split('/')?.[0]);
        const tabPath = normalized.path || normalized.base || `UNTITLED_${tabIdCounter + 1}`;

        // Check if tab already exists
        const existingTab = findOpenTab(tabPath, { exact: true });
        if (existingTab) {
            switchTab(existingTab.id);
            return existingTab;
        }

        const id = `tab_${++tabIdCounter}`;
        const tabState = state || { current: tabPath };
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
            disposables: []  // Store disposables for cleanup
        };
        openTabs.push(tab);

        // Create Monaco model for this tab (for instant switching)
        if (typeof monaco !== 'undefined') {
            const model = monaco.editor.createModel(content, 'mumps');
            tabModels.set(id, model);

            // Track changes for dirty state - STORE the disposable!
            const changeDisposable = model.onDidChangeContent(() => {
                const t = openTabs.find(x => x.id === id);
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

    function switchTab(tabId) {
        const tab = openTabs.find(t => t.id === tabId);
        if (!tab) return;
        if (tabId === activeTabId) return; // Already active

        // Save current tab content
        if (activeTabId && activeEditor) {
            const currentTab = openTabs.find(t => t.id === activeTabId);
            if (currentTab) {
                currentTab.content = activeEditor.getValue();
            }
        }

        activeTabId = tabId;
        const normalizedTarget = normalizeRoutineTarget(tab.path || tab.name);
        if (tab.state) {
            tab.state.current = normalizedTarget.path || tab.state.current;
        }
        if (routineStateRef) {
            routineStateRef.current = normalizedTarget.path || normalizedTarget.base;
        }

        // Switch Monaco model (instant, no setValue)
        if (activeEditor) {
            const model = tabModels.get(tabId);
            if (model) {
                activeEditor.setModel(model);
            } else {
                // Fallback: create model if missing
                const newModel = monaco.editor.createModel(tab.content, 'mumps');
                tabModels.set(tabId, newModel);
                activeEditor.setModel(newModel);
            }
        }

        setCurrentRoutine(normalizedTarget.path || normalizedTarget.base);
        logger.info('TAB_SWITCH', { tabId, path: normalizedTarget.path || normalizedTarget.base });
        if (dbgStateRef) {
            decorateBreakpoints(activeEditor, dbgStateRef);
            renderBreakpoints(dbgStateRef);
        }
        renderTabs();

        // Scroll active tab into view
        scrollActiveTabIntoView();
    }

    function closeTab(tabId, force = false) {
        const tab = openTabs.find(t => t.id === tabId);
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

    function performCloseTab(tabId) {
        const index = openTabs.findIndex(t => t.id === tabId);
        if (index === -1) return;
        const closing = openTabs[index];

        // Dispose all event listeners for this tab
        if (closing.disposables) {
            closing.disposables.forEach(d => d.dispose());
            closing.disposables = [];
        }

        // Dispose Monaco model
        const model = tabModels.get(tabId);
        if (model) {
            model.dispose();
            tabModels.delete(tabId);
        }

        openTabs.splice(index, 1);

        // Switch to adjacent tab (PhpStorm: activate tab to the left, or right if none)
        if (openTabs.length > 0) {
            const newIndex = Math.min(index, openTabs.length - 1);
            switchTab(openTabs[newIndex].id);
        } else {
            activeTabId = null;
            if (activeEditor) {
                const emptyModel = monaco.editor.createModel('', 'mumps');
                activeEditor.setModel(emptyModel);
            }
            setCurrentRoutine('');
            renderTabs();
        }
        logger.info('TAB_CLOSED', { tabId, path: closing?.path });
    }

    function markTabDirty(tabId, isDirty = true) {
        const tab = openTabs.find(t => t.id === tabId);
        if (tab && tab.isDirty !== isDirty) {
            tab.isDirty = isDirty;
            renderTabs();
        }
    }

    function markCurrentTabClean() {
        if (activeTabId) {
            markTabDirty(activeTabId, false);
        }
    }

    // Cycle through tabs (Ctrl+Tab / Ctrl+Shift+Tab)
    function cycleTab(direction = 1) {
        if (openTabs.length <= 1) return;
        const currentIndex = openTabs.findIndex(t => t.id === activeTabId);
        if (currentIndex === -1) return;

        let newIndex = (currentIndex + direction + openTabs.length) % openTabs.length;
        switchTab(openTabs[newIndex].id);
    }

    function bindTabKeyboardShortcuts() {
        if (tabShortcutsBound) return;
        const handler = (e) => {
            const key = (e.key || '').toLowerCase();
            if (key !== 'tab') return;
            if (!(e.ctrlKey || e.metaKey)) return;
            e.preventDefault();
            e.stopPropagation();
            cycleTab(e.shiftKey ? -1 : 1);
        };
        window.addEventListener('keydown', handler, true);
        tabShortcutsBound = true;
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

    function renderTabs() {
        const tabBar = $('#tabBar');
        if (!tabBar.length) return;

        tabBar.empty();

        openTabs.forEach(tab => {
            const isActive = tab.id === activeTabId;
            const tabElement = $('<div class="tab"></div>');
            if (isActive) tabElement.addClass('active');
            if (tab.isDirty) tabElement.addClass('modified');
            const tabTooltip = tab.path || tab.name || 'Untitled';
            tabElement.attr('title', tabTooltip);

            // File icon (PhpStorm style)
            const iconSpan = $('<span class="tab-icon"></span>');
            iconSpan.html(tab.icon || tabMumpsIcon);
            tabElement.append(iconSpan);

            // File name
            const nameSpan = $('<span class="tab-name"></span>');
            nameSpan.text(tab.name || 'Untitled');
            tabElement.append(nameSpan);

            // Close button
            const closeBtn = $('<span class="tab-close">×</span>');
            closeBtn.on('click', (e) => {
                e.stopPropagation();
                closeTab(tab.id);
            });
            tabElement.append(closeBtn);

            // Click to switch
            tabElement.on('click', (e) => {
                if (!$(e.target).hasClass('tab-close')) {
                    switchTab(tab.id);
                }
            });

            // Middle-click to close (PhpStorm behavior)
            tabElement.on('mousedown', (e) => {
                if (e.which === 2) { // Middle button
                    e.preventDefault();
                    closeTab(tab.id);
                }
            });

            // Right-click context menu
            tabElement.on('contextmenu', (e) => {
                e.preventDefault();
                showTabContextMenu(e.clientX, e.clientY, tab.id);
            });

            tabBar.append(tabElement);
        });

        // Add "+" button for new tab
        const newBtn = $('<div class="tab ghost">+</div>');
        newBtn.attr('title', 'New Routine');
        newBtn.on('click', () => {
            showCustomPrompt('New Routine', 'Routine name (e.g., NEWRTN)', (name) => {
                if (name) {
                    createTab(name.toUpperCase(), `${name.toUpperCase()}\t; New routine\n\tQUIT\n`);
                    if (globalTerminalState) {
                        appendOutput(`✓ Created new routine: ${name.toUpperCase()}`, globalTerminalState);
                    }
                }
            });
        });
        tabBar.append(newBtn);
    }

    function showTabContextMenu(x, y, tabId) {
        const tab = openTabs.find(t => t.id === tabId);
        if (!tab) return;

        $('.tab-context-menu').remove();
        const menu = $('<div class="routines-context-menu tab-context-menu"></div>');
        menu.css({ position: 'fixed', top: y, left: x, zIndex: 5000 });

        const menuItems = [
            { label: 'Close', action: () => closeTab(tabId) },
            { label: 'Close Others', action: () => closeOtherTabs(tabId) },
            { label: 'Close All', action: () => closeAllTabs() },
            { separator: true },
            { label: 'Close Tabs to the Left', action: () => closeTabsToSide(tabId, 'left') },
            { label: 'Close Tabs to the Right', action: () => closeTabsToSide(tabId, 'right') }
        ];

        menuItems.forEach(item => {
            if (item.separator) {
                menu.append('<div class="ctx-separator"></div>');
            } else {
                const menuItem = $('<div class="ctx-item"></div>');
                menuItem.html(`<span class="ctx-label">${item.label}</span>`);
                menuItem.on('click', () => {
                    menu.remove();
                    item.action();
                });
                menu.append(menuItem);
            }
        });

        $('body').append(menu);
        setTimeout(() => $(document).one('click', () => menu.remove()), 100);
    }

    function closeOtherTabs(keepTabId) {
        const tabsToClose = openTabs.filter(t => t.id !== keepTabId);
        tabsToClose.forEach(t => performCloseTab(t.id));
    }

    function closeAllTabs() {
        const allTabs = [...openTabs];
        allTabs.forEach(t => performCloseTab(t.id));
    }

    function closeTabsToSide(tabId, side) {
        const index = openTabs.findIndex(t => t.id === tabId);
        if (index === -1) return;

        const tabsToClose = side === 'left'
            ? openTabs.slice(0, index)
            : openTabs.slice(index + 1);

        tabsToClose.forEach(t => performCloseTab(t.id));
    }

    function showConfirmDialog(title, message, onConfirm) {
        const overlay = $('<div class="prompt-overlay"></div>');
        const dialog = $('<div class="prompt-dialog"></div>');
        dialog.html(`
            <div class="prompt-title">${title}</div>
            <div class="prompt-message">${message}</div>
            <div class="prompt-buttons">
                <button class="btn primary prompt-ok">Discard</button>
                <button class="btn ghost prompt-cancel">Cancel</button>
            </div>
        `);

        overlay.append(dialog);
        $('body').append(overlay);

        dialog.find('.prompt-ok').on('click', () => {
            overlay.remove();
            onConfirm();
        });

        dialog.find('.prompt-cancel').on('click', () => {
            overlay.remove();
        });
    }

    function showCustomPrompt(title, placeholder, callback) {
        const overlay = $('<div class="prompt-overlay"></div>');
        const dialog = $('<div class="prompt-dialog"></div>');
        dialog.html(`
            <div class="prompt-title">${title}</div>
            <input type="text" class="prompt-input" placeholder="${placeholder}" autofocus>
            <div class="prompt-buttons">
                <button class="btn primary prompt-ok">OK</button>
                <button class="btn ghost prompt-cancel">Cancel</button>
            </div>
        `);

        overlay.append(dialog);
        $('body').append(overlay);

        const input = dialog.find('.prompt-input');
        input.focus();

        const handleOk = () => {
            const value = input.val().trim();
            overlay.remove();
            if (value) callback(value);
        };

        const handleCancel = () => {
            overlay.remove();
        };

        dialog.find('.prompt-ok').on('click', handleOk);
        dialog.find('.prompt-cancel').on('click', handleCancel);
        input.on('keydown', (e) => {
            if (e.key === 'Enter') handleOk();
            if (e.key === 'Escape') handleCancel();
        });
    }

    // ============================================
    // PhpStorm-style Project Tree Context Menu
    // ============================================

    function showProjectContextMenu(x, y, options = {}) {
        const { type, path, name, routineStateRef, editorRef, folderName } = options;
        $('.routines-context-menu').remove();

        const menu = $('<div class="routines-context-menu"></div>');
        menu.css({ position: 'fixed', top: y, left: x, zIndex: 5000 });

        const resolvedPath = path || (folderName ? `${folderName}/${name || ''}` : null);
        const projectRoot = currentProject?.projectPath || '';
        const copyPathText = resolvedPath ? `${projectRoot ? projectRoot + '/' : ''}${resolvedPath}` : projectRoot || '';

        const copyToClipboard = async (text, label = 'Path copied') => {
            try {
                await navigator.clipboard.writeText(text);
                showToast('success', 'Copied', label);
            } catch (err) {
                showToast('error', 'Copy failed', err.message || 'Could not copy');
            }
        };

        const ensureGitPanel = () => {
            openGitToolWindow();
        };

        const gitActions = {
            add: async (target) => {
                if (!target) return;
                ensureGitPanel();
                await runGitQuickCmd(`git add -- "${target}"`, { toastLabel: 'Git Add' });
                document.getElementById('gitStatusBtn')?.click();
            },
            commitFile: async (target) => {
                ensureGitPanel();
                const msg = document.getElementById('gitCommitMessage');
                const diffPath = document.getElementById('gitDiffPath');
                if (diffPath) diffPath.value = target || '';
                await runGitQuickCmd(`git add -- "${target}"`, { toastLabel: 'Git Commit' });
                showToast('info', 'Commit File', 'Staged file. Enter message in Git tool window to commit.');
                msg?.focus();
                document.getElementById('gitStatusBtn')?.click();
            },
            history: async (target) => {
                ensureGitPanel();
                await runGitQuickCmd(`git log --oneline -- "${target}"`, { toastLabel: 'Git History' });
                const input = document.getElementById('gitDiffPath');
                if (input) input.value = target || '';
                document.getElementById('gitLogBtn')?.click();
            },
            compareHead: async (target) => {
                ensureGitPanel();
                const input = document.getElementById('gitDiffPath');
                if (input) input.value = target || '';
                document.getElementById('gitDiffFileBtn')?.click();
            },
            rollback: async (target) => {
                ensureGitPanel();
                await runGitQuickCmd(`git checkout -- "${target}"`, { toastLabel: 'Git Rollback' });
                document.getElementById('gitStatusBtn')?.click();
            }
        };

        const menuItems = [];

        const normalizeSeparators = (items) => {
            const cleaned = [];
            let lastWasSeparator = false;
            items.forEach(item => {
                if (item.separator) {
                    if (!cleaned.length || lastWasSeparator) return;
                    cleaned.push(item);
                    lastWasSeparator = true;
                } else {
                    cleaned.push(item);
                    lastWasSeparator = false;
                }
            });
            if (cleaned.length && cleaned[cleaned.length - 1].separator) {
                cleaned.pop();
            }
            return cleaned;
        };

        if (type === 'folder' || type === 'root') {
            menuItems.push({
                label: 'New…',
                submenu: [
                    {
                        label: 'MUMPS Routine',
                        action: () => {
                            showCustomPrompt('New MUMPS Routine', 'Routine name (e.g., MYRTN)', (val) => {
                                if (!val) return;
                                const routineName = val.toUpperCase();
                                const targetFolder = folderName || 'routines';
                                const fullPath = `${targetFolder}/${routineName}`;
                                const newContent = `${routineName}\t; New routine created\n\tQUIT\n`;
                                createTab(routineName, newContent, { current: fullPath });
                                editorRef?.setValue(newContent);
                                if (routineStateRef) routineStateRef.current = fullPath;
                                setCurrentRoutine(fullPath);
                                showToast('success', 'Created', `New routine: ${routineName}`);
                            });
                        }
                    },
                    { label: 'Folder', action: () => showToast('info', 'NOT IMPLEMENTED YET', 'Folder creation requires file system access') }
                ]
            });
            menuItems.push({ separator: true });
        }

        if (type === 'file') {
            menuItems.push({
                label: 'Open',
                action: async () => {
                    if (path && routineStateRef && editorRef) {
                        await loadRoutineByName(path, routineStateRef, editorRef);
                    }
                }
            });
            menuItems.push({ separator: true });
        }

        // Core actions
        menuItems.push({ label: 'Cut', disabled: true, action: () => showToast('info', 'NOT IMPLEMENTED YET', 'Cut requires filesystem access') });
        menuItems.push({ label: 'Copy', disabled: true, action: () => showToast('info', 'NOT IMPLEMENTED YET', 'Copy requires filesystem access') });
        menuItems.push({ label: 'Paste', disabled: true, action: () => showToast('info', 'NOT IMPLEMENTED YET', 'Paste requires filesystem access') });
        menuItems.push({ separator: true });

        if (type === 'file') {
            menuItems.push({
                label: 'Rename…',
                shortcut: 'F2',
                disabled: true
            });
        }
        menuItems.push({
            label: 'Delete',
            shortcut: '⌦',
            disabled: true
        });
        menuItems.push({ separator: true });

        menuItems.push({ label: 'Refactor', action: () => showToast('info', 'NOT IMPLEMENTED YET', 'Refactor operations not available yet') });
        menuItems.push({ separator: true });

        menuItems.push({
            label: 'Copy Path',
            action: () => {
                if (copyPathText) copyToClipboard(copyPathText, 'Path copied');
            }
        });
        menuItems.push({
            label: 'Copy Reference',
            action: () => {
                const ref = resolvedPath || '';
                copyToClipboard(ref, 'Reference copied');
            }
        });
        menuItems.push({ separator: true });

        menuItems.push({
            label: 'Reveal in File Explorer',
            action: () => {
                if (window.ahmadIDE?.revealInExplorer) {
                    window.ahmadIDE.revealInExplorer(copyPathText || currentProject?.projectPath);
                } else {
                    showToast('info', 'Not Available', 'File explorer integration not available');
                }
            }
        });
        menuItems.push({ separator: true });

        // Git submenu
        menuItems.push({
            label: 'Git',
            submenu: [
                { label: 'Add', action: () => gitActions.add(resolvedPath || '.') },
                { label: 'Commit…', action: () => gitActions.commitFile(resolvedPath || '.') },
                { label: 'Show History', action: () => gitActions.history(resolvedPath || '.') },
                { label: 'Compare with Latest Version', action: () => gitActions.compareHead(resolvedPath || '.') },
                { label: 'Revert / Rollback', action: () => gitActions.rollback(resolvedPath || '.') }
            ]
        });
        menuItems.push({ separator: true });

        menuItems.push({
            label: 'Refresh',
            action: async () => {
                if (routineStateRef && editorRef) {
                    await loadRoutineList(routineStateRef, editorRef);
                    showToast('success', 'Refreshed', 'Project tree updated');
                }
            }
        });

        // Build DOM
        const createMenuItem = (item) => {
            if (item.separator) return $('<div class="ctx-separator"></div>');
            const el = $('<div class="ctx-item"></div>');
            if (item.disabled) el.addClass('disabled');
            el.append($('<span class="ctx-label"></span>').text(item.label));
            if (item.shortcut) el.append($('<span class="ctx-shortcut"></span>').text(item.shortcut));
            if (item.submenu) {
                el.addClass('has-submenu');
                el.append($('<span class="ctx-arrow">▸</span>'));
                const sub = $('<div class="ctx-submenu"></div>');
                item.submenu.forEach(subItem => sub.append(createMenuItem(subItem)));
                el.append(sub);
            } else if (item.action && !item.disabled) {
                el.on('click', (e) => {
                    e.stopPropagation();
                    menu.remove();
                    item.action();
                });
            }
            return el;
        };

        normalizeSeparators(menuItems).forEach(i => menu.append(createMenuItem(i)));
        $('body').append(menu);

        const rect = menu[0].getBoundingClientRect();
        if (rect.right > window.innerWidth) menu.css('left', window.innerWidth - rect.width - 10);
        if (rect.bottom > window.innerHeight) menu.css('top', window.innerHeight - rect.height - 10);
        setTimeout(() => $(document).one('click', () => menu.remove()), 100);
        return false;
    }

    // Legacy wrapper for compatibility
    function showRoutinesFolderContextMenu(x, y, routineStateRef, editorRef) {
        return showProjectContextMenu(x, y, {
            type: 'folder',
            routineStateRef,
            editorRef
        });
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

    // ============================================
    // PhpStorm-style SVG Icons for Project Tree
    // ============================================
    const treeIcons = {
        folder: `<svg width="16" height="16" viewBox="0 0 16 16" fill="#8c7a65"><path d="M2 3h5l1 1h6v9H2V3zm1 1v8h10V5H7.5L6.5 4H3z"/></svg>`,
        folderOpen: `<svg width="16" height="16" viewBox="0 0 16 16" fill="#c4a97a"><path d="M2 3h5l1 1h6v2h-1l-1 7H3L2 6V3zm1 1v1.5l.8 6.5h8.4l.8-5H4V4z"/></svg>`,
        project: `<svg width="16" height="16" viewBox="0 0 16 16"><rect x="1" y="1" width="14" height="14" rx="2" fill="#6b8dad"/><path d="M4 5h8v1H4zm0 3h8v1H4zm0 3h5v1H4z" fill="#fff" opacity="0.8"/></svg>`,
        mumps: mumpsFileIconSvg,
        arrow: `<svg width="10" height="10" viewBox="0 0 10 10" fill="currentColor"><path d="M3 1l5 4-5 4V1z"/></svg>`
    };

    let projectTreeRenderPending = null;
    let projectTreeRenderArgs = null;
    let projectTreeRenderToken = 0;
    let lastSelectedTreeItem = null;

    function renderProjectTreeLoading(message = 'Loading routines…') {
        const host = document.getElementById('projectTree');
        if (!host) return;
        host.innerHTML = `<div class="tree-item disabled">${message}</div>`;
    }

    function renderProjectTree(routines = [], routineStateRef = null, editorRef = null) {
        const token = ++projectTreeRenderToken;
        projectTreeRenderArgs = { routines, routineStateRef, editorRef, token };
        if (projectTreeRenderPending) return;
        projectTreeRenderPending = window.requestAnimationFrame(() => {
            projectTreeRenderPending = null;
            const args = projectTreeRenderArgs || { routines: [], routineStateRef: null, editorRef: null, token: projectTreeRenderToken };
            projectTreeRenderArgs = null;
            renderProjectTreeImmediate(args.routines, args.routineStateRef, args.editorRef, args.token);
        });
    }

    function renderProjectTreeImmediate(routines = [], routineStateRef = null, editorRef = null, token = projectTreeRenderToken) {
        if (token !== projectTreeRenderToken) return;
        const host = $('#projectTree');
        if (!host.length) return;
        const hostEl = host[0];
        hostEl.innerHTML = '';
        const frag = document.createDocumentFragment();
        const normalizedCurrent = normalizeRoutineTarget(routineStateRef?.current || '').path;
        lastSelectedTreeItem = null;

        // PhpStorm-style tree item builder
        const createTreeItem = (label, options = {}) => {
            const { icon, isFolder, hasChildren, expanded, depth = 0, togglePath, onClick, onContext, isActive, isDisabled } = options;

            const item = $('<div class="tree-item"></div>');
            if (isFolder) item.addClass('folder');
            if (isActive) item.addClass('active');
            if (isDisabled) item.addClass('disabled');

            // Indentation based on depth
            item.css('padding-left', `${4 + depth * 16}px`);

            // Expand/collapse arrow (PhpStorm style)
            if (hasChildren) {
                const arrow = $('<span class="tree-arrow"></span>');
                arrow.html(treeIcons.arrow);
                if (expanded) arrow.addClass('expanded');
                arrow.on('click', (e) => {
                    e.stopPropagation();
                    if (togglePath) {
                        if (collapsedTreeNodes.has(togglePath)) {
                            collapsedTreeNodes.delete(togglePath);
                        } else {
                            collapsedTreeNodes.add(togglePath);
                        }
                        renderProjectTree(routines, routineStateRef, editorRef);
                    }
                });
                item.append(arrow);
            } else {
                // Empty placeholder for alignment
                const arrow = $('<span class="tree-arrow empty"></span>');
                item.append(arrow);
            }

            // Icon
            const iconSpan = $('<span class="tree-icon"></span>');
            iconSpan.html(icon || treeIcons.mumps);
            item.append(iconSpan);

            // Label
            const labelSpan = $('<span class="tree-label"></span>').text(label);
            item.append(labelSpan);

            // Click handler
            if (onClick && !isDisabled) {
                item.on('click', (e) => {
                    if (!$(e.target).hasClass('tree-arrow') && !$(e.target).closest('.tree-arrow').length) {
                        onClick(e);
                    }
                });
            }

            // Context menu handler
            if (onContext) {
                item.on('contextmenu', (e) => {
                    e.preventDefault();
                    onContext(e);
                });
            }

            return item;
        };

        const scheduleIdle = (cb) => {
            if (window.requestIdleCallback) return window.requestIdleCallback(cb, { timeout: 50 });
            if (window.requestAnimationFrame) return window.requestAnimationFrame(cb);
            return setTimeout(cb, 16);
        };

        const renderRoutineItemsChunked = (list, childrenContainer, opts = {}) => {
            const { folderName, depth, routinesSource } = opts;
            const batchSize = 200;
            const maxMs = 10;
            let idx = 0;

            const renderBatch = () => {
                if (token !== projectTreeRenderToken) return;
                const start = performance.now();
                let added = 0;
                while (idx < list.length && added < batchSize && (performance.now() - start) < maxMs) {
                    const routinePath = list[idx++];
                    const displayName = routinePath.replace(/^(localr|routines)\//, '');
                    const isCurrentRoutine = normalizeRoutineTarget(routinePath).path === normalizedCurrent;

                    const fileItem = createTreeItem(displayName, {
                        icon: treeIcons.mumps,
                        depth: depth + 1,
                        isActive: isCurrentRoutine,
                        onClick: async () => {
                            if (lastSelectedTreeItem) {
                                lastSelectedTreeItem.removeClass('selected active');
                            }
                            fileItem.addClass('selected active');
                            lastSelectedTreeItem = fileItem;
                            await loadRoutineByName(routinePath, routineStateRef, editorRef || activeEditor, routinesSource);
                        },
                        onContext: (e) => showProjectContextMenu(e.clientX, e.clientY, {
                            type: 'file',
                            path: routinePath,
                            name: displayName,
                            folderName: folderName,
                            routineStateRef,
                            editorRef
                        })
                    });
                    childrenContainer.append(fileItem);
                    added += 1;
                }
                if (idx < list.length) {
                    scheduleIdle(renderBatch);
                }
            };

            renderBatch();
        };

        // Project root
        const rootPath = 'root';
        const rootExpanded = !collapsedTreeNodes.has(rootPath);
        const projectName = currentProject?.projectPath?.split('/').pop() || 'Project';

        const rootItem = createTreeItem(projectName, {
            icon: rootExpanded ? treeIcons.folderOpen : treeIcons.project,
            isFolder: true,
            hasChildren: true,
            expanded: rootExpanded,
            depth: 0,
            togglePath: rootPath,
            onContext: (e) => showProjectContextMenu(e.clientX, e.clientY, {
                type: 'root',
                routineStateRef,
                editorRef
            })
        });
        frag.appendChild(rootItem[0]);

        if (!rootExpanded) {
            hostEl.appendChild(frag);
            return; // Don't render children if collapsed
        }

        // Group routines by folder
        const routinesByFolder = { localr: [], routines: [] };
        (routines || []).forEach(r => {
            if (r.startsWith('localr/')) {
                routinesByFolder.localr.push(r);
            } else if (r.startsWith('routines/')) {
                routinesByFolder.routines.push(r);
            }
        });

        // Helper to create folder with routines
        const createRoutineFolder = (folderName, folderRoutines, depth) => {
            const folderPath = `root/${folderName}`;
            const folderExpanded = !collapsedTreeNodes.has(folderPath);
            const hasRoutines = folderRoutines.length > 0;

            const container = $('<div></div>');

            const folderItem = createTreeItem(folderName, {
                icon: folderExpanded ? treeIcons.folderOpen : treeIcons.folder,
                isFolder: true,
                hasChildren: hasRoutines,
                expanded: folderExpanded,
                depth: depth,
                togglePath: folderPath,
                onContext: (e) => showProjectContextMenu(e.clientX, e.clientY, {
                    type: 'folder',
                    folderName: folderName,
                    routineStateRef,
                    editorRef
                })
            });
            container.append(folderItem);

            if (folderExpanded) {
                const children = $('<div class="tree-children"></div>');

                const filteredRoutines = folderRoutines.filter(r =>
                    !routineFilterTerm || r.toLowerCase().includes(routineFilterTerm.toLowerCase())
                );

                if (!filteredRoutines.length) {
                    const msg = routineFilterTerm ? `No match for "${routineFilterTerm}"` : '(empty)';
                    const emptyItem = createTreeItem(msg, {
                        icon: treeIcons.mumps,
                        depth: depth + 1,
                        isDisabled: true
                    });
                    children.append(emptyItem);
                } else {
                    const routinesSource = routines;
                    if (filteredRoutines.length > 400) {
                        renderRoutineItemsChunked(filteredRoutines, children, {
                            folderName,
                            depth,
                            routinesSource
                        });
                    } else {
                        filteredRoutines.forEach(routinePath => {
                            const displayName = routinePath.replace(/^(localr|routines)\//, '');
                            const isCurrentRoutine = normalizeRoutineTarget(routinePath).path === normalizedCurrent;

                            const fileItem = createTreeItem(displayName, {
                                icon: treeIcons.mumps,
                                depth: depth + 1,
                                isActive: isCurrentRoutine,
                                onClick: async () => {
                                    if (lastSelectedTreeItem) {
                                        lastSelectedTreeItem.removeClass('selected active');
                                    }
                                    fileItem.addClass('selected active');
                                    lastSelectedTreeItem = fileItem;
                                    await loadRoutineByName(routinePath, routineStateRef, editorRef || activeEditor, routinesSource);
                                },
                                onContext: (e) => showProjectContextMenu(e.clientX, e.clientY, {
                                    type: 'file',
                                    path: routinePath,
                                    name: displayName,
                                    folderName: folderName,
                                    routineStateRef,
                                    editorRef
                                })
                            });
                            children.append(fileItem);
                        });
                    }
                }
                container.append(children);
            }

            return container;
        };

        // Create folders
        const rootChildren = $('<div class="tree-children"></div>');

        if (routinesByFolder.localr.length > 0) {
            rootChildren.append(createRoutineFolder('localr', routinesByFolder.localr, 1));
        }

        if (routinesByFolder.routines.length > 0) {
            rootChildren.append(createRoutineFolder('routines', routinesByFolder.routines, 1));
        }

        if (routinesByFolder.localr.length === 0 && routinesByFolder.routines.length === 0) {
            const emptyItem = createTreeItem('No routines found', {
                icon: treeIcons.mumps,
                depth: 1,
                isDisabled: true
            });
            rootChildren.append(emptyItem);
        }

        frag.appendChild(rootChildren[0]);
        hostEl.appendChild(frag);
    }

    function defaultShortcut(actionId) {
        return shortcutDefaults[actionId] || null;
    }

    function setCollapseStateAll(collapsed) {
        collapsedTreeNodes.clear();
        if (collapsed) {
            collapsedTreeNodes.add('root');
            collapsedTreeNodes.add('root/routines');
        }
        renderProjectTree(routineState?._cacheFull || routineState?._lastRoutines || [], routineState, activeEditor);
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
        const menu = $('#editorContextMenu');
        if (!menu.length || !editor) return;

        const getEditor = () => activeEditor || editor;
        const hideMenu = () => menu.hide();
        const toggleDisabled = (actionId, disabled) => {
            const el = menu.find(`[data-action="${actionId}"]`);
            el.toggleClass('disabled', !!disabled);
        };

        if (!menu.data('ctx-bound')) {
            $(document).on('click', hideMenu);
            $(document).on('keydown', (e) => {
                if (e.key === 'Escape') hideMenu();
            });
            menu.data('ctx-bound', true);
        }

        const gitContextActions = (targetPath) => {
            const ensurePath = (fnLabel) => {
                if (!targetPath) {
                    showToast('info', 'Git', `NOT IMPLEMENTED YET: Git → ${fnLabel} (no active file)`);
                    return null;
                }
                return targetPath;
            };
            return {
                'ctx-git-add': () => {
                    const target = ensurePath('Add');
                    if (target) runGitContextAction('add', target);
                },
                'ctx-git-commit-file': () => {
                    const target = ensurePath('Commit…');
                    if (target) runGitContextAction('commit', target);
                },
                'ctx-git-history': () => {
                    const target = ensurePath('Show History');
                    if (target) runGitContextAction('history', target);
                },
                'ctx-git-compare': () => {
                    const target = ensurePath('Compare with Latest Version');
                    if (target) runGitContextAction('compare', target);
                },
                'ctx-git-rollback': () => {
                    const target = ensurePath('Revert / Rollback');
                    if (target) runGitContextAction('rollback', target);
                }
            };
        };

        const actionMap = {
            'ctx-cut': () => document.execCommand('cut'),
            'ctx-copy': () => document.execCommand('copy'),
            'ctx-paste': () => document.execCommand('paste'),
            'ctx-format': async () => {
                const ed = getEditor();
                if (!ed) return;
                const action = ed.getAction && ed.getAction('editor.action.formatDocument');
                if (action && action.isSupported && action.isSupported()) {
                    try {
                        await action.run();
                        return;
                    } catch (err) {
                        // Fall through to toast when formatter rejects
                    }
                }
                showToast('info', 'Format Code', 'NOT IMPLEMENTED YET for this file type');
            },
            'ctx-declare': () => goToDeclaration(getEditor(), null, { silentIfMissing: false }),
            'ctx-usages': () => showToast('info', 'Find Usages', 'NOT IMPLEMENTED YET'),
            'ctx-comment': () => {
                const ed = getEditor();
                if (ed) ed.trigger('keyboard', 'editor.action.commentLine', null);
            },
            'ctx-run': () => $('#runBtn').trigger('click'),
            'ctx-debug': () => $('#debugStartBtn').trigger('click')
        };

        const bindActions = (map) => {
            Object.entries(map).forEach(([id, fn]) => {
                const item = menu.find(`[data-action="${id}"]`);
                item.off('click').on('click', async (e) => {
                    e.stopPropagation();
                    hideMenu();
                    if (item.hasClass('disabled')) return;
                    try {
                        await fn();
                    } catch (err) {
                        console.error('Context menu action failed', err);
                    }
                });
            });
        };
        bindActions(actionMap);

        const showEditorMenu = (clientX, clientY) => {
            const ed = getEditor();
            const model = ed?.getModel();
            const position = ed?.getPosition();
            // parseRoutineReferenceAtPosition is now defined above, no typeof check needed
            const ref = (model && position)
                ? parseRoutineReferenceAtPosition(model, position)
                : null;
            toggleDisabled('ctx-declare', !ref);
            const activePath = getActiveRoutine();
            bindActions(gitContextActions(activePath));
            menu.css({ top: clientY, left: clientX }).show();
        };

        editor.onContextMenu((e) => {
            e.event.preventDefault();
            e.event.stopPropagation();
            const evt = e.event.browserEvent || e.event;
            const x = (evt && evt.clientX != null) ? evt.clientX : e.event.posx || 0;
            const y = (evt && evt.clientY != null) ? evt.clientY : e.event.posy || 0;
            showEditorMenu(x, y);
        });

        const domNode = editor.getDomNode();
        if (domNode && !domNode._ctxMenuHooked) {
            const handleDomContextMenu = (ev) => {
                ev.preventDefault();
                ev.stopPropagation();
                showEditorMenu(ev.clientX, ev.clientY);
            };
            domNode.addEventListener('contextmenu', handleDomContextMenu, true); // capture to bypass Monaco handlers
            domNode._ctxMenuHooked = true;
        }
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

    const escapeRegex = (str = '') => str.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
    const escapeHtml = (str = '') => str
        .replace(/&/g, '&amp;')
        .replace(/</g, '&lt;')
        .replace(/>/g, '&gt;')
        .replace(/"/g, '&quot;')
        .replace(/'/g, '&#39;');
    // Promise helper to avoid hanging UI operations forever
    const withTimeout = (promise, ms, label = 'Operation') => {
        return new Promise((resolve, reject) => {
            let settled = false;
            const timer = setTimeout(() => {
                if (settled) return;
                settled = true;
                reject(new Error(`${label} timed out after ${ms}ms`));
            }, ms);
            promise.then((res) => {
                if (settled) return;
                settled = true;
                clearTimeout(timer);
                resolve(res);
            }).catch((err) => {
                if (settled) return;
                settled = true;
                clearTimeout(timer);
                reject(err);
            });
        });
    };

    const getSelectedText = () => {
        if (!activeEditor) return '';
        const sel = activeEditor.getSelection();
        const model = activeEditor.getModel();
        if (sel && model) {
            const text = model.getValueInRange(sel).trim();
            return text;
        }
        return '';
    };

    const searchDebounce = (() => {
        let t = null;
        return (fn, ms = 220) => {
            clearTimeout(t);
            t = setTimeout(fn, ms);
        };
    })();

    const getScopeInfo = () => {
        const active = getActiveRoutine();
        const normalized = normalizeRoutineTarget(active);
        const folder = normalized.folder || (normalized.path?.includes('/') ? normalized.path.split('/')[0] : null) || 'localr';
        const root = (currentProject?.routinesPath || '').replace(/\\/g, '/');
        const scopePath = root && folder ? `${root}/${folder}` : (folder || 'UNKNOWN – NEED DESIGN DECISION: localR / routines paths');
        return { folder, scopePath };
    };

    async function listScopedRoutines(folder) {
        try {
            const query = folder ? `${folder}/` : '';
            const res = await withTimeout(window.ahmadIDE.listRoutines(query), 8000, 'List routines');
            if (res?.ok && Array.isArray(res.routines)) {
                routinesCache = res.routines || routinesCache;
            }
        } catch (e) {
            // fall through to cache
        }
        const prefix = folder ? `${folder.toLowerCase()}/` : '';
        return (routinesCache || []).filter(r => !prefix || r.toLowerCase().startsWith(prefix));
    }

    const buildSearchRegex = (term, opts = {}) => {
        if (opts.regex) {
            try {
                return new RegExp(term, opts.matchCase ? 'g' : 'gi');
            } catch (err) {
                return null;
            }
        }
        const safe = escapeRegex(term);
        const pattern = opts.wholeWords ? `\\b${safe}\\b` : safe;
        return new RegExp(pattern, opts.matchCase ? 'g' : 'gi');
    };

    const renderFindResults = (hits = [], term = '', mode = 'find') => {
        const host = document.getElementById('findResults');
        if (!host) return;
        if (!hits.length) {
            host.textContent = 'No matches found.';
            return;
        }
        host.innerHTML = '';
        const limit = hits.slice(0, 400);
        const markRegex = term ? buildSearchRegex(term, findReplaceState.options) : null;
        limit.forEach(hit => {
            const row = document.createElement('div');
            row.className = 'search-hit';
            const header = document.createElement('div');
            header.className = 'search-hit-header';
            const title = document.createElement('span');
            title.textContent = `${hit.file}:${hit.line}`;
            title.className = 'search-hit-file';
            const chip = document.createElement('span');
            chip.className = 'pill subtle';
            chip.textContent = hit.kind || (mode === 'replace' ? 'Replace' : 'Match');
            header.appendChild(chip);
            header.appendChild(title);

            const snippet = document.createElement('div');
            snippet.className = 'search-hit-snippet';
            if (hit.snippet && markRegex) {
                snippet.innerHTML = escapeHtml(hit.snippet).replace(markRegex, (m) => `<span class="search-hit-mark">${m}</span>`);
            } else {
                snippet.textContent = hit.snippet || '';
            }

            row.appendChild(header);
            row.appendChild(snippet);
            row.onclick = async () => {
                await loadRoutineByName(hit.file, routineState, activeEditor, routinesCache, globalTerminalState);
                revealLine(hit.line);
                closeFindReplaceDialog();
            };
            host.appendChild(row);
        });

        if (hits.length > limit.length) {
            const more = document.createElement('div');
            more.className = 'search-hit-file';
            more.textContent = `Showing first ${limit.length} of ${hits.length} matches. Refine your query to narrow results.`;
            host.appendChild(more);
        }
    };

    const updateFindScopeLabels = () => {
        const { scopePath, folder } = getScopeInfo();
        const scopeLabel = document.getElementById('findScopeLabel');
        const scopePathEl = document.getElementById('findScopePath');
        findReplaceState.scopeFolder = folder;
        if (scopeLabel) scopeLabel.textContent = `Scope: ${scopePath}`;
        if (scopePathEl) scopePathEl.textContent = `Scope: ${scopePath}`;
    };

    const toggleFindMode = (mode) => {
        findReplaceState.mode = mode;
        const dialog = document.getElementById('findDialog');
        const replaceRow = document.getElementById('replaceRow');
        const replaceAllBtn = document.getElementById('replaceAllBtn');
        const toggleBtn = document.getElementById('findReplaceToggleBtn');
        const pill = document.getElementById('findModePill');
        const title = document.getElementById('findDialogTitle');
        if (dialog) dialog.dataset.mode = mode;
        if (replaceRow) replaceRow.style.display = mode === 'replace' ? 'flex' : 'none';
        if (replaceAllBtn) replaceAllBtn.style.display = mode === 'replace' ? 'inline-flex' : 'none';
        if (toggleBtn) toggleBtn.textContent = mode === 'replace' ? 'Switch to Find' : 'Switch to Replace';
        if (pill) pill.textContent = mode === 'replace' ? 'Replace' : 'Find';
        if (title) title.textContent = mode === 'replace' ? 'Replace in Files' : 'Find in Files';
    };

    function openFindReplaceDialog(mode = 'find', prefill = '') {
        toggleFindMode(mode);
        updateFindScopeLabels();
        document.getElementById('findOverlay')?.classList.remove('hidden');
        document.getElementById('findDialog')?.classList.remove('hidden');
        logger.info('SEARCH_DIALOG_OPEN', { mode, prefill });
        const findInput = document.getElementById('findQueryInput');
        if (findInput) {
            findInput.value = prefill || findInput.value;
            findInput.focus();
            if (prefill) findInput.select();
        }
        const replaceInput = document.getElementById('replaceQueryInput');
        if (mode === 'replace' && replaceInput && !replaceInput.value) {
            replaceInput.value = '';
        }
        executeFindReplacePreview();
    }

    function closeFindReplaceDialog() {
        document.getElementById('findOverlay')?.classList.add('hidden');
        document.getElementById('findDialog')?.classList.add('hidden');
        findReplaceState.token += 1; // cancel inflight searches
    }

    const executeFindReplacePreview = async (applyReplace = false) => {
        const term = (document.getElementById('findQueryInput')?.value || '').trim();
        const replaceTerm = (document.getElementById('replaceQueryInput')?.value || '').trim();
        const host = document.getElementById('findResults');
        if (!host) return;

        const fail = (msg) => {
            host.textContent = msg;
            showToast('error', 'Find', msg);
        };

        try {
            if (!term) {
                host.textContent = 'Enter text to search.';
                return;
            }

            const options = {
                matchCase: !!document.getElementById('findCaseOption')?.checked,
                wholeWords: !!document.getElementById('findWholeOption')?.checked,
                regex: !!document.getElementById('findRegexOption')?.checked
            };
            findReplaceState.options = options;
            const regex = buildSearchRegex(term, options);
            if (!regex) {
                host.textContent = 'Invalid regular expression.';
                return;
            }

            const token = ++findReplaceState.token;
            const { folder } = getScopeInfo();
            host.textContent = applyReplace ? 'Replacing across files…' : 'Searching…';
            logger.info('SEARCH_FIND_IN_PATH_QUERY', { term, replaceTerm: applyReplace ? replaceTerm : undefined, scopeFolder: folder, options, mode: findReplaceState.mode });

            // Fast path: ask backend to grep all routines in scope
            if (!applyReplace && window.ahmadIDE.searchRoutines) {
                try {
                    const fast = await withTimeout(
                        window.ahmadIDE.searchRoutines(term, { folder, ...options }),
                        10000,
                        'Search routines (fast)'
                    );
                    if (fast?.ok) {
                        const hits = fast.hits || [];
                        renderFindResults(hits, term, findReplaceState.mode);
                        logger.info('SEARCH_FIND_IN_PATH_RESULTS', { term, count: hits.length, mode: findReplaceState.mode });
                        return;
                    }
                    console.warn('Fast search failed, falling back to slow scan:', fast?.error);
                } catch (err) {
                    console.warn('Fast search errored, falling back:', err);
                }
            }

            const routines = await listScopedRoutines(folder);
            if (!routines.length) {
                host.textContent = `No routines found in scope "${folder || 'UNKNOWN – NEED DESIGN DECISION: localR / routines paths'}".`;
                return;
            }

            const total = routines.length;
            let processed = 0;
            host.textContent = applyReplace
                ? `Replacing across files… (0/${total})`
                : `Searching… (0/${total})`;
            let hits = [];
            let replaceCount = 0;
            let failed = false;

            const searchOne = async (routine) => {
                if (token !== findReplaceState.token || failed) return;
                let read = null;
                try {
                    read = await withTimeout(window.ahmadIDE.readRoutine(routine), 8000, `Read ${routine}`);
                } catch (err) {
                    console.error('Find/Replace read failed:', err);
                    failed = true;
                    fail(`Search failed on ${routine}: ${err?.message || err}`);
                    return;
                }
                if (!read?.ok) return;

                const code = read.code || '';
                const lines = code.split(/\r?\n/);
                let mutated = code;
                lines.forEach((line, idx) => {
                    regex.lastIndex = 0;
                    if (regex.test(line)) {
                        hits.push({ file: routine, line: idx + 1, snippet: line });
                    }
                });

                if (applyReplace && findReplaceState.mode === 'replace' && replaceTerm) {
                    mutated = code.replace(regex, replaceTerm);
                    if (mutated !== code) {
                        replaceCount += 1;
                        try {
                            await window.ahmadIDE.saveRoutine(routine, mutated);
                        } catch (err) {
                            console.error('Replace failed to save', err);
                        }
                        const openTab = findOpenTab(routine, { exact: true });
                        if (openTab) {
                            openTab.content = mutated;
                            const model = tabModels.get(openTab.id);
                            if (model) {
                                model.setValue(mutated);
                                openTab.isDirty = false;
                                renderTabs();
                            } else if (activeTabId === openTab.id && activeEditor) {
                                activeEditor.setValue(mutated);
                            }
                        }
                    }
                }
            };

            // Concurrency-limited search to speed up large projects
            const limit = Math.min(12, Math.max(2, navigator.hardwareConcurrency || 4));
            let cursor = 0;
            const worker = async () => {
                while (cursor < total && token === findReplaceState.token && !failed) {
                    const idx = cursor++;
                    const routine = routines[idx];
                    await searchOne(routine);
                    processed += 1;
                    if (!failed && token === findReplaceState.token) {
                        if (!applyReplace && processed % 20 === 0) {
                            host.textContent = `Searching… (${processed}/${total})`;
                        } else if (applyReplace && processed % 20 === 0) {
                            host.textContent = `Replacing across files… (${processed}/${total})`;
                        }
                        if (!applyReplace && hits.length && hits.length % 50 === 0) {
                            renderFindResults(hits, term, findReplaceState.mode);
                        }
                    }
                }
            };
            const workers = Array.from({ length: limit }, () => worker());
            await Promise.all(workers);

            if (token !== findReplaceState.token || failed) return;
            renderFindResults(hits, term, findReplaceState.mode);
            logger.info('SEARCH_FIND_IN_PATH_RESULTS', { term, count: hits.length, mode: findReplaceState.mode });
            if (applyReplace && host) {
                const summary = document.createElement('div');
                summary.className = 'search-hit-file';
                summary.textContent = replaceCount
                    ? `Applied replacements in ${replaceCount} file(s).`
                    : 'No replacements applied.';
                host.appendChild(summary);
            }
        } catch (err) {
            console.error('Find/Replace failed:', err);
            fail(`Search failed: ${err?.message || err}`);
        }
    };

    const confirmAndReplaceAll = () => {
        const term = (document.getElementById('findQueryInput')?.value || '').trim();
        if (!term) return;
        const replaceTerm = (document.getElementById('replaceQueryInput')?.value || '').trim();
        if (!replaceTerm) {
            showToast('error', 'Replace', 'Enter replacement text before replacing.');
            return;
        }
        const ok = window.confirm(`Replace all occurrences of "${term}" in the current folder?`);
        if (!ok) return;
        executeFindReplacePreview(true);
    };

    const openSearchEverywhereResult = async (path) => {
        if (!path) return;
        try {
            logger.info('SEARCH_EVERYWHERE_OPEN_RESULT', { path });
            const ok = await loadRoutineByName(path, routineState, activeEditor, routinesCache, globalTerminalState);
            if (ok !== false) closeSearchEverywhere();
        } catch (err) {
            showToast('error', 'Search Everywhere', err?.message || 'Could not open selection.');
            logger.error('SEARCH_EVERYWHERE_OPEN_ERROR', { path, message: err?.message, stack: err?.stack });
        }
    };

    const openSearchEverywhere = async (prefill = '') => {
        document.getElementById('searchEverywhereOverlay')?.classList.remove('hidden');
        document.getElementById('searchEverywhereDialog')?.classList.remove('hidden');
        const input = document.getElementById('searchEverywhereInput');
        const resultsHost = document.getElementById('searchEverywhereResults');
        searchEverywhereState.open = true;
        searchEverywhereState.selectedIndex = 0;
        logger.info('SEARCH_EVERYWHERE_OPEN', { prefill });
        if (input) {
            input.value = prefill || '';
            input.focus();
            if (prefill) input.select();
        }
        if (resultsHost) resultsHost.textContent = 'Indexing project files…';
        if (!searchEverywhereState.index.length) {
            const res = await window.ahmadIDE.listRoutines('');
            if (res?.ok) {
                routinesCache = res.routines || routinesCache;
                // UNKNOWN – NEED DESIGN DECISION: non-project items in Search Everywhere (only routines are indexed here)
                searchEverywhereState.index = (res.routines || []).map(r => ({
                    path: r,
                    name: r.split('/').pop(),
                    folder: r.split('/')[0] || ''
                }));
            } else if (resultsHost) {
                resultsHost.textContent = res?.error || 'Unable to index project files.';
                return;
            }
        }
        renderSearchEverywhereResults(input?.value || '');
    };

    const closeSearchEverywhere = () => {
        document.getElementById('searchEverywhereOverlay')?.classList.add('hidden');
        document.getElementById('searchEverywhereDialog')?.classList.add('hidden');
        searchEverywhereState.open = false;
    };

    const renderSearchEverywhereResults = (query = '') => {
        const host = document.getElementById('searchEverywhereResults');
        if (!host) return;
        const q = (query || '').toLowerCase();
        logger.debug('SEARCH_EVERYWHERE_QUERY', { query });
        const scored = searchEverywhereState.index
            .map(item => {
                const name = (item.name || '').toLowerCase();
                const path = (item.path || '').toLowerCase();
                const idx = name.indexOf(q);
                const pathIdx = path.indexOf(q);
                const hit = q ? Math.min(idx === -1 ? 9999 : idx, pathIdx === -1 ? 9999 : pathIdx) : 0;
                return { item, score: hit };
            })
            .filter(entry => q ? entry.score < 9999 : true)
            .sort((a, b) => a.score - b.score || a.item.name.localeCompare(b.item.name));

        const results = scored.slice(0, 60).map(s => s.item);
        host.innerHTML = '';
        if (!results.length) {
            searchEverywhereState.selectedIndex = 0;
            host.textContent = q ? 'No matches.' : 'No files indexed yet.';
            logger.info('SEARCH_EVERYWHERE_RESULTS', { query, count: 0 });
            return;
        }
        logger.info('SEARCH_EVERYWHERE_RESULTS', { query, count: results.length });
        searchEverywhereState.selectedIndex = Math.max(0, Math.min(searchEverywhereState.selectedIndex, results.length - 1));

        results.forEach((res, idx) => {
            const row = document.createElement('div');
            row.className = 'search-everywhere-item' + (idx === searchEverywhereState.selectedIndex ? ' active' : '');
            row.dataset.path = res.path;
            const title = document.createElement('span');
            title.textContent = res.name;
            const path = document.createElement('span');
            path.className = 'search-everywhere-path';
            path.textContent = res.path;
            row.appendChild(title);
            row.appendChild(path);
            row.onclick = () => openSearchEverywhereResult(res.path);
            host.appendChild(row);
        });
    };
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
        renderShortcutsPanel();
        const panel = document.getElementById('shortcutsPanel');
        const overlay = document.getElementById('shortcutsOverlay');
        const select = document.getElementById('shortcutSelect');
        const input = document.getElementById('shortcutInput');
        if (select) {
            select.value = 'duplicate-line';
        }
        if (input) {
            input.value = describeBinding((loadShortcutPrefs() || {})['duplicate-line'] || defaultShortcut('duplicate-line'));
        }
        panel?.classList.remove('hidden');
        overlay?.classList.remove('hidden');
    }

    function closeShortcutsPanel() {
        document.getElementById('shortcutsPanel')?.classList.add('hidden');
        document.getElementById('shortcutsOverlay')?.classList.add('hidden');
    }

    function openNewProjectPanel() {
        const panel = document.getElementById('newProjectPanel');
        const overlay = document.getElementById('newProjectOverlay');
        panel?.classList.remove('hidden');
        overlay?.classList.remove('hidden');

        // Update structure preview when project name changes
        const updatePreview = () => {
            const name = $('#projectName').val() || 'my-project';
            const fetchRoutines = $('#fetchRoutines').is(':checked');

            let preview = `${name}/\n└── routines/\n`;
            if (fetchRoutines) {
                preview += `    ├── localr/\n    │   └── (MUMPS .m files from localr)\n`;
                preview += `    └── routines/\n        └── (MUMPS .m files from routines)`;
            } else {
                preview += `    ├── localr/ (empty)\n    └── routines/ (empty)`;
            }

            $('#structurePreview').text(preview);
        };

        $('#projectName, #fetchRoutines').on('change keyup', updatePreview);
        updatePreview();
    }

    function closeNewProjectPanel() {
        document.getElementById('newProjectPanel')?.classList.add('hidden');
        document.getElementById('newProjectOverlay')?.classList.add('hidden');
    }

    async function createNewFile() {
        showToast('info', 'New File', 'Feature coming soon');
    }

    function closeCurrentProject() {
        showToast('info', 'Close Project', 'Feature coming soon');
    }

    async function openProjectDialog() {
        // Create a simple dialog using jQuery
        const dialogHtml = `
            <div class="connections-overlay" id="openProjectOverlay" style="display:block;"></div>
            <div class="connections-panel" id="openProjectDialog" style="display:block;">
                <div class="connections-header">
                    <div>
                        <div class="pane-title">Open Project</div>
                        <div class="pane-subtitle">Enter project path</div>
                    </div>
                    <button class="btn ghost icon-btn" id="closeOpenProjectDialog">✕</button>
                </div>
                <div class="connections-grid">
                    <div class="connection-card">
                        <label class="pane-subtitle">Project Path</label>
                        <div style="display:flex;gap:8px;margin-bottom:12px;">
                            <input class="ssh-input" id="openProjectPath" placeholder="/home/ahmad/projects/my-project" style="flex:1;">
                            <button class="btn ghost" id="browseOpenProjectPath">Browse...</button>
                        </div>
                        <div style="display:flex;gap:8px;">
                            <button class="btn primary" id="confirmOpenProject">Open</button>
                            <button class="btn ghost" id="cancelOpenProject">Cancel</button>
                        </div>
                    </div>
                </div>
            </div>
        `;

        logger.info('PROJECT_OPEN_DIALOG', {});
        $('body').append(dialogHtml);

        const closeDialog = () => {
            $('#openProjectOverlay, #openProjectDialog').remove();
        };

        document.getElementById('closeOpenProjectDialog').addEventListener('click', closeDialog);
        document.getElementById('cancelOpenProject').addEventListener('click', closeDialog);
        document.getElementById('openProjectOverlay').addEventListener('click', closeDialog);

        // Browse button handler
        document.getElementById('browseOpenProjectPath').addEventListener('click', async () => {
            if (window.ahmadIDE && window.ahmadIDE.openFolderDialog) {
                const result = await window.ahmadIDE.openFolderDialog();
                if (result.ok && result.path) {
                    document.getElementById('openProjectPath').value = result.path;
                }
            }
        });

        document.getElementById('confirmOpenProject').addEventListener('click', async () => {
            const projectPath = document.getElementById('openProjectPath').value.trim();
            logger.info('PROJECT_OPEN_REQUEST', { projectPath });
            if (!projectPath) {
                showToast('error', 'Error', 'Please enter a path');
                return;
            }

            closeDialog();

            try {
                const result = await window.ahmadIDE.openProject(projectPath);
                if (result.ok) {
                    showToast('success', 'Project Opened', result.message || `Loaded: ${result.projectPath}`);
                    logger.info('PROJECT_OPEN_SUCCESS', { projectPath: result.projectPath, message: result.message });
                    loadProjectIntoTree(result);
                } else {
                    showToast('error', 'Open Failed', result.error);
                    logger.error('PROJECT_OPEN_FAIL', { projectPath, error: result.error });
                }
            } catch (err) {
                showToast('error', 'Error', err.message);
                logger.error('PROJECT_OPEN_ERROR', { projectPath, message: err.message, stack: err.stack });
            }
        });

        // Focus the input
        setTimeout(() => {
            const input = document.getElementById('openProjectPath');
            if (input) input.focus();
        }, 100);
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

    let mumpsAutocompleteCache = null;
    async function loadAutocompleteData() {
        if (mumpsAutocompleteCache) return mumpsAutocompleteCache;
        const url = './assets/mumps/autocomplete-data.json';
        try {
            if ($ && $.getJSON) {
                mumpsAutocompleteCache = await new Promise((resolve, reject) => {
                    $.getJSON(url, resolve).fail((_, textStatus, err) => reject(err || textStatus));
                });
            } else {
                const res = await fetch(url);
                if (!res.ok) throw new Error(res.statusText);
                mumpsAutocompleteCache = await res.json();
            }
        } catch (e) {
            mumpsAutocompleteCache = null;
        }
        return mumpsAutocompleteCache;
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
                    openShortcutsPanel();
                    return;
                case 'settings':
                    openSettingsPanel();
                    return;
                case 'new-project':
                    openNewProjectPanel();
                    return;
                case 'open-project':
                    await openProjectDialog();
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
                case 'appearance':
                case 'recent-files':
                case 'refactor-extract':
                case 'window-store':
                case 'docs':
                case 'about':
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
            if (!host) return;
            host.innerHTML = '';

            menuConfig.forEach(menu => {
                const item = document.createElement('div');
                item.className = 'menu-item';
                item.dataset.menu = menu.id;
                item.textContent = menu.label;

                const dropdown = document.createElement('div');
                dropdown.className = 'menu-dropdown';

                menu.items.forEach(entry => {
                    if (entry.separator) {
                        const sep = document.createElement('div');
                        sep.className = 'menu-separator';
                        dropdown.appendChild(sep);
                        return;
                    }
                    const btn = document.createElement('button');
                    btn.className = 'menu-action';
                    btn.dataset.action = entry.action;
                    btn.textContent = entry.label + (entry.implemented === false ? ' (Not implemented)' : '');
                    dropdown.appendChild(btn);
                });

                item.appendChild(dropdown);
                host.appendChild(item);
            });
        };

        buildMenuBar();

        const wireMenuClicks = () => {
            const menuItems = document.querySelectorAll('.menu-item');
            menuItems.forEach(item => {
                item.addEventListener('click', (e) => {
                    e.stopPropagation();
                    const open = item.classList.contains('open');
                    document.querySelectorAll('.menu-item').forEach(el => el.classList.remove('open'));
                    if (!open) item.classList.add('open');
                });
            });
            document.addEventListener('click', () => {
                document.querySelectorAll('.menu-item').forEach(el => el.classList.remove('open'));
            });

            document.querySelectorAll('.menu-action').forEach((btn) => {
                btn.addEventListener('click', async (e) => {
                    e.stopPropagation();
                    const action = btn.getAttribute('data-action');
                    await runMenuAction(action);
                    document.querySelectorAll('.menu-item').forEach(el => el.classList.remove('open'));
                });
            });
        };

        wireMenuClicks();
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
        document.querySelectorAll('.debug-tab').forEach(tab => {
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

    const loaderScript = document.createElement('script');
    loaderScript.src = './node_modules/monaco-editor/min/vs/loader.js';
    loaderScript.onload = bootstrapMonaco;
    document.head.appendChild(loaderScript);

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

            const rootStyles = getComputedStyle(document.documentElement);
            const codeFont = (rootStyles.getPropertyValue('--font-code') || '').trim() || 'JetBrains Mono';
            const codeFontSizeValue = (rootStyles.getPropertyValue('--font-size-code') || '').trim();
            const codeFontSize = parseInt(codeFontSizeValue, 10) || 13;

            const editor = monaco.editor.create(editorHost, {
                value: sampleMumps(),
                language: 'mumps',
                theme: currentCodeTheme || defaultCodeTheme,
                fontSize: codeFontSize,
                fontFamily: codeFont,
                fontLigatures: true,
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
                quickSuggestions: false,  // Disable for speed
                suggestOnTriggerCharacters: false,
                folding: false,  // Disable code folding for performance
                foldingHighlight: false,
                showFoldingControls: 'never',
                occurrencesHighlight: 'off',  // Disable occurrence highlighting
                renderWhitespace: 'none',  // Don't render whitespace
                overviewRulerBorder: false,
                scrollBeyondLastLine: false,
                wordBasedSuggestions: 'off',
                parameterHints: { enabled: false },
                // Additional performance optimizations
                matchBrackets: 'never',  // Disable bracket matching
                selectionHighlight: false,  // Disable selection highlighting
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
            initExtensionsView();

            // PhpStorm-style search bindings
            updateFindScopeLabels();
            document.getElementById('findReplaceToggleBtn')?.addEventListener('click', () => {
                const nextMode = findReplaceState.mode === 'replace' ? 'find' : 'replace';
                toggleFindMode(nextMode);
                executeFindReplacePreview(false);
            });
            document.getElementById('closeFindDialog')?.addEventListener('click', closeFindReplaceDialog);
            document.getElementById('findOverlay')?.addEventListener('click', closeFindReplaceDialog);
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

            document.getElementById('searchEverywhereOverlay')?.addEventListener('click', closeSearchEverywhere);
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
            $('#svcDockerListBtn')?.on('click', async () => {
                const out = document.getElementById('svcDockerOutput');
                if (out) out.textContent = 'Loading...';
                const res = await window.ahmadIDE.listDocker?.();
                if (res?.ok) {
                    out.textContent = res.containers.map(c => `${c.name} (${c.id}) :: ${c.status}`).join('\n') || 'No running containers.';
                } else {
                    out.textContent = res?.error || res?.stderr || 'Docker query failed';
                }
            });
            $('#svcDockerRefreshBtn')?.on('click', async () => {
                $('#svcDockerListBtn').trigger('click');
            });
            $('#svcSshRunBtn')?.on('click', async () => {
                const cmd = ($('#svcSshCmd').val() || '').trim();
                const out = document.getElementById('svcSshOutput');
                if (!cmd) {
                    if (out) out.textContent = 'Enter a command.';
                    return;
                }
                if (out) out.textContent = `Running: ${cmd}`;
                const res = await window.ahmadIDE.hostExec(cmd);
                if (out) {
                    if (res.ok) out.textContent = res.stdout || '(no output)';
                    else out.textContent = res.error || res.stderr || 'SSH command failed';
                }
            });

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

            const relayout = () => editor.layout();
            window.addEventListener('resize', relayout);

            // --- Validation wiring ---
            let validateTimer = null;
            const triggerValidate = () => {
                clearTimeout(validateTimer);
                validateTimer = setTimeout(() => validateMumps(editor.getModel()), 400);

                // Mark current tab as dirty when content changes
                if (activeTabId) {
                    markTabDirty(activeTabId, true);
                }
            };
            editor.onDidChangeModelContent(triggerValidate);
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

            const closeRunConfigMenu = () => runConfigMenu?.classList.add('hidden');
            const toggleRunConfigMenu = () => {
                if (!runConfigMenu) return;
                const shouldOpen = runConfigMenu.classList.contains('hidden');
                closeRunConfigMenu();
                if (shouldOpen) runConfigMenu.classList.remove('hidden');
            };

            runConfigBtn?.addEventListener('click', (e) => {
                e.stopPropagation();
                toggleRunConfigMenu();
            });

            runConfigMenu?.querySelectorAll('.run-config-item').forEach((btn) => {
                if (btn.classList.contains('disabled')) return;
                btn.addEventListener('click', (e) => {
                    e.stopPropagation();
                    const cfg = btn.getAttribute('data-config');
                    setRunConfig(cfg);
                    closeRunConfigMenu();
                });
            });

            document.addEventListener('click', (e) => {
                if (!runConfigMenu || !runConfigBtn) return;
                if (!runConfigMenu.contains(e.target) && !runConfigBtn.contains(e.target)) {
                    closeRunConfigMenu();
                }
            });

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
                            console.log('[RUN] Debugger already ready, sending Continue command');
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
            } else {
                const runBtn = document.getElementById('runBtn');
                runBtn?.addEventListener('click', async () => {
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
                            console.log('[RUN] Debugger already ready, sending Continue command');
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

                const debugStartBtn = document.getElementById('debugStartBtn');
                debugStartBtn?.addEventListener('click', () => {
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

            if ($) {
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
            } else {
                document.getElementById('saveRoutineBtn')?.addEventListener('click', async () => {
                    await saveRoutineFlow(editor, routineState, terminalState);
                });
                document.getElementById('newRoutineBtn')?.addEventListener('click', async () => {
                    await newRoutineFlow(editor, routineState, terminalState);
                });
            }

            if ($) {
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
            } else {
                document.getElementById('lintBtn')?.addEventListener('click', async () => {
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
            }

            // --- SSH / Docker handling ---
            const connectionsPanel = document.getElementById('connectionsPanel');
            const connectionsOverlay = document.getElementById('connectionsOverlay');
            const closeConnectionsBtn = document.getElementById('closeConnectionsBtn');
            const dockerListEl = document.getElementById('dockerList');
            const refreshDockerBtn = document.getElementById('refreshDockerBtn');
            const useLocalDockerBtn = document.getElementById('useLocalDockerBtn');
            const sshHostInput = document.getElementById('sshHostInput');
            const sshPortInput = document.getElementById('sshPortInput');
            const sshEnvInput = document.getElementById('sshEnvInput');
            const sshUserInput = document.getElementById('sshUserInput');
            const sshPassInput = document.getElementById('sshPassInput');
            const sshConnectBtn = document.getElementById('sshConnectBtn');
            const sshFormStatus = document.getElementById('sshFormStatus');
            const connectionsBtn = document.getElementById('toggleConnections');
            const sshSavedList = document.getElementById('sshSavedList');
            const sshSaveEnvBtn = document.getElementById('sshSaveEnvBtn');

            let savedProfiles = [];

            function readSavedSshProfiles() {
                try {
                    const raw = localStorage.getItem('ahmadIDE:sshList');
                    const parsed = raw ? JSON.parse(raw) : [];
                    if (Array.isArray(parsed)) return parsed.filter(Boolean);
                    if (parsed && typeof parsed === 'object') return [parsed];
                    return [];
                } catch (e) {
                    return [];
                }
            }

            function persistSavedSshProfiles(list) {
                try {
                    localStorage.setItem('ahmadIDE:sshList', JSON.stringify(list || []));
                } catch (e) {
                    // ignore persistence errors
                }
            }

            function fillSshForm(entry) {
                if (!entry) return;
                if (sshHostInput && entry.host) sshHostInput.value = entry.host;
                if (sshUserInput && entry.username) sshUserInput.value = entry.username;
                if (sshPortInput && entry.port) sshPortInput.value = entry.port;
                if (sshEnvInput && entry.envKey) sshEnvInput.value = entry.envKey;
            }

            function renderSavedSshProfiles(list) {
                if (!sshSavedList) return;
                sshSavedList.innerHTML = '';
                if (!list || !list.length) {
                    sshSavedList.textContent = 'No saved environments.';
                    return;
                }
                list.forEach((item) => {
                    const pill = document.createElement('div');
                    pill.className = 'saved-env-pill';
                    pill.title = `${item.username || ''}@${item.host || ''}:${item.port || 22}`;
                    pill.onclick = () => fillSshForm(item);

                    const keyEl = document.createElement('span');
                    keyEl.className = 'env-key';
                    keyEl.textContent = (item.envKey || 'env').toUpperCase();

                    const metaEl = document.createElement('span');
                    metaEl.className = 'env-meta';
                    metaEl.textContent = `${item.username || ''}@${item.host || ''}`;

                    const removeBtn = document.createElement('button');
                    removeBtn.className = 'saved-env-remove';
                    removeBtn.textContent = '✕';
                    removeBtn.title = 'Remove';
                    removeBtn.onclick = (e) => {
                        e.stopPropagation();
                        savedProfiles = savedProfiles.filter(p =>
                            (p.envKey || '').toLowerCase() !== (item.envKey || '').toLowerCase()
                        );
                        persistSavedSshProfiles(savedProfiles);
                        renderSavedSshProfiles(savedProfiles);
                    };

                    pill.appendChild(keyEl);
                    pill.appendChild(metaEl);
                    pill.appendChild(removeBtn);
                    sshSavedList.appendChild(pill);
                });
            }

            function upsertSavedProfile(entry) {
                const key = (entry?.envKey || '').toLowerCase();
                if (!key) return;
                const existingIdx = savedProfiles.findIndex(
                    p => (p.envKey || '').toLowerCase() === key
                );
                const payload = {
                    envKey: entry.envKey,
                    host: entry.host,
                    port: entry.port || 22,
                    username: entry.username
                };
                if (existingIdx >= 0) {
                    savedProfiles[existingIdx] = { ...savedProfiles[existingIdx], ...payload };
                } else {
                    savedProfiles.push(payload);
                }
                persistSavedSshProfiles(savedProfiles);
                renderSavedSshProfiles(savedProfiles);
            }

            const savedSsh = (() => {
                try {
                    const raw = localStorage.getItem('ahmadIDE:ssh');
                    return raw ? JSON.parse(raw) : null;
                } catch (e) {
                    return null;
                }
            })();
            savedProfiles = readSavedSshProfiles();
            if (savedSsh && Object.keys(savedSsh).length) {
                const exists = savedProfiles.some(
                    p => (p.envKey || '').toLowerCase() === (savedSsh.envKey || '').toLowerCase()
                );
                if (!exists) {
                    savedProfiles.push(savedSsh);
                    persistSavedSshProfiles(savedProfiles);
                }
            }
            renderSavedSshProfiles(savedProfiles);

            if (savedSsh) {
                fillSshForm(savedSsh);
            } else if (savedProfiles.length) {
                fillSshForm(savedProfiles[0]);
            }
            if (sshEnvInput && !sshEnvInput.value) sshEnvInput.value = 'cc';

            const markSshStatus = (text, severity = 'info') => {
                if (!sshFormStatus) return;
                sshFormStatus.textContent = text;
                sshFormStatus.style.background = severity === 'error'
                    ? 'rgba(248,113,113,0.18)'
                    : 'rgba(91,213,255,0.12)';
                sshFormStatus.style.color = severity === 'error'
                    ? '#fecdd3'
                    : '#38bdf8';
            };

            const openConnectionsPanel = (focusSsh = false) => {
                connectionsPanel?.classList.remove('hidden');
                connectionsOverlay?.classList.remove('hidden');
                renderSavedSshProfiles(savedProfiles);
                if (focusSsh && sshHostInput) {
                    setTimeout(() => sshHostInput.focus(), 40);
                }
            };

            const closeConnectionsPanel = () => {
                connectionsPanel?.classList.add('hidden');
                connectionsOverlay?.classList.add('hidden');
            };


            async function refreshDockerList() {
                if (dockerListEl) dockerListEl.textContent = 'Loading...';
                await window.ahmadIDE.setConnection('docker');
                setConnStatus('Docker (local)', 'info');
                appendOutput('🐳 Listing containers...', terminalState);
                const res = await window.ahmadIDE.listDocker();
                if (res.ok) {
                    if (!res.containers.length) {
                        if (dockerListEl) dockerListEl.textContent = 'No running containers.';
                        appendOutput('No running containers.', terminalState);
                    } else {
                        res.containers.forEach(c =>
                            appendOutput(`- ${c.name} (${c.id}) :: ${c.status}`, terminalState)
                        );
                        renderDocker(res.containers, routineState, editor, {
                            onSelect: async () => {
                                closeConnectionsPanel();
                                appendOutput('✓ Docker target selected', terminalState);
                                await loadRoutineList(
                                    routineState,
                                    editor,
                                    routineSearch?.value || ''
                                );
                            }
                        });
                    }
                } else {
                    if (dockerListEl) dockerListEl.textContent = res.error || res.stderr || 'Docker error';
                    appendOutput(`✗ Docker error: ${res.error || res.stderr}`, terminalState);
                }
            }

            async function handleSshConnect() {
                if (!sshHostInput || !sshUserInput || !sshPassInput || !sshPortInput) return;
                const host = sshHostInput.value.trim();
                const username = sshUserInput.value.trim();
                const password = sshPassInput.value;
                const port = parseInt(sshPortInput.value || '22', 10) || 22;
                const envKey = (sshEnvInput?.value || 'cc').trim() || 'cc';

                if (!host || !username || !password) {
                    markSshStatus('Host, user, and password are required', 'error');
                    return;
                }

                markSshStatus(`Connecting to ${username}@${host}:${port}...`, 'info');
                if (sshConnectBtn) sshConnectBtn.disabled = true;
                appendOutput(`🔌 SSH connecting to ${username}@${host}:${port}...`, terminalState);

                const res = await window.ahmadIDE.sshConnect({
                    host,
                    port,
                    username,
                    password,
                    envKey
                });

                if (res.ok) {
                    const entry = { host, port, username, envKey };
                    await window.ahmadIDE.setConnection('ssh', {
                        ssh: { ...entry, password }
                    });
                    setConnStatus('SSH connected', 'success');
                    markSshStatus('SSH connected', 'success');
                    appendOutput('✓ SSH connected', terminalState);
                    try {
                        localStorage.setItem('ahmadIDE:ssh', JSON.stringify(entry));
                    } catch (e) {
                        // ignore persistence errors
                    }
                    upsertSavedProfile(entry);
                    await loadRoutineList(routineState, editor);
                    closeConnectionsPanel();
                } else {
                    const msg = res.error || res.stderr || 'SSH connect failed';
                    markSshStatus(msg, 'error');
                    setConnStatus('SSH error', 'error');
                    appendOutput(`✗ SSH connect failed: ${msg}`, terminalState);
                }

                if (sshConnectBtn) sshConnectBtn.disabled = false;
            }

            connectionsBtn?.addEventListener('click', () => {
                openConnectionsPanel();
                refreshDockerList();
            });

            refreshDockerBtn?.addEventListener('click', refreshDockerList);
            useLocalDockerBtn?.addEventListener('click', async () => {
                await window.ahmadIDE.setConnection('docker');
                setConnStatus('Docker (local)', 'info');
                appendOutput('✓ Using default Docker connection', terminalState);
                closeConnectionsPanel();
                await loadRoutineList(routineState, editor);
            });

            connectionsOverlay?.addEventListener('click', closeConnectionsPanel);
            closeConnectionsBtn?.addEventListener('click', closeConnectionsPanel);
            sshConnectBtn?.addEventListener('click', handleSshConnect);
            sshSaveEnvBtn?.addEventListener('click', () => {
                if (!sshHostInput || !sshUserInput || !sshEnvInput) return;
                const host = sshHostInput.value.trim();
                const username = sshUserInput.value.trim();
                const port = parseInt(sshPortInput?.value || '22', 10) || 22;
                const envKey = (sshEnvInput.value || 'cc').trim() || 'cc';
                if (!host || !username) {
                    markSshStatus('Host and user required to save.', 'error');
                    return;
                }
                upsertSavedProfile({ host, username, port, envKey });
                markSshStatus(`Saved ${envKey}`, 'info');
            });
            [sshHostInput, sshUserInput, sshPassInput, sshPortInput, sshEnvInput].forEach(input => {
                input?.addEventListener('keydown', (e) => {
                    if (e.key === 'Enter') handleSshConnect();
                });
            });

            document.getElementById('closeShortcutsBtn')?.addEventListener('click', closeShortcutsPanel);
            document.getElementById('shortcutsOverlay')?.addEventListener('click', closeShortcutsPanel);
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
            document.getElementById('closeSettingsBtn')?.addEventListener('click', () => {
                document.getElementById('settingsPanel')?.classList.add('hidden');
                document.getElementById('settingsOverlay')?.classList.add('hidden');
            });

            // DevTools toggle
            document.getElementById('toggleDevTools')?.addEventListener('click', async () => {
                if (window.ahmadIDE && window.ahmadIDE.toggleDevTools) {
                    await window.ahmadIDE.toggleDevTools();
                }
            });

            // New Project panel handlers
            document.getElementById('closeNewProjectBtn')?.addEventListener('click', closeNewProjectPanel);
            document.getElementById('newProjectOverlay')?.addEventListener('click', closeNewProjectPanel);

            // Browse button handler - moved to openNewProjectPanel function
            $('#createProjectBtn').on('click', async () => {
                const projectName = $('#projectName').val().trim();
                const projectPath = $('#projectPath').val().trim();
                const fetchRoutines = $('#fetchRoutines').is(':checked');

                if (!projectName) {
                    showToast('error', 'Validation', 'Project name is required');
                    return;
                }

                if (!projectPath) {
                    showToast('error', 'Validation', 'Project path is required');
                    return;
                }

                $('#projectCreationStatus').text('Creating project...');
                $('#createProjectBtn').prop('disabled', true);

                try {
                    const result = await window.ahmadIDE.createProject({
                        projectName,
                        projectPath,
                        fetchRoutines
                    });

                    if (result.ok) {
                        showToast('success', 'Project Created', result.message || 'Project created successfully');
                        $('#projectCreationStatus').text(`Created: ${result.projectPath}`);

                        // Auto-open the project in the file tree
                        loadProjectIntoTree(result);

                        setTimeout(() => {
                            closeNewProjectPanel();
                            // Reset form
                            $('#projectName').val('');
                            $('#projectCreationStatus').text('Ready');
                        }, 2000);
                    } else {
                        showToast('error', 'Creation Failed', result.error);
                        $('#projectCreationStatus').text(`Error: ${result.error}`);
                    }
                } catch (err) {
                    showToast('error', 'Error', err.message);
                    $('#projectCreationStatus').text(`Error: ${err.message}`);
                } finally {
                    $('#createProjectBtn').prop('disabled', false);
                }
            });
            document.getElementById('settingsOverlay')?.addEventListener('click', () => {
                document.getElementById('settingsPanel')?.classList.add('hidden');
                document.getElementById('settingsOverlay')?.classList.add('hidden');
            });
            const gitOutput = (text) => {
                const out = document.getElementById('gitOutput');
                if (out) {
                    out.textContent += `${text}\n`;
                    out.scrollTop = out.scrollHeight;
                }
            };
            const gitError = (text) => {
                const message = normalizeGitError(text);
                const out = document.getElementById('gitOutput');
                if (out) {
                    out.textContent += `✗ ${message}\n`;
                    out.scrollTop = out.scrollHeight;
                }
                showToast('error', 'Git', message);
            };
            const setDiffPanes = (left, right) => {
                const l = document.getElementById('gitDiffLeft');
                const r = document.getElementById('gitDiffRight');
                if (l) l.textContent = left || 'No data';
                if (r) r.textContent = right || 'No data';
            };

            const renderSideBySideDiff = (diffText) => {
                const l = document.getElementById('gitDiffLeft');
                const r = document.getElementById('gitDiffRight');
                if (!l || !r) return;
                const lines = (diffText || '').split('\n');
                const leftLines = [];
                const rightLines = [];
                let leftNo = 0;
                let rightNo = 0;
                lines.forEach(line => {
                    if (line.startsWith('+++') || line.startsWith('---')) return;
                    if (line.startsWith('@@')) {
                        leftLines.push(`<span class="diff-line diff-hunk">${line}</span>`);
                        rightLines.push(`<span class="diff-line diff-hunk">${line}</span>`);
                        const match = line.match(/-([0-9]+)/);
                        const matchR = line.match(/\+([0-9]+)/);
                        leftNo = match ? parseInt(match[1], 10) - 1 : leftNo;
                        rightNo = matchR ? parseInt(matchR[1], 10) - 1 : rightNo;
                        return;
                    }
                    if (line.startsWith('+')) {
                        rightNo += 1;
                        rightLines.push(
                            `<span class="diff-line diff-add"><span class="lineno">${rightNo}</span>${line.replace(/</g, '&lt;')}</span>`
                        );
                    } else if (line.startsWith('-')) {
                        leftNo += 1;
                        leftLines.push(
                            `<span class="diff-line diff-del"><span class="lineno">${leftNo}</span>${line.replace(/</g, '&lt;')}</span>`
                        );
                    } else {
                        leftNo += 1;
                        rightNo += 1;
                        const safe = line.replace(/</g, '&lt;');
                        leftLines.push(`<span class="diff-line diff-context"><span class="lineno">${leftNo}</span>${safe}</span>`);
                        rightLines.push(`<span class="diff-line diff-context"><span class="lineno">${rightNo}</span>${safe}</span>`);
                    }
                });
                l.innerHTML = leftLines.join('') || 'No left changes';
                r.innerHTML = rightLines.join('') || 'No right changes';
            };

            const gitSelected = { staged: new Set(), unstaged: new Set() };

            const renderGitChanges = (entries = []) => {
                const unstagedHost = document.getElementById('gitChangesUnstaged');
                const stagedHost = document.getElementById('gitChangesStaged');
                if (unstagedHost) unstagedHost.innerHTML = '';
                if (stagedHost) stagedHost.innerHTML = '';
                const render = (host, list, staged) => {
                    if (!host) return;
                    if (!list.length) {
                        host.textContent = staged ? 'No staged files.' : 'No local changes.';
                        return;
                    }
                    list.forEach(ent => {
                        const row = document.createElement('div');
                        row.className = 'git-change-row' + (staged ? ' staged' : '');
                        const checkbox = document.createElement('input');
                        checkbox.type = 'checkbox';
                        checkbox.checked = staged ? gitSelected.staged.has(ent.path) : gitSelected.unstaged.has(ent.path);
                        checkbox.addEventListener('change', () => {
                            const targetSet = staged ? gitSelected.staged : gitSelected.unstaged;
                            checkbox.checked ? targetSet.add(ent.path) : targetSet.delete(ent.path);
                        });
                        const status = document.createElement('span');
                        status.className = 'git-change-status';
                        status.textContent = ent.status;
                        const path = document.createElement('span');
                        path.className = 'git-change-path';
                        path.textContent = ent.path;
                        row.appendChild(checkbox);
                        row.appendChild(status);
                        row.appendChild(path);
                        host.appendChild(row);
                    });
                };
                const stagedList = entries.filter(e => e.staged);
                const unstagedList = entries.filter(e => !e.staged);
                render(unstagedHost, unstagedList, false);
                render(stagedHost, stagedList, true);
            };

            const renderGitHistory = (lines = []) => {
                const host = document.getElementById('gitHistoryList');
                if (!host) return;
                host.innerHTML = '';
                if (!lines.length) {
                    host.textContent = 'No history yet.';
                    return;
                }
                lines.forEach(line => {
                    const div = document.createElement('div');
                    div.textContent = line;
                    host.appendChild(div);
                });
            };

            const runGit = async (cmd, opts = {}) => {
                if (!opts.silent) gitOutput(`$ ${cmd}`);
                const res = await window.ahmadIDE.git(cmd);
                if (res.ok) {
                    if (opts.onSuccess) opts.onSuccess(res.stdout || '');
                    if (!opts.silent) {
                        if (res.stdout) gitOutput(res.stdout);
                        if (res.stderr) gitOutput(res.stderr);
                    }
                } else {
                    gitError(res.error || res.stderr || 'Git command failed');
                }
                return res;
            };

            const refreshGitStatus = async () => {
                gitSelected.staged.clear();
                gitSelected.unstaged.clear();
                const statusRes = await runGit('git status --short --branch', {
                    onSuccess: (out) => {
                        const lines = (out || '').split('\n').filter(Boolean);
                        const entries = [];
                        lines.forEach(line => {
                            if (line.startsWith('##')) return; // branch info
                            const indexStatus = line[0];
                            const worktreeStatus = line[1];
                            const path = line.slice(3).trim();
                            const staged = indexStatus !== ' ' && indexStatus !== '?';
                            const status = staged ? indexStatus : worktreeStatus;
                            entries.push({ status, path, staged });
                        });
                        renderGitChanges(entries);
                    }
                });
                if (!statusRes.ok) return statusRes;
                return runGit('git branch --format="%(refname:short)"', {
                    silent: true,
                    onSuccess: (out) => {
                        const select = document.getElementById('gitBranchSelect');
                        if (!select) return;
                        select.innerHTML = '';
                        (out || '').split('\n').filter(Boolean).forEach(b => {
                            const opt = document.createElement('option');
                            opt.value = b;
                            opt.textContent = b;
                            select.appendChild(opt);
                        });
                    }
                });
                if (statusRes.ok) {
                    fetchCurrentBranch().catch(() => { });
                }
            };

            const loadGitHistory = async () => {
                await runGit('git log -10 --oneline', {
                    onSuccess: (out) => renderGitHistory((out || '').split('\n').filter(Boolean))
                });
            };

            openGitToolWindow = (opts = {}) => {
                toggleToolWindowPanel('gitToolPanel', 'bottom');
                if (!opts.skipRefresh) {
                    refreshGitStatus().catch(() => { });
                    loadGitHistory().catch(() => { });
                }
            };

            openCommitToolWindow = () => {
                openGitToolWindow();
                const msg = document.getElementById('gitCommitMessage');
                msg?.focus();
            };

            // Keep legacy alias in sync for older callers
            openGitPanel = () => openGitToolWindow();

            document.getElementById('gitRefreshBtn')?.addEventListener('click', refreshGitStatus);
            document.getElementById('gitStatusBtn')?.addEventListener('click', refreshGitStatus);
            document.getElementById('gitLogBtn')?.addEventListener('click', loadGitHistory);
            document.getElementById('gitDiffBtn')?.addEventListener('click', () => runGit('git diff --stat'));
            document.getElementById('gitClearBtn')?.addEventListener('click', () => {
                document.getElementById('gitOutput').textContent = 'Git ready.';
            });

            const stageOrUnstage = async (targetSet, staged) => {
                const files = Array.from(targetSet);
                if (!files.length) {
                    gitOutput(staged ? 'No staged selection.' : 'No unstaged selection.');
                    return;
                }
                const cmd = staged
                    ? `git restore --staged ${files.map(f => `"${f.replace(/"/g, '\\"')}"`).join(' ')}`
                    : `git add ${files.map(f => `"${f.replace(/"/g, '\\"')}"`).join(' ')}`;
                await runGit(cmd);
                await refreshGitStatus();
            };

            document.getElementById('gitStageSelectedBtn')?.addEventListener('click', () => stageOrUnstage(gitSelected.unstaged, false));
            document.getElementById('gitUnstageSelectedBtn')?.addEventListener('click', () => stageOrUnstage(gitSelected.staged, true));

            document.getElementById('gitDiffSelectedBtn')?.addEventListener('click', async () => {
                const path = [...gitSelected.unstaged, ...gitSelected.staged][0];
                if (!path) {
                    gitError('Select a file to diff');
                    return;
                }
                await runGit(`git diff -- ${path}`);
            });

            document.getElementById('gitCommitBtn')?.addEventListener('click', async () => {
                const msgEl = document.getElementById('gitCommitMessage');
                const message = msgEl?.value.trim() || '';
                if (!message) {
                    gitOutput('✗ Commit message required');
                    return;
                }
                const res = await runGit(`git commit -m "${message.replace(/"/g, '\\"')}"`);
                if (res?.ok) {
                    msgEl.value = '';
                    await refreshGitStatus();
                    await loadGitHistory();
                }
            });
            document.getElementById('gitPushBtn')?.addEventListener('click', () => runGit('git push'));
            document.getElementById('gitPullBtn')?.addEventListener('click', () => runGit('git pull'));
            document.getElementById('gitFetchBtn')?.addEventListener('click', () => runGit('git fetch'));
            document.getElementById('gitCheckoutBtn')?.addEventListener('click', async () => {
                const select = document.getElementById('gitBranchSelect');
                const input = document.getElementById('gitBranchInput');
                const target = (input?.value.trim()) || (select?.value || '');
                if (!target) {
                    gitOutput('✗ No branch specified');
                    return;
                }
                await runGit(`git checkout ${target}`);
                await refreshGitStatus();
            });
            document.getElementById('gitDiffFileBtn')?.addEventListener('click', async () => {
                const path = document.getElementById('gitDiffPath')?.value.trim();
                if (!path) {
                    gitError('No path provided for diff');
                    return;
                }
                const diffRes = await runGit(`git diff -- ${path}`);
                const headRes = await window.ahmadIDE.git(`git show HEAD:"${path.replace(/"/g, '\\"')}"`);
                const workRes = await window.ahmadIDE.hostExec(`cat "${path.replace(/"/g, '\\"')}"`);
                setDiffPanes(
                    headRes.ok ? headRes.stdout || '(empty)' : '(no HEAD version)',
                    workRes.ok ? workRes.stdout || '(empty)' : workRes.error || '(cannot read)'
                );
                if (diffRes.ok && diffRes.stdout) renderSideBySideDiff(diffRes.stdout);
            });
            document.getElementById('gitHistoryFileBtn')?.addEventListener('click', async () => {
                const path = document.getElementById('gitDiffPath')?.value.trim();
                if (!path) {
                    gitError('No path provided for history');
                    return;
                }
                await runGit(`git log --oneline -- ${path}`);
            });
            document.getElementById('gitCompareBtn')?.addEventListener('click', async () => {
                const a = document.getElementById('gitComparePathA')?.value.trim();
                const b = document.getElementById('gitComparePathB')?.value.trim();
                if (!a || !b) {
                    gitError('Provide both paths to compare');
                    return;
                }
                const aRes = await window.ahmadIDE.hostExec(`cat "${a.replace(/"/g, '\\"')}"`);
                const bRes = await window.ahmadIDE.hostExec(`cat "${b.replace(/"/g, '\\"')}"`);
                setDiffPanes(
                    aRes.ok ? aRes.stdout || '(empty)' : aRes.error || '(cannot read)',
                    bRes.ok ? bRes.stdout || '(empty)' : bRes.error || '(cannot read)'
                );
                const diffRes = await runGit(`git diff -- ${a} ${b}`);
                if (diffRes.ok && diffRes.stdout) renderSideBySideDiff(diffRes.stdout);
            });

            document.getElementById('toolbarGitBtn')?.addEventListener('click', () => {
                openGitToolWindow();
            });

            document.getElementById('toolbarCommitBtn')?.addEventListener('click', () => {
                openCommitToolWindow();
            });

            const vcsWidget = document.getElementById('vcsWidget');
            const vcsToggle = document.getElementById('vcsWidgetBtn');
            const vcsMenu = document.getElementById('vcsWidgetMenu');
            const handleVcsMenuAction = (action) => {
                switch (action) {
                    case 'commit':
                        openCommitToolWindow();
                        break;
                    case 'history':
                        openGitToolWindow();
                        document.getElementById('gitLogBtn')?.click();
                        break;
                    case 'push':
                        document.getElementById('gitPushBtn')
                            ? document.getElementById('gitPushBtn').click()
                            : showToast('info', 'Git', 'NOT IMPLEMENTED YET: Push');
                        break;
                    case 'pull': {
                        const pullBtn = document.getElementById('gitPullBtn') || document.getElementById('gitFetchBtn');
                        if (pullBtn) pullBtn.click();
                        else showToast('info', 'Git', 'NOT IMPLEMENTED YET: Pull / Fetch');
                        break;
                    }
                    case 'open-git':
                        openGitToolWindow();
                        break;
                    default:
                        showToast('info', 'Git', `NOT IMPLEMENTED YET: ${action}`);
                }
            };

            const closeVcsMenu = () => vcsMenu?.classList.add('hidden');

            vcsToggle?.addEventListener('click', (e) => {
                e.stopPropagation();
                if (!vcsMenu) return;
                const shouldOpen = vcsMenu.classList.contains('hidden');
                closeVcsMenu();
                if (shouldOpen) vcsMenu.classList.remove('hidden');
            });

            vcsMenu?.querySelectorAll('.vcs-menu-item').forEach((btn) => {
                btn.addEventListener('click', (e) => {
                    e.stopPropagation();
                    handleVcsMenuAction(btn.getAttribute('data-action'));
                    closeVcsMenu();
                });
            });

            document.addEventListener('click', (e) => {
                if (vcsWidget && !vcsWidget.contains(e.target)) {
                    closeVcsMenu();
                }
            });

            // Initial refresh when tool window wires up
            refreshGitStatus();
            loadGitHistory(); // TODO: add pagination / filters for larger histories



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

            // Ctrl+Hover: Change cursor to pointer only when hovering over valid tag/routine
            let isCtrlPressed = false;
            let currentHoverDecoration = [];

            document.addEventListener('keydown', (e) => {
                if (e.ctrlKey || e.metaKey) {
                    isCtrlPressed = true;
                }
            });

            document.addEventListener('keyup', (e) => {
                if (!e.ctrlKey && !e.metaKey) {
                    isCtrlPressed = false;
                    // Clear cursor override
                    const editorDom = editor.getDomNode();
                    if (editorDom) {
                        editorDom.style.cursor = '';
                    }
                    // Clear hover decoration
                    if (currentHoverDecoration.length > 0) {
                        editor.deltaDecorations(currentHoverDecoration, []);
                        currentHoverDecoration = [];
                    }
                }
            });

            // Track mouse movement to detect hovering over clickable targets
            editor.onMouseMove((e) => {
                if (!isCtrlPressed || !e.target.position) {
                    // Clear cursor and decoration if not Ctrl+hovering
                    const editorDom = editor.getDomNode();
                    if (editorDom && !isCtrlPressed) {
                        editorDom.style.cursor = '';
                    }
                    if (currentHoverDecoration.length > 0 && !isCtrlPressed) {
                        editor.deltaDecorations(currentHoverDecoration, []);
                        currentHoverDecoration = [];
                    }
                    return;
                }

                const model = editor.getModel();
                if (!model) return;

                const ref = parseRoutineReferenceAtPosition(model, e.target.position);

                if (ref) {
                    // Valid target detected - change cursor to pointer
                    const editorDom = editor.getDomNode();
                    if (editorDom) {
                        editorDom.style.cursor = 'pointer';
                    }

                    // Add underline decoration to show it's clickable
                    const lineContent = model.getLineContent(e.target.position.lineNumber);
                    const column = e.target.position.column;

                    // Find the exact range of the tag/routine text
                    let startCol = column;
                    let endCol = column;

                    // Expand left to find start of word
                    while (startCol > 1 && /[A-Z0-9%^]/.test(lineContent[startCol - 2])) {
                        startCol--;
                    }
                    // Expand right to find end of word
                    while (endCol <= lineContent.length && /[A-Z0-9%^]/.test(lineContent[endCol - 1])) {
                        endCol++;
                    }

                    currentHoverDecoration = editor.deltaDecorations(currentHoverDecoration, [{
                        range: new monaco.Range(
                            e.target.position.lineNumber,
                            startCol,
                            e.target.position.lineNumber,
                            endCol
                        ),
                        options: {
                            inlineClassName: 'ctrl-hover-underline'
                        }
                    }]);
                } else {
                    // No valid target - reset cursor
                    const editorDom = editor.getDomNode();
                    if (editorDom) {
                        editorDom.style.cursor = '';
                    }
                    if (currentHoverDecoration.length > 0) {
                        editor.deltaDecorations(currentHoverDecoration, []);
                        currentHoverDecoration = [];
                    }
                }
            });

            // --- Breakpoint gutter toggle + Ctrl+Click navigation ---
            editor.onMouseDown(async (e) => {
                const t = e.target.type;

                // Handle breakpoint toggle in gutter
                if (
                    t === monaco.editor.MouseTargetType.GUTTER_GLYPH_MARGIN ||
                    t === monaco.editor.MouseTargetType.GUTTER_LINE_NUMBERS
                ) {
                    const line = e.target.position && e.target.position.lineNumber;
                    if (!line) return;
                    toggleBreakpoint(line, dbgState, editor);
                    e.event.preventDefault();
                    return;
                }

                // Handle Ctrl+Click navigation
                const isCtrlPressed = e.event.ctrlKey || e.event.metaKey;
                if (isCtrlPressed && e.target.position) {
                    const handled = await goToDeclaration(editor, e.target.position, { silentIfMissing: true });
                    if (handled) {
                        e.event.preventDefault();
                        e.event.stopPropagation();
                    }
                }
            });
        });
    }

    // ---------- Terminal & Output ----------

    // Lazy terminal loader; prefers local node_modules/xterm, falls back to CDN if offline assets are missing. TODO: bundle xterm locally once network install is available.
    const terminalEngineSources = [
        './node_modules/xterm/lib/xterm.js',
        'https://cdn.jsdelivr.net/npm/xterm@5.3.0/lib/xterm.js'
    ];
    let terminalEnginePromise = null;
    let terminalFallbackMode = false;
    let terminalResizeObserver = null;

    const getTerminalCwd = () => terminalConfig.startDir || currentProject?.projectPath || envInfoCache?.cwd || (typeof process !== 'undefined' && process.cwd ? process.cwd() : '');
    const focusTerminal = () => {
        const tab = getActiveTerminalTab(globalTerminalState);
        if (tab?.term) {
            tab.term.focus();
        }
    };
    const isTerminalFocused = () => {
        const active = document.activeElement;
        if (!active) return false;
        return !!active.closest?.('#terminalViewport') || !!active.closest?.('.xterm');
    };

    function createTerminalState() {
        return { tabs: [], active: null, counter: 0, sessionMap: {}, _wiredTerminalEvents: false };
    }

    function getActiveTerminalTab(state) {
        if (!state) return null;
        return state.tabs.find(t => t.id === state.active) || null;
    }

    function loadScript(src) {
        return new Promise((resolve, reject) => {
            const existing = document.querySelector(`script[data-src="${src}"]`);
            if (existing) {
                if (existing.dataset.loaded === 'true') {
                    resolve();
                    return;
                }
                existing.addEventListener('load', () => resolve());
                existing.addEventListener('error', () => reject(new Error(`Failed to load ${src}`)));
                return;
            }
            const s = document.createElement('script');
            s.src = src;
            s.async = true;
            s.dataset.src = src;
            s.onload = () => {
                s.dataset.loaded = 'true';
                resolve();
            };
            s.onerror = () => reject(new Error(`Failed to load ${src}`));
            document.head.appendChild(s);
        });
    }

    async function ensureTerminalEngine() {
        if (window.Terminal) {
            terminalFallbackMode = false;
            return window.Terminal;
        }
        if (terminalEnginePromise) return terminalEnginePromise;
        terminalEnginePromise = (async () => {
            let lastErr = null;
            for (const src of terminalEngineSources) {
                try {
                    await loadScript(src);
                    if (window.Terminal) {
                        terminalFallbackMode = false;
                        return window.Terminal;
                    }
                } catch (err) {
                    lastErr = err;
                }
            }
            throw new Error(`Terminal engine unavailable. Install xterm locally or allow CDN. ${lastErr ? lastErr.message : ''}`);
        })();
        try {
            const term = await terminalEnginePromise;
            terminalFallbackMode = false;
            return term;
        } catch (err) {
            terminalEnginePromise = null;
            terminalFallbackMode = true;
            throw err;
        }
    }

    function setTerminalError(message) {
        const errorBox = document.getElementById('terminalError');
        if (!errorBox) return;
        if (!message) {
            errorBox.classList.add('hidden');
            errorBox.textContent = '';
            return;
        }
        errorBox.classList.remove('hidden');
        errorBox.textContent = message;
    }

    function updateTerminalStatusPill() {
        const pill = document.getElementById('terminalStatusPill');
        if (!pill) return;
        const shellLabel = terminalConfig.shellPath ? `Shell: ${terminalConfig.shellPath}` : 'Shell: Default';
        const cwdLabel = getTerminalCwd() || '';
        pill.textContent = shellLabel;
        pill.title = `${shellLabel}\nStart in: ${cwdLabel || 'Project root'}`;
    }

    function ensureTerminalListeners(state) {
        if (!window.ahmadIDE.onTerminalData || state._wiredTerminalEvents) return;
        window.ahmadIDE.onTerminalData((payload) => {
            if (!payload) return;
            const tabId = state.sessionMap[payload.id];
            const tab = state.tabs.find(t => t.id === tabId);
            if (!tab) return;
            if (tab.term) {
                tab.term.write(payload.data || '');
            } else {
                tab.buffer = tab.buffer || [];
                tab.buffer.push(payload.data || '');
            }
        });
        window.ahmadIDE.onTerminalExit((payload) => {
            const tabId = state.sessionMap[payload.id];
            const tab = state.tabs.find(t => t.id === tabId);
            if (!tab) return;
            tab.exited = payload.code;
            tab.sessionId = null;
            delete state.sessionMap[payload.id];
            const message = `\r\n[Process exited with code ${payload.code ?? ''}]`;
            if (tab.term) tab.term.write(message);
            if (tab.container) tab.container.classList.add('exited');
            renderTerminalTabs(state);
        });
        state._wiredTerminalEvents = true;
    }

    function renderTerminalTabs(state) {
        const host = document.getElementById('terminalTabs');
        if (!host) return;
        host.innerHTML = '';
        state.tabs.forEach(t => {
            const tab = document.createElement('div');
            tab.className = 'terminal-tab' + (t.id === state.active ? ' active' : '') + (t.exited !== undefined ? ' exited' : '');
            tab.textContent = t.name;
            tab.onclick = () => activateTerminalTab(state, t.id);
            const close = document.createElement('span');
            close.className = 'tab-close';
            close.textContent = '✕';
            close.title = 'Close tab';
            close.onclick = (e) => {
                e.stopPropagation();
                closeTerminalTab(state, t.id);
            };
            tab.appendChild(close);
            host.appendChild(tab);
        });
    }

    function activateTerminalTab(state, tabId) {
        if (!state) return;
        state.active = tabId;
        renderTerminalTabs(state);
        const viewport = document.getElementById('terminalViewport');
        if (viewport) {
            viewport.querySelectorAll('.terminal-instance').forEach(el => {
                el.classList.toggle('active', el.dataset.tabId === tabId);
            });
        }
        refreshTerminalLayout(state);
        setTimeout(() => focusTerminal(), 30);
    }

    function createTerminalContainer(tabId) {
        const viewport = document.getElementById('terminalViewport');
        if (!viewport) return null;
        const container = document.createElement('div');
        container.className = 'terminal-instance';
        container.dataset.tabId = tabId;
        viewport.appendChild(container);
        return container;
    }

    function buildTerminalOptions() {
        const styles = getComputedStyle(document.documentElement);
        const font = (styles.getPropertyValue('--font-code') || '').trim() || 'monospace';
        const fontSize = parseInt((styles.getPropertyValue('--font-size-code') || '').trim(), 10) || 13;
        const background = (styles.getPropertyValue('--terminal-input-bg') || styles.getPropertyValue('--editor-bg') || '#1e1e1e').trim();
        const foreground = (styles.getPropertyValue('--text') || '#ffffff').trim();
        const selection = (styles.getPropertyValue('--selection-bg') || 'rgba(53,116,240,0.25)').trim();
        return {
            allowProposedApi: true,
            convertEol: true,
            fontFamily: font,
            fontSize,
            lineHeight: 1.2,
            disableStdin: false,
            cursorBlink: true,
            theme: {
                background,
                foreground,
                cursor: foreground,
                selection
            }
        };
    }

    function measureTerminal(tab) {
        if (!tab?.term || !tab?.container) return null;
        const rect = tab.container.getBoundingClientRect();
        if (rect.width < 10 || rect.height < 10) return null;
        if (terminalFallbackMode) return null;
        let cellWidth = null;
        let cellHeight = null;
        const coreDims = tab.term._core?._renderService?.dimensions;
        if (coreDims) {
            cellWidth = coreDims.actualCellWidth || coreDims.css?.cellWidth || null;
            cellHeight = coreDims.actualCellHeight || coreDims.css?.cellHeight || null;
        }
        if (!cellWidth || !cellHeight) {
            const rowEl = tab.container.querySelector('.xterm-rows > div');
            if (rowEl) {
                const rowRect = rowEl.getBoundingClientRect();
                if (rowRect.width && rowRect.height) {
                    cellHeight = rowRect.height;
                    cellWidth = rowRect.width / (tab.term.cols || 80);
                }
            }
        }
        if (!cellWidth || !cellHeight) return null;
        const cols = Math.max(20, Math.floor(rect.width / cellWidth));
        const rows = Math.max(5, Math.floor(rect.height / cellHeight));
        return { cols, rows };
    }

    function refreshTerminalLayout(state, { resizeSession = true } = {}) {
        if (!state) return;
        const tab = getActiveTerminalTab(state);
        if (!tab?.term) return;
        const dims = measureTerminal(tab);
        if (!dims) return;
        if (tab.lastSize && tab.lastSize.cols === dims.cols && tab.lastSize.rows === dims.rows) return;
        try {
            tab.term.resize(dims.cols, dims.rows);
        } catch (e) {
            console.warn('Terminal resize failed', e);
        }
        tab.lastSize = dims;
        if (resizeSession && tab.sessionId && window.ahmadIDE.terminalResize) {
            window.ahmadIDE.terminalResize(tab.sessionId, dims.cols, dims.rows);
        }
    }

    function ensureTerminalResizeObserver(state) {
        if (terminalResizeObserver) return;
        const viewport = document.getElementById('terminalViewport');
        if (!viewport) return;
        terminalResizeObserver = new ResizeObserver(() => refreshTerminalLayout(state));
        terminalResizeObserver.observe(viewport);
    }

    function flushBufferedOutput(tab) {
        if (!tab?.term || !tab?.buffer || !tab.buffer.length) return;
        tab.buffer.forEach(chunk => tab.term.write(chunk));
        tab.buffer = [];
    }

    async function startTerminalSession(tab, state) {
        updateTerminalStatusPill();
        const dims = measureTerminal(tab);
        const sessionOptions = {
            shell: terminalConfig.shellPath || undefined,
            cwd: getTerminalCwd(),
            cols: dims?.cols,
            rows: dims?.rows
        };
        if (!window.ahmadIDE.terminalCreate) {
            tab.term?.writeln('Terminal backend unavailable.');
            return;
        }
        const res = await window.ahmadIDE.terminalCreate(sessionOptions);
        if (res && res.ok) {
            tab.sessionId = res.id;
            state.sessionMap[res.id] = tab.id;
            if (dims && !tab._plain) {
                await window.ahmadIDE.terminalResize(tab.sessionId, dims.cols, dims.rows);
            }
            flushBufferedOutput(tab);
            setTerminalError(null);
            if (tab._plain) {
                tab.term?.writeln('Running with basic renderer. Install xterm for full TUI/ANSI support.');
            }
        } else {
            const msg = res?.error || 'Unknown error starting terminal';
            tab.term?.writeln(`\x1b[31m✗ Failed to start terminal: ${msg}\x1b[0m`);
            setTerminalError(msg);
        }
    }

    function createPlainTerminalTab(id, name, container, state) {
        container.classList.add('plain-terminal');
        const output = document.createElement('pre');
        output.className = 'plain-terminal-output';
        output.textContent = 'Starting...';
        const hiddenInput = document.createElement('textarea');
        hiddenInput.className = 'plain-terminal-input-hidden';
        hiddenInput.setAttribute('aria-label', 'Terminal input');
        container.appendChild(output);
        container.appendChild(hiddenInput);

        const tab = {
            id,
            name,
            buffer: '',
            sessionId: null,
            term: null,
            container,
            lastSize: null,
            _plain: true
        };

        const render = () => {
            output.textContent = tab.buffer || '';
            output.scrollTop = output.scrollHeight;
        };

        const send = async (data) => {
            if (tab.sessionId && window.ahmadIDE.terminalWrite) {
                await window.ahmadIDE.terminalWrite(tab.sessionId, data);
            }
        };

        tab.term = {
            write: (data) => {
                tab.buffer += data || '';
                render();
            },
            writeln: (data) => {
                tab.buffer += `${data ?? ''}\n`;
                render();
            },
            reset: () => {
                tab.buffer = '';
                render();
            },
            resize: () => { },
            dispose: () => { },
            focus: () => hiddenInput.focus()
        };

        const handleKey = async (e) => {
            if (e.key === 'Escape' && terminalConfig.escapeToEditor && !terminalConfig.overrideIdeShortcuts) {
                e.preventDefault();
                activeEditor?.focus();
                return;
            }
            if (e.ctrlKey && e.key.toLowerCase() === 'c') {
                e.preventDefault();
                await sendCtrlC(state);
                return;
            }
            if (!tab.sessionId) return;
            if (e.key === 'Enter') {
                e.preventDefault();
                await send('\r');
                return;
            }
            if (e.key === 'Backspace') {
                e.preventDefault();
                await send('\x7f');
                return;
            }
            if (e.key === 'Tab') {
                e.preventDefault();
                await send('\t');
                return;
            }
            if (e.key.length === 1 && !e.metaKey) {
                e.preventDefault();
                await send(e.key);
            }
        };

        hiddenInput.addEventListener('keydown', handleKey);
        container.addEventListener('mousedown', () => hiddenInput.focus());
        hiddenInput.tabIndex = 0;

        render();
        return tab;
    }

    async function addTerminalTab(state, isDefault = false) {
        try {
            logger.info('TERMINAL_NEW_TAB', { isDefault, existingTabs: state?.tabs?.length });
            let terminalCtor = null;
            try {
                terminalCtor = await ensureTerminalEngine();
            } catch (err) {
                console.warn('Falling back to basic terminal renderer', err);
                setTerminalError(err?.message || 'Terminal engine unavailable; using fallback renderer');
            }
            ensureTerminalListeners(state);
            ensureTerminalResizeObserver(state);
            state.counter += 1;
            const id = `term${state.counter}`;
            const tabName = isDefault ? 'Local' : `Local ${state.counter}`;
            const container = createTerminalContainer(id);
            if (!container) {
                showToast('error', 'Terminal', 'Missing terminal container');
                return;
            }
            const tab = terminalCtor && !terminalFallbackMode
                ? {
                    id,
                    name: tabName,
                    buffer: [],
                    sessionId: null,
                    term: new terminalCtor(buildTerminalOptions()),
                    container,
                    lastSize: null
                }
                : createPlainTerminalTab(id, tabName, container, state);
            state.tabs.push(tab);
            state.active = id;

            if (tab.term && !tab._plain) {
                tab.term.onData(async (data) => {
                    if (tab.sessionId && window.ahmadIDE.terminalWrite) {
                        await window.ahmadIDE.terminalWrite(tab.sessionId, data);
                    }
                });
                tab.term.onResize(({ cols, rows }) => {
                    tab.lastSize = { cols, rows };
                    if (tab.sessionId && window.ahmadIDE.terminalResize) {
                        window.ahmadIDE.terminalResize(tab.sessionId, cols, rows);
                    }
                });
                tab.term.attachCustomKeyEventHandler((ev) => {
                    if (
                        terminalConfig.escapeToEditor &&
                        !terminalConfig.overrideIdeShortcuts &&
                        ev.key === 'Escape' &&
                        !ev.altKey && !ev.ctrlKey && !ev.metaKey && !ev.shiftKey
                    ) {
                        activeEditor?.focus();
                        return false;
                    }
                    return true;
                });
                tab.term.open(container);
            }
            renderTerminalTabs(state);
            activateTerminalTab(state, id);
            refreshTerminalLayout(state, { resizeSession: false });
            setTimeout(() => refreshTerminalLayout(state), 50);
            flushBufferedOutput(tab);
            await startTerminalSession(tab, state);
            logger.info('TERMINAL_TAB_READY', { id, sessionId: tab.sessionId });
        } catch (err) {
            console.error('Failed to create terminal tab', err);
            setTerminalError(err?.message || 'Terminal engine unavailable');
            showToast('error', 'Terminal', err?.message || 'Unable to start terminal');
            logger.error('TERMINAL_START_ERROR', { message: err?.message, stack: err?.stack });
        }
    }

    async function closeTerminalTab(state, id) {
        if (!state) return;
        const idx = state.tabs.findIndex(t => t.id === id);
        if (idx === -1) return;
        const tab = state.tabs[idx];
        logger.info('TERMINAL_CLOSE_TAB', { id, sessionId: tab?.sessionId });
        if (tab.sessionId && window.ahmadIDE.terminalClose) {
            await window.ahmadIDE.terminalClose(tab.sessionId);
            delete state.sessionMap[tab.sessionId];
        }
        if (tab.term) {
            tab.term.dispose();
        }
        tab.container?.remove();
        state.tabs.splice(idx, 1);
        if (state.active === id) {
            state.active = state.tabs.length ? state.tabs[state.tabs.length - 1].id : null;
        }
        renderTerminalTabs(state);
        refreshTerminalLayout(state);
    }

    function appendOutput(text, state) {
        if (!state || !state.tabs.length) return;
        const tab = getActiveTerminalTab(state);
        if (!tab) return;
        const lines = Array.isArray(text) ? text : [text];
        if (tab.term) {
            lines.forEach(line => tab.term.writeln(line || ''));
        } else {
            tab.buffer = tab.buffer || [];
            lines.forEach(line => tab.buffer.push((line || '') + '\n'));
        }
        flushBufferedOutput(tab);
    }

    function clearOutput(state) {
        if (!state) return;
        const tab = getActiveTerminalTab(state);
        if (!tab?.term) return;
        tab.term.reset();
        tab.buffer = [];
        tab.lastSize = null;
        refreshTerminalLayout(state);
    }

    async function sendCtrlC(state) {
        const tab = getActiveTerminalTab(state);
        if (!tab || !tab.sessionId || !window.ahmadIDE.terminalWrite) return;
        await window.ahmadIDE.terminalWrite(tab.sessionId, '\u0003');
        tab.term?.write('^C\r\n');
    }

    async function execTerminalCommand(cmd, state) {
        const tab = getActiveTerminalTab(state);
        if (!tab) return;
        const payload = `${cmd}\n`;
        if (tab.sessionId && window.ahmadIDE.terminalWrite) {
            await window.ahmadIDE.terminalWrite(tab.sessionId, payload);
        }
    }

    // ---------- Breakpoints & Debug UI ----------

    const bpKeyFor = (file, line) => `${file || 'Untitled'}::${line}`;
    const parseBpKey = (key) => {
        const parts = key.split('::');
        const line = parseInt(parts.pop(), 10);
        const file = parts.join('::') || 'Untitled';
        return { file, line, key };
    };

    const getActiveRoutine = () => {
        const normalized = normalizeRoutineTarget(activeRoutineName);
        if (normalized.path || normalized.base) {
            return normalized.path || normalized.base;
        }
        const tab = openTabs.find(t => t.id === activeTabId);
        const tabNorm = normalizeRoutineTarget(tab?.path || tab?.name || '');
        return tabNorm.path || tabNorm.base || 'Untitled';
    };

    function toggleBreakpoint(line, dbgState, editor) {
        dbgState.breakpoints = dbgState.breakpoints || new Set();
        const file = getActiveRoutine();
        const key = bpKeyFor(file, line);
        if (dbgState.breakpoints.has(key)) {
            dbgState.breakpoints.delete(key);
            logger.info('DEBUG_BREAKPOINT_REMOVED', { file, line });
        } else {
            dbgState.breakpoints.add(key);
            logger.info('DEBUG_BREAKPOINT_SET', { file, line });
        }
        renderBreakpoints(dbgState);
        decorateBreakpoints(editor, dbgState);
    }

    function renderBreakpoints(dbgState) {
        const host = document.getElementById('breakpointsList');
        if (!host) return;
        host.innerHTML = '';
        host.classList.add('bp-list');
        const list = Array.from(dbgState.breakpoints || [])
            .map(parseBpKey)
            .filter(bp => !isNaN(bp.line))
            .sort((a, b) => a.file.localeCompare(b.file) || a.line - b.line);

        if (!list.length) {
            host.innerHTML = '<li>No breakpoints</li>';
            return;
        }
        list.forEach(bp => {
            const li = document.createElement('li');
            li.className = 'bp-item';
            const isCurrentFile = bp.file === getActiveRoutine();
            if (isCurrentFile) li.classList.add('active');
            li.style.cursor = 'pointer';
            li.onclick = async () => {
                await focusDebugLocation(bp.file, bp.line, globalTerminalState, dbgStateRef);
                highlightLine(activeEditor, bp.line);
            };

            const meta = document.createElement('div');
            meta.className = 'bp-meta';

            const dot = document.createElement('span');
            dot.className = 'bp-dot';

            const file = document.createElement('span');
            file.className = 'bp-file';
            file.textContent = bp.file;

            const line = document.createElement('span');
            line.className = 'bp-line';
            line.textContent = `:${bp.line}`;

            meta.appendChild(dot);
            meta.appendChild(file);
            meta.appendChild(line);

            const remove = document.createElement('button');
            remove.className = 'bp-remove';
            remove.textContent = '✕';
            remove.title = 'Remove breakpoint';
            remove.onclick = (e) => {
                e.stopPropagation();
                dbgState.breakpoints.delete(bp.key);
                renderBreakpoints(dbgState);
                decorateBreakpoints(activeEditor, dbgState);
            };
            li.appendChild(meta);
            li.appendChild(remove);
            host.appendChild(li);
        });
    }

    function decorateBreakpoints(editor, dbgState) {
        if (!editor) return;
        const currentFile = getActiveRoutine();
        const ranges = Array.from(dbgState.breakpoints || [])
            .map(parseBpKey)
            .filter(bp => bp.file === currentFile)
            .map(({ line }) => ({
                range: new monaco.Range(line, 1, line, 1),
                options: {
                    isWholeLine: false,
                    glyphMarginClassName: 'breakpoint-glyph'
                }
            }));
        editor._bpDecorations = editor.deltaDecorations(editor._bpDecorations || [], ranges);
    }

    function renderLocals(locals) {
        const host = document.getElementById('localsList');
        if (!host) return;
        host.innerHTML = '';
        const entries = locals ? Object.entries(locals) : [];
        if (!entries.length) {
            host.innerHTML = '<li>None</li>';
            return;
        }
        entries.forEach(([k, v]) => {
            const li = document.createElement('li');

            // Check if it's an array
            if (typeof v === 'object' && v._isArray) {
                const elementCount = Object.keys(v._elements || {}).length;
                const expanded = expandedArrayKeys.has(k);

                const toggle = document.createElement('button');
                toggle.className = 'locals-toggle';
                toggle.textContent = expanded ? '-' : '+';
                toggle.title = expanded ? 'Collapse' : 'Expand';
                toggle.onclick = (e) => {
                    e.stopPropagation();
                    if (expanded) expandedArrayKeys.delete(k);
                    else expandedArrayKeys.add(k);
                    renderLocals(locals);
                };

                const label = document.createElement('span');
                label.className = 'locals-array-label';
                label.innerHTML = `<strong>${k}</strong> (Array, ${elementCount} elements)`;
                label.onclick = toggle.onclick;

                li.appendChild(toggle);
                li.appendChild(label);

                if (expanded) {
                    const ul = document.createElement('ul');
                    ul.className = 'locals-children';
                    const sortedKeys = Object.keys(v._elements).sort();
                    const limit = 50;
                    sortedKeys.slice(0, limit).forEach(key => {
                        const subLi = document.createElement('li');
                        subLi.textContent = `${k}${key} = "${v._elements[key]}"`;
                        ul.appendChild(subLi);
                    });
                    if (sortedKeys.length > limit) {
                        const moreLi = document.createElement('li');
                        moreLi.textContent = `... and ${sortedKeys.length - limit} more (truncated)`;
                        moreLi.style.fontStyle = 'italic';
                        ul.appendChild(moreLi);
                    }
                    li.appendChild(ul);
                }
            } else {
                // Simple variable
                li.innerHTML = `<span style="color: #9cdcfe">${k}</span> = <span style="color: #ce9178">"${v}"</span>`;
            }

            host.appendChild(li);
        });
    }

    function renderStack(stack) {
        const host = document.getElementById('stackList');
        if (!host) return;
        host.innerHTML = '';
        if (!stack || !stack.length) {
            host.innerHTML = '<li>Empty</li>';
            return;
        }
        stack.forEach((frame, idx) => {
            const li = document.createElement('li');
            li.textContent = `#${idx} ${frame}`;
            li.style.cursor = 'pointer';
            li.onclick = async () => {
                const match = `${frame}`.match(/^([^:]+):(\d+)/);
                const routine = match ? match[1] : null;
                const line = match ? parseInt(match[2], 10) : null;
                if (routine && line) {
                    await focusDebugLocation(routine, line, globalTerminalState, dbgStateRef);
                    highlightLine(activeEditor, line);
                } else {
                    showToast('info', 'Debug', 'Frame navigation not available (TODO: richer frame data).');
                }
            };
            host.appendChild(li);
        });
    }

    function renderDebugConsole(lines) {
        const host = document.getElementById('debugOutput');
        if (!host) return;
        host.textContent = (lines || []).join('\n');
        host.scrollTop = host.scrollHeight;
    }

    function logDebug(lines, termState, replace = false) {
        const host = document.getElementById('debugOutput');
        if (!host) return;
        const existing = replace ? [] : (host.textContent ? host.textContent.split('\n') : []);
        const next = existing.concat(lines || []);
        host.textContent = next.join('\n');
        host.scrollTop = host.scrollHeight;
        if (termState) appendOutput(lines.join('\n'), termState);
    }

    async function execTerminalCommand(cmd, state) {
        ensureTerminalListeners(state);
        appendOutput(`$ ${cmd}`, state);
        if (cmd.toLowerCase() === 'clear') {
            clearOutput(state);
            return;
        }
        const active = state.tabs.find(t => t.id === state.active);
        if (!active) return;

        if (!active.sessionId && window.ahmadIDE.terminalCreate) {
            const resCreate = await window.ahmadIDE.terminalCreate({
                shell: terminalConfig.shellPath || undefined,
                cwd: getTerminalCwd()
            });
            if (resCreate && resCreate.ok) {
                active.sessionId = resCreate.id;
            } else {
                appendOutput(`✗ Unable to start terminal: ${resCreate?.error || 'unknown error'}`, state);
                showToast('error', 'Terminal', resCreate?.error || 'Unable to start terminal');
                return;
            }
        }

        if (active.sessionId && window.ahmadIDE.terminalWrite) {
            await window.ahmadIDE.terminalWrite(active.sessionId, `${cmd}\n`);
        } else {
            const res = await window.ahmadIDE.hostExec(cmd);
            if (res.ok) {
                if (res.stdout) appendOutput(res.stdout.trimEnd(), state);
                if (res.stderr) appendOutput(res.stderr.trimEnd(), state);
            } else {
                appendOutput(`✗ ${res.error || res.stderr || 'Command failed'}`, state);
                showToast('error', 'Terminal', res.error || res.stderr || 'Command failed');
            }
        }
    }

    // Where to stop: QUIT / Q / EOF
    function isStopLine(editor, lineNumber) {
        const model = editor.getModel();
        if (!model) return true;
        const total = model.getLineCount();
        if (lineNumber > total) return true;

        const text = model.getLineContent(lineNumber).trim().toUpperCase();
        if (text.startsWith('QUIT') || text === 'Q') return true;

        return false;
    }

    // Helper function to check if line contains WRITE or READ and extract the expression
    function parseWriteReadCommand(lineText) {
        if (!lineText) return null;

        const trimmed = lineText.trim();

        // Check for WRITE command (W or WRITE)
        const writeMatch = trimmed.match(/^\s*(?:W|WRITE)\s+(.+)$/i);
        if (writeMatch) {
            return {
                type: 'WRITE',
                expression: writeMatch[1].trim()
            };
        }

        // Check for READ command (R or READ)
        const readMatch = trimmed.match(/^\s*(?:R|READ)\s+(.+)$/i);
        if (readMatch) {
            return {
                type: 'READ',
                expression: readMatch[1].trim()
            };
        }

        return null;
    }

    // Helper function to show WRITE/READ in terminal during debug
    function logDebugIO(editor, lineNum, termState) {
        const model = editor.getModel();
        if (!model) return;

        const lineText = model.getLineContent(lineNum);
        const ioCommand = parseWriteReadCommand(lineText);

        if (ioCommand) {
            const msg = `[Line ${lineNum}] ${ioCommand.type}: ${ioCommand.expression}`;
            appendOutput(`  📝 ${msg}`, termState);
        }
    }

    function normalizeCallStack(stack) {
        if (!Array.isArray(stack)) return [];
        return stack.map(frame => {
            if (typeof frame === 'string') return frame;
            const routine = routineKey(frame?.routine || frame?.returnRoutine || 'TMPDBG');
            const line = frame?.line || frame?.returnLine || '?';
            const tag = frame?.tag || frame?.returnTag;
            return tag ? `${routine}:${line} (${tag})` : `${routine}:${line}`;
        });
    }

    const routineKey = (value, fallback = 'TMPDBG') => {
        const info = normalizeRoutineTarget(value);
        return info.path || info.base || fallback;
    };

    async function focusDebugLocation(routineName, lineNumber, termState, dbgState) {
        const targetInfo = normalizeRoutineTarget(routineName);
        if (!targetInfo.base) return false;

        // TMPDBG (or the active buffer) should map back to the user's current tab
        const homeRoutine = normalizeRoutineTarget(dbgState?.homeRoutine || '');
        if (targetInfo.base === 'TMPDBG' || (homeRoutine.base && targetInfo.base === homeRoutine.base)) {
            if (dbgState?.homeTabId) {
                switchTab(dbgState.homeTabId);
            }
        } else {
            const targetRoutineKey = targetInfo.path || targetInfo.base;
            let tab = findOpenTab(targetRoutineKey);
            if (!tab) {
                const readRes = await window.ahmadIDE.readRoutine(targetRoutineKey);
                if (!readRes.ok) {
                    appendOutput(`✗ Failed to load ${targetRoutineKey}: ${readRes.error || 'Unable to read routine'}`, termState);
                    showToast('error', 'Debug', `Cannot open ${targetRoutineKey}`);
                    return false;
                }
                tab = createTab(targetRoutineKey, readRes.code || '');
            }
            switchTab(tab.id);
        }

        if (activeEditor && lineNumber) {
            activeEditor.revealLineInCenter(lineNumber);
            activeEditor.setPosition({ lineNumber, column: 1 });
        }
        return true;
    }

    // --- Debugger Functions ---

    let currentDebugSession = null;

    function showDebugError(msg) {
        console.error(msg);
        alert(msg);
    }

    function gotoEditorLine(lineNumber) {
        if (!activeEditor || !lineNumber) return;
        activeEditor.revealLineInCenter(lineNumber);
        activeEditor.setPosition({ lineNumber: lineNumber, column: 1 });
        highlightLine(activeEditor, lineNumber);
    }

    function showDebugOutput(output) {
        if (!output || !output.trim()) return;
        if (globalTerminalState) {
            const rawLines = Array.isArray(output) ? output : output.split(/\r?\n/);
            const lines = rawLines.filter((line, idx) => line.length || idx < rawLines.length - 1);
            appendOutput(lines, globalTerminalState);
        }
    }

    async function startDebugSession(editorParam = activeEditor, dbgStateParam = dbgStateRef, terminalState = globalTerminalState, debugBarEl = document.getElementById('debugBar'), bpLinesOverride = null) {
        const editorInstance = editorParam || activeEditor;
        if (!editorInstance) return;
        const code = editorInstance.getValue();

        // Use real breakpoints from the current UI state (fall back to legacy global if present)
        const inferredBps = Array.isArray(bpLinesOverride)
            ? bpLinesOverride
            : (typeof getBpLines === 'function' ? getBpLines() : []);
        const legacyBps = (window.editorBreakpoints && Array.isArray(window.editorBreakpoints))
            ? window.editorBreakpoints
            : [];
        const bpLines = Array.from(new Set([...(inferredBps || []), ...(legacyBps || [])]))
            .map(n => parseInt(n, 10))
            .filter(n => Number.isInteger(n) && n > 0);
        window.editorBreakpoints = bpLines; // keep legacy helpers in sync
        const breakpoints = bpLines.map(line => ({ line }));

        // Reset UI before starting to ensure clean state, but keep debug mode toggle untouched
        resetDebugUI(false);

        console.log('Starting debug session...', { breakpoints });

        showToast('info', 'Debug', 'Initializing debug session...');

        let res;
        try {
            res = await window.ahmadIDE.debugStart(code, breakpoints, null);
        } catch (e) {
            showToast('error', 'Debug', 'Start exception: ' + e.message);
            console.error(e);
            return;
        }

        if (!res || !res.ok) {
            const rawError = res?.error || 'Unknown error';
            const outputText = (res && res.output) ? res.output.trim() : '';
            const firstLine = outputText
                ? (outputText.split(/\r?\n/).find(Boolean) || rawError)
                : rawError;
            const shortMsg = firstLine.length > 140 ? `${firstLine.slice(0, 140)}…` : firstLine;
            const toastMsg = outputText ? `${shortMsg} (see Terminal for details)` : shortMsg;
            console.error('Start failed:', rawError, outputText);

            if (outputText) {
                ensureBottomPanel('terminalPanel');
                appendOutput([
                    '--- Debug start failed ---',
                    rawError,
                    outputText,
                    '--- end ---'
                ], globalTerminalState);
            }

            showToast('error', 'Debug', toastMsg);
            resetDebugUI(true, true); // keep armed so user can fix and retry
            return;
        }

        console.log('debugStart response OK', res);
        console.log('[DEBUG] Full response object:', JSON.stringify(res, null, 2));

        currentDebugSession = {
            id: res.sessionId,
            engine: res.engine || 'zstep',
            currentLine: res.currentLine,
            currentRoutine: res.currentRoutine || 'TMPDBG',
            stack: res.stack || [],
            locals: res.locals || {},
            ready: res.ready || false
        };
        if (dbgStateParam) {
            dbgStateParam.sessionId = res.sessionId;
            dbgStateParam.locals = currentDebugSession.locals;
            dbgStateParam.state = currentDebugSession.ready ? 'ready' : 'paused';
            dbgStateParam.currentLine = currentDebugSession.currentLine || null;
            dbgStateParam.currentRoutine = currentDebugSession.currentRoutine;
        }

        console.log('Debug Session Started:', currentDebugSession);

        // Show initial variables and stack
        renderLocals(currentDebugSession.locals);
        renderStack(currentDebugSession.stack);
        updateDebugButtonState();

        // Remember currently open bottom panel so we can restore after debug
        if (dbgStateParam) {
            dbgStateParam.previousBottomPanel = toolWindowState?.bottom?.activePanel || null;
        }

        // Show debug bar if hidden
        const bar = debugBarEl || document.getElementById('debugBar');
        console.log('[DEBUG UI] debugBar element:', bar);
        if (bar) {
            bar.classList.remove('hidden');
            bar.style.display = 'flex'; // Force show
            console.log('[DEBUG UI] debugBar shown');
        }

        // Auto-open Debug panel without hiding the terminal permanently
        ensureBottomPanel('debugPanel');

        // If the engine is ready but not executing, automatically run to first breakpoint (or stop on entry if none)
        if (currentDebugSession.ready && !currentDebugSession.currentLine) {
            try {
                const initialResult = (breakpoints && breakpoints.length)
                    ? await window.ahmadIDE.debugContinue(currentDebugSession.id)
                    : await window.ahmadIDE.debugStep(currentDebugSession.id, 'into');
                handleDebugResult(initialResult);
                return;
            } catch (err) {
                showToast('error', 'Debug', 'Failed to start execution: ' + err.message);
                return;
            }
        }

        // If the backend already paused us somewhere, reflect that immediately
        if (currentDebugSession.currentLine) {
            console.log('[DEBUG] Debugger in PAUSED state - enabling all buttons');
            setDebugButtons(true);
            gotoEditorLine(currentDebugSession.currentLine);
            showToast('success', 'Debug', 'Debugger paused at line ' + currentDebugSession.currentLine);
            return;
        }

        console.log('[DEBUG] Debugger initialized without a pause location (unexpected).');
    }

    async function debugStepInto() {
        if (!currentDebugSession || !currentDebugSession.id) {
            showToast('warn', 'Debug', 'No active debug session');
            return;
        }
        const result = await window.ahmadIDE.debugStep(currentDebugSession.id, 'into');
        handleDebugResult(result);
    }

    async function debugStepOver() {
        if (!currentDebugSession || !currentDebugSession.id) {
            showToast('warn', 'Debug', 'No active debug session');
            return;
        }
        const result = await window.ahmadIDE.debugStep(currentDebugSession.id, 'over');
        handleDebugResult(result);
    }

    async function debugStepOut() {
        if (!currentDebugSession || !currentDebugSession.id) {
            showToast('warn', 'Debug', 'No active debug session');
            return;
        }
        const result = await window.ahmadIDE.debugStep(currentDebugSession.id, 'out');
        handleDebugResult(result);
    }

    async function debugContinue() {
        console.log('[DEBUG] debugContinue called, session:', currentDebugSession);
        if (!currentDebugSession || !currentDebugSession.id) {
            showToast('warn', 'Debug', 'No active debug session');
            return;
        }
        const result = await window.ahmadIDE.debugContinue(currentDebugSession.id);
        handleDebugResult(result);
    }

    async function debugStop() {
        if (!currentDebugSession || !currentDebugSession.id) {
            // Just reset UI if no session
            resetDebugUI(true);
            return;
        }
        try {
            const result = await window.ahmadIDE.debugStop(currentDebugSession.id);
            if (result && result.ok) {
                showDebugOutput(result.output || '');
                resetDebugUI(true, true); // keep armed so next Run still debugs
                showToast('info', 'Debug', 'Debug session stopped');
            } else {
                showDebugError('Stop failed: ' + (result?.error || 'Unknown'));
            }
        } catch (err) {
            showDebugError('Stop failed: ' + (err?.message || 'Unknown'));
        } finally {
            updateDebugButtonState();
        }
    }

    function handleDebugResult(result) {
        if (!result) {
            showToast('error', 'Debug', 'No response from debugger');
            return;
        }

        showDebugOutput(result.output || '');

        if (!result.ok) {
            const msg = result?.error || 'Unknown error';
            if (msg === 'Program finished' || msg === 'end') {
                resetDebugUI(true, true); // keep armed so Run will debug again
                showToast('success', 'Debug', 'Program finished');
            } else {
                // Non-fatal error - show error but keep debug session active if it still exists
                showToast('error', 'Debug', msg);
                console.error('[DEBUG] Debug error:', msg);

                // Only reset UI if error indicates session is dead
                if (msg.includes('Session not found') || msg.includes('process error') ||
                    msg.includes('Failed to send command') || result.output) {
                    resetDebugUI(true, true); // keep armed for retry
                }
            }
            return;
        }

        if (result.currentLine) {
            // Execution has started - transition from "ready" to "running"
            const wasReady = currentDebugSession?.ready && !currentDebugSession?.currentLine;

            if (!currentDebugSession) {
                currentDebugSession = { id: result.sessionId || null, engine: 'zstep' };
            }

            currentDebugSession.currentLine = result.currentLine;
            currentDebugSession.currentRoutine = result.currentRoutine || currentDebugSession.currentRoutine;
            currentDebugSession.stack = result.stack || currentDebugSession.stack || [];
            // CRITICAL: Always update locals from result (don't use fallback to old locals)
            currentDebugSession.locals = result.locals || {};
            currentDebugSession.ready = false; // No longer in ready state

            if (dbgStateRef) {
                dbgStateRef.locals = currentDebugSession.locals;
                dbgStateRef.currentLine = result.currentLine;
                dbgStateRef.state = 'paused';
                dbgStateRef.currentRoutine = currentDebugSession.currentRoutine;
            }

            // If we stopped on a user breakpoint, show that exact line.
            // Otherwise (step/continue), show the previously executed line so locals feel in-sync.
            const bpLines = (window.editorBreakpoints && Array.isArray(window.editorBreakpoints))
                ? window.editorBreakpoints
                : [];
            const isUserBreakpoint = bpLines.includes(result.currentLine);
            const displayLine = isUserBreakpoint
                ? result.currentLine
                : (result.currentLine > 1 ? result.currentLine - 1 : result.currentLine);
            gotoEditorLine(displayLine);

            // Update Variables and Stack panels IMMEDIATELY
            console.log('[DEBUG] Updating variables panel with', Object.keys(currentDebugSession.locals).length, 'variables');
            renderLocals(currentDebugSession.locals);
            renderStack(currentDebugSession.stack);

            // Enable all debug buttons (debugger is paused at a line)
            setDebugButtons(true);

            if (wasReady) {
                console.log('[DEBUG] Execution started (transitioned from ready to running)');
            }

            console.log('Debug State Updated:', currentDebugSession);
            console.log('[DEBUG] Current variables:', currentDebugSession.locals);
        }
    }

    function updateDebugButtonState() {
        const btn = document.getElementById('debugStartBtn');
        if (!btn) return;
        const armed = !!(dbgStateRef && dbgStateRef.debugModeEnabled);
        const live = !!(currentDebugSession && currentDebugSession.id);
        btn.classList.toggle('armed', armed);
        btn.classList.toggle('live', live);
        btn.title = live
            ? 'Debugging (session active)'
            : armed
                ? 'Debug armed (Run to start)'
                : 'Debug (Shift+F9)';
    }

    function resetDebugUI(clearSession = false, keepArmed = false) {
        setDebugButtons(false);
        const bar = document.getElementById('debugBar');
        if (bar) {
            bar.classList.add('hidden');
            bar.style.display = 'none';
        }

        // Clear editor decorations
        if (activeEditor) {
            // Remove highlight line
            highlightLine(activeEditor, null);
        }

        // Clear stack and locals panels
        renderStack([]);
        renderLocals({});

        // Reset session
        if (clearSession) {
            currentDebugSession = null;
            if (dbgStateRef) {
                dbgStateRef.sessionId = null;
                dbgStateRef.locals = {};
                dbgStateRef.state = 'stopped';
                dbgStateRef.currentLine = null;
                if (dbgStateRef.previousBottomPanel) {
                    ensureBottomPanel(dbgStateRef.previousBottomPanel);
                    dbgStateRef.previousBottomPanel = null;
                }
            }
            if (!keepArmed && dbgStateRef) {
                dbgStateRef.debugModeEnabled = false;
            }
        }
        updateDebugButtonState();
    }

    // Helper to toggle button states
    function setDebugButtons(enabled) {
        const ids = [
            'dbgStepIntoBtn', 'dbgStepOverBtn', 'dbgStepOutBtn',
            'dbgContinueBtn', 'dbgStopBtn', 'dbgRestartBtn', 'dbgPauseBtn'
        ];
        console.log('[DEBUG] setDebugButtons called with enabled =', enabled);
        ids.forEach(id => {
            const el = document.getElementById(id);
            if (el) {
                if (enabled) {
                    el.removeAttribute('disabled');
                    el.disabled = false;
                } else {
                    el.setAttribute('disabled', 'true');
                    el.disabled = true;
                }
                console.log(`[DEBUG] Button ${id}: disabled = ${el.disabled}, hasAttribute = ${el.hasAttribute('disabled')}`);
            } else {
                console.log(`[DEBUG] Button ${id} not found in DOM`);
            }
        });
    }

    // Wire buttons (called during init or effectively replaced here)
    // We'll wrap this in a safe executor or assume the DOM is ready since this file runs after DOM?
    // Actually renderer.js usually runs after DOM or is deferred.
    // We will attach listeners if elements exist.
    setTimeout(() => {
        const bind = (id, fn) => {
            const el = document.getElementById(id);
            if (el) {
                // Remove old listeners involves cloning or removing event listener if we had the ref.
                // Simpler: clone node to wipe old listeners
                const newEl = el.cloneNode(true);
                el.parentNode.replaceChild(newEl, el);
                newEl.addEventListener('click', fn);
            }
        };

        // Note: adapting to ACTUAL HTML IDs found in index.html
        // DEBUG BUTTON REMOVED: Debug button only toggles mode, doesn't start session
        // Session starts when RUN button is clicked (handled in runBtn click handler)
        // bind('debugStartBtn', startDebugSession); // REMOVED
        bind('dbgStepIntoBtn', debugStepInto);
        bind('dbgStepOverBtn', debugStepOver);
        bind('dbgStepOutBtn', debugStepOut);
        bind('dbgContinueBtn', debugContinue);
        bind('dbgStopBtn', debugStop);

        // Also bind the toolbar Step buttons if they exist separately?
        // In the HTML I saw specific IDs on the debug bar.
    }, 500);


    // ---------- Problems / Docker / Misc ----------

    function renderProblems(items) {
        const list = document.getElementById('problemsList');
        if (!list) return;

        const problems = Array.isArray(items) ? items : [];
        const limited = problems.slice(0, maxProblemItems);
        const trimmed = problems.length > limited.length;

        // Use DocumentFragment to batch DOM operations (reduces reflows)
        const fragment = document.createDocumentFragment();

        if (!limited.length) {
            const li = document.createElement('li');
            li.textContent = 'No problems.';
            fragment.appendChild(li);
            list.innerHTML = '';
            list.appendChild(fragment);
            return;
        }

        const iconFor = (sev) => {
            const s = (sev || 'info').toLowerCase();
            if (s.startsWith('err')) return '⛔';
            if (s.startsWith('warn')) return '⚠';
            return 'ℹ';
        };

        limited.forEach(item => {
            const li = document.createElement('li');
            const sev = item.severity || 'info';
            li.className = `problem-item ${sev.toLowerCase()}`;
            li.dataset.line = item.line || '';

            const icon = document.createElement('span');
            icon.className = 'problem-icon';
            icon.textContent = iconFor(sev);

            const text = document.createElement('span');
            text.className = 'problem-text';
            const lineInfo = item.line ? ` (line ${item.line})` : '';
            const codeInfo = item.code ? ` [${item.code}]` : '';
            const msg = item.message || '';
            text.textContent = `${sev}${codeInfo}: ${msg}${lineInfo}`;
            li.title = `${sev.toUpperCase()}${codeInfo} ${msg}${lineInfo}`;

            li.appendChild(icon);
            li.appendChild(text);
            li.onclick = () => {
                const ln = parseInt(li.dataset.line || '0', 10);
                if (ln) revealLine(ln);
            };
            fragment.appendChild(li);
        });
        if (trimmed) {
            const li = document.createElement('li');
            li.className = 'problem-item info';
            li.textContent = `Showing first ${maxProblemItems} issues...`;
            fragment.appendChild(li);
        }

        // Single DOM operation: clear and append all at once
        list.innerHTML = '';
        list.appendChild(fragment);

        updateProblemSummary(limited);
        setActiveDebugTab(activeDebugTab);
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
                await window.ahmadIDE.setConnection('docker', { docker: { containerId: c.id } });
                setConnStatus(`Docker: ${c.name}`, 'success');
                await loadRoutineList(
                    routineState,
                    editor,
                    document.getElementById('routineSearch')?.value || '',
                    null
                );
                if (onSelect) onSelect();
                if (collapseEl) collapseEl.classList.add('collapsed');
                setTimeout(() => editor.layout(), 120);
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

    function loadExtensionState() {
        try {
            const raw = localStorage.getItem('ahmadIDE:extensions');
            if (raw) {
                const parsed = JSON.parse(raw);
                extensionsState.enabled = parsed.enabled || {};
                extensionsState.selectedId = parsed.selectedId || null;
            }
        } catch (e) {
            // ignore storage errors
        }
        updateDebugButtonState();
    }

    function persistExtensionState() {
        try {
            localStorage.setItem('ahmadIDE:extensions', JSON.stringify({
                enabled: extensionsState.enabled,
                selectedId: extensionsState.selectedId
            }));
        } catch (e) {
            // ignore storage failures
        }
    }

    function initExtensionsData() {
        extensionsState.installed = [
            {
                id: 'mumps-lint',
                name: 'MUMPS Lint',
                description: 'Runs MUMPS lint checks on save.',
                icon: '🧹'
            },
            {
                id: 'code-formatter',
                name: 'Code Formatter',
                description: 'Formats MUMPS routines using built-in formatter.',
                icon: '✨'
            },
            {
                id: 'ssh-tools',
                name: 'SSH Tools',
                description: 'Adds SSH utilities and quick commands.',
                icon: '🔑'
            }
        ];
        extensionsState.installed.forEach(ext => {
            if (extensionsState.enabled[ext.id] === undefined) {
                extensionsState.enabled[ext.id] = true;
            }
        });
        if (!extensionsState.selectedId && extensionsState.installed.length) {
            extensionsState.selectedId = extensionsState.installed[0].id;
        }
    }

    function renderExtensionsDetail(ext) {
        const host = document.getElementById('extensionDetail');
        if (!host) return;
        if (!ext) {
            host.innerHTML = '<div class="pane-title">Select an extension</div><div class="pane-subtitle">Details will appear here.</div>';
            return;
        }
        const enabled = !!extensionsState.enabled[ext.id];
        host.innerHTML = `
            <div class="detail-title">${ext.name}</div>
            <div class="detail-desc">${ext.description || 'No description available.'}</div>
            <div class="detail-status">Status: <strong>${enabled ? 'Enabled' : 'Disabled'}</strong></div>
            <button class="btn ${enabled ? 'ghost' : 'primary'}" id="extToggleBtn">${enabled ? 'Disable' : 'Enable'}</button>
        `;
        document.getElementById('extToggleBtn')?.addEventListener('click', () => {
            extensionsState.enabled[ext.id] = !enabled;
            persistExtensionState();
            renderExtensionsList();
        });
    }

    function renderExtensionsList() {
        const host = document.getElementById('extensionsList');
        if (!host) return;
        host.innerHTML = '';
        if (!extensionsState.installed.length) {
            host.textContent = 'No extensions installed.';
            return;
        }
        extensionsState.installed.forEach(ext => {
            const row = document.createElement('div');
            row.className = 'extension-row';
            if (extensionsState.selectedId === ext.id) row.classList.add('active');
            const icon = document.createElement('div');
            icon.className = 'extension-icon';
            icon.textContent = ext.icon || '⋯';
            const meta = document.createElement('div');
            meta.className = 'extension-meta';
            const name = document.createElement('div');
            name.className = 'extension-name';
            name.textContent = ext.name;
            const desc = document.createElement('div');
            desc.className = 'extension-desc';
            desc.textContent = ext.description || '';
            meta.appendChild(name);
            meta.appendChild(desc);
            const toggle = document.createElement('input');
            toggle.type = 'checkbox';
            toggle.className = 'extension-toggle';
            toggle.checked = !!extensionsState.enabled[ext.id];
            toggle.addEventListener('click', (e) => {
                e.stopPropagation();
                extensionsState.enabled[ext.id] = !extensionsState.enabled[ext.id];
                persistExtensionState();
                renderExtensionsDetail(ext);
            });
            row.appendChild(icon);
            row.appendChild(meta);
            row.appendChild(toggle);
            row.addEventListener('click', () => {
                extensionsState.selectedId = ext.id;
                persistExtensionState();
                renderExtensionsList();
                renderExtensionsDetail(ext);
            });
            host.appendChild(row);
        });
        const selected = extensionsState.installed.find(e => e.id === extensionsState.selectedId);
        renderExtensionsDetail(selected);
    }

    function initExtensionsView() {
        loadExtensionState();
        initExtensionsData();
        renderExtensionsList();
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
        return [
            'HELLO ; sample routine',
            '    WRITE "Hello, Ahmad IDE!", !',
            '    SET X=1',
            '    IF X=1 WRITE "X is one", !',
            '    QUIT'
        ].join('\n');
    }

    function registerMumpsLanguage() {
        monaco.languages.register({
            id: 'mumps',
            extensions: ['.m', '.mps', '.mumps'],
            aliases: ['MUMPS', 'M'],
        });

        monaco.languages.setLanguageConfiguration('mumps', {
            comments: { lineComment: ';' },
            brackets: [['(', ')']],
            autoClosingPairs: [
                { open: '(', close: ')' },
                { open: '"', close: '"', notIn: ['string'] }
            ],
            surroundingPairs: [
                { open: '(', close: ')' },
                { open: '"', close: '"' }
            ],
            wordPattern: /\$?[A-Za-z%][\w.%]*/,
            indentationRules: {
                increaseIndentPattern: /^\s*\b(IF|ELSE|FOR|DO)\b.*$/i,
                decreaseIndentPattern: /^\s*\b(QUIT|Q)\b/i
            }
        });

        monaco.languages.setMonarchTokensProvider('mumps', {
            defaultToken: '',
            ignoreCase: true,
            tokenizer: {
                root: [
                    [/^[A-Za-z%][A-Za-z0-9]*/, 'label'],
                    [/;.*/, 'comment'],
                    [/"([^"]|"")*"/, 'string'],
                    [/\$[A-Z][A-Z0-9]*/, 'predefined'],
                    [/\b(SET|S|NEW|N|KILL|K|DO|D|IF|ELSE|FOR|F|QUIT|Q|WRITE|W|READ|R|GOTO|G|HANG|H|OPEN|O|CLOSE|C|MERGE|M|VIEW|USE|LOCK|L|XECUTE|X)\b/, 'keyword'],
                    [/[0-9]+(\.[0-9]+)?/, 'number'],
                    [/\^[A-Za-z][\w]*/, 'type.identifier'],
                    [/[$A-Za-z%][\w.]*/, 'identifier'],
                ]
            }
        });
    }

    function registerMumpsThemes() {
        monaco.editor.defineTheme('mumps-light', {
            base: 'vs',
            inherit: true,
            rules: [
                { token: 'comment', foreground: '8c8c8c', fontStyle: 'italic' },
                { token: 'string', foreground: '6a8759' },
                { token: 'number', foreground: '1750eb' },
                { token: 'keyword', foreground: '0033b3', fontStyle: 'bold' },
                { token: 'predefined', foreground: '871094', fontStyle: 'bold' },
                { token: 'label', foreground: '0033b3', fontStyle: 'bold' },
                { token: 'type.identifier', foreground: '000000' },
            ],
            colors: {
                'editor.background': '#ffffff',
                'editorGutter.background': '#f0f0f0',
                'editorLineNumber.foreground': '#999999',
                'editorLineNumber.activeForeground': '#4d4d4d',
                'editor.selectionBackground': '#a6d2ff',
                'editor.selectionHighlightBackground': '#e8f2ff',
                'editor.inactiveSelectionBackground': '#d4d4d4',
                'editor.lineHighlightBackground': '#fcfcfc',
                'editor.lineHighlightBorder': '#efefef',
                'editorCursor.foreground': '#000000',
                'editorBracketMatch.border': '#4a9eff',
                'editorIndentGuide.background': '#d3d3d3',
                'editorIndentGuide.activeBackground': '#939393',
                'editorWhitespace.foreground': '#d3d3d3'
            }
        });

        monaco.editor.defineTheme('mumps-dark', {
            base: 'vs-dark',
            inherit: true,
            rules: [
                { token: 'comment', foreground: '808080', fontStyle: 'italic' },
                { token: 'string', foreground: '6a8759' },
                { token: 'number', foreground: '6897bb' },
                { token: 'keyword', foreground: 'cc7832', fontStyle: 'bold' },
                { token: 'predefined', foreground: 'ffc66d', fontStyle: 'bold' },
                { token: 'label', foreground: 'ffc66d', fontStyle: 'bold' },
                { token: 'type.identifier', foreground: 'a9b7c6' },
            ],
            colors: {
                'editor.background': '#2b2b2b',
                'editorGutter.background': '#313335',
                'editorLineNumber.foreground': '#606366',
                'editorLineNumber.activeForeground': '#a4a3a3',
                'editor.selectionBackground': '#214283',
                'editor.selectionHighlightBackground': '#3a3d41',
                'editor.inactiveSelectionBackground': '#3a3d41',
                'editor.lineHighlightBackground': '#323232',
                'editor.lineHighlightBorder': '#3a3d41',
                'editorCursor.foreground': '#bbbbbb',
                'editorBracketMatch.border': '#4a9eff',
                'editorIndentGuide.background': '#3c3f41',
                'editorIndentGuide.activeBackground': '#515658',
                'editorWhitespace.foreground': '#3c3f41'
            }
        });

        monaco.editor.defineTheme('mumps-earth', {
            base: 'vs-dark',
            inherit: true,
            rules: [
                { token: 'comment', foreground: '808080', fontStyle: 'italic' },
                { token: 'string', foreground: '6a8759' },
                { token: 'number', foreground: '6897bb' },
                { token: 'keyword', foreground: 'cc7832', fontStyle: 'bold' },
                { token: 'predefined', foreground: 'ffc66d', fontStyle: 'bold' },
                { token: 'label', foreground: 'ffc66d', fontStyle: 'bold' },
                { token: 'type.identifier', foreground: 'a9b7c6' },
            ],
            colors: {
                'editor.background': '#2b2b2b',
                'editorGutter.background': '#313335',
                'editorLineNumber.foreground': '#606366',
                'editorLineNumber.activeForeground': '#a4a3a3',
                'editor.selectionBackground': '#214283',
                'editor.selectionHighlightBackground': '#3a3d41',
                'editor.inactiveSelectionBackground': '#3a3d41',
                'editor.lineHighlightBackground': '#323232',
                'editor.lineHighlightBorder': '#3a3d41',
                'editorCursor.foreground': '#bbbbbb',
                'editorBracketMatch.border': '#4a9eff',
                'editorIndentGuide.background': '#3c3f41',
                'editorIndentGuide.activeBackground': '#515658',
                'editorWhitespace.foreground': '#3c3f41'
            }
        });
    }

    function registerMumpsCompletion(data) {
        const fallbackKeywords = [
            'SET', 'NEW', 'KILL', 'DO', 'IF', 'ELSE', 'FOR', 'QUIT',
            'WRITE', 'READ', 'GOTO', 'HANG', 'OPEN', 'CLOSE', 'MERGE',
            'VIEW', 'USE', 'LOCK', 'XECUTE', 'BREAK', 'HALT', 'JOB'
        ];
        const fallbackSysvars = [
            '$T', '$D', '$O', '$P', '$L', '$E', '$JOB', '$IO', '$ZT', '$ZB', '$ZEOF', '$ZTRAP', '$ZERROR',
            '$PIECE', '$EXTRACT', '$FIND', '$QLENGTH', '$QSUBSCRIPT', '$QUERY', '$ZDATE', '$ZTIME', '$HOROLOG'
        ];
        const fallbackSnippets = [
            {
                label: 'IF/ELSE',
                kind: monaco.languages.CompletionItemKind.Snippet,
                insertText: 'IF ${1:condition} {\n  ${2:; code}\n} ELSE  {\n  ${3:; code}\n}\n',
                documentation: 'IF/ELSE structure',
                insertTextRules: monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet
            },
            {
                label: 'FOR loop',
                kind: monaco.languages.CompletionItemKind.Snippet,
                insertText: 'FOR ${1:i}=1:1:${2:n} {\n  ${3:; code}\n}\n',
                documentation: 'FOR loop snippet',
                insertTextRules: monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet
            },
            {
                label: 'Label template',
                kind: monaco.languages.CompletionItemKind.Snippet,
                insertText: 'MAIN ; Routine\n    SET ${1:var}=0\n    QUIT\n',
                documentation: 'Simple routine template',
                insertTextRules: monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet
            }
        ];

        const kindMap = {
            Command: monaco.languages.CompletionItemKind.Keyword,
            Function: monaco.languages.CompletionItemKind.Function,
            Variable: monaco.languages.CompletionItemKind.Variable,
            Snippet: monaco.languages.CompletionItemKind.Snippet
        };

        const buildSuggestion = (entry) => {
            if (!entry || !entry.label) return null;
            const insertText = entry.insertText || entry.label;
            const isSnippet = entry.kind === 'Snippet' || /\$\{\d+:?/.test(insertText);
            return {
                label: entry.label,
                kind: kindMap[entry.kind] || monaco.languages.CompletionItemKind.Text,
                insertText,
                detail: entry.detail || entry.abbr || '',
                documentation: entry.documentation || '',
                filterText: entry.abbr || entry.label,
                sortText: entry.abbr ? `0_${entry.abbr}` : undefined,
                insertTextRules: isSnippet ? monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet : undefined
            };
        };

        const buildDataset = () => {
            const suggestions = [];
            if (data && typeof data === 'object') {
                ['commands', 'intrinsicFunctions', 'intrinsicVariables', 'snippets'].forEach((key) => {
                    (data[key] || []).forEach((entry) => {
                        const sug = buildSuggestion(entry);
                        if (sug) suggestions.push(sug);
                    });
                });
            }

            if (!suggestions.length) {
                fallbackKeywords.forEach(k => {
                    suggestions.push({
                        label: k,
                        kind: monaco.languages.CompletionItemKind.Keyword,
                        insertText: k + ' ',
                        documentation: `${k} command`
                    });
                });
                fallbackSysvars.forEach(v => {
                    suggestions.push({
                        label: v,
                        kind: monaco.languages.CompletionItemKind.Variable,
                        insertText: v,
                        documentation: 'System variable'
                    });
                });
                suggestions.push(...fallbackSnippets);
            }
            return suggestions;
        };

        monaco.languages.registerCompletionItemProvider('mumps', {
            triggerCharacters: [' ', '$', '^', '.'],
            provideCompletionItems: () => ({ suggestions: buildDataset() })
        });
    }

    function markerSeverity(sev) {
        if (sev === 'error') return monaco.MarkerSeverity.Error;
        if (sev === 'warning') return monaco.MarkerSeverity.Warning;
        return monaco.MarkerSeverity.Info;
    }

    function normalizeSeverity(sev) {
        if (!sev) return 'info';
        const lower = (sev + '').toLowerCase();
        if (lower.startsWith('err')) return 'error';
        if (lower.startsWith('warn')) return 'warning';
        return 'info';
    }

    function updateProblemSummary(items) {
        const pill = document.getElementById('problemsSummary');
        if (!pill) return;
        const problems = Array.isArray(items) ? items : [];
        const total = problems.length;
        const highest = problems.reduce((acc, cur) => {
            const sev = normalizeSeverity(cur.severity);
            if (sev === 'error') return 'error';
            if (sev === 'warning' && acc !== 'error') return 'warning';
            return acc;
        }, 'info');

        pill.textContent = `Problems: ${total}`;
        if (highest === 'error') {
            pill.style.background = 'rgba(248,113,113,0.18)';
            pill.style.color = '#fecdd3';
            pill.style.borderColor = 'rgba(248,113,113,0.35)';
        } else if (highest === 'warning') {
            pill.style.background = 'rgba(252,211,77,0.18)';
            pill.style.color = '#fcd34d';
            pill.style.borderColor = 'rgba(252,211,77,0.35)';
        } else {
            pill.style.background = '';
            pill.style.color = '';
            pill.style.borderColor = '';
        }
    }

    function hasLintRules(linter) {
        return !!(linter && linter.rules && Object.keys(linter.rules || {}).length);
    }

    function applyLintMarkers(model, issues) {
        const markers = (issues || []).map(issue => ({
            severity: markerSeverity(issue.severity),
            message: issue.message || issue.description || 'Issue',
            startLineNumber: issue.line || 1,
            startColumn: issue.column || 1,
            endLineNumber: issue.line || 1,
            endColumn: (issue.column || 1) + 1
        }));
        monaco.editor.setModelMarkers(model, 'mumps-check', markers);
    }

    function validateMumps(model) {
        if (!model) return;
        const text = model.getValue();
        const isHuge = text.length > maxLintTextLength;
        if (isHuge) {
            monaco.editor.setModelMarkers(model, 'mumps-check', []);
            renderProblems([{
                severity: 'info',
                message: `Lint disabled for large file (${Math.round(text.length / 1000)} KB)`,
                line: 1,
                code: 'LINT_SKIPPED'
            }]);
            if (!lintSkipNotified) {
                showToast('info', 'Linting paused', 'Large file detected; skipping lint to keep typing responsive.');
                lintSkipNotified = true;
            }
            return [];
        }
        // Reset the skip notification only after we are back under the threshold
        if (lintSkipNotified && !isHuge) {
            lintSkipNotified = false;
        }
        const linter = window._mumpsLinter || mumpsLinter;
        const combinedMarkers = [];
        const problems = [];
        const Parser = window._mumpsParserClass || MUMPSParserClass;

        if (hasLintRules(linter)) {
            const res = linter.lint(text || '', { mode: 'edit' });
            (res.issues || []).forEach(issue => {
                const sev = normalizeSeverity(issue.severity);
                problems.push({
                    severity: sev,
                    message: issue.message || issue.description || 'Issue',
                    line: issue.line || null,
                    code: issue.ruleId || issue.code || null
                });
                combinedMarkers.push({
                    severity: markerSeverity(sev),
                    message: issue.ruleId ? `[${issue.ruleId}] ${issue.message || issue.description || 'Issue'}` : (issue.message || issue.description || 'Issue'),
                    startLineNumber: issue.line || 1,
                    startColumn: issue.column || 1,
                    endLineNumber: issue.line || 1,
                    endColumn: (issue.column || 1) + 1
                });
            });
        } else {
            const lines = text.split('\n');
            let openQuotes = 0;
            let parenBalance = 0;
            lines.forEach((line, idx) => {
                const lineNo = idx + 1;
                const quoteCount = (line.match(/\"/g) || []).length;
                openQuotes = (openQuotes + quoteCount) % 2;
                if (openQuotes === 1) {
                    problems.push({
                        severity: 'warning',
                        message: 'Unclosed string literal',
                        line: lineNo,
                        code: 'LEX_UNCLOSED_STRING'
                    });
                    combinedMarkers.push({
                        severity: monaco.MarkerSeverity.Warning,
                        message: '[LEX_UNCLOSED_STRING] Unclosed string literal',
                        startLineNumber: lineNo,
                        startColumn: 1,
                        endLineNumber: lineNo,
                        endColumn: line.length + 1
                    });
                }
                const opens = (line.match(/\(/g) || []).length;
                const closes = (line.match(/\)/g) || []).length;
                parenBalance += opens - closes;
                if (/^[^A-Za-z%;\s]/.test(line)) {
                    problems.push({
                        severity: 'warning',
                        message: 'Line should start with a label, command, or comment',
                        line: lineNo,
                        code: 'LEX_LINE_START'
                    });
                    combinedMarkers.push({
                        severity: monaco.MarkerSeverity.Warning,
                        message: '[LEX_LINE_START] Line should start with a label, command, or comment',
                        startLineNumber: lineNo,
                        startColumn: 1,
                        endLineNumber: lineNo,
                        endColumn: line.length + 1
                    });
                }
                if (/[{}\[\]\\]/.test(line)) {
                    problems.push({
                        severity: 'info',
                        message: 'Suspicious character for MUMPS',
                        line: lineNo,
                        code: 'LEX_SUSPICIOUS_CHAR'
                    });
                    combinedMarkers.push({
                        severity: monaco.MarkerSeverity.Info,
                        message: '[LEX_SUSPICIOUS_CHAR] Suspicious character for MUMPS',
                        startLineNumber: lineNo,
                        startColumn: 1,
                        endLineNumber: lineNo,
                        endColumn: line.length + 1
                    });
                }
            });
            if (parenBalance !== 0) {
                problems.push({
                    severity: 'warning',
                    message: 'Unbalanced parentheses detected',
                    line: 1,
                    code: 'LEX_PAREN_BALANCE'
                });
                combinedMarkers.push({
                    severity: monaco.MarkerSeverity.Warning,
                    message: '[LEX_PAREN_BALANCE] Unbalanced parentheses detected',
                    startLineNumber: 1,
                    startColumn: 1,
                    endLineNumber: lines.length,
                    endColumn: 1
                });
            }
        }

        const Lexer = window._mumpsLexerClass || MUMPSLexerClass;
        if (Lexer) {
            try {
                const lexer = new Lexer(text || '');
                lexer.tokenize();
                (lexer.errors || []).forEach(err => {
                    problems.push({
                        severity: 'error',
                        message: err.message || 'Syntax error',
                        line: err.line || null,
                        code: err.code || 'LEX_ERROR'
                    });
                    combinedMarkers.push({
                        severity: monaco.MarkerSeverity.Error,
                        message: `[${err.code || 'LEX_ERROR'}] ${err.message || 'Syntax error'}`,
                        startLineNumber: err.line || 1,
                        startColumn: err.column || 1,
                        endLineNumber: err.line || 1,
                        endColumn: (err.column || 1) + 1
                    });
                });
            } catch (e) {
                // ignore lexer failures
            }
        }

        // TEMPORARILY DISABLED: Parser is causing IDE to freeze
        // TODO: Fix MUMPSParser infinite loop issue
        if (false && Parser) {
            try {
                const parser = new Parser();
                parser.parse(text || '');
                (parser.getErrors ? parser.getErrors() : parser.errors || []).forEach(err => {
                    const sev = normalizeSeverity(err.severity || 'error');
                    problems.push({
                        severity: sev,
                        message: err.message || 'Parse error',
                        line: err.line || null,
                        code: err.code || 'PARSE_ERROR'
                    });
                    combinedMarkers.push({
                        severity: markerSeverity(sev),
                        message: `[${err.code || 'PARSE_ERROR'}] ${err.message || 'Parse error'}`,
                        startLineNumber: err.line || 1,
                        startColumn: err.column || 1,
                        endLineNumber: err.line || 1,
                        endColumn: (err.column || 1) + 1
                    });
                });
            } catch (e) {
                // ignore parser failures to avoid blocking editing
            }
        }

        const limitedMarkers = combinedMarkers.slice(0, maxProblemItems);
        monaco.editor.setModelMarkers(model, 'mumps-check', limitedMarkers);
        renderProblems(problems);
        return limitedMarkers;
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
        const normalized = normalizeRoutineTarget(name);
        activeRoutineName = normalized.path || normalized.base || null;
        const displayName = normalized.base || (name || 'None');
        const displayFolder = normalized.folder || 'routines';
        const label = document.getElementById('currentRoutineLabel');
        if (label) label.textContent = normalized.base || 'None';

        // Update PhpStorm-style breadcrumbs
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
            routinesCache = res.routines || routinesCache;
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
        const modelToValidate = editor?.getModel ? editor.getModel() : activeEditor?.getModel?.();
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
            return;
        }
        if (mumpsValidator) {
            // Extract just the routine name (remove folder path if present)
            const routineNameOnly = name.includes('/') ? name.split('/').pop() : name;
            const check = mumpsValidator.validateRoutineName(routineNameOnly.toUpperCase());
            if (!check.valid) {
                appendOutput(`✗ Invalid routine: ${check.errors.join('; ')}`, termState);
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
            if (summary.errors > 0) {
                appendOutput(
                    `✗ Cannot save: ${summary.errors} error(s) found. Warnings and info are allowed.`,
                    termState
                );
                return;
            }
        }

        logger.debug('FILE_SAVE', { routine: name });
        const res = await window.ahmadIDE.saveRoutine(name, code);
        if (res.ok) {
            const savedPath = res.folder ? `${res.folder}/${res.routine}` : (res.routine || name);
            state.current = savedPath;
            setCurrentRoutine(state.current);
            appendOutput(`✓ Saved routine ${state.current}`, termState);
            await window.ahmadIDE.zlinkRoutine(state.current);
            appendOutput(`✓ ZLINK ${state.current}`, termState);
            await loadRoutineList(state, editor, '', termState);
            logger.info('FILE_SAVE_SUCCESS', { routine: savedPath });

            // Clear dirty state for current tab
            if (activeTabId) {
                markTabDirty(activeTabId, false);
                // Update tab name if it changed
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
            appendOutput(`✗ Save failed: ${res.error || res.stderr}`, termState);
            logger.error('FILE_SAVE_FAIL', { routine: name, error: res.error || res.stderr });
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
                const targetInfo = normalizeRoutineTarget(routineName);
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

    function registerMumpsHover() {
        monaco.languages.registerHoverProvider('mumps', {
            provideHover: function (model, position) {
                // Only show hover during active debug session
                if (!currentDebugSession || !currentDebugSession.locals) {
                    console.log('[HOVER] No debug session or locals');
                    return null;
                }
                // Don't show hover in "ready" state (before execution starts)
                if (!currentDebugSession.currentLine) {
                    return null;
                }

                const word = model.getWordAtPosition(position);
                if (!word) {
                    return null;
                }

                let varName = word.word.toUpperCase();

                // Handle Mumps % variables (e.g., %Z) which getWordAtPosition might split
                // Check character immediately before the word
                if (word.startColumn > 1) {
                    const prevCharRange = new monaco.Range(position.lineNumber, word.startColumn - 1, position.lineNumber, word.startColumn);
                    const prevChar = model.getValueInRange(prevCharRange);
                    if (prevChar === '%') {
                        varName = '%' + varName;
                    }
                }

                const subMatch = varName.match(/^([A-Z%][A-Z0-9]*)(\(.+\))$/);
                const baseName = subMatch ? subMatch[1] : null;
                const subscriptKey = subMatch ? subMatch[2] : null;

                // Check locals for the variable
                // 1. Exact match (as returned by debugger - should already be uppercase)
                let val = currentDebugSession.locals[varName];

                // Fallback: try to find case-insensitive match if exact fails
                if (val === undefined) {
                    const keys = Object.keys(currentDebugSession.locals);
                    const match = keys.find(k => k.toUpperCase() === varName);
                    if (match) {
                        val = currentDebugSession.locals[match];
                    }
                }

                // If hovering an array element (ARR(1)), show that specific value when available
                if (val === undefined && baseName && currentDebugSession.locals[baseName]) {
                    const arrVal = currentDebugSession.locals[baseName];
                    if (arrVal && typeof arrVal === 'object' && arrVal._isArray && subscriptKey) {
                        const elementVal = arrVal._elements ? arrVal._elements[subscriptKey] : undefined;
                        if (elementVal !== undefined) {
                            return {
                                contents: [
                                    { value: `**${baseName}${subscriptKey}**` },
                                    { value: '`' + String(elementVal) + '`' }
                                ]
                            };
                        }
                        val = arrVal;
                    }
                }

                if (val !== undefined && val !== null) {
                    let contents = [];
                    if (typeof val === 'object' && val._isArray) {
                        const len = Object.keys(val._elements || {}).length;
                        const keys = Object.keys(val._elements || {}).slice(0, 6);
                        const preview = keys.map(k => `• ${baseName || varName}${k} = \`${val._elements[k]}\``).join('\n');
                        contents = [
                            { value: `**${baseName || varName}** (Array, ${len} item${len === 1 ? '' : 's'})` },
                            { value: preview || '_No elements_' }
                        ];
                        if (len > keys.length) {
                            contents.push({ value: `_…${len - keys.length} more_` });
                        }
                    } else {
                        contents = [
                            { value: `**${varName}**` },
                            { value: '`' + String(val) + '`' }
                        ];
                    }

                    return {
                        range: new monaco.Range(position.lineNumber, word.startColumn - (varName.startsWith('%') ? 1 : 0), position.lineNumber, word.endColumn),
                        contents
                    };
                }

                return null;
            }
        });
    }

})();
