(() => {
    function wireEditorKeybindings({ deps } = {}) {
        const editor = deps?.editor;
        const loadShortcutPrefs = deps?.loadShortcutPrefs;
        const setShortcutDefaults = deps?.setShortcutDefaults;
        const duplicateLine = deps?.duplicateLine;
        const registerKeybinding = deps?.registerKeybinding;
        const clickEl = deps?.clickEl;
        const toggleSidebar = deps?.toggleSidebar;
        const toggleTerminal = deps?.toggleTerminal;
        const terminalToolApiRef = deps?.terminalToolApiRef;
        const openSearchEverywhere = deps?.openSearchEverywhere;
        const showToast = deps?.showToast;
        const openFindReplaceDialog = deps?.openFindReplaceDialog;
        const getSelectedText = deps?.getSelectedText;
        const toggleToolWindowPanel = deps?.toggleToolWindowPanel;
        const openGitToolWindow = deps?.openGitToolWindow;
        const openCommitToolWindow = deps?.openCommitToolWindow;

        const shortcutPrefs = loadShortcutPrefs();
        let shortcutDefaults = null;
        shortcutDefaults = {
            // Existing shortcuts
            'duplicate-line': monaco.KeyMod.CtrlCmd | monaco.KeyCode.KeyD,
            'run-code': monaco.KeyMod.CtrlCmd | monaco.KeyCode.Enter,
            'lint-code': monaco.KeyMod.CtrlCmd | monaco.KeyCode.KeyL,
            'toggle-sidebar': monaco.KeyMod.CtrlCmd | monaco.KeyCode.KeyB,
            'toggle-terminal': monaco.KeyMod.CtrlCmd | monaco.KeyCode.KeyJ,
            'new-terminal': monaco.KeyMod.CtrlCmd | monaco.KeyMod.Shift | monaco.KeyCode.KeyT,

            //  Navigation shortcuts
            'goto-file': monaco.KeyMod.CtrlCmd | monaco.KeyMod.Shift | monaco.KeyCode.KeyN,
            'goto-line': monaco.KeyMod.CtrlCmd | monaco.KeyCode.KeyG,
            'recent-files': monaco.KeyMod.CtrlCmd | monaco.KeyCode.KeyE,

            //  Editing shortcuts
            'delete-line': monaco.KeyMod.CtrlCmd | monaco.KeyCode.KeyY,
            'comment-line': monaco.KeyMod.CtrlCmd | monaco.KeyCode.Slash,
            'extend-selection': monaco.KeyMod.CtrlCmd | monaco.KeyCode.KeyW,

            //  Search shortcuts
            'find': monaco.KeyMod.CtrlCmd | monaco.KeyCode.KeyF,
            'replace': monaco.KeyMod.CtrlCmd | monaco.KeyCode.KeyR,
            'find-in-folder': monaco.KeyMod.CtrlCmd | monaco.KeyMod.Shift | monaco.KeyCode.KeyF,
            'replace-in-folder': monaco.KeyMod.CtrlCmd | monaco.KeyMod.Shift | monaco.KeyCode.KeyR,

            //  Code shortcuts
            'format-code': monaco.KeyMod.CtrlCmd | monaco.KeyMod.Alt | monaco.KeyCode.KeyL,
            'optimize-imports': monaco.KeyMod.CtrlCmd | monaco.KeyMod.Alt | monaco.KeyCode.KeyO,

            //  Refactoring
            'rename': monaco.KeyCode.F2,

            //  Tool Windows (Alt+Number)
            'tool-project': monaco.KeyMod.Alt | monaco.KeyCode.Digit1,
            'tool-favorites': monaco.KeyMod.Alt | monaco.KeyCode.Digit2,
            'tool-find': monaco.KeyMod.Alt | monaco.KeyCode.Digit3,
            'tool-run': monaco.KeyMod.Alt | monaco.KeyCode.Digit4,
            'tool-debug': monaco.KeyMod.Alt | monaco.KeyCode.Digit5,
            'tool-todo': monaco.KeyMod.Alt | monaco.KeyCode.Digit6, // Problems
            'tool-structure': monaco.KeyMod.Alt | monaco.KeyCode.Digit7,
            'tool-services': monaco.KeyMod.Alt | monaco.KeyCode.Digit8,
            'tool-git': monaco.KeyMod.Alt | monaco.KeyCode.Digit9,
            'tool-commit': monaco.KeyMod.Alt | monaco.KeyCode.Digit0,
            'tool-terminal': monaco.KeyMod.Alt | monaco.KeyCode.F12
        };
        setShortcutDefaults(shortcutDefaults);
        const dupBinding = shortcutPrefs['duplicate-line'] || shortcutDefaults['duplicate-line'];
        const dupHandler = () => duplicateLine(editor);
        editor.addCommand(dupBinding, dupHandler);
        registerKeybinding(editor, 'Duplicate Line', 'duplicate-line', dupHandler, shortcutDefaults['duplicate-line']);
        registerKeybinding(editor, 'Run', 'run-code', () => clickEl('runBtn'), shortcutDefaults['run-code']);
        registerKeybinding(editor, 'Lint', 'lint-code', () => clickEl('lintBtn'), shortcutDefaults['lint-code']);
        registerKeybinding(editor, 'Toggle Sidebar', 'toggle-sidebar', () => toggleSidebar(), shortcutDefaults['toggle-sidebar']);
        registerKeybinding(editor, 'Toggle Terminal', 'toggle-terminal', () => toggleTerminal(), shortcutDefaults['toggle-terminal']);
        registerKeybinding(editor, 'New Terminal', 'new-terminal', () => {
            terminalToolApiRef.value?.openTerminalToolWindow?.({ source: 'shortcut:new-terminal' });
            terminalToolApiRef.value?.newTab?.().catch?.(() => { });
        }, shortcutDefaults['new-terminal']);

        //  Navigation shortcuts
        registerKeybinding(editor, 'Go to File', 'goto-file', () => openSearchEverywhere(''), shortcutDefaults['goto-file']);
        registerKeybinding(editor, 'Go to Line', 'goto-line', () => editor.trigger('keyboard', 'editor.action.gotoLine', null), shortcutDefaults['goto-line']);
        registerKeybinding(editor, 'Recent Files', 'recent-files', () => showToast('info', 'Recent Files', 'Feature coming soon'), shortcutDefaults['recent-files']);

        //  Editing shortcuts
        registerKeybinding(editor, 'Delete Line', 'delete-line', () => editor.trigger('keyboard', 'editor.action.deleteLines', null), shortcutDefaults['delete-line']);
        registerKeybinding(editor, 'Comment Line', 'comment-line', () => editor.trigger('keyboard', 'editor.action.commentLine', null), shortcutDefaults['comment-line']);
        registerKeybinding(editor, 'Extend Selection', 'extend-selection', () => editor.trigger('keyboard', 'editor.action.smartSelect.expand', null), shortcutDefaults['extend-selection']);

        //  Search shortcuts
        registerKeybinding(editor, 'Find', 'find', () => editor.trigger('keyboard', 'actions.find', null), shortcutDefaults['find']);
        registerKeybinding(editor, 'Replace', 'replace', () => editor.trigger('keyboard', 'editor.action.startFindReplaceAction', null), shortcutDefaults['replace']);
        registerKeybinding(editor, 'Find in Files', 'find-in-folder', () => openFindReplaceDialog('find', getSelectedText()), shortcutDefaults['find-in-folder']);
        registerKeybinding(editor, 'Replace in Files', 'replace-in-folder', () => openFindReplaceDialog('replace', getSelectedText()), shortcutDefaults['replace-in-folder']);

        //  Code shortcuts
        registerKeybinding(editor, 'Format Code', 'format-code', () => editor.trigger('keyboard', 'editor.action.formatDocument', null), shortcutDefaults['format-code']);
        registerKeybinding(editor, 'Optimize Imports', 'optimize-imports', () => showToast('info', 'Optimize Imports', 'Feature coming soon'), shortcutDefaults['optimize-imports']);

        //  Refactoring
        registerKeybinding(editor, 'Rename', 'rename', () => editor.trigger('keyboard', 'editor.action.rename', null), shortcutDefaults['rename']);

        //  Tool Windows (Alt+Number to toggle panels)
        registerKeybinding(editor, 'Tool: Project', 'tool-project', () => toggleToolWindowPanel('projectPanel', 'left'), shortcutDefaults['tool-project']);
        registerKeybinding(editor, 'Tool: Find', 'tool-find', () => openFindReplaceDialog('find', ''), shortcutDefaults['tool-find']);
        registerKeybinding(editor, 'Tool: Run', 'tool-run', () => toggleToolWindowPanel('terminalPanel', 'bottom'), shortcutDefaults['tool-run']);
        registerKeybinding(editor, 'Tool: Debug', 'tool-debug', () => toggleToolWindowPanel('debugPanel', 'bottom'), shortcutDefaults['tool-debug']);
        registerKeybinding(editor, 'Tool: Problems', 'tool-todo', () => toggleToolWindowPanel('problemsPanel', 'bottom'), shortcutDefaults['tool-todo']);
        registerKeybinding(editor, 'Tool: Structure', 'tool-structure', () => toggleToolWindowPanel('structurePanel', 'left'), shortcutDefaults['tool-structure']);
        registerKeybinding(editor, 'Tool: Services', 'tool-services', () => toggleToolWindowPanel('servicesPanel', 'bottom'), shortcutDefaults['tool-services']);
        registerKeybinding(editor, 'Tool: Git', 'tool-git', () => openGitToolWindow({ source: 'shortcut' }), shortcutDefaults['tool-git']);
        registerKeybinding(editor, 'Tool: Commit', 'tool-commit', () => openCommitToolWindow({ source: 'shortcut' }), shortcutDefaults['tool-commit']);
        registerKeybinding(editor, 'Tool: Terminal', 'tool-terminal', () => toggleTerminal(), shortcutDefaults['tool-terminal']);
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.renderer = window.AhmadIDEModules.renderer || {};
        window.AhmadIDEModules.renderer.shortcuts = window.AhmadIDEModules.renderer.shortcuts || {};
        window.AhmadIDEModules.renderer.shortcuts.wireEditorKeybindings = wireEditorKeybindings;
    }
})();
