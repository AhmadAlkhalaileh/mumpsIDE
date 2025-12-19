(() => {
    function createMenuRegistry() {
        const menus = new Map(); // id -> (ctx) => items

        const register = (id, provider) => {
            const key = String(id || '').trim();
            if (!key) return false;
            if (typeof provider !== 'function') return false;
            menus.set(key, provider);
            return true;
        };

        const get = (id, ctx) => {
            const key = String(id || '').trim();
            const provider = menus.get(key);
            if (!provider) return [];
            try {
                const res = provider(ctx || {});
                return Array.isArray(res) ? res : [];
            } catch (_) {
                return [];
            }
        };

        const list = () => Array.from(menus.keys()).sort();

        return { register, get, list };
    }

    const withSeparatorCleanup = (items) => {
        const out = [];
        let lastSep = true;
        (items || []).forEach((it) => {
            if (!it) return;
            const isSep = it.type === 'separator' || it.separator === true;
            if (isSep) {
                if (lastSep) return;
                out.push({ type: 'separator' });
                lastSep = true;
                return;
            }
            out.push(it);
            lastSep = false;
        });
        while (out.length && out[out.length - 1]?.type === 'separator') out.pop();
        return out;
    };

    const defineDefaultMenus = (registry) => {
        registry.register('menubar', () => ([
            {
                id: 'file',
                label: 'File',
                items: withSeparatorCleanup([
                    {
                        id: 'file.new',
                        label: 'New',
                        icon: 'add',
                        submenu: [
                            { id: 'file.new.file', label: 'File…', action: 'new-file', icon: 'add' },
                            { id: 'file.new.project', label: 'Project…', action: 'new-project', icon: 'add' }
                        ]
                    },
                    { id: 'file.open', label: 'Open…', action: 'open-project', icon: 'folder-open', shortcut: 'Ctrl+O' },
                    { id: 'file.closeProject', label: 'Close Project', action: 'close-project', icon: 'close' },
                    { type: 'separator' },
                    { id: 'file.saveAll', label: 'Save All', action: 'save-all', icon: 'save', shortcut: 'Ctrl+Shift+S' },
                    { type: 'separator' },
                    { id: 'file.settings', label: 'Settings…', action: 'settings', icon: 'settings', shortcut: 'Ctrl+,' },
                    { id: 'file.exit', label: 'Exit', action: 'exit-app', icon: 'exit', shortcut: 'Alt+F4' }
                ])
            },
            {
                id: 'edit',
                label: 'Edit',
                items: withSeparatorCleanup([
                    { id: 'edit.undo', label: 'Undo', action: 'undo', shortcut: 'Ctrl+Z' },
                    { id: 'edit.redo', label: 'Redo', action: 'redo', shortcut: 'Ctrl+Y' },
                    { type: 'separator' },
                    { id: 'edit.cut', label: 'Cut', action: 'cut', icon: 'cut', shortcut: 'Ctrl+X' },
                    { id: 'edit.copy', label: 'Copy', action: 'copy', icon: 'copy', shortcut: 'Ctrl+C' },
                    { id: 'edit.paste', label: 'Paste', action: 'paste', icon: 'paste', shortcut: 'Ctrl+V' },
                    { type: 'separator' },
                    {
                        id: 'edit.findGroup',
                        label: 'Find',
                        icon: 'search',
                        submenu: [
                            { id: 'edit.find', label: 'Find…', action: 'find', shortcut: 'Ctrl+F' },
                            { id: 'edit.replace', label: 'Replace…', action: 'replace', shortcut: 'Ctrl+H' },
                            { type: 'separator' },
                            { id: 'edit.findInFiles', label: 'Find in Files…', action: 'find-in-folder', shortcut: 'Ctrl+Shift+F' },
                            { id: 'edit.replaceInFiles', label: 'Replace in Files…', action: 'replace-in-folder', shortcut: 'Ctrl+Shift+R' }
                        ]
                    },
                    { id: 'edit.duplicate', label: 'Duplicate Line', action: 'duplicate-line', shortcut: 'Ctrl+D' },
                    { id: 'edit.comment', label: 'Toggle Comment', action: 'comment', icon: 'comment', shortcut: 'Ctrl+/' }
                ])
            },
            {
                id: 'view',
                label: 'View',
                items: withSeparatorCleanup([
                    {
                        id: 'view.sidebar',
                        label: 'Tool Windows',
                        submenu: [
                            { id: 'view.tw.project', label: 'Project', action: 'toggle-sidebar', type: 'checkbox', checked: (ctx) => !!ctx?.toolWindows?.leftVisible },
                            { id: 'view.tw.terminal', label: 'Terminal', action: 'toggle-terminal', type: 'checkbox', checked: (ctx) => !!ctx?.toolWindows?.bottomVisible }
                        ]
                    },
                    { type: 'separator' },
                    { id: 'view.appearance', label: 'Appearance', action: 'appearance', disabled: true }
                ])
            },
            {
                id: 'navigate',
                label: 'Navigate',
                items: withSeparatorCleanup([
                    { id: 'nav.file', label: 'Go to File…', action: 'goto-file', shortcut: 'Ctrl+N' },
                    { id: 'nav.line', label: 'Go to Line…', action: 'goto-line', shortcut: 'Ctrl+L' },
                    { type: 'separator' },
                    { id: 'nav.recent', label: 'Recent Files', action: 'recent-files', disabled: true }
                ])
            },
            {
                id: 'code',
                label: 'Code',
                items: withSeparatorCleanup([
                    { id: 'code.format', label: 'Format Code', action: 'reformat', icon: 'format', shortcut: 'Ctrl+Alt+L' },
                    { id: 'code.comment', label: 'Comment/Uncomment', action: 'comment', icon: 'comment' },
                    { id: 'code.rename', label: 'Rename…', action: 'rename', shortcut: 'Shift+F6' }
                ])
            },
            {
                id: 'refactor',
                label: 'Refactor',
                items: withSeparatorCleanup([
                    { id: 'ref.rename', label: 'Rename…', action: 'rename', shortcut: 'Shift+F6' },
                    { id: 'ref.extract', label: 'Extract Method…', action: 'refactor-extract', disabled: true }
                ])
            },
            {
                id: 'run',
                label: 'Run',
                items: withSeparatorCleanup([
                    { id: 'run.run', label: 'Run', action: 'run', icon: 'run', shortcut: 'Ctrl+Enter' },
                    { id: 'run.debug', label: 'Debug', action: 'debug', icon: 'debug', shortcut: 'Shift+F9' },
                    { id: 'run.stop', label: 'Stop', action: 'stop-debug', icon: 'stop', shortcut: 'Ctrl+F2' }
                ])
            },
            {
                id: 'tools',
                label: 'Tools',
                items: (() => {
                    console.log('[MenuRegistry] Defining Tools menu items...');
                    const items = [
                        { id: 'tools.terminal', label: 'Terminal', action: 'terminal', icon: 'terminal', shortcut: 'Alt+F12' },
                        { id: 'tools.lint', label: 'Lint', action: 'lint' },
                        { id: 'tools.shortcuts', label: 'Shortcuts', action: 'shortcuts' },
                        { id: 'tools.extensions', label: 'Extensions', action: 'extensions' }
                    ];
                    console.log('[MenuRegistry] Tools items:', items);
                    return withSeparatorCleanup(items);
                })()
            },
            {
                id: 'git',
                label: 'Git',
                items: withSeparatorCleanup([
                    { id: 'git.tool', label: 'Git Tool Window', action: 'git', icon: 'git' },
                    { type: 'separator' },
                    { id: 'git.status', label: 'Status', action: 'git-status' },
                    { id: 'git.diff', label: 'Diff', action: 'git-diff' },
                    { id: 'git.history', label: 'History', action: 'git-history' }
                ])
            },
            {
                id: 'window',
                label: 'Window',
                items: withSeparatorCleanup([
                    { id: 'win.sidebar', label: 'Toggle Sidebar', action: 'toggle-sidebar' },
                    { id: 'win.terminal', label: 'Toggle Terminal', action: 'toggle-terminal' },
                    { type: 'separator' },
                    { id: 'win.store', label: 'Store Layout', action: 'window-store', disabled: true }
                ])
            },
            {
                id: 'help',
                label: 'Help',
                items: withSeparatorCleanup([
                    { id: 'help.docs', label: 'Docs', action: 'docs', disabled: true },
                    { id: 'help.about', label: 'About', action: 'about' },
                    { type: 'separator' },
                    { id: 'help.menuSelfTest', label: 'Menu System Self-Test', action: 'menu-self-test' }
                ])
            }
        ]));

        registry.register('toolbar.runConfig', (ctx) => ([
            {
                id: 'rc.run',
                label: 'Current file (Run)',
                type: 'radio',
                radioGroup: 'runCfg',
                action: 'run-config:run-current',
                checked: (c) => String(c?.runConfig?.active || '') === 'run-current'
            },
            {
                id: 'rc.debug',
                label: 'Current file (Debug)',
                type: 'radio',
                radioGroup: 'runCfg',
                action: 'run-config:debug-current',
                checked: (c) => String(c?.runConfig?.active || '') === 'debug-current'
            }
        ]));

        registry.register('toolbar.vcs', () => ([
            { id: 'vcs.commit', label: 'Commit…', action: 'vcs:commit', icon: 'git' },
            { id: 'vcs.history', label: 'Show History', action: 'vcs:history', icon: 'history' },
            { type: 'separator' },
            { id: 'vcs.push', label: 'Push', action: 'vcs:push', icon: 'upload' },
            { id: 'vcs.pull', label: 'Pull / Fetch', action: 'vcs:pull', icon: 'download' },
            { type: 'separator' },
            { id: 'vcs.openGit', label: 'Open Git Tool Window', action: 'vcs:open-git', icon: 'git' }
        ]));

        registry.register('context.editor', (ctx) => {
            const hasDecl = !!ctx?.hasDeclaration;
            const activePath = String(ctx?.activePath || '');
            return withSeparatorCleanup([
                { id: 'ed.cut', label: 'Cut', action: 'cut', icon: 'cut', shortcut: 'Ctrl+X' },
                { id: 'ed.copy', label: 'Copy', action: 'copy', icon: 'copy', shortcut: 'Ctrl+C' },
                { id: 'ed.paste', label: 'Paste', action: 'paste', icon: 'paste', shortcut: 'Ctrl+V' },
                { type: 'separator' },
                { id: 'ed.format', label: 'Format Code', action: 'reformat', icon: 'format' },
                { id: 'ed.decl', label: 'Go to Declaration', action: 'goto-declaration', icon: 'arrow-right', disabled: !hasDecl },
                { id: 'ed.usages', label: 'Find Usages', action: 'find-usages', icon: 'search', disabled: true },
                { type: 'separator' },
                { id: 'ed.comment', label: 'Toggle Comment', action: 'comment', icon: 'comment' },
                { type: 'separator' },
                {
                    id: 'ed.git',
                    label: 'Git',
                    icon: 'git',
                    disabled: !activePath,
                    submenu: [
                        { id: 'ed.git.add', label: 'Add', action: 'gitctx:add' },
                        { id: 'ed.git.commit', label: 'Commit…', action: 'gitctx:commit' },
                        { id: 'ed.git.history', label: 'Show History', action: 'gitctx:history' },
                        { id: 'ed.git.compare', label: 'Compare with Latest Version', action: 'gitctx:compare' },
                        { id: 'ed.git.rollback', label: 'Revert / Rollback', action: 'gitctx:rollback' }
                    ]
                }
            ]);
        });

        registry.register('context.project', (ctx) => {
            const type = String(ctx?.type || '');
            const isFile = type === 'file';
            const isFolder = type === 'folder' || type === 'root';
            const canOpen = isFile && !!ctx?.path;
            const canGit = !!ctx?.path || type === 'root';
            return withSeparatorCleanup([
                ...(isFolder ? [
                    {
                        id: 'prj.new',
                        label: 'New',
                        icon: 'add',
                        submenu: [
                            { id: 'prj.new.routine', label: 'MUMPS Routine…', action: 'prj:new-routine' },
                            { id: 'prj.new.folder', label: 'Folder…', action: 'prj:new-folder' }
                        ]
                    },
                    { type: 'separator' }
                ] : []),
                ...(canOpen ? [
                    { id: 'prj.open', label: 'Open', action: 'prj:open' },
                    { type: 'separator' }
                ] : []),
                { id: 'prj.rename', label: 'Rename…', action: 'prj:rename', disabled: true, shortcut: 'F2' },
                { id: 'prj.delete', label: 'Delete', action: 'prj:delete', disabled: true, shortcut: 'Del' },
                { type: 'separator' },
                { id: 'prj.copyPath', label: 'Copy Path', action: 'prj:copy-path' },
                { id: 'prj.copyRef', label: 'Copy Reference', action: 'prj:copy-ref' },
                { type: 'separator' },
                { id: 'prj.reveal', label: 'Reveal in File Explorer', action: 'prj:reveal' },
                { type: 'separator' },
                {
                    id: 'prj.git',
                    label: 'Git',
                    icon: 'git',
                    disabled: !canGit,
                    submenu: [
                        { id: 'prj.git.add', label: 'Add', action: 'gitctx:add' },
                        { id: 'prj.git.commit', label: 'Commit…', action: 'gitctx:commit' },
                        { id: 'prj.git.history', label: 'Show History', action: 'gitctx:history' },
                        { id: 'prj.git.compare', label: 'Compare with Latest Version', action: 'gitctx:compare' },
                        { id: 'prj.git.rollback', label: 'Revert / Rollback', action: 'gitctx:rollback' }
                    ]
                },
                { type: 'separator' },
                { id: 'prj.refresh', label: 'Refresh', action: 'prj:refresh', icon: 'refresh' }
            ]);
        });

        registry.register('context.tabs', () => ([
            { id: 'tab.close', label: 'Close', action: 'tab:close', shortcut: 'Ctrl+W' },
            { id: 'tab.closeOthers', label: 'Close Others', action: 'tab:close-others' },
            { id: 'tab.closeAll', label: 'Close All', action: 'tab:close-all' },
            { type: 'separator' },
            { id: 'tab.closeLeft', label: 'Close Tabs to the Left', action: 'tab:close-left' },
            { id: 'tab.closeRight', label: 'Close Tabs to the Right', action: 'tab:close-right' }
        ]));
    };

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.app = window.AhmadIDEModules.app || {};
        if (!window.AhmadIDEModules.app.menuRegistry) {
            const registry = createMenuRegistry();
            defineDefaultMenus(registry);
            window.AhmadIDEModules.app.menuRegistry = registry;
        }
    }
})();
