// This file intentionally defines `runMenuAction` in the global scope.
// renderer.js's bindGlobalShortcuts() references `runMenuAction` (free variable),
// so providing it here makes shortcuts + all menu triggers share one action runner.

async function runMenuAction(action, ctx = {}) {
    const clickEl = (id) => {
        try {
            if (typeof $ !== 'undefined' && $) {
                const $el = $('#' + id);
                if ($el && $el.length) $el.trigger('click');
                return;
            }
        } catch (_) { }
        document.getElementById(id)?.click?.();
    };

    const safeEditor = () => {
        try {
            return (typeof activeEditor !== 'undefined' && activeEditor) ? activeEditor : null;
        } catch (_) {
            return null;
        }
    };

    const notImplemented = (label) => {
        try {
            if (typeof showToast === 'function') {
                showToast('info', 'Not implemented', `${label || 'This action'} is not implemented yet.`);
            }
        } catch (_) { }
    };

    const ed = safeEditor();

    switch (action) {
        case 'save':
        case 'save-all':
            clickEl('saveRoutineBtn');
            return;
        case 'undo':
            ed?.trigger('keyboard', 'undo', null);
            return;
        case 'redo':
            ed?.trigger('keyboard', 'redo', null);
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
            ed?.trigger('keyboard', 'actions.find', null);
            return;
        case 'replace':
            ed?.trigger('keyboard', 'editor.action.startFindReplaceAction', null);
            return;
        case 'find-in-folder':
            try { openFindReplaceDialog?.('find', getSelectedText?.() || ''); } catch (_) { }
            return;
        case 'replace-in-folder':
            try { openFindReplaceDialog?.('replace', getSelectedText?.() || ''); } catch (_) { }
            return;
        case 'search-everywhere':
        case 'goto-file':
            try { openSearchEverywhere?.(''); } catch (_) { }
            return;
        case 'comment':
            ed?.trigger('keyboard', 'editor.action.commentLine', null);
            return;
        case 'duplicate-line':
            try { duplicateLine?.(ed); } catch (_) { }
            return;
        case 'goto-line':
            ed?.trigger('keyboard', 'editor.action.gotoLine', null);
            return;
        case 'reformat':
            ed?.trigger('keyboard', 'editor.action.formatDocument', null);
            return;
        case 'rename':
            ed?.trigger('keyboard', 'editor.action.rename', null);
            return;
        case 'lint':
            clickEl('lintBtn');
            return;
        case 'toggle-sidebar':
            try { toggleSidebar?.(); } catch (_) { }
            return;
        case 'toggle-terminal':
            try { toggleTerminal?.(); } catch (_) { }
            return;
        case 'terminal':
            try { toggleToolWindowPanel?.('terminalPanel', 'bottom'); } catch (_) { }
            return;
        case 'connections':
            if (window.AhmadIDEModules?.app?.dialogRegistry?.open('connections')) {
                return;
            }
            clickEl('toggleConnections');
            return;
        case 'extensions':
            try { toggleToolWindowPanel?.('extensionsPanel', 'bottom'); } catch (_) { }
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
            try { openShortcutsPanel?.(); } catch (_) { }
            return;
        case 'settings':
            try { openSettingsPanel?.(); } catch (_) { }
            return;
        case 'new-project':
            try { openNewProjectPanel?.(); } catch (_) { }
            return;
        case 'open-project':
            try { await openProjectDialog?.(); } catch (_) { }
            return;
        case 'close-project':
            try { closeCurrentProject?.(); } catch (_) { }
            return;
        case 'new-file':
            try { await createNewFile?.(); } catch (_) { }
            return;
        case 'git':
            try { openGitToolWindow?.(); } catch (_) { }
            return;
        case 'git-status':
            try { openGitToolWindow?.(); } catch (_) { }
            try { document.getElementById('gitStatusBtn')?.click?.(); } catch (_) { }
            return;
        case 'git-diff':
            try { openGitToolWindow?.(); } catch (_) { }
            try { document.getElementById('gitDiffBtn')?.click?.(); } catch (_) { }
            return;
        case 'git-history':
            try { openGitToolWindow?.(); } catch (_) { }
            try { document.getElementById('gitLogBtn')?.click?.(); } catch (_) { }
            return;
        case 'goto-declaration': {
            const pos = ed?.getPosition?.();
            try { await goToDeclaration?.(ed, pos, { silentIfMissing: false }); } catch (_) { }
            return;
        }
        case 'gitctx:add':
        case 'gitctx:commit':
        case 'gitctx:history':
        case 'gitctx:compare':
        case 'gitctx:rollback': {
            const map = {
                'gitctx:add': 'add',
                'gitctx:commit': 'commit',
                'gitctx:history': 'history',
                'gitctx:compare': 'compare',
                'gitctx:rollback': 'rollback'
            };
            const gitAction = map[action];
            const target = String(ctx?.path || ctx?.activePath || '').trim();
            try { await runGitContextAction?.(gitAction, target); } catch (_) { }
            return;
        }
        case 'vcs:commit':
            try { openCommitToolWindow?.(); } catch (_) { }
            return;
        case 'vcs:history':
            try { openGitToolWindow?.(); } catch (_) { }
            try { document.getElementById('gitLogBtn')?.click?.(); } catch (_) { }
            return;
        case 'vcs:push': {
            const btn = document.getElementById('gitPushBtn');
            if (btn) btn.click();
            else notImplemented('Push');
            return;
        }
        case 'vcs:pull': {
            const btn = document.getElementById('gitPullBtn') || document.getElementById('gitFetchBtn');
            if (btn) btn.click();
            else notImplemented('Pull / Fetch');
            return;
        }
        case 'vcs:open-git':
            try { openGitToolWindow?.(); } catch (_) { }
            return;
        case 'run-config:run-current':
            document.querySelector('#runConfigMenu .run-config-item[data-config="run-current"]')?.click?.();
            return;
        case 'run-config:debug-current':
            document.querySelector('#runConfigMenu .run-config-item[data-config="debug-current"]')?.click?.();
            return;
        case 'prj:open':
        case 'prj:new-routine':
        case 'prj:new-folder':
        case 'prj:copy-path':
        case 'prj:copy-ref':
        case 'prj:reveal':
        case 'prj:refresh':
            // These are handled via the Phase 3A menus bootstrap (needs richer deps).
            // Keep as no-op here to avoid feature coupling.
            return;
        case 'tab:close':
        case 'tab:close-others':
        case 'tab:close-all':
        case 'tab:close-left':
        case 'tab:close-right':
            // Handled in Phase 3A menus bootstrap (needs tab id).
            return;
        default:
            notImplemented(action);
    }
}

