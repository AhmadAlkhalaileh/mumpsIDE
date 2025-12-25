(() => {
    function createEditorContextMenuManager({ deps } = {}) {
        const showToast = deps?.showToast || (() => { });
        const getActiveEditor = deps?.getActiveEditor || (() => null);
        const parseRoutineReferenceAtPosition = deps?.parseRoutineReferenceAtPosition || null;
        const getActiveRoutine = deps?.getActiveRoutine || (() => null);
        const runGitContextAction = deps?.runGitContextAction || (() => { });
        const goToDeclaration = deps?.goToDeclaration || (() => false);
        const menuRegistry = deps?.menuRegistry || window.AhmadIDEModules?.app?.menuRegistry;
        const menuController = deps?.menuController
            || window.AhmadIDEModules?.ui?.menu?.controller
            || window.AhmadIDEModules?.ui?.menu?.createMenuController?.({});
        const openContextMenu = deps?.openContextMenu || window.AhmadIDEModules?.ui?.menu?.openContextMenu;

        const runAction = deps?.runMenuAction || (async (action, ctx) => {
            if (typeof runMenuAction === 'function') return runMenuAction(action, ctx);
            const ed = getActiveEditor();
            switch (action) {
                case 'cut':
                    document.execCommand('cut');
                    return;
                case 'copy':
                    document.execCommand('copy');
                    return;
                case 'paste':
                    document.execCommand('paste');
                    return;
                case 'comment':
                    ed?.trigger('keyboard', 'editor.action.commentLine', null);
                    return;
                case 'reformat': {
                    const actionObj = ed?.getAction && ed.getAction('editor.action.formatDocument');
                    if (actionObj && actionObj.isSupported && actionObj.isSupported()) {
                        try { await actionObj.run(); return; } catch (_) { }
                    }
                    showToast('info', 'Format Code', 'Not available for this file type');
                    return;
                }
                case 'goto-declaration':
                    await goToDeclaration(ed, null, { silentIfMissing: false });
                    return;
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
                    const target = String(ctx?.activePath || '').trim();
                    if (target) runGitContextAction(map[action], target);
                    return;
                }
                case 'compare-with-release': {
                    // Trigger Compare with Release extension
                    const handler = window.AhmadIDEModules?.extensions?.compareWithRelease?.handleCompareWithRelease;
                    if (handler) {
                        await handler(ctx);
                    } else {
                        showToast('error', 'Compare with Release', 'Extension not available');
                    }
                    return;
                }
                default:
                    return;
            }
        });

        const getContext = (editor) => {
            let hasDeclaration = false;
            try {
                if (parseRoutineReferenceAtPosition && editor) {
                    const model = editor.getModel?.();
                    const pos = editor.getPosition?.();
                    hasDeclaration = !!(model && pos && parseRoutineReferenceAtPosition(model, pos));
                }
            } catch (_) { }
            const activePath = String(getActiveRoutine() || '').trim();
            return { hasDeclaration, activePath };
        };

        function bindEditorContextMenu(editor) {
            if (!editor || !menuRegistry || (!menuController && !openContextMenu)) return;

            const openAt = (x, y) => {
                const ctx = getContext(editor);
                const items = menuRegistry.get('context.editor', ctx);
                const payload = {
                    controller: menuController,
                    x,
                    y,
                    items,
                    ctx,
                    onAction: async (action, itemCtx) => {
                        try { await runAction(action, itemCtx || ctx); } catch (_) { }
                    }
                };
                if (typeof openContextMenu === 'function') {
                    openContextMenu(payload);
                    return;
                }
                menuController.openAtPoint(payload);
            };

            if (!bindEditorContextMenu.__bound) {
                editor.onContextMenu((e) => {
                    try { e.event.preventDefault(); } catch (_) { }
                    try { e.event.stopPropagation(); } catch (_) { }
                    const be = e.event?.browserEvent || e.event;
                    const x = be?.clientX ?? e.event?.posx ?? 0;
                    const y = be?.clientY ?? e.event?.posy ?? 0;
                    openAt(x, y);
                });
                bindEditorContextMenu.__bound = true;
            }

            const domNode = editor.getDomNode?.();
            if (domNode && !domNode._ctxMenuHooked) {
                const handleDomContextMenu = (ev) => {
                    ev.preventDefault();
                    ev.stopPropagation();
                    openAt(ev.clientX, ev.clientY);
                };
                domNode.addEventListener('contextmenu', handleDomContextMenu, true);
                domNode._ctxMenuHooked = true;
            }
        }

        return { bindEditorContextMenu };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.ui = window.AhmadIDEModules.ui || {};
        window.AhmadIDEModules.ui.createEditorContextMenuManager = createEditorContextMenuManager;
    }
})();
