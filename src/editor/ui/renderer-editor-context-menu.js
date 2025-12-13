(() => {
    function createEditorContextMenuManager({ deps } = {}) {
        const $ = deps?.$ || (typeof window !== 'undefined' ? (window.$ || window.jQuery || null) : null);
        const showToast = deps?.showToast || (() => { });
        const getActiveEditor = deps?.getActiveEditor || (() => null);
        const parseRoutineReferenceAtPosition = deps?.parseRoutineReferenceAtPosition;
        const getActiveRoutine = deps?.getActiveRoutine || (() => null);
        const runGitContextAction = deps?.runGitContextAction || (() => { });
        const goToDeclaration = deps?.goToDeclaration || (() => false);

        if (!$) {
            throw new Error('createEditorContextMenuManager requires jQuery ($)');
        }
        if (!parseRoutineReferenceAtPosition || typeof parseRoutineReferenceAtPosition !== 'function') {
            throw new Error('createEditorContextMenuManager requires deps.parseRoutineReferenceAtPosition');
        }

        function bindEditorContextMenu(editor) {
            const menu = $('#editorContextMenu');
            if (!menu.length || !editor) return;

            const getEditor = () => getActiveEditor() || editor;
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

        return {
            bindEditorContextMenu
        };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.ui = window.AhmadIDEModules.ui || {};
        window.AhmadIDEModules.ui.createEditorContextMenuManager = createEditorContextMenuManager;
    }
})();
