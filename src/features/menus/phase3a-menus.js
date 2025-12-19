(() => {
    const safe = (fn, fallback = null) => {
        try { return fn(); } catch (_) { return fallback; }
    };

    const getToolWindowCtx = () => {
        const leftVisible = safe(() => !!toolWindowState?.left?.visible, false);
        const bottomVisible = safe(() => !!toolWindowState?.bottom?.visible, false);
        return { leftVisible, bottomVisible };
    };

    const getRunConfigActive = () => {
        const active = document.querySelector('#runConfigMenu .run-config-item.active')?.getAttribute('data-config') || '';
        return String(active || '').trim();
    };

    const openMenuAt = (controller, anchorEl, menuId, ctxExtra = {}) => {
        const registry = window.AhmadIDEModules?.app?.menuRegistry;
        if (!registry) return;
        const items = registry.get(menuId, ctxExtra);
        controller.openAtElement({
            anchorEl,
            items,
            ctx: ctxExtra,
            onAction: (action, ctx) => runMenuAction(action, ctxExtra || ctx || {})
        });

        // Special-case radio state for Run config: mark checked based on current DOM.
        if (menuId === 'toolbar.runConfig') {
            const active = getRunConfigActive();
            try {
                const menuEl = document.querySelector('.ui-menu');
                if (menuEl) {
                    menuEl.querySelectorAll('.ui-menu-item').forEach((row) => {
                        const idx = Number(row.dataset.index || '-1');
                        const def = items[idx];
                        if (!def || def.type === 'separator') return;
                        const cfg = def.action === 'run-config:run-current' ? 'run-current' :
                            def.action === 'run-config:debug-current' ? 'debug-current' : '';
                        const iconCell = row.querySelector('.ui-menu-col--icon');
                        if (!iconCell) return;
                        iconCell.innerHTML = '';
                        if (cfg && cfg === active) {
                            iconCell.appendChild(window.AhmadIDEModules.ui.icons.createIcon('radio', { size: 16 }));
                        }
                    });
                }
            } catch (_) { }
        }
    };

    const mountMenuBar = (controller) => {
        const host = document.getElementById('mainMenu');
        if (!host) return;
        const registry = window.AhmadIDEModules?.app?.menuRegistry;
        if (!registry) return;

        const menus = registry.get('menubar', {});
        const menuBarFactory = window.AhmadIDEModules?.ui?.menu?.createMenuBar;
        if (!menuBarFactory) return;

        // Replace legacy menu DOM.
        host.innerHTML = '';
        const menuBar = menuBarFactory({
            host,
            menus,
            controller,
            onAction: (action, ctx) => runMenuAction(action, ctx),
            getContext: () => ({ toolWindows: getToolWindowCtx() })
        });
        menuBar.mount();
    };

    const bindToolbarMenus = (controller) => {
        const runCfgBtn = document.getElementById('runConfigBtn');
        if (runCfgBtn) {
            runCfgBtn.addEventListener('click', (e) => {
                e.preventDefault();
                e.stopImmediatePropagation();
                openMenuAt(controller, runCfgBtn, 'toolbar.runConfig', {
                    toolWindows: getToolWindowCtx(),
                    runConfig: { active: getRunConfigActive() }
                });
            }, true);
        }

        const vcsBtn = document.getElementById('vcsWidgetBtn');
        if (vcsBtn) {
            vcsBtn.addEventListener('click', (e) => {
                e.preventDefault();
                e.stopImmediatePropagation();
                openMenuAt(controller, vcsBtn, 'toolbar.vcs', {
                    toolWindows: getToolWindowCtx()
                });
            }, true);
        }
    };

    const bindEditorContextMenu = (controller) => {
        const tryBind = () => {
            const ed = safe(() => activeEditor, null);
            if (!ed || !window.monaco) return false;

            // Remove legacy static menu markup so the old renderer has nowhere to show.
            document.getElementById('editorContextMenu')?.remove?.();

            const registry = window.AhmadIDEModules?.app?.menuRegistry;
            if (!registry) return true;

            const openAt = (x, y) => {
                const model = ed.getModel();
                const pos = ed.getPosition();
                let hasDeclaration = false;
                try {
                    hasDeclaration = !!(model && pos && parseRoutineReferenceAtPosition?.(model, pos));
                } catch (_) { }
                const activePath = safe(() => getActiveRoutine?.(), '');
                const ctx = {
                    hasDeclaration,
                    activePath
                };
                const items = registry.get('context.editor', ctx);
                controller.openAtPoint({
                    x,
                    y,
                    items,
                    ctx,
                    onAction: (action, itemCtx) => runMenuAction(action, itemCtx)
                });
            };

            if (!bindEditorContextMenu.__bound) {
                ed.onContextMenu((e) => {
                    try { e.event.preventDefault(); } catch (_) { }
                    try { e.event.stopPropagation(); } catch (_) { }
                    const be = e.event?.browserEvent || e.event;
                    const x = be?.clientX ?? e.event?.posx ?? 0;
                    const y = be?.clientY ?? e.event?.posy ?? 0;
                    openAt(x, y);
                });
                bindEditorContextMenu.__bound = true;
            }
            return true;
        };

        if (tryBind()) return;
        const t = setInterval(() => {
            if (tryBind()) clearInterval(t);
        }, 120);
        setTimeout(() => clearInterval(t), 10_000);
    };

    const overrideProjectContextMenu = (controller) => {
        const registry = window.AhmadIDEModules?.app?.menuRegistry;
        if (!registry) return;

        const openProjectMenu = (x, y, options = {}) => {
            const ctx = { ...options, path: options.path || '', toolWindows: getToolWindowCtx() };
            const items = registry.get('context.project', ctx);
            controller.openAtPoint({
                x,
                y,
                items,
                ctx,
                onAction: async (action) => {
                    // Project-specific actions implemented here to keep menuActions generic.
                    const type = String(ctx.type || '');
                    const path = String(ctx.path || '');
                    const name = String(ctx.name || '');
                    const folderName = String(ctx.folderName || '');
                    const routineStateRef = ctx.routineStateRef || null;
                    const editorRef = ctx.editorRef || null;

                    const getCurrentProjectSafe = () => safe(() => getCurrentProject?.(), null) || safe(() => currentProject, null);
                    const current = getCurrentProjectSafe();
                    const projectRoot = String(current?.projectPath || '');
                    const resolvedPath = path || (folderName ? `${folderName}/${name || ''}` : '') || '';

                    const copyToClipboard = async (text, label = 'Copied') => {
                        try {
                            await navigator.clipboard.writeText(text);
                            showToast?.('success', 'Copied', label);
                        } catch (err) {
                            showToast?.('error', 'Copy failed', err?.message || 'Could not copy');
                        }
                    };

                    if (action === 'prj:open') {
                        if (type === 'file' && resolvedPath && typeof loadRoutineByName === 'function') {
                            await loadRoutineByName(resolvedPath, routineStateRef, editorRef || safe(() => activeEditor, null));
                        }
                        return;
                    }
                    if (action === 'prj:new-routine') {
                        showCustomPrompt?.('New MUMPS Routine', 'Routine name (e.g., MYRTN)', async (val) => {
                            const v = String(val || '').trim();
                            if (!v) return;
                            const routine = v.toUpperCase();
                            const fullPath = folderName ? `${folderName}/${routine}` : routine;
                            try {
                                await window.ahmadIDE?.createRoutine?.(fullPath);
                                showToast?.('success', 'Created', `Routine created: ${fullPath}`);
                                if (routineStateRef && editorRef && typeof loadRoutineList === 'function') {
                                    await loadRoutineList(routineStateRef, editorRef);
                                }
                            } catch (err) {
                                showToast?.('error', 'Failed', err?.message || 'Could not create routine');
                            }
                        });
                        return;
                    }
                    if (action === 'prj:new-folder') {
                        showCustomPrompt?.('New Folder', 'Folder path (e.g., /workspace/myFolder)', async (val) => {
                            const folderPath = String(val || '').trim();
                            if (!folderPath) return;
                            try {
                                const fullPath = folderPath.startsWith('/') ? folderPath : `/workspace/${folderPath}`;
                                const result = await window.ahmadIDE?.createDirectoryInCurrentEnv?.(fullPath);
                                if (result?.ok) {
                                    showToast?.('success', 'Created', `Folder created: ${fullPath}`);
                                    if (routineStateRef && editorRef && typeof loadRoutineList === 'function') {
                                        await loadRoutineList(routineStateRef, editorRef);
                                    }
                                } else {
                                    showToast?.('error', 'Failed', result?.error || 'Could not create folder');
                                }
                            } catch (err) {
                                showToast?.('error', 'Failed', err?.message || 'Could not create folder');
                            }
                        });
                        return;
                    }
                    if (action === 'prj:copy-path') {
                        const text = resolvedPath ? `${projectRoot ? projectRoot + '/' : ''}${resolvedPath}` : (projectRoot || '');
                        if (text) await copyToClipboard(text, 'Path copied');
                        return;
                    }
                    if (action === 'prj:copy-ref') {
                        await copyToClipboard(resolvedPath || projectRoot || '', 'Reference copied');
                        return;
                    }
                    if (action === 'prj:reveal') {
                        if (window.ahmadIDE?.revealInExplorer) {
                            window.ahmadIDE.revealInExplorer(projectRoot ? (resolvedPath ? `${projectRoot}/${resolvedPath}` : projectRoot) : resolvedPath);
                        } else {
                            showToast?.('info', 'Not Available', 'File explorer integration not available');
                        }
                        return;
                    }
                    if (action === 'prj:refresh') {
                        if (routineStateRef && editorRef && typeof loadRoutineList === 'function') {
                            await loadRoutineList(routineStateRef, editorRef);
                            showToast?.('success', 'Refreshed', 'Project tree updated');
                        }
                        return;
                    }

                    await runMenuAction(action, { path: resolvedPath });
                }
            });
            return true;
        };

        // Override the global hooks used by the project tree module.
        try { window.showProjectContextMenu = openProjectMenu; } catch (_) { }
        try { window.showRoutinesFolderContextMenu = (x, y, routineStateRef, editorRef) => openProjectMenu(x, y, { type: 'folder', folderName: 'routines', routineStateRef, editorRef }); } catch (_) { }
    };

    const bindTabsContextMenu = (controller) => {
        const registry = window.AhmadIDEModules?.app?.menuRegistry;
        if (!registry) return;

        const findTabIdFromEvent = (evt) => {
            const tabEl = evt.target?.closest?.('.tab');
            if (!tabEl || tabEl.classList.contains('ghost')) return null;
            const title = tabEl.getAttribute('title') || '';
            const nameEl = tabEl.querySelector('.tab-name');
            const label = (nameEl?.textContent || '').trim();

            const tabs = safe(() => openTabs, []) || [];
            const exact = tabs.find((t) => (t.path || '') === title) || tabs.find((t) => (t.name || '') === label);
            return exact?.id || null;
        };

        document.addEventListener('contextmenu', (e) => {
            const inTabBar = !!e.target?.closest?.('#tabBar');
            if (!inTabBar) return;
            const tabId = findTabIdFromEvent(e);
            if (!tabId) return;
            e.preventDefault();
            e.stopImmediatePropagation();

            const ctx = { tabId };
            const items = registry.get('context.tabs', ctx);
            controller.openAtPoint({
                x: e.clientX,
                y: e.clientY,
                items,
                ctx,
                onAction: async (action) => {
                    if (action === 'tab:close') return closeTab?.(tabId);
                    if (action === 'tab:close-others') return closeOtherTabs?.(tabId);
                    if (action === 'tab:close-all') return closeAllTabs?.();
                    if (action === 'tab:close-left') return closeTabsToSide?.(tabId, 'left');
                    if (action === 'tab:close-right') return closeTabsToSide?.(tabId, 'right');
                }
            });
        }, true);
    };

    const bindEditorContextMenuRemoval = () => {
        // Remove legacy Git submenu arrow behavior on the old DOM menu if it still exists.
        // (Our menu system does not use .git-parent/.git-submenu at all.)
        try { document.getElementById('editorContextMenu')?.remove?.(); } catch (_) { }
    };

    function bootstrap() {
        const controller = window.AhmadIDEModules?.ui?.menu?.controller;
        if (!controller) return;

        mountMenuBar(controller);
        bindToolbarMenus(controller);
        bindEditorContextMenuRemoval();
        bindEditorContextMenu(controller);
        overrideProjectContextMenu(controller);
        bindTabsContextMenu(controller);
    }

    if (document.readyState === 'loading') {
        document.addEventListener('DOMContentLoaded', bootstrap, { once: true });
    } else {
        bootstrap();
    }
})();

