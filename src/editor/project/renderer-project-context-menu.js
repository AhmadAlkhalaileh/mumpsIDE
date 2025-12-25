(() => {
    function createProjectContextMenuManager({ deps } = {}) {
        const showToast = deps?.showToast || (() => { });
        const showCustomPrompt = deps?.showCustomPrompt || (() => { });
        const createTab = deps?.createTab || (() => { });
        const setCurrentRoutine = deps?.setCurrentRoutine || (() => { });
        const loadRoutineByName = deps?.loadRoutineByName || (async () => { });
        const loadRoutineList = deps?.loadRoutineList || (async () => { });
        const getCurrentProject = deps?.getCurrentProject || (() => null);
        const openGitToolWindow = deps?.openGitToolWindow || (() => { });
        const runGitQuickCmd = deps?.runGitQuickCmd || (async () => ({ ok: false }));
        const toGitPathspec = deps?.toGitPathspec || ((p) => p);
        const menuRegistry = deps?.menuRegistry || window.AhmadIDEModules?.app?.menuRegistry;
        const menuController = deps?.menuController
            || window.AhmadIDEModules?.ui?.menu?.controller
            || window.AhmadIDEModules?.ui?.menu?.createMenuController?.({});
        const openContextMenu = deps?.openContextMenu || window.AhmadIDEModules?.ui?.menu?.openContextMenu;

        const copyToClipboard = async (text, label) => {
            if (!text) return;
            try {
                await navigator.clipboard.writeText(text);
                showToast('success', 'Copied', label || 'Copied');
            } catch (err) {
                showToast('error', 'Copy failed', err?.message || 'Could not copy');
            }
        };

        const runGitAction = async (action, target) => {
            const targetPath = target || '.';
            const gitPath = toGitPathspec(targetPath) || targetPath;
            const safe = String(gitPath).replace(/"/g, '\\"');
            openGitToolWindow();
            if (action === 'add') {
                await runGitQuickCmd(`git add -- "${safe}"`, { toastLabel: 'Git Add' });
                document.getElementById('gitStatusBtn')?.click();
                return;
            }
            if (action === 'commit') {
                const msg = document.getElementById('gitCommitMessage');
                const diffPath = document.getElementById('gitDiffPath');
                if (diffPath) diffPath.value = gitPath || '';
                await runGitQuickCmd(`git add -- "${safe}"`, { toastLabel: 'Git Commit' });
                showToast('info', 'Commit File', 'Staged file. Enter message in Git tool window to commit.');
                msg?.focus();
                document.getElementById('gitStatusBtn')?.click();
                return;
            }
            if (action === 'history') {
                await runGitQuickCmd(`git log --oneline -- "${safe}"`, { toastLabel: 'Git History' });
                const input = document.getElementById('gitDiffPath');
                if (input) input.value = gitPath || '';
                document.getElementById('gitLogBtn')?.click();
                return;
            }
            if (action === 'compare') {
                const input = document.getElementById('gitDiffPath');
                if (input) input.value = gitPath || '';
                document.getElementById('gitDiffFileBtn')?.click();
                return;
            }
            if (action === 'rollback') {
                await runGitQuickCmd(`git checkout -- "${safe}"`, { toastLabel: 'Git Rollback' });
                document.getElementById('gitStatusBtn')?.click();
            }
        };

        const resolvePath = (options) => {
            if (options?.path) return String(options.path);
            const folderName = String(options?.folderName || '');
            const name = String(options?.name || '');
            if (folderName && name) return `${folderName}/${name}`;
            return name || '';
        };

        const handleAction = async (action, ctx) => {
            const type = String(ctx?.type || '');
            const resolvedPath = String(ctx?.path || '');
            const folderName = String(ctx?.folderName || '');
            const routineStateRef = ctx?.routineStateRef || null;
            const editorRef = ctx?.editorRef || null;

            const currentProject = getCurrentProject() || {};
            const projectRoot = String(currentProject.projectPath || '');
            const fullPath = resolvedPath ? `${projectRoot ? projectRoot + '/' : ''}${resolvedPath}` : projectRoot;

            if (action === 'prj:open') {
                if (type === 'file' && resolvedPath) {
                    await loadRoutineByName(resolvedPath, routineStateRef, editorRef);
                }
                return;
            }
            if (action === 'prj:new-routine') {
                showCustomPrompt('New MUMPS Routine', 'Routine name (e.g., MYRTN)', async (val) => {
                    const v = String(val || '').trim();
                    if (!v) return;
                    const routine = v.toUpperCase();
                    const baseFolder = folderName || 'routines';
                    const full = baseFolder ? `${baseFolder}/${routine}` : routine;
                    try {
                        if (window.ahmadIDE?.createRoutine) {
                            await window.ahmadIDE.createRoutine(full);
                        } else {
                            const newContent = `${routine}\t; New routine created\n\tQUIT\n`;
                            createTab(routine, newContent, { current: full });
                            editorRef?.setValue?.(newContent);
                            if (routineStateRef) routineStateRef.current = full;
                            setCurrentRoutine(full);
                        }
                        showToast('success', 'Created', `Routine created: ${full}`);
                        if (routineStateRef && editorRef) {
                            await loadRoutineList(routineStateRef, editorRef);
                        }
                    } catch (err) {
                        showToast('error', 'Failed', err?.message || 'Could not create routine');
                    }
                });
                return;
            }
            if (action === 'prj:new-folder') {
                showCustomPrompt('New Folder', 'Folder path (e.g., /workspace/myFolder)', async (val) => {
                    const folderPath = String(val || '').trim();
                    if (!folderPath) return;
                    try {
                        const full = folderPath.startsWith('/') ? folderPath : `/workspace/${folderPath}`;
                        const result = await window.ahmadIDE?.createDirectoryInCurrentEnv?.(full);
                        if (result?.ok) {
                            showToast('success', 'Created', `Folder created: ${full}`);
                            if (routineStateRef && editorRef) {
                                await loadRoutineList(routineStateRef, editorRef);
                            }
                        } else {
                            showToast('error', 'Failed', result?.error || 'Could not create folder');
                        }
                    } catch (err) {
                        showToast('error', 'Failed', err?.message || 'Could not create folder');
                    }
                });
                return;
            }
            if (action === 'prj:copy-path') {
                if (fullPath) await copyToClipboard(fullPath, 'Path copied');
                return;
            }
            if (action === 'prj:copy-ref') {
                if (resolvedPath) await copyToClipboard(resolvedPath, 'Reference copied');
                return;
            }
            if (action === 'prj:reveal') {
                if (window.ahmadIDE?.revealInExplorer) {
                    window.ahmadIDE.revealInExplorer(fullPath || projectRoot || resolvedPath);
                } else {
                    showToast('info', 'Not Available', 'File explorer integration not available');
                }
                return;
            }
            if (action === 'prj:refresh') {
                if (routineStateRef && editorRef) {
                    await loadRoutineList(routineStateRef, editorRef);
                    showToast('success', 'Refreshed', 'Project tree updated');
                }
                return;
            }
            if (action === 'gitctx:add') return runGitAction('add', resolvedPath || '.');
            if (action === 'gitctx:commit') return runGitAction('commit', resolvedPath || '.');
            if (action === 'gitctx:history') return runGitAction('history', resolvedPath || '.');
            if (action === 'gitctx:compare') return runGitAction('compare', resolvedPath || '.');
            if (action === 'gitctx:rollback') return runGitAction('rollback', resolvedPath || '.');
            if (action === 'compare-with-release') {
                // Handle Compare with Release from project tree
                const handler = window.AhmadIDEModules?.extensions?.compareWithRelease?.handleCompareWithRelease;
                if (handler) {
                    // Open the file first if not already open
                    if (fullPath && fullPath.endsWith('.m')) {
                        try {
                            await loadRoutineByName(fullPath);
                        } catch (e) {
                            console.error('[Project Context Menu] Failed to load routine:', e);
                        }
                    }
                    await handler(ctx);
                } else {
                    showToast('error', 'Compare with Release', 'Extension not available');
                }
                return;
            }
            if (typeof runMenuAction === 'function') await runMenuAction(action, ctx || {});
        };

        function showProjectContextMenu(x, y, options = {}) {
            if (!menuRegistry || (!menuController && !openContextMenu)) return false;
            const ctx = {
                ...options,
                path: resolvePath(options)
            };
            const items = menuRegistry.get('context.project', ctx);
            const payload = {
                controller: menuController,
                x,
                y,
                items,
                ctx,
                onAction: (action, itemCtx) => handleAction(action, itemCtx || ctx)
            };
            if (typeof openContextMenu === 'function') {
                openContextMenu(payload);
                return false;
            }
            menuController.openAtPoint(payload);
            return false;
        }

        function showRoutinesFolderContextMenu(x, y, routineStateRef, editorRef) {
            return showProjectContextMenu(x, y, {
                type: 'folder',
                folderName: 'routines',
                routineStateRef,
                editorRef
            });
        }

        return { showProjectContextMenu, showRoutinesFolderContextMenu };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.projectContextMenu = window.AhmadIDEModules.projectContextMenu || {};
        window.AhmadIDEModules.projectContextMenu.createProjectContextMenuManager = createProjectContextMenuManager;
    }
})();
