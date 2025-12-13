(() => {
    function createProjectContextMenuManager({ deps } = {}) {
        const $ = deps?.$ || (typeof window !== 'undefined' ? (window.$ || window.jQuery || null) : null);
        const showToast = deps?.showToast || (() => { });
        const showCustomPrompt = deps?.showCustomPrompt || (() => { });
        const createTab = deps?.createTab || (() => { });
        const setCurrentRoutine = deps?.setCurrentRoutine || (() => { });
        const loadRoutineByName = deps?.loadRoutineByName || (async () => { });
        const loadRoutineList = deps?.loadRoutineList || (async () => { });
        const getCurrentProject = deps?.getCurrentProject || (() => null);
        const openGitToolWindow = deps?.openGitToolWindow || (() => { });
        const runGitQuickCmd = deps?.runGitQuickCmd || (async () => ({ ok: false }));

        if (!$) {
            throw new Error('createProjectContextMenuManager requires jQuery ($)');
        }

        function normalizeContextMenuSeparators(items) {
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
        }

        function createContextMenuItemElement(item, menu) {
            if (item.separator) return $('<div class="ctx-separator"></div>');
            const el = $('<div class="ctx-item"></div>');
            if (item.disabled) el.addClass('disabled');
            el.append($('<span class="ctx-label"></span>').text(item.label));
            if (item.shortcut) el.append($('<span class="ctx-shortcut"></span>').text(item.shortcut));
            if (item.submenu) {
                el.addClass('has-submenu');
                el.append($('<span class="ctx-arrow">▸</span>'));
                const sub = $('<div class="ctx-submenu"></div>');
                item.submenu.forEach(subItem => sub.append(createContextMenuItemElement(subItem, menu)));
                el.append(sub);
            } else if (item.action && !item.disabled) {
                el.on('click', (e) => {
                    e.stopPropagation();
                    menu.remove();
                    item.action();
                });
            }
            return el;
        }

        function showProjectContextMenu(x, y, options = {}) {
            const { type, path, name, routineStateRef, editorRef, folderName } = options;
            $('.routines-context-menu').remove();

            const menu = $('<div class="routines-context-menu"></div>');
            menu.css({ position: 'fixed', top: y, left: x, zIndex: 5000 });

            const resolvedPath = path || (folderName ? `${folderName}/${name || ''}` : null);
            const currentProject = getCurrentProject();
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
                        {
                            label: 'Folder',
                            action: () => {
                                showCustomPrompt('New Folder', 'Folder path (e.g., /workspace/myFolder)', async (val) => {
                                    if (!val) return;
                                    const folderPath = val.trim();
                                    if (!folderPath) {
                                        showToast('error', 'Invalid path', 'Folder path cannot be empty');
                                        return;
                                    }

                                    // If relative path, prepend /workspace (universal mode default)
                                    const fullPath = folderPath.startsWith('/') ? folderPath : `/workspace/${folderPath}`;

                                    // Create directory in current environment (Docker or SSH)
                                    const result = await window.ahmadIDE.createDirectoryInCurrentEnv(fullPath);

                                    if (result.ok) {
                                        showToast('success', 'Created', `Folder created: ${fullPath}`);
                                        // Refresh project tree
                                        if (routineStateRef && editorRef) {
                                            await loadRoutineList(routineStateRef, editorRef);
                                        }
                                    } else {
                                        showToast('error', 'Failed', result.error || 'Could not create folder');
                                    }
                                });
                            }
                        }
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
            normalizeContextMenuSeparators(menuItems).forEach(i => menu.append(createContextMenuItemElement(i, menu)));
            $('body').append(menu);

            const rect = menu[0].getBoundingClientRect();
            if (rect.right > window.innerWidth) menu.css('left', window.innerWidth - rect.width - 10);
            if (rect.bottom > window.innerHeight) menu.css('top', window.innerHeight - rect.height - 10);
            setTimeout(() => $(document).one('click', () => menu.remove()), 100);
            return false;
        }

        function showRoutinesFolderContextMenu(x, y, routineStateRef, editorRef) {
            return showProjectContextMenu(x, y, {
                type: 'folder',
                routineStateRef,
                editorRef
            });
        }

        return {
            showProjectContextMenu,
            showRoutinesFolderContextMenu
        };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.projectContextMenu = window.AhmadIDEModules.projectContextMenu || {};
        window.AhmadIDEModules.projectContextMenu.createProjectContextMenuManager = createProjectContextMenuManager;
    }
})();
