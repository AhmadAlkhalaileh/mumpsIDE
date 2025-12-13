(() => {
    function createProjectTreeManager({ state, deps } = {}) {
        if (!state?.collapsedTreeNodes) {
            throw new Error('createProjectTreeManager requires { state.collapsedTreeNodes }');
        }

        const $ = deps?.$ || (typeof window !== 'undefined' ? (window.$ || window.jQuery || null) : null);
        const normalizeRoutineTarget = deps?.normalizeRoutineTarget;
        const loadRoutineByName = deps?.loadRoutineByName || (() => { });
        const showProjectContextMenu = deps?.showProjectContextMenu || (() => false);
        const getCurrentProject = deps?.getCurrentProject || (() => null);
        const getActiveEditor = deps?.getActiveEditor || (() => null);
        const getRoutineFilterTerm = deps?.getRoutineFilterTerm || (() => '');
        const getRoutineState = deps?.getRoutineState || (() => null);

        if (!$) {
            throw new Error('createProjectTreeManager requires jQuery ($)');
        }
        if (!normalizeRoutineTarget || typeof normalizeRoutineTarget !== 'function') {
            throw new Error('createProjectTreeManager requires deps.normalizeRoutineTarget');
        }

        const collapsedTreeNodes = state.collapsedTreeNodes;

        // ============================================
        // PhpStorm-style SVG Icons for Project Tree
        // ============================================
        const treeIcons = {
            folder: `<svg width="16" height="16" viewBox="0 0 16 16" fill="#8c7a65"><path d="M2 3h5l1 1h6v9H2V3zm1 1v8h10V5H7.5L6.5 4H3z"/></svg>`,
            folderOpen: `<svg width="16" height="16" viewBox="0 0 16 16" fill="#c4a97a"><path d="M2 3h5l1 1h6v2h-1l-1 7H3L2 6V3zm1 1v1.5l.8 6.5h8.4l.8-5H4V4z"/></svg>`,
            project: `<svg width="16" height="16" viewBox="0 0 16 16"><rect x="1" y="1" width="14" height="14" rx="2" fill="#6b8dad"/><path d="M4 5h8v1H4zm0 3h8v1H4zm0 3h5v1H4z" fill="#fff" opacity="0.8"/></svg>`,
            mumps: state.mumpsFileIconSvg || '',
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
            const routineFilterTerm = getRoutineFilterTerm();

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
                                await loadRoutineByName(routinePath, routineStateRef, editorRef || getActiveEditor(), routinesSource);
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
            const projectName = getCurrentProject()?.projectPath?.split('/').pop() || 'Project';

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
                                        await loadRoutineByName(routinePath, routineStateRef, editorRef || getActiveEditor(), routinesSource);
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

        function setCollapseStateAll(collapsed) {
            collapsedTreeNodes.clear();
            if (collapsed) {
                collapsedTreeNodes.add('root');
                collapsedTreeNodes.add('root/routines');
            }
            const routineState = getRoutineState();
            renderProjectTree(routineState?._cacheFull || routineState?._lastRoutines || [], routineState, getActiveEditor());
        }

        return {
            renderProjectTreeLoading,
            renderProjectTree,
            setCollapseStateAll
        };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.projectTree = window.AhmadIDEModules.projectTree || {};
        window.AhmadIDEModules.projectTree.createProjectTreeManager = createProjectTreeManager;
    }
})();
