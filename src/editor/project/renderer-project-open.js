(() => {
    function createProjectOpenManager({ deps } = {}) {
        const $ = deps?.$ || (typeof window !== 'undefined' ? (window.$ || window.jQuery || null) : null);
        const logger = deps?.logger || {
            info: () => { },
            warn: () => { },
            error: () => { }
        };
        const showToast = deps?.showToast || (() => { });
        const getActiveEditor = deps?.getActiveEditor || (() => null);
        const setCurrentProject = deps?.setCurrentProject || (() => { });

        if (!$) {
            throw new Error('createProjectOpenManager requires jQuery ($)');
        }

        function loadProjectIntoTree(projectData) {
            setCurrentProject(projectData);

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
                                        const editor = getActiveEditor();
                                        if (editor) {
                                            editor.setValue(result.code);
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

        return {
            loadProjectIntoTree,
            openProjectDialog
        };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.projectOpen = window.AhmadIDEModules.projectOpen || {};
        window.AhmadIDEModules.projectOpen.createProjectOpenManager = createProjectOpenManager;
    }
})();

