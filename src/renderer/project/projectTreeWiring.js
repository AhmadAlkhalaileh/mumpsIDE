(() => {
    function createProjectTreeWiring({ deps } = {}) {
        const $ = deps?.$;
        const showToast = deps?.showToast;
        const getActiveEditor = deps?.getActiveEditor;

        function renderProjectIntoTree(projectData) {
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
                                        const activeEditor = getActiveEditor();
                                        if (activeEditor) {
                                            // Use setValue (faster than creating new model)
                                            activeEditor.setValue(result.code);

                                            // FORCE theme re-application to trigger immediate color rendering
                                            const currentTheme = activeEditor._themeService?._theme?.themeName || 'mumps-dark';
                                            monaco.editor.setTheme(currentTheme);

                                            // Force layout and render
                                            activeEditor.layout();
                                            activeEditor.render(true);

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

        return { renderProjectIntoTree };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.renderer = window.AhmadIDEModules.renderer || {};
        window.AhmadIDEModules.renderer.project = window.AhmadIDEModules.renderer.project || {};
        window.AhmadIDEModules.renderer.project.createProjectTreeWiring = createProjectTreeWiring;
    }
})();
