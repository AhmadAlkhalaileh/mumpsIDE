(() => {
    function createProjectCreateManager({ deps } = {}) {
        const $ = deps?.$ || (typeof window !== 'undefined' ? (window.$ || window.jQuery || null) : null);
        const showToast = deps?.showToast || (() => { });
        const loadProjectIntoTree = deps?.loadProjectIntoTree || (() => { });

        if (!$) {
            throw new Error('createProjectCreateManager requires jQuery ($)');
        }

        function openNewProjectPanel() {
            const panel = document.getElementById('newProjectPanel');
            const overlay = document.getElementById('newProjectOverlay');
            panel?.classList.remove('hidden');
            overlay?.classList.remove('hidden');

            // Update structure preview when project name changes
            const updatePreview = () => {
                const name = $('#projectName').val() || 'my-project';
                const fetchRoutines = $('#fetchRoutines').is(':checked');

                let preview = `${name}/\n└── routines/\n`;
                if (fetchRoutines) {
                    preview += `    ├── localr/\n    │   └── (MUMPS .m files from localr)\n`;
                    preview += `    └── routines/\n        └── (MUMPS .m files from routines)`;
                } else {
                    preview += `    ├── localr/ (empty)\n    └── routines/ (empty)`;
                }

                $('#structurePreview').text(preview);
            };

            $('#projectName, #fetchRoutines').on('change keyup', updatePreview);
            updatePreview();
        }

        function closeNewProjectPanel() {
            document.getElementById('newProjectPanel')?.classList.add('hidden');
            document.getElementById('newProjectOverlay')?.classList.add('hidden');
        }

        function wireNewProjectPanel() {
            // New Project panel handlers
            document.getElementById('closeNewProjectBtn')?.addEventListener('click', closeNewProjectPanel);
            document.getElementById('newProjectOverlay')?.addEventListener('click', closeNewProjectPanel);

            // Browse button handler - moved to openNewProjectPanel function
            $('#createProjectBtn').on('click', async () => {
                const projectName = $('#projectName').val().trim();
                const projectPath = $('#projectPath').val().trim();
                const fetchRoutines = $('#fetchRoutines').is(':checked');

                if (!projectName) {
                    showToast('error', 'Validation', 'Project name is required');
                    return;
                }

                if (!projectPath) {
                    showToast('error', 'Validation', 'Project path is required');
                    return;
                }

                $('#projectCreationStatus').text('Creating project...');
                $('#createProjectBtn').prop('disabled', true);

                try {
                    const result = await window.ahmadIDE.createProject({
                        projectName,
                        projectPath,
                        fetchRoutines
                    });

                    if (result.ok) {
                        showToast('success', 'Project Created', result.message || 'Project created successfully');
                        $('#projectCreationStatus').text(`Created: ${result.projectPath}`);

                        // Auto-open the project in the file tree
                        loadProjectIntoTree(result);

                        setTimeout(() => {
                            closeNewProjectPanel();
                            // Reset form
                            $('#projectName').val('');
                            $('#projectCreationStatus').text('Ready');
                        }, 2000);
                    } else {
                        showToast('error', 'Creation Failed', result.error);
                        $('#projectCreationStatus').text(`Error: ${result.error}`);
                    }
                } catch (err) {
                    showToast('error', 'Error', err.message);
                    $('#projectCreationStatus').text(`Error: ${err.message}`);
                } finally {
                    $('#createProjectBtn').prop('disabled', false);
                }
            });
        }

        return {
            openNewProjectPanel,
            closeNewProjectPanel,
            wireNewProjectPanel
        };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.projectCreate = window.AhmadIDEModules.projectCreate || {};
        window.AhmadIDEModules.projectCreate.createProjectCreateManager = createProjectCreateManager;
    }
})();
