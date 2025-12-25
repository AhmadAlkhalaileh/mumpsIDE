(() => {
    /**
     * New Project Dialog
     * Form-based dialog for creating new MUMPS projects
     *
     * Checklist: NEW-001 to NEW-007
     */
    function createNewProjectDialog({ deps } = {}) {
        const createDialog = deps?.createDialog || window.AhmadIDEModules?.ui?.createDialog;
        const primitives = deps?.primitives || window.AhmadIDEModules?.ui?.primitives;

        if (!createDialog || !primitives) {
            throw new Error('NewProjectDialog requires ui primitives');
        }

        let dialogApi = null;

        const buildForm = () => {
            const container = document.createElement('div');
            container.className = 'new-project-form';
            container.style.cssText = 'padding:var(--ui-space-6);min-width:520px;';

            const { createInput, createSelect, createCheckbox } = primitives;

            const nameGroup = createFormGroup('Project Name *', createInput({
                id: 'newProjName',
                placeholder: 'my-mumps-project',
                value: ''
            }));

            const locationGroup = createFormGroup('Location *', (() => {
                const wrapper = document.createElement('div');
                wrapper.style.cssText = 'display:flex;gap:var(--ui-space-2);flex:1;';
                const input = createInput({ id: 'newProjLocation', placeholder: '/path/to/projects', value: '' });
                const browseBtn = document.createElement('button');
                browseBtn.className = 'ui-btn ui-btn--ghost';
                browseBtn.textContent = 'Browse…';
                browseBtn.addEventListener('click', async () => {
                    if (typeof window.electronAPI?.selectDirectory === 'function') {
                        const path = await window.electronAPI.selectDirectory();
                        if (path) input.value = path;
                    }
                });
                wrapper.appendChild(input);
                wrapper.appendChild(browseBtn);
                return wrapper;
            })());

            const typeGroup = createFormGroup('Project Type', createSelect({
                id: 'newProjType',
                options: [
                    { value: 'mumps', label: 'MUMPS Project' },
                    { value: 'mixed', label: 'Mixed (MUMPS + JavaScript)' }
                ],
                value: 'mumps'
            }));

            const gitGroup = createFormGroup('', createCheckbox({
                id: 'newProjGit',
                label: 'Initialize Git repository',
                checked: true
            }));

            container.appendChild(nameGroup);
            container.appendChild(locationGroup);
            container.appendChild(typeGroup);
            container.appendChild(gitGroup);

            return container;
        };

        const createFormGroup = (label, control) => {
            const group = document.createElement('div');
            group.className = 'ui-settings-row';
            group.style.marginBottom = 'var(--ui-space-4)';
            const lbl = document.createElement('div');
            lbl.className = 'ui-settings-row__label';
            lbl.textContent = label;
            const ctl = document.createElement('div');
            ctl.className = 'ui-settings-row__control';
            ctl.appendChild(control);
            group.appendChild(lbl);
            group.appendChild(ctl);
            return group;
        };

        const validateForm = (formEl) => {
            const name = formEl.querySelector('#newProjName')?.value?.trim();
            const location = formEl.querySelector('#newProjLocation')?.value?.trim();
            if (!name) return 'Project name is required';
            if (!location) return 'Project location is required';
            return null;
        };

        const ensureDialog = () => {
            if (dialogApi) return;

            dialogApi = createDialog({
                ariaLabel: 'New Project',
                closeOnEscape: true,
                closeOnBackdrop: false,
                onClose: () => { }
            });

            const wrapper = document.createElement('div');
            wrapper.className = 'ui-dialog-layout';

            const header = document.createElement('div');
            header.className = 'ui-dialog-header';
            const headerLeft = document.createElement('div');
            headerLeft.className = 'ui-dialog-header__left';
            const title = document.createElement('div');
            title.className = 'ui-dialog-title';
            title.textContent = 'New Project';
            headerLeft.appendChild(title);

            const headerRight = document.createElement('div');
            headerRight.className = 'ui-dialog-header__right';
            const closeBtn = document.createElement('button');
            closeBtn.className = 'ui-dialog-close';
            closeBtn.type = 'button';
            closeBtn.title = 'Close';
            closeBtn.textContent = '✕';
            closeBtn.addEventListener('click', () => dialogApi.close('x'));
            headerRight.appendChild(closeBtn);

            header.appendChild(headerLeft);
            header.appendChild(headerRight);

            const body = document.createElement('div');
            body.style.cssText = 'flex:1;overflow:auto;';
            const form = buildForm();
            body.appendChild(form);

            const footer = document.createElement('div');
            footer.className = 'ui-dialog-footer';
            const { createButton } = primitives;
            const cancelBtn = createButton({ label: 'Cancel', variant: 'ghost', onClick: () => dialogApi.close('cancel') });
            const createBtn = createButton({
                label: 'Create',
                variant: 'primary',
                onClick: async () => {
                    const error = validateForm(form);
                    if (error) {
                        alert(error);
                        return;
                    }
                    const name = form.querySelector('#newProjName').value.trim();
                    const location = form.querySelector('#newProjLocation').value.trim();
                    const type = form.querySelector('#newProjType').value;
                    const initGit = form.querySelector('#newProjGit').checked;

                    // TODO: Implement actual project creation via IPC
                    if (typeof window.electronAPI?.createProject === 'function') {
                        await window.electronAPI.createProject({ name, location, type, initGit });
                    }
                    dialogApi.close('ok');
                }
            });
            footer.appendChild(cancelBtn);
            footer.appendChild(createBtn);

            wrapper.appendChild(header);
            wrapper.appendChild(body);
            wrapper.appendChild(footer);

            dialogApi.setContent(wrapper);
        };

        const open = () => {
            ensureDialog();
            dialogApi.open();
            requestAnimationFrame(() => {
                const nameInput = document.getElementById('newProjName');
                nameInput?.focus();
            });
        };

        return { open };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.features = window.AhmadIDEModules.features || {};
        window.AhmadIDEModules.features.projects = window.AhmadIDEModules.features.projects || {};
        window.AhmadIDEModules.features.projects.createNewProjectDialog = createNewProjectDialog;
    }
})();
