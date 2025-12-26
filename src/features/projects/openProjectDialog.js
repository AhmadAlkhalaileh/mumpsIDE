(() => {
    /**
     * Open Project Dialog
     * File browser dialog for opening existing projects
     *
     * Checklist: OPN-001 to OPN-005
     */
    function createOpenProjectDialog({ deps } = {}) {
        const createDialog = deps?.createDialog || window.AhmadIDEModules?.ui?.createDialog;
        const primitives = deps?.primitives || window.AhmadIDEModules?.ui?.primitives;

        if (!createDialog || !primitives) {
            throw new Error('OpenProjectDialog requires ui primitives');
        }

        let dialogApi = null;

        let pathInputElement = null;

        const buildContent = () => {
            const container = document.createElement('div');
            container.style.cssText = 'padding:var(--ui-space-6);min-width:520px;';

            const { createInput } = primitives;

            const pathGroup = document.createElement('div');
            pathGroup.style.cssText = 'margin-bottom:var(--ui-space-4);';
            const pathLabel = document.createElement('div');
            pathLabel.textContent = 'Project Path';
            pathLabel.style.cssText = 'margin-bottom:var(--ui-space-2);font-weight:600;';

            const pathRow = document.createElement('div');
            pathRow.style.cssText = 'display:flex;gap:var(--ui-space-2);';
            pathInputElement = createInput({ placeholder: '/path/to/project', value: '' });
            pathInputElement.id = 'openProjPath';
            const browseBtn = document.createElement('button');
            browseBtn.className = 'ui-btn ui-btn--ghost';
            browseBtn.textContent = 'Browse…';
            browseBtn.addEventListener('click', async () => {
                if (typeof window.ahmadIDE?.openFolderDialog === 'function') {
                    const res = await window.ahmadIDE.openFolderDialog();
                    if (res?.ok && res.path) pathInputElement.value = res.path;
                }
            });
            pathRow.appendChild(pathInputElement);
            pathRow.appendChild(browseBtn);

            pathGroup.appendChild(pathLabel);
            pathGroup.appendChild(pathRow);
            container.appendChild(pathGroup);

            const recentSection = document.createElement('div');
            recentSection.style.cssText = 'margin-top:var(--ui-space-6);';
            const recentTitle = document.createElement('div');
            recentTitle.textContent = 'Recent Projects';
            recentTitle.style.cssText = 'margin-bottom:var(--ui-space-2);font-weight:600;';
            recentSection.appendChild(recentTitle);

            const recentList = document.createElement('div');
            recentList.className = 'recent-projects-list';
            recentList.style.cssText = 'border:1px solid var(--ui-border-subtle);border-radius:var(--ui-radius-2);padding:var(--ui-space-2);max-height:200px;overflow:auto;';

            const recents = getRecentProjects();
            if (recents.length === 0) {
                recentList.innerHTML = '<div style="padding:var(--ui-space-3);color:rgba(255,255,255,0.5);text-align:center;">No recent projects</div>';
            } else {
                recents.forEach((proj) => {
                    const item = document.createElement('button');
                    item.className = 'ui-btn ui-btn--ghost';
                    item.style.cssText = 'width:100%;text-align:left;margin-bottom:2px;';
                    item.textContent = proj.name || proj.path;
                    item.title = proj.path;
                    item.addEventListener('click', () => {
                        pathInput.value = proj.path;
                    });
                    recentList.appendChild(item);
                });
            }

            recentSection.appendChild(recentList);
            container.appendChild(recentSection);

            return container;
        };

        const getRecentProjects = () => {
            try {
                const raw = localStorage.getItem('ahmadIDE:recentProjects');
                return raw ? JSON.parse(raw) : [];
            } catch (_) {
                return [];
            }
        };

        const ensureDialog = () => {
            if (dialogApi) return;

            dialogApi = createDialog({
                ariaLabel: 'Open Project',
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
            title.textContent = 'Open Project';
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
            body.appendChild(buildContent());

            const footer = document.createElement('div');
            footer.className = 'ui-dialog-footer';
            const { createButton } = primitives;
            const cancelBtn = createButton({ label: 'Cancel', variant: 'ghost', onClick: () => dialogApi.close('cancel') });
            const openBtn = createButton({
                label: 'Open',
                variant: 'primary',
                onClick: async () => {
                    const path = pathInputElement?.value?.trim();
                    if (!path) {
                        alert('Please select a project path');
                        return;
                    }
                    if (typeof window.ahmadIDE?.openProject === 'function') {
                        const result = await window.ahmadIDE.openProject(path);
                        if (result?.ok) {
                            if (typeof window.loadProjectIntoTree === 'function') {
                                window.loadProjectIntoTree(result);
                            }
                            // Ensure Git gets the project path
                            const gitRepoManager = window.AhmadIDEModules?.git?.repoManager;
                            if (gitRepoManager?.setProject) {
                                gitRepoManager.setProject(result.projectPath)
                                    .catch(err => console.error("[OpenProjectDialog] setProject failed:", err));
                            } else {
                                console.error("[OpenProjectDialog] gitRepoManager.setProject not found");
                            }
                        }
                    }
                    dialogApi.close('ok');
                }
            });
            footer.appendChild(cancelBtn);
            footer.appendChild(openBtn);

            wrapper.appendChild(header);
            wrapper.appendChild(body);
            wrapper.appendChild(footer);

            dialogApi.setContent(wrapper);
        };

        const open = () => {
            ensureDialog();
            dialogApi.open();
        };

        return { open };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.features = window.AhmadIDEModules.features || {};
        window.AhmadIDEModules.features.projects = window.AhmadIDEModules.features.projects || {};
        window.AhmadIDEModules.features.projects.createOpenProjectDialog = createOpenProjectDialog;
    }
})();
