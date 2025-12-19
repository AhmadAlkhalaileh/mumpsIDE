(() => {
    /**
     * Connections Dialog
     * Two-panel layout: left (SSH/Docker profiles), right (connection details)
     * Matches PhpStorm 2025.3 New UI connections panel
     *
     * Checklist: CON-001 to CON-012
     */
    function createConnectionsDialog({ deps } = {}) {
        const createDialog = deps?.createDialog || window.AhmadIDEModules?.ui?.createDialog;
        const createDialogLayout = deps?.createDialogLayout || window.AhmadIDEModules?.ui?.createDialogLayout;
        const primitives = deps?.primitives || window.AhmadIDEModules?.ui?.primitives;

        if (!createDialog || !createDialogLayout || !primitives) {
            throw new Error('ConnectionsDialog requires ui primitives');
        }

        let dialogApi = null;
        let layout = null;
        let activeProfile = null;
        let profiles = { ssh: [], docker: [] };
        let connectionStatus = { connected: false, type: null, profile: null };

        const loadProfiles = () => {
            try {
                const raw = localStorage.getItem('ahmadIDE:connectionProfiles');
                profiles = raw ? JSON.parse(raw) : { ssh: [], docker: [] };
            } catch (_) {
                profiles = { ssh: [], docker: [] };
            }
        };

        const saveProfiles = () => {
            try {
                localStorage.setItem('ahmadIDE:connectionProfiles', JSON.stringify(profiles));
            } catch (_) { }
        };

        const renderProfilesList = () => {
            if (!layout) return;
            const { createButton } = primitives;

            const container = document.createElement('div');
            container.className = 'connections-profiles';

            // SSH Section
            const sshSection = document.createElement('div');
            sshSection.className = 'connections-section';
            const sshTitle = document.createElement('div');
            sshTitle.className = 'connections-section__title';
            sshTitle.textContent = 'SSH Connections';
            sshSection.appendChild(sshTitle);

            if (!profiles.ssh || profiles.ssh.length === 0) {
                const empty = document.createElement('div');
                empty.className = 'connections-empty';
                empty.textContent = 'No SSH profiles';
                sshSection.appendChild(empty);
            } else {
                profiles.ssh.forEach((p) => {
                    const item = document.createElement('button');
                    item.type = 'button';
                    item.className = 'connections-profile-item' +
                        (activeProfile?.id === p.id ? ' active' : '') +
                        (connectionStatus.connected && connectionStatus.profile?.id === p.id ? ' connected' : '');
                    item.innerHTML = `
                        <div class="connections-profile-item__main">
                            <div class="connections-profile-item__name">${escapeHtml(p.name || 'Unnamed')}</div>
                            <div class="connections-profile-item__info">${escapeHtml(p.host || '')}${p.port ? ':' + p.port : ''}</div>
                        </div>
                        ${connectionStatus.connected && connectionStatus.profile?.id === p.id ? '<div class="connections-profile-item__badge">Connected</div>' : ''}
                    `;
                    item.addEventListener('click', () => {
                        activeProfile = p;
                        renderProfilesList();
                        renderConnectionForm();
                    });
                    sshSection.appendChild(item);
                });
            }

            // Docker Section
            const dockerSection = document.createElement('div');
            dockerSection.className = 'connections-section';
            const dockerTitle = document.createElement('div');
            dockerTitle.className = 'connections-section__title';
            dockerTitle.textContent = 'Docker Containers';
            dockerSection.appendChild(dockerTitle);

            if (!profiles.docker || profiles.docker.length === 0) {
                const empty = document.createElement('div');
                empty.className = 'connections-empty';
                empty.textContent = 'No Docker containers';
                dockerSection.appendChild(empty);
            } else {
                profiles.docker.forEach((p) => {
                    const item = document.createElement('button');
                    item.type = 'button';
                    item.className = 'connections-profile-item' +
                        (activeProfile?.id === p.id ? ' active' : '') +
                        (connectionStatus.connected && connectionStatus.profile?.id === p.id ? ' connected' : '');
                    item.innerHTML = `
                        <div class="connections-profile-item__main">
                            <div class="connections-profile-item__name">${escapeHtml(p.name || 'Unnamed')}</div>
                            <div class="connections-profile-item__info">${escapeHtml(p.containerId || '')}</div>
                        </div>
                        ${connectionStatus.connected && connectionStatus.profile?.id === p.id ? '<div class="connections-profile-item__badge">Connected</div>' : ''}
                    `;
                    item.addEventListener('click', () => {
                        activeProfile = p;
                        renderProfilesList();
                        renderConnectionForm();
                    });
                    dockerSection.appendChild(item);
                });
            }

            container.appendChild(sshSection);
            container.appendChild(dockerSection);

            // Add new profile button
            const addBtn = createButton({
                label: '+ New Profile',
                variant: 'ghost',
                onClick: () => {
                    activeProfile = { id: crypto.randomUUID(), type: 'ssh', name: '', host: '', port: 22, user: '' };
                    renderProfilesList();
                    renderConnectionForm();
                }
            });
            container.appendChild(addBtn);

            layout.elements.content.innerHTML = '';
            layout.elements.content.appendChild(container);
        };

        const renderConnectionForm = () => {
            if (!layout || !activeProfile) {
                if (layout) {
                    layout.elements.content.innerHTML = '<div class="connections-empty">Select or create a profile</div>';
                }
                return;
            }

            const { createInput, createButton, createSelect } = primitives;
            const form = document.createElement('div');
            form.className = 'connections-form';

            const typeGroup = createFormGroup('Connection Type', createSelect({
                options: [
                    { value: 'ssh', label: 'SSH' },
                    { value: 'docker', label: 'Docker' }
                ],
                value: activeProfile.type || 'ssh',
                onChange: (val) => { activeProfile.type = val; renderConnectionForm(); }
            }));

            const nameGroup = createFormGroup('Profile Name', createInput({
                value: activeProfile.name || '',
                placeholder: 'My Server',
                onChange: (val) => { activeProfile.name = val; }
            }));

            let specificFields = null;
            if (activeProfile.type === 'ssh') {
                const hostGroup = createFormGroup('Host', createInput({
                    value: activeProfile.host || '',
                    placeholder: '192.168.1.100',
                    onChange: (val) => { activeProfile.host = val; }
                }));
                const portGroup = createFormGroup('Port', createInput({
                    value: String(activeProfile.port || 22),
                    type: 'number',
                    placeholder: '22',
                    onChange: (val) => { activeProfile.port = parseInt(val, 10) || 22; }
                }));
                const userGroup = createFormGroup('User', createInput({
                    value: activeProfile.user || '',
                    placeholder: 'root',
                    onChange: (val) => { activeProfile.user = val; }
                }));
                const passGroup = createFormGroup('Password', createInput({
                    value: activeProfile.password || '',
                    type: 'password',
                    placeholder: '(optional)',
                    onChange: (val) => { activeProfile.password = val; }
                }));
                specificFields = [hostGroup, portGroup, userGroup, passGroup];
            } else {
                const containerGroup = createFormGroup('Container ID/Name', createInput({
                    value: activeProfile.containerId || '',
                    placeholder: 'yottadb-container',
                    onChange: (val) => { activeProfile.containerId = val; }
                }));
                specificFields = [containerGroup];
            }

            form.appendChild(typeGroup);
            form.appendChild(nameGroup);
            specificFields.forEach((f) => form.appendChild(f));

            const actions = document.createElement('div');
            actions.className = 'connections-actions';
            const testBtn = createButton({
                label: 'Test Connection',
                variant: 'ghost',
                onClick: async () => {
                    // TODO: implement test connection
                    alert('Test connection not implemented yet');
                }
            });
            const saveBtn = createButton({
                label: 'Save Profile',
                variant: 'primary',
                onClick: () => {
                    const existing = (profiles[activeProfile.type] || []).find((p) => p.id === activeProfile.id);
                    if (existing) {
                        Object.assign(existing, activeProfile);
                    } else {
                        profiles[activeProfile.type] = profiles[activeProfile.type] || [];
                        profiles[activeProfile.type].push({ ...activeProfile });
                    }
                    saveProfiles();
                    renderProfilesList();
                }
            });
            const deleteBtn = createButton({
                label: 'Delete',
                variant: 'danger',
                onClick: () => {
                    if (!confirm('Delete this profile?')) return;
                    profiles[activeProfile.type] = (profiles[activeProfile.type] || []).filter((p) => p.id !== activeProfile.id);
                    saveProfiles();
                    activeProfile = null;
                    renderProfilesList();
                    renderConnectionForm();
                }
            });

            actions.appendChild(testBtn);
            actions.appendChild(saveBtn);
            if ((profiles[activeProfile.type] || []).find((p) => p.id === activeProfile.id)) {
                actions.appendChild(deleteBtn);
            }
            form.appendChild(actions);

            layout.elements.content.innerHTML = '';
            layout.elements.content.appendChild(form);
        };

        const createFormGroup = (label, control) => {
            const group = document.createElement('div');
            group.className = 'ui-settings-row';
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

        const escapeHtml = (str) => String(str || '').replace(/[&<>"']/g, (m) => ({
            '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;'
        })[m]);

        const updateFooter = () => {
            if (!layout) return;
            const { createButton } = primitives;
            const isConnected = connectionStatus.connected && activeProfile && connectionStatus.profile?.id === activeProfile.id;
            const connectBtn = createButton({
                label: isConnected ? 'Disconnect' : 'Connect',
                variant: isConnected ? 'danger' : 'primary',
                disabled: !activeProfile || !activeProfile.name,
                onClick: async () => {
                    if (isConnected) {
                        // Disconnect
                        connectionStatus = { connected: false, type: null, profile: null };
                        updateStatusBar();
                        updateTitleWithStatus();
                        updateFooter();
                        renderProfilesList();
                    } else {
                        // Connect
                        connectionStatus = { connected: true, type: activeProfile.type, profile: { ...activeProfile } };
                        updateStatusBar();
                        updateTitleWithStatus();
                        updateFooter();
                        renderProfilesList();
                    }
                }
            });
            const closeBtn = createButton({ label: 'Close', variant: 'ghost', onClick: () => dialogApi?.close('close') });
            layout.setFooter([closeBtn, connectBtn]);
        };

        const updateStatusBar = () => {
            const statusEl = document.getElementById('connStatus');
            if (!statusEl) return;
            if (connectionStatus.connected) {
                statusEl.textContent = `${connectionStatus.type?.toUpperCase()}: ${connectionStatus.profile?.name || 'Connected'}`;
                statusEl.className = 'pill success';
            } else {
                statusEl.textContent = 'Ready';
                statusEl.className = 'pill subtle';
            }
        };

        const updateTitleWithStatus = () => {
            if (!layout) return;
            const baseTitle = 'Connections';
            if (connectionStatus.connected && connectionStatus.profile) {
                const statusText = ` – ${connectionStatus.type?.toUpperCase()}: ${connectionStatus.profile.name}`;
                layout.setTitle(baseTitle + statusText);
            } else {
                layout.setTitle(baseTitle);
            }
        };

        const ensureDialog = () => {
            if (dialogApi && layout) return;

            dialogApi = createDialog({
                ariaLabel: 'Connections',
                closeOnEscape: true,
                closeOnBackdrop: false,
                onClose: () => { }
            });

            layout = createDialogLayout({ title: 'Connections', searchPlaceholder: 'Filter connections' });
            layout.setItems([{ id: 'connections', label: 'Connections' }]);
            layout.elements.closeBtn.addEventListener('click', () => dialogApi.close('x'));

            dialogApi.setContent(layout.root);
            updateFooter();
            updateTitleWithStatus();
        };

        const open = () => {
            ensureDialog();
            loadProfiles();
            renderProfilesList();
            renderConnectionForm();
            dialogApi.open();
            return true;
        };

        return { open, getConnectionStatus: () => connectionStatus };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.features = window.AhmadIDEModules.features || {};
        window.AhmadIDEModules.features.connections = window.AhmadIDEModules.features.connections || {};
        window.AhmadIDEModules.features.connections.createConnectionsDialog = createConnectionsDialog;
    }
})();
