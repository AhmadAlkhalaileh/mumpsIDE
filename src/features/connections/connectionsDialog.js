(() => {
    /**
     * Connections Dialog
     * Two-panel layout: left (SSH/Docker profiles), right (connection details)
     * Matches  2025.3 New UI connections panel
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
                            <div class="connections-profile-item__name">${escapeHtml(p.name || 'Unnamed')}${p.isDefault ? ' <span style="color:var(--accent);font-size:0.8em">(Default)</span>' : ''}</div>
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

            // Release Connection Section (for Compare with Release feature)
            const releaseSection = document.createElement('div');
            releaseSection.className = 'connections-section';
            const releaseTitle = document.createElement('div');
            releaseTitle.className = 'connections-section__title';
            releaseTitle.textContent = 'Release Server';
            releaseSection.appendChild(releaseTitle);

            const releaseConnection = window.AhmadIDEModules?.features?.releaseConnection?.loadConnection?.();
            if (releaseConnection) {
                const item = document.createElement('button');
                item.type = 'button';
                item.className = 'connections-profile-item';
                item.innerHTML = `
                    <div class="connections-profile-item__main">
                        <div class="connections-profile-item__name">${escapeHtml(releaseConnection.name || 'Release Connection')}</div>
                        <div class="connections-profile-item__info">${escapeHtml(releaseConnection.host || '')}${releaseConnection.port ? ':' + releaseConnection.port : ''}</div>
                    </div>
                `;
                item.addEventListener('click', () => {
                    // Open release connection configuration
                    const renderReleaseForm = window.AhmadIDEModules?.features?.releaseConnection?.renderConfigForm;
                    if (renderReleaseForm && layout) {
                        layout.elements.content.innerHTML = '';
                        const formContainer = document.createElement('div');
                        formContainer.style.padding = '20px';
                        renderReleaseForm(formContainer);
                        layout.elements.content.appendChild(formContainer);
                    }
                });
                releaseSection.appendChild(item);
            } else {
                const empty = document.createElement('div');
                empty.className = 'connections-empty';
                empty.textContent = 'Not configured';
                releaseSection.appendChild(empty);

                const configBtn = createButton({
                    label: 'Configure Release Connection',
                    variant: 'ghost',
                    onClick: () => {
                        const renderReleaseForm = window.AhmadIDEModules?.features?.releaseConnection?.renderConfigForm;
                        if (renderReleaseForm && layout) {
                            layout.elements.content.innerHTML = '';
                            const formContainer = document.createElement('div');
                            formContainer.style.padding = '20px';
                            renderReleaseForm(formContainer);
                            layout.elements.content.appendChild(formContainer);
                        }
                    }
                });
                releaseSection.appendChild(configBtn);
            }

            container.appendChild(releaseSection);

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
                // [NEW] Default Checkbox
                const defaultGroup = document.createElement('div');
                defaultGroup.className = 'ui-settings-row';
                const defaultLabel = document.createElement('div');
                defaultLabel.className = 'ui-settings-row__label';
                defaultLabel.textContent = 'Auto-Connect';
                const defaultControl = document.createElement('div');
                defaultControl.className = 'ui-settings-row__control';

                const { createCheckbox } = primitives; // Assume createCheckbox is available or simple input
                const checkbox = document.createElement('input');
                checkbox.type = 'checkbox';
                checkbox.checked = !!activeProfile.isDefault;
                checkbox.style.accentColor = 'var(--accent)';
                checkbox.onchange = (e) => {
                    activeProfile.isDefault = e.target.checked;
                    // Ensure mutual exclusivity immediately (visual only, actual save handles persistence)
                    if (activeProfile.isDefault) {
                        profiles.docker.forEach(p => { if (p.id !== activeProfile.id) p.isDefault = false; });
                    }
                };

                const cbLabel = document.createElement('label');
                cbLabel.style.display = 'flex';
                cbLabel.style.alignItems = 'center';
                cbLabel.style.gap = '8px';
                cbLabel.style.cursor = 'pointer';
                cbLabel.style.color = 'var(--text)';
                cbLabel.appendChild(checkbox);
                cbLabel.appendChild(document.createTextNode('Make Default (Auto-Connect on startup)'));

                defaultControl.appendChild(cbLabel);
                defaultGroup.appendChild(defaultLabel);
                defaultGroup.appendChild(defaultControl);

                specificFields = [containerGroup, defaultGroup];
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
                    // Start of save logic
                    const list = profiles[activeProfile.type] || [];
                    const existingIndex = list.findIndex((p) => p.id === activeProfile.id);

                    // Handle mutual exclusivity for default
                    if (activeProfile.type === 'docker' && activeProfile.isDefault) {
                        list.forEach(p => p.isDefault = false);
                    }

                    if (existingIndex >= 0) {
                        list[existingIndex] = { ...activeProfile };
                    } else {
                        list.push({ ...activeProfile });
                    }
                    profiles[activeProfile.type] = list;

                    saveProfiles();
                    renderProfilesList();
                    // End of save logic
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

            // [NEW] Status Indicator above buttons
            const statusIndicator = document.createElement('div');
            statusIndicator.style.marginRight = 'auto';
            statusIndicator.style.display = 'flex';
            statusIndicator.style.alignItems = 'center';
            statusIndicator.style.gap = '8px';

            if (isConnected) {
                const dot = document.createElement('div');
                dot.style.width = '10px';
                dot.style.height = '10px';
                dot.style.borderRadius = '50%';
                dot.style.backgroundColor = '#4ade80'; // Green
                statusIndicator.appendChild(dot);

                const label = document.createElement('span');
                label.textContent = 'Connected';
                label.style.color = '#4ade80';
                label.style.fontWeight = '500';
                statusIndicator.appendChild(label);
            }

            const connectBtn = createButton({
                label: isConnected ? 'Disconnect' : 'Connect',
                variant: isConnected ? 'danger' : 'primary',
                disabled: !activeProfile || !activeProfile.name,
                onClick: async () => {
                    // Toggle Logic
                    if (isConnected) {
                        // Disconnect logic
                        // If it's docker, we might want to "unset" the connection in the backend
                        if (activeProfile.type === 'docker' && window.ahmadIDE?.setConnection) {
                            // Reset to universal/default or just clear
                            // For now, we update local status. 
                            // To properly disconnect Docker, we might need a specific backend call 
                            // or just setting connection to something empty.
                            // setConnection('docker', { docker: {} }) resets to universal usually.
                            try {
                                await window.ahmadIDE.setConnection('docker', { docker: {} });
                            } catch (e) { console.error('Disconnect failed', e); }
                        } else if (activeProfile.type === 'ssh' && window.ahmadIDE?.sshDisconnect) {
                            try { await window.ahmadIDE.sshDisconnect(); } catch (e) { }
                        }

                        connectionStatus = { connected: false, type: null, profile: null };
                        updateStatusBar();
                        updateTitleWithStatus();
                        updateFooter();
                        renderProfilesList();
                    } else {
                        // Connect logic
                        // Dispatch event to renderer-connections.js to handle actual connection
                        // But since we are inside the dialog, we might need to invoke the manager directly if possible
                        // OR we emit an event.
                        // However, existing SSH logic was inside renderer-connections.js handled by 'handleSshConnect'.
                        // Docker connect was via list item click.

                        // We'll emit a custom event that renderer-connections listens to, 
                        // OR we can rely on shared module methods if we exposed them.
                        // Ideally we update `connectionStatus` AFTER the actual connection succeeds.
                        // But for now, let's assume successful trigger and let the side-effect happen.

                        // Actually, to support "Connect" button from here for Docker, we need the logic from renderer-connections.
                        // We can't easily import it if it's not exposed globally.
                        // Let's fire a global event: 'ahmadIDE:connect-profile'

                        const event = new CustomEvent('ahmadIDE:connect-profile', {
                            detail: { profile: activeProfile }
                        });
                        window.dispatchEvent(event);

                        // We optimistically set connected for UI feedback, 
                        // but ideally `renderer-connections` should call back to update status.
                        // For this task, I'll update status locally to show response.
                        // The actual connection logic will be handled by the event listener we'll add in renderer-connections.

                        // connectionStatus set is deferred to the event handler response if possible, 
                        // but let's set a "Connecting..." state?
                    }
                }
            });
            const closeBtn = createButton({ label: 'Close', variant: 'ghost', onClick: () => dialogApi?.close('close') });
            layout.setFooter([statusIndicator, closeBtn, connectBtn]);
        };

        // Expose method to update connection status from outside
        const setConnectionStatus = (status) => {
            connectionStatus = status;
            if (layout) {
                updateStatusBar();
                updateTitleWithStatus();
                updateFooter();
                renderProfilesList();
            }
        };

        return { open, getConnectionStatus: () => connectionStatus, setConnectionStatus };
    }

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

    return { open, getConnectionStatus: () => connectionStatus, setConnectionStatus };
}

    if (typeof window !== 'undefined') {
    window.AhmadIDEModules = window.AhmadIDEModules || {};
    window.AhmadIDEModules.features = window.AhmadIDEModules.features || {};
    window.AhmadIDEModules.features.connections = window.AhmadIDEModules.features.connections || {};
    window.AhmadIDEModules.features.connections.createConnectionsDialog = createConnectionsDialog;
}
}) ();
