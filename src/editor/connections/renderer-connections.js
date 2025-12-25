(() => {
    function createConnectionsManager({ deps } = {}) {
        const appendOutput = deps?.appendOutput || (() => { });
        const loadRoutineList = deps?.loadRoutineList || (async () => { });
        const renderDocker = deps?.renderDocker || (() => { });
        const setConnStatus = deps?.setConnStatus || (() => { });
        const getRoutineSearchValue = deps?.getRoutineSearchValue || (() => {
            try {
                if (typeof routineSearch !== 'undefined' && routineSearch && routineSearch.value != null) {
                    return routineSearch.value || '';
                }
            } catch (e) {
                // ignore global lookup errors
            }
            return document.getElementById('routineSearch')?.value || '';
        });

        let cachedApi = null;
        let lazyHooked = false;

        function wireConnectionsPanel({ editor, routineState, terminalState } = {}) {
            if (cachedApi) return cachedApi;

            const featureRegistry = window.AhmadIDEModules?.app?.featureRegistry;
            const ensureMounted = () => {
                try {
                    featureRegistry?.ensureById?.('connectionsPanel');
                } catch (_) {
                    // ignore
                }
            };

            const connectionsBtn = document.getElementById('toggleConnections');
            if (connectionsBtn && !connectionsBtn.dataset.mideConnectionsWired) {
                // Left click: open Connections menu (Shift+Click opens the panel).
                connectionsBtn.addEventListener('click', (e) => {
                    const createConnectionsMenu = window.AhmadIDEModules?.features?.menus?.createConnectionsMenu;
                    if (!e.shiftKey && createConnectionsMenu) {
                        const menu = createConnectionsMenu();
                        if (menu?.showForAnchor) {
                            menu.showForAnchor(connectionsBtn);
                            return;
                        }
                        if (menu?.showAtPoint) {
                            const r = connectionsBtn.getBoundingClientRect();
                            menu.showAtPoint(Math.round(r.left), Math.round(r.top));
                            return;
                        }
                        if (menu?.show) {
                            const r = connectionsBtn.getBoundingClientRect();
                            menu.show(Math.round(r.left), Math.round(r.top));
                            return;
                        }
                    }
                    ensureMounted();
                    const api = wireConnectionsPanel({ editor, routineState, terminalState });
                    api?.openConnectionsPanel?.();
                    api?.refreshDockerList?.();
                });

                // Right click: Show context menu
                connectionsBtn.addEventListener('contextmenu', (e) => {
                    e.preventDefault();
                    e.stopPropagation();

                    const createConnectionsMenu = window.AhmadIDEModules?.features?.menus?.createConnectionsMenu;
                    if (createConnectionsMenu) {
                        const menu = createConnectionsMenu();
                        if (menu?.showAtPoint) return menu.showAtPoint(e.clientX, e.clientY);
                        if (menu?.show) return menu.show(e.clientX, e.clientY);
                    }
                });

                connectionsBtn.dataset.mideConnectionsWired = '1';
            }

            // If the panel hasn't been mounted yet, defer wiring until first open.
            if (!document.getElementById('dockerList')) {
                if (!lazyHooked && featureRegistry?.onMounted) {
                    featureRegistry.onMounted('connectionsPanel', () => {
                        lazyHooked = false;
                        wireConnectionsPanel({ editor, routineState, terminalState });
                    });
                    lazyHooked = true;
                }
                return null;
            }

            // --- SSH / Docker handling ---
            const connectionsPanel = document.getElementById('connectionsPanel');
            const connectionsOverlay = document.getElementById('connectionsOverlay');
            const closeConnectionsBtn = document.getElementById('closeConnectionsBtn');
            const dockerListEl = document.getElementById('dockerList');
            const refreshDockerBtn = document.getElementById('refreshDockerBtn');
            const useLocalDockerBtn = document.getElementById('useLocalDockerBtn');
            const dockerEnvKeyInput = document.getElementById('dockerEnvKeyInput');
            const dockerYdbPathInput = document.getElementById('dockerYdbPathInput');
            const dockerGldPathInput = document.getElementById('dockerGldPathInput');
            const dockerRoutinesPathInput = document.getElementById('dockerRoutinesPathInput');
            const dockerSaveConfigBtn = document.getElementById('dockerSaveConfigBtn');
            const dockerConfigStatus = document.getElementById('dockerConfigStatus');
            const sshHostInput = document.getElementById('sshHostInput');
            const sshPortInput = document.getElementById('sshPortInput');
            const sshEnvInput = document.getElementById('sshEnvInput');
            const sshUserInput = document.getElementById('sshUserInput');
            const sshPassInput = document.getElementById('sshPassInput');
            const sshConnectBtn = document.getElementById('sshConnectBtn');
            const sshFormStatus = document.getElementById('sshFormStatus');
            const sshSavedList = document.getElementById('sshSavedList');
            const sshSaveEnvBtn = document.getElementById('sshSaveEnvBtn');
            const releaseConnectionCard = document.getElementById('releaseConnectionCard');
            const releaseConnHostInput = document.getElementById('releaseConnHostInput');
            const releaseConnPortInput = document.getElementById('releaseConnPortInput');
            const releaseConnUserInput = document.getElementById('releaseConnUserInput');
            const releaseConnPassInput = document.getElementById('releaseConnPassInput');
            const releaseConnTestBtn = document.getElementById('releaseConnTestBtn');
            const releaseConnSaveBtn = document.getElementById('releaseConnSaveBtn');
            const releaseConnDeleteBtn = document.getElementById('releaseConnDeleteBtn');
            const releaseConnFocusCompareBtn = document.getElementById('releaseConnFocusCompareBtn');
            const releaseConnStatus = document.getElementById('releaseConnStatus');

            let savedProfiles = [];
            let dockerConfig = null;

            // Docker config management
            function loadDockerConfig() {
                try {
                    const raw = localStorage.getItem('ahmadIDE:dockerConfig');
                    return raw ? JSON.parse(raw) : null;
                } catch (e) {
                    return null;
                }
            }

            function saveDockerConfig(config) {
                try {
                    localStorage.setItem('ahmadIDE:dockerConfig', JSON.stringify(config));
                } catch (e) {
                    // ignore persistence errors
                }
            }

            function fillDockerConfigForm(config) {
                if (!config) return;
                if (dockerEnvKeyInput && config.envKey) dockerEnvKeyInput.value = config.envKey;
                if (dockerYdbPathInput && config.ydbPath) dockerYdbPathInput.value = config.ydbPath;
                if (dockerGldPathInput && config.gldPath) dockerGldPathInput.value = config.gldPath;
                if (dockerRoutinesPathInput && config.routinesPath) dockerRoutinesPathInput.value = config.routinesPath;
            }

            function updateDockerConfigStatus(message, severity = 'info') {
                if (!dockerConfigStatus) return;
                dockerConfigStatus.textContent = message;
                dockerConfigStatus.style.background = severity === 'error'
                    ? 'rgba(248,113,113,0.18)'
                    : 'rgba(91,213,255,0.12)';
                dockerConfigStatus.style.color = severity === 'error'
                    ? '#fecdd3'
                    : '#38bdf8';
            }

            // Load and display saved Docker config
            dockerConfig = loadDockerConfig();
            if (dockerConfig) {
                fillDockerConfigForm(dockerConfig);
                updateDockerConfigStatus('Config loaded');
            }

            function readSavedSshProfiles() {
                try {
                    const raw = localStorage.getItem('ahmadIDE:sshList');
                    const parsed = raw ? JSON.parse(raw) : [];
                    if (Array.isArray(parsed)) return parsed.filter(Boolean);
                    if (parsed && typeof parsed === 'object') return [parsed];
                    return [];
                } catch (e) {
                    return [];
                }
            }

            function persistSavedSshProfiles(list) {
                try {
                    localStorage.setItem('ahmadIDE:sshList', JSON.stringify(list || []));
                } catch (e) {
                    // ignore persistence errors
                }
            }

            function fillSshForm(entry) {
                if (!entry) return;
                if (sshHostInput && entry.host) sshHostInput.value = entry.host;
                if (sshUserInput && entry.username) sshUserInput.value = entry.username;
                if (sshPortInput && entry.port) sshPortInput.value = entry.port;
                if (sshEnvInput && entry.envKey) sshEnvInput.value = entry.envKey;
            }

            function renderSavedSshProfiles(list) {
                if (!sshSavedList) return;
                sshSavedList.innerHTML = '';
                if (!list || !list.length) {
                    sshSavedList.textContent = 'No saved environments.';
                    return;
                }
                list.forEach((item) => {
                    const pill = document.createElement('div');
                    pill.className = 'saved-env-pill';
                    pill.title = `${item.username || ''}@${item.host || ''}:${item.port || 22}`;
                    pill.onclick = () => fillSshForm(item);

                    const keyEl = document.createElement('span');
                    keyEl.className = 'env-key';
                    keyEl.textContent = (item.envKey || 'env').toUpperCase();

                    const metaEl = document.createElement('span');
                    metaEl.className = 'env-meta';
                    metaEl.textContent = `${item.username || ''}@${item.host || ''}`;

                    const removeBtn = document.createElement('button');
                    removeBtn.className = 'saved-env-remove';
                    removeBtn.textContent = '✕';
                    removeBtn.title = 'Remove';
                    removeBtn.onclick = (e) => {
                        e.stopPropagation();
                        savedProfiles = savedProfiles.filter(p =>
                            (p.envKey || '').toLowerCase() !== (item.envKey || '').toLowerCase()
                        );
                        persistSavedSshProfiles(savedProfiles);
                        renderSavedSshProfiles(savedProfiles);
                    };

                    pill.appendChild(keyEl);
                    pill.appendChild(metaEl);
                    pill.appendChild(removeBtn);
                    sshSavedList.appendChild(pill);
                });
            }

            function upsertSavedProfile(entry) {
                const key = (entry?.envKey || '').toLowerCase();
                if (!key) return;
                const existingIdx = savedProfiles.findIndex(
                    p => (p.envKey || '').toLowerCase() === key
                );
                const payload = {
                    envKey: entry.envKey,
                    host: entry.host,
                    port: entry.port || 22,
                    username: entry.username
                };
                if (existingIdx >= 0) {
                    savedProfiles[existingIdx] = { ...savedProfiles[existingIdx], ...payload };
                } else {
                    savedProfiles.push(payload);
                }
                persistSavedSshProfiles(savedProfiles);
                renderSavedSshProfiles(savedProfiles);
            }

            const savedSsh = (() => {
                try {
                    const raw = localStorage.getItem('ahmadIDE:ssh');
                    return raw ? JSON.parse(raw) : null;
                } catch (e) {
                    return null;
                }
            })();
            savedProfiles = readSavedSshProfiles();
            if (savedSsh && Object.keys(savedSsh).length) {
                const exists = savedProfiles.some(
                    p => (p.envKey || '').toLowerCase() === (savedSsh.envKey || '').toLowerCase()
                );
                if (!exists) {
                    savedProfiles.push(savedSsh);
                    persistSavedSshProfiles(savedProfiles);
                }
            }
            renderSavedSshProfiles(savedProfiles);

            if (savedSsh) {
                fillSshForm(savedSsh);
            } else if (savedProfiles.length) {
                fillSshForm(savedProfiles[0]);
            }
            if (sshEnvInput && !sshEnvInput.value) sshEnvInput.value = 'cc';

            const markSshStatus = (text, severity = 'info') => {
                if (!sshFormStatus) return;
                sshFormStatus.textContent = text;
                sshFormStatus.style.background = severity === 'error'
                    ? 'rgba(248,113,113,0.18)'
                    : 'rgba(91,213,255,0.12)';
                sshFormStatus.style.color = severity === 'error'
                    ? '#fecdd3'
                    : '#38bdf8';
            };

            const openConnectionsPanel = (focusSsh = false) => {
                connectionsPanel?.classList.remove('hidden');
                connectionsOverlay?.classList.remove('hidden');
                renderSavedSshProfiles(savedProfiles);
                if (focusSsh && sshHostInput) {
                    setTimeout(() => sshHostInput.focus(), 40);
                }
            };

            const closeConnectionsPanel = () => {
                connectionsPanel?.classList.add('hidden');
                connectionsOverlay?.classList.add('hidden');
            };

            async function refreshDockerList() {
                if (dockerListEl) dockerListEl.textContent = 'Loading...';
                appendOutput('🐳 Listing containers...', terminalState);
                const res = await window.ahmadIDE.listDocker();
                if (res.ok) {
                    if (!res.containers.length) {
                        if (dockerListEl) dockerListEl.textContent = 'No running containers.';
                        appendOutput('No running containers.', terminalState);
                    } else {
                        res.containers.forEach(c =>
                            appendOutput(`- ${c.name} (${c.id}) :: ${c.status}`, terminalState)
                        );
                        renderDocker(res.containers, routineState, editor, {
                            onSelect: async () => {
                                closeConnectionsPanel();
                                appendOutput('✓ Docker target selected', terminalState);
                                await loadRoutineList(
                                    routineState,
                                    editor,
                                    getRoutineSearchValue() || '',
                                    null
                                );
                            }
                        });
                    }
                } else {
                    // Show detailed permission error message
                    if (res.permissionError && res.message) {
                        if (dockerListEl) {
                            dockerListEl.innerHTML = `<div style="color: #f48771; white-space: pre-wrap; font-family: monospace; padding: 12px; background: rgba(244, 135, 113, 0.1); border-radius: 4px; border-left: 3px solid #f48771;">${res.message}</div>`;
                        }
                        appendOutput(`✗ ${res.error}`, terminalState);
                        appendOutput(res.message, terminalState);
                    } else {
                        if (dockerListEl) dockerListEl.textContent = res.error || res.stderr || 'Docker error';
                        appendOutput(`✗ Docker error: ${res.error || res.stderr}`, terminalState);
                    }
                }
            }

            async function handleSshConnect() {
                if (!sshHostInput || !sshUserInput || !sshPassInput || !sshPortInput) return;
                const host = sshHostInput.value.trim();
                const username = sshUserInput.value.trim();
                const password = sshPassInput.value;
                const port = parseInt(sshPortInput.value || '22', 10) || 22;
                const envKey = (sshEnvInput?.value || 'cc').trim() || 'cc';

                if (!host || !username || !password) {
                    markSshStatus('Host, user, and password are required', 'error');
                    return;
                }

                markSshStatus(`Connecting to ${username}@${host}:${port}...`, 'info');
                if (sshConnectBtn) sshConnectBtn.disabled = true;
                appendOutput(`🔌 SSH connecting to ${username}@${host}:${port}...`, terminalState);

                const res = await window.ahmadIDE.sshConnect({
                    host,
                    port,
                    username,
                    password,
                    envKey
                });

                if (res.ok) {
                    try {
                        if (res.sessionId) localStorage.setItem('ahmadIDE:sshSessionId', String(res.sessionId));
                    } catch (_) { }
                    const entry = { host, port, username, envKey };
                    await window.ahmadIDE.setConnection('ssh', {
                        ssh: { ...entry, password }
                    });
                    setConnStatus(`SSH: ${host}`, 'success');
                    markSshStatus(`Connected to ${username}@${host}:${port}`, 'success');
                    appendOutput('✓ SSH connected', terminalState);
                    try {
                        localStorage.setItem('ahmadIDE:ssh', JSON.stringify(entry));
                    } catch (e) {
                        // ignore persistence errors
                    }
                    upsertSavedProfile(entry);
                    await loadRoutineList(routineState, editor);
                    closeConnectionsPanel();
                } else {
                    // Show detailed permission error message
                    if (res.permissionError && res.message) {
                        markSshStatus('Permission denied - see instructions below', 'error');
                        setConnStatus('SSH permission error', 'error');
                        appendOutput(`✗ ${res.error}`, terminalState);
                        appendOutput('', terminalState);
                        appendOutput(res.message, terminalState);
                    } else {
                        const msg = res.error || res.stderr || 'SSH connect failed';
                        markSshStatus(msg, 'error');
                        setConnStatus('SSH error', 'error');
                        appendOutput(`✗ SSH connect failed: ${msg}`, terminalState);
                    }
                }

                if (sshConnectBtn) sshConnectBtn.disabled = false;
            }

            // --- Release Connection (Compare with Release) ---
            const releaseApi = window.AhmadIDEModules?.features?.releaseConnection;
            const readReleaseForm = () => ({
                host: (releaseConnHostInput?.value || '').trim(),
                port: parseInt(releaseConnPortInput?.value || '22', 10) || 22,
                username: (releaseConnUserInput?.value || '').trim(),
                password: releaseConnPassInput?.value || ''
            });

            const setReleaseStatus = (message, severity = 'info') => {
                if (!releaseConnStatus) return;
                releaseConnStatus.textContent = message;
                releaseConnStatus.style.background = severity === 'error'
                    ? 'rgba(248,113,113,0.18)'
                    : severity === 'success'
                        ? 'rgba(74,222,128,0.14)'
                        : 'rgba(91,213,255,0.12)';
                releaseConnStatus.style.color = severity === 'error'
                    ? '#fecdd3'
                    : severity === 'success'
                        ? '#4ade80'
                        : '#38bdf8';
            };

            const loadReleaseIntoForm = () => {
                if (!releaseApi || !releaseConnHostInput || !releaseConnUserInput || !releaseConnPortInput) return;
                const cfg = releaseApi.loadConnection?.() || null;
                if (cfg?.host) releaseConnHostInput.value = cfg.host;
                if (cfg?.username) releaseConnUserInput.value = cfg.username;
                releaseConnPortInput.value = String(cfg?.port || 22);
                if (releaseConnPassInput) releaseConnPassInput.value = '';
                if (cfg?.host && cfg?.username) {
                    setReleaseStatus(`Configured: ${cfg.username}@${cfg.host}:${cfg.port || 22}`, 'info');
                    if (releaseConnDeleteBtn) releaseConnDeleteBtn.disabled = false;
                } else {
                    setReleaseStatus('Not configured', 'info');
                    if (releaseConnDeleteBtn) releaseConnDeleteBtn.disabled = true;
                }
            };

            if (connectionsPanel && connectionsPanel.dataset.wired !== '1') {
                connectionsPanel.dataset.wired = '1';

                refreshDockerBtn?.addEventListener('click', refreshDockerList);
                useLocalDockerBtn?.addEventListener('click', async () => {
                    // Get last selected container ID from localStorage
                    let lastContainerId = null;
                    try {
                        lastContainerId = localStorage.getItem('ahmadIDE:lastContainerId');
                    } catch (e) {
                        // ignore
                    }

                    if (!lastContainerId) {
                        markSshStatus('Please select a container from the list first', 'error');
                        appendOutput('✗ No container selected. Click on a container above first.', terminalState);
                        return;
                    }

                    const config = dockerConfig || {};
                    config.containerId = lastContainerId;
                    await window.ahmadIDE.setConnection('docker', { docker: config });
                    const modeLabel = config.ydbPath ? 'Docker (configured)' : 'Docker (universal)';
                    setConnStatus(modeLabel, 'info');
                    appendOutput(`✓ Using ${modeLabel.toLowerCase()} connection`, terminalState);
                    closeConnectionsPanel();
                    await loadRoutineList(routineState, editor);
                });

                dockerSaveConfigBtn?.addEventListener('click', () => {
                    const envKey = dockerEnvKeyInput?.value?.trim() || '';
                    const ydbPath = dockerYdbPathInput?.value?.trim() || '';
                    const gldPath = dockerGldPathInput?.value?.trim() || '';
                    const routinesPath = dockerRoutinesPathInput?.value?.trim() || '';

                    const config = {};
                    if (envKey) config.envKey = envKey;
                    if (ydbPath) config.ydbPath = ydbPath;
                    if (gldPath) config.gldPath = gldPath;
                    if (routinesPath) config.routinesPath = routinesPath;

                    saveDockerConfig(config);
                    dockerConfig = config;

                    const hasConfig = Object.keys(config).length > 0;
                    updateDockerConfigStatus(
                        hasConfig ? 'Docker config saved' : 'Config cleared (universal mode)',
                        hasConfig ? 'info' : 'info'
                    );
                });

                connectionsOverlay?.addEventListener('click', closeConnectionsPanel);
                closeConnectionsBtn?.addEventListener('click', closeConnectionsPanel);
                sshConnectBtn?.addEventListener('click', handleSshConnect);
                sshSaveEnvBtn?.addEventListener('click', () => {
                    if (!sshHostInput || !sshUserInput || !sshEnvInput) return;
                    const host = sshHostInput.value.trim();
                    const username = sshUserInput.value.trim();
                    const port = parseInt(sshPortInput?.value || '22', 10) || 22;
                    const envKey = (sshEnvInput.value || 'cc').trim() || 'cc';
                    if (!host || !username) {
                        markSshStatus('Host and user required to save.', 'error');
                        return;
                    }
                    upsertSavedProfile({ host, username, port, envKey });
                    markSshStatus(`Saved ${envKey}`, 'info');
                });
                [sshHostInput, sshUserInput, sshPassInput, sshPortInput, sshEnvInput].forEach(input => {
                    input?.addEventListener('keydown', (e) => {
                        if (e.key === 'Enter') handleSshConnect();
                    });
                });

                // Wire Release Connection card (hidden unless extension enabled).
                loadReleaseIntoForm();
                window.addEventListener('ahmadIDE:connections-updated', () => loadReleaseIntoForm());

                releaseConnTestBtn?.addEventListener('click', async () => {
                    if (!releaseApi?.testConnection) return;
                    const cfg = readReleaseForm();
                    if (!cfg.host || !cfg.username) return setReleaseStatus('Host and username are required', 'error');
                    setReleaseStatus('Testing connection…', 'info');
                    releaseConnTestBtn.disabled = true;
                    try {
                        const result = await releaseApi.testConnection(cfg);
                        if (result?.success) setReleaseStatus(`✓ ${result.message || 'Connection successful'}`, 'success');
                        else setReleaseStatus(`✗ ${result?.message || 'Connection failed'}`, 'error');
                    } catch (e) {
                        setReleaseStatus(`✗ ${String(e?.message || e || 'Connection failed')}`, 'error');
                    } finally {
                        releaseConnTestBtn.disabled = false;
                    }
                });

                releaseConnSaveBtn?.addEventListener('click', async () => {
                    if (!releaseApi?.saveConnection) return;
                    const cfg = readReleaseForm();
                    if (!cfg.host || !cfg.username) return setReleaseStatus('Host and username are required', 'error');
                    releaseConnSaveBtn.disabled = true;
                    setReleaseStatus('Saving…', 'info');
                    try {
                        const ok = releaseApi.saveConnection(cfg);
                        if (!ok) throw new Error('Failed to save connection');
                        // Ensure password persistence (releaseConnection.saveConnection may fire-and-forget).
                        if (cfg.password && releaseApi.storePasswordSecurely) {
                            await releaseApi.storePasswordSecurely(cfg.password);
                        }
                        setReleaseStatus('✓ Saved (password stored securely)', 'success');
                        if (releaseConnPassInput) releaseConnPassInput.value = '';
                        window.dispatchEvent(new CustomEvent('ahmadIDE:connections-updated'));
                    } catch (e) {
                        setReleaseStatus(`✗ ${String(e?.message || e || 'Failed to save')}`, 'error');
                    } finally {
                        releaseConnSaveBtn.disabled = false;
                        loadReleaseIntoForm();
                    }
                });

                releaseConnDeleteBtn?.addEventListener('click', async () => {
                    if (!releaseApi?.deleteConnection) return;
                    const cfg = releaseApi.loadConnection?.() || null;
                    if (!cfg) return setReleaseStatus('Nothing to delete', 'info');
                    if (!confirm('Delete Release Connection?')) return;
                    releaseConnDeleteBtn.disabled = true;
                    try {
                        const ok = releaseApi.deleteConnection();
                        if (!ok) throw new Error('Failed to delete');
                        setReleaseStatus('✓ Deleted', 'success');
                        window.dispatchEvent(new CustomEvent('ahmadIDE:connections-updated'));
                    } catch (e) {
                        setReleaseStatus(`✗ ${String(e?.message || e || 'Failed to delete')}`, 'error');
                    } finally {
                        releaseConnDeleteBtn.disabled = false;
                        loadReleaseIntoForm();
                    }
                });

                releaseConnFocusCompareBtn?.addEventListener('click', async () => {
                    try { closeConnectionsPanel(); } catch (_) { }
                    try { window.toggleToolWindowPanel?.('comparePanel', 'bottom'); } catch (_) { }
                    const handler = window.AhmadIDEModules?.extensions?.compareWithRelease?.handleCompareWithRelease;
                    if (typeof handler === 'function') {
                        try { await handler({ source: 'connectionsPanel' }); } catch (_) { }
                    }
                });

                // Keep release connection card hidden unless Compare with Release is enabled.
                const applyReleaseVisibility = () => {
                    if (!releaseConnectionCard) return;
                    const extensionsService = window.AhmadIDEModules?.services?.extensionsService;
                    const enabled = !!extensionsService?.isEnabled?.('compare-with-release');
                    releaseConnectionCard.classList.toggle('hidden', !enabled);
                };
                applyReleaseVisibility();
                try { window.AhmadIDEModules?.services?.extensionsService?.onChange?.(applyReleaseVisibility); } catch (_) { }
            }

            // Wire up event from connectionsDialog.js
            window.addEventListener('ahmadIDE:connect-profile', async (e) => {
                const profile = e.detail?.profile;
                if (!profile) return;

                // Update status in dialog to "Connecting..." (optional, handled by dialog optimistically?)
                // Here we perform the action
                if (profile.type === 'docker') {
                    await connectToDocker({
                        containerId: profile.containerId,
                        name: profile.name,
                        routineState,
                        editor,
                        terminalState
                    }, {
                        appendOutput,
                        setConnStatus,
                        closeConnectionsPanel,
                        loadRoutineList: async (rs, ed) => {
                            await loadRoutineList(rs, ed, getRoutineSearchValue() || '', null);
                        }
                    });

                    // Update dialog status
                    const dialogImpl = window.AhmadIDEModules?.features?.connections?.createConnectionsDialog?.();
                    if (dialogImpl && dialogImpl.setConnectionStatus) {
                        dialogImpl.setConnectionStatus({ connected: true, type: 'docker', profile });
                    }
                } else if (profile.type === 'ssh') {
                    // Reuse existing SSH logic by populating inputs and clicking connect?
                    // Or invoke sshConnect method directly?
                    // Existing handleSshConnect reads from inputs.
                    // Let's populate inputs then trigger it to reuse logic (safe, lazy way)
                    if (sshHostInput) sshHostInput.value = profile.host || '';
                    if (sshPortInput) sshPortInput.value = profile.port || '22';
                    if (sshUserInput) sshUserInput.value = profile.user || '';
                    if (sshPassInput) sshPassInput.value = profile.password || ''; // Password might not be saved?
                    if (sshEnvInput) sshEnvInput.value = profile.envKey || ''; // Profile structure might differ slightly?

                    // Actually profiles in dialog are stored as: { id, type, name, host, port, user, password, containerId }
                    // handleSshConnect expects inputs validation.

                    await handleSshConnect();

                    // Update dialog status if successful (handleSshConnect sets internal status, but dialog needs explicit update?)
                    // handleSshConnect closes the panel on success.
                    // The dialog (if open) needs to know.
                    // If handleSshConnect succeeds, we should update the dialog state.
                }
            });

            cachedApi = {
                openConnectionsPanel,
                closeConnectionsPanel,
                refreshDockerList,
                handleSshConnect,
                connectToDocker, // Expose for auto-connect
                checkForAutoConnect
            };
            return cachedApi;
        }

        // New Helper: Connect to a specific Docker container
        async function connectToDocker({ containerId, name, routineState, editor, terminalState, config = null }, callbacks = {}) {
            const appendOutput = callbacks.appendOutput || (() => { });
            const setConnStatus = callbacks.setConnStatus || (() => { });
            const closePanel = callbacks.closeConnectionsPanel || (() => { });

            if (!containerId) {
                appendOutput('✗ No container ID provided for Docker connection.', terminalState);
                return false;
            }

            const dockerConfig = config || (() => {
                try { return JSON.parse(localStorage.getItem('ahmadIDE:dockerConfig') || '{}'); } catch (e) { return {}; }
            })();

            // Merge container ID into config or use universal if no extra config
            const fullConfig = { containerId, ...dockerConfig };

            appendOutput(`🐳 Connecting to ${name || containerId}...`, terminalState);

            // Validate the container is running (prevents "connected" UI when Docker isn't reachable).
            try {
                const listRes = await window.ahmadIDE.listDocker?.();
                if (!listRes?.ok) {
                    const msg = listRes?.message || listRes?.error || listRes?.stderr || 'Docker is not available';
                    appendOutput(`✗ Docker unavailable: ${msg}`, terminalState);
                    setConnStatus('Ready', 'subtle');
                    return false;
                }
                const match = (listRes.containers || []).find((c) => {
                    const id = String(c?.id || c?.containerId || '').trim();
                    if (!id) return false;
                    return id === containerId || id.startsWith(containerId) || containerId.startsWith(id);
                });
                if (!match) {
                    appendOutput('✗ Selected container is not running. Refresh Docker list and try again.', terminalState);
                    setConnStatus('Ready', 'subtle');
                    return false;
                }
                // Prefer the actual name from Docker when available.
                if (!name && match?.name) name = match.name;
            } catch (e) {
                appendOutput(`✗ Docker validation failed: ${String(e?.message || e)}`, terminalState);
                setConnStatus('Ready', 'subtle');
                return false;
            }

            const res = await window.ahmadIDE.setConnection('docker', { docker: fullConfig });

            if (res) { // Assuming setConnection returns something truthy or void on success (it's usually void but awaited)
                // Persistence for last used
                try { localStorage.setItem('ahmadIDE:lastContainerId', containerId); } catch (e) { }

                const modeLabel = fullConfig.ydbPath ? 'Docker (configured)' : 'Docker (universal)';
                setConnStatus(name ? `Docker: ${name}` : modeLabel, 'success');
                appendOutput(`✓ Connected to ${name || containerId}`, terminalState);

                if (callbacks.loadRoutineList) {
                    await callbacks.loadRoutineList(routineState, editor);
                }

                closePanel();
                return true;
            }
            return false;
        }

        // New Helper: Auto-connect check
        async function checkForAutoConnect({ editor, routineState, terminalState }, callbacks = {}) {
            try {
                // Check docker config for default container
                const dockerConfigRaw = localStorage.getItem('ahmadIDE:dockerConfig');
                if (!dockerConfigRaw) return;
                const dockerConfig = JSON.parse(dockerConfigRaw);

                if (dockerConfig && dockerConfig.defaultContainerId) {
                    callbacks.appendOutput('⚡ Auto-connecting to default Docker container...', terminalState);
                    await connectToDocker({
                        containerId: dockerConfig.defaultContainerId,
                        name: dockerConfig.defaultContainerName,
                        routineState,
                        editor,
                        terminalState,
                        config: dockerConfig
                    }, callbacks);
                }
            } catch (e) {
                console.warn('Auto-connect failed', e);
            }
        }

        return {
            wireConnectionsPanel,
            connectToDocker,
            checkForAutoConnect
        };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.connections = window.AhmadIDEModules.connections || {};
        window.AhmadIDEModules.connections.createConnectionsManager = createConnectionsManager;
    }
})();
