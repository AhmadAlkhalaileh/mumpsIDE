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

        function wireConnectionsPanel({ editor, routineState, terminalState } = {}) {
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
            const connectionsBtn = document.getElementById('toggleConnections');
            const sshSavedList = document.getElementById('sshSavedList');
            const sshSaveEnvBtn = document.getElementById('sshSaveEnvBtn');

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
                await window.ahmadIDE.setConnection('docker');
                setConnStatus('Docker (local)', 'info');
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
                    const entry = { host, port, username, envKey };
                    await window.ahmadIDE.setConnection('ssh', {
                        ssh: { ...entry, password }
                    });
                    setConnStatus('SSH connected', 'success');
                    markSshStatus('SSH connected', 'success');
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

            connectionsBtn?.addEventListener('click', () => {
                openConnectionsPanel();
                refreshDockerList();
            });

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

            return {
                openConnectionsPanel,
                closeConnectionsPanel,
                refreshDockerList,
                handleSshConnect
            };
        }

        return {
            wireConnectionsPanel
        };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.connections = window.AhmadIDEModules.connections || {};
        window.AhmadIDEModules.connections.createConnectionsManager = createConnectionsManager;
    }
})();
