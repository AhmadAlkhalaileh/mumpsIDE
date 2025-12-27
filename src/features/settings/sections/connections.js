(() => {
    function renderConnectionsSection(ctx) {
        const primitives = ctx?.primitives || window.AhmadIDEModules?.ui?.primitives || {};
        const fallbackCreateButton = (opts = {}) => {
            const { label = '', variant = 'default', size = 'md', onClick = null, type = 'button' } = opts;
            const btn = document.createElement('button');
            btn.type = type;
            btn.className = `ui-btn ui-btn--${variant} ui-btn--${size}`;
            const text = document.createElement('span');
            text.className = 'ui-btn__label';
            text.textContent = label;
            btn.appendChild(text);
            if (typeof onClick === 'function') btn.addEventListener('click', (e) => onClick(e));
            return btn;
        };
        const fallbackCreateInput = (opts = {}) => {
            const { value = '', placeholder = '', type = 'text', disabled = false, onInput = null, onChange = null } = opts;
            const input = document.createElement('input');
            input.className = 'ui-input';
            input.type = type;
            input.value = value ?? '';
            input.placeholder = placeholder ?? '';
            input.disabled = !!disabled;
            if (typeof onInput === 'function') input.addEventListener('input', (e) => onInput(e));
            if (typeof onChange === 'function') input.addEventListener('change', (e) => onChange(e));
            return input;
        };
        const fallbackCreateSelect = (opts = {}) => {
            const { value = '', options = [], disabled = false, onChange = null } = opts;
            const select = document.createElement('select');
            select.className = 'ui-select';
            select.disabled = !!disabled;
            (options || []).forEach((opt) => {
                const o = document.createElement('option');
                if (typeof opt === 'string') {
                    o.value = opt;
                    o.textContent = opt;
                } else {
                    o.value = String(opt.value);
                    o.textContent = String(opt.label ?? opt.value);
                    if (opt.disabled) o.disabled = true;
                }
                select.appendChild(o);
            });
            select.value = value ?? '';
            if (typeof onChange === 'function') select.addEventListener('change', (e) => onChange(e));
            return select;
        };
        const createButton = primitives.createButton || fallbackCreateButton;
        const createInput = primitives.createInput || fallbackCreateInput;
        const createSelect = primitives.createSelect || fallbackCreateSelect;
        const root = document.createElement('div');
        root.dataset.filterText = 'connections ssh docker remote deployment env key host port';
        const notify = (type, title, message) => {
            try { if (typeof showToast === 'function') { showToast(type, title, message); return; } } catch (_) { }
            try { alert(`${title}\n\n${message}`); } catch (_) { }
        };
        const readJson = (key, fallback) => { try { const raw = localStorage.getItem(key); return raw ? JSON.parse(raw) : fallback; } catch (_) { return fallback; } };
        const writeJson = (key, value) => { try { localStorage.setItem(key, JSON.stringify(value)); } catch (_) { } };
        const setStatus = (text, severity = 'info') => {
            try { if (typeof window.setConnStatus === 'function') { window.setConnStatus(text, severity); return; } } catch (_) { }
            const pill = document.getElementById('connStatus'); if (!pill) return; pill.textContent = text;
            if (severity === 'subtle') { pill.style.background = ''; pill.style.color = ''; return; }
            pill.style.background = severity === 'error' ? 'rgba(248,113,113,0.18)' : 'rgba(14,165,233,0.12)';
            pill.style.color = severity === 'error' ? '#fecdd3' : '#38bdf8';
        };
        const readStatusText = () => { const pill = document.getElementById('connStatus'); const text = String(pill?.textContent || '').trim(); return text || 'Ready'; };
        const parseConnectionState = (raw) => {
            const text = String(raw || '').trim();
            if (!text || text.toLowerCase() === 'ready') return { connected: false, type: null, label: 'Ready', raw: 'Ready' };
            let type = null; let label = text; const match = text.match(/^\s*(ssh|docker)\s*[:\-]\s*(.*)$/i);
            if (match) { type = match[1].toLowerCase(); label = String(match[2] || '').trim() || text; }
            else { if (/\bssh\b/i.test(text)) type = 'ssh'; if (/\bdocker\b/i.test(text)) type = 'docker'; }
            return { connected: true, type, label, raw: text };
        };
        const refreshRoutines = async () => { try { await window.AhmadIDEModules?.app?.refreshRoutines?.(); } catch (_) { } };
        const openConnectionsPanel = () => {
            const btn = document.getElementById('toggleConnections');
            if (btn) { btn.dispatchEvent(new MouseEvent('click', { bubbles: true, cancelable: true, shiftKey: true })); return true; }
            notify('info', 'Connections', 'Connections panel is not available.'); return false;
        };
        const loadSshProfiles = () => { const parsed = readJson('ahmadIDE:sshList', []); if (Array.isArray(parsed)) return parsed.filter(Boolean); if (parsed && typeof parsed === 'object') return [parsed]; return []; };
        const saveSshProfiles = (list) => writeJson('ahmadIDE:sshList', list || []);
        const loadDefaultSsh = () => readJson('ahmadIDE:ssh', null);
        const saveDefaultSsh = (entry) => writeJson('ahmadIDE:ssh', entry || {});
        const loadDockerConfig = () => readJson('ahmadIDE:dockerConfig', {});
        const saveDockerConfig = (config) => writeJson('ahmadIDE:dockerConfig', config || {});
        const loadDockerCache = () => readJson('ahmadIDE:dockerContainers', []);
        const saveDockerCache = (list) => writeJson('ahmadIDE:dockerContainers', list || []);
        const makeRow = (label, control, filterText) => {
            const row = document.createElement('div'); row.className = 'ui-settings-row'; row.dataset.filterText = (filterText || label).toLowerCase();
            const l = document.createElement('div'); l.className = 'ui-settings-row__label'; l.textContent = label;
            const c = document.createElement('div'); c.className = 'ui-settings-row__control'; c.appendChild(control);
            row.appendChild(l); row.appendChild(c); return row;
        };
        const statusGroup = document.createElement('div'); statusGroup.className = 'ui-settings-group'; statusGroup.dataset.filterText = 'connections status panel';
        const statusTitle = document.createElement('div'); statusTitle.className = 'ui-settings-group__title'; statusTitle.textContent = 'Connections';
        const statusHint = document.createElement('div'); statusHint.className = 'ui-settings-group__hint'; statusHint.textContent = 'Manage SSH and Docker defaults used by the Connections panel.';
        statusGroup.appendChild(statusTitle); statusGroup.appendChild(statusHint);
        const statusWrap = document.createElement('div'); statusWrap.style.display = 'flex'; statusWrap.style.alignItems = 'center'; statusWrap.style.gap = '8px';
        const statusPill = document.createElement('span'); statusPill.className = 'pill subtle';
        const refreshBtn = createButton({ label: 'Refresh', variant: 'ghost', size: 'sm', onClick: () => updateConnectionUi() });
        statusWrap.appendChild(statusPill); statusWrap.appendChild(refreshBtn);
        statusGroup.appendChild(makeRow('Status', statusWrap, 'status active connection'));
        const actionsWrap = document.createElement('div'); actionsWrap.style.display = 'flex'; actionsWrap.style.flexWrap = 'wrap'; actionsWrap.style.gap = '8px';
        const openConnectionsBtn = createButton({ label: 'Open Connections Panel', variant: 'primary', onClick: openConnectionsPanel });
        actionsWrap.appendChild(openConnectionsBtn);
        statusGroup.appendChild(makeRow('Actions', actionsWrap, 'open connections panel'));
        root.appendChild(statusGroup);
        const sshGroup = document.createElement('div'); sshGroup.className = 'ui-settings-group'; sshGroup.dataset.filterText = 'ssh profiles host port user env key';
        const sshTitle = document.createElement('div'); sshTitle.className = 'ui-settings-group__title'; sshTitle.textContent = 'SSH Profiles';
        const sshHint = document.createElement('div'); sshHint.className = 'ui-settings-group__hint'; sshHint.textContent = 'Passwords are not stored. Enter them to connect or use the Connections panel.';
        sshGroup.appendChild(sshTitle); sshGroup.appendChild(sshHint);
        let sshProfiles = loadSshProfiles(); let defaultSsh = loadDefaultSsh(); const NEW_VALUE = '__new__'; let activeIndex = -1;
        if (defaultSsh?.envKey) activeIndex = sshProfiles.findIndex((p) => String(p?.envKey || '').toLowerCase() === String(defaultSsh.envKey || '').toLowerCase());
        if (activeIndex < 0 && sshProfiles.length) activeIndex = 0;
        const buildSshOptions = (list) => {
            const opts = [{ value: NEW_VALUE, label: 'New Profile...' }];
            list.forEach((p, idx) => {
                const envKey = String(p?.envKey || '').trim(); const host = String(p?.host || '').trim(); const user = String(p?.username || '').trim();
                const name = envKey ? envKey.toUpperCase() : `PROFILE ${idx + 1}`; const meta = [user, host].filter(Boolean).join('@');
                const label = meta ? `${name} - ${meta}` : name; opts.push({ value: String(idx), label });
            });
            return opts;
        };
        const sshSelect = createSelect({
            options: buildSshOptions(sshProfiles), value: activeIndex >= 0 ? String(activeIndex) : NEW_VALUE,
            onChange: () => {
                const val = sshSelect.value; if (val === NEW_VALUE) { activeIndex = -1; fillSshForm({ port: 22 }); return; }
                const idx = parseInt(val, 10); activeIndex = Number.isFinite(idx) ? idx : -1; fillSshForm(sshProfiles[activeIndex] || {});
            }
        });
        const envKeyInput = createInput({ value: '', placeholder: 'auto' });
        const hostInput = createInput({ value: '', placeholder: '192.168.1.100' });
        const portInput = createInput({ value: '22', type: 'number', placeholder: '22' });
        const userInput = createInput({ value: '', placeholder: 'root' });
        const passInput = createInput({ value: '', type: 'password', placeholder: 'Password' });
        const fillSshForm = (profile) => { envKeyInput.value = String(profile?.envKey || ''); hostInput.value = String(profile?.host || ''); portInput.value = String(profile?.port || 22); userInput.value = String(profile?.username || ''); passInput.value = ''; };
        const readSshForm = () => ({ envKey: envKeyInput.value.trim(), host: hostInput.value.trim(), port: parseInt(portInput.value || '22', 10) || 22, username: userInput.value.trim(), password: passInput.value || '' });
        const refreshSshSelect = (selectedIdx = -1) => {
            sshSelect.innerHTML = ''; buildSshOptions(sshProfiles).forEach((opt) => { const o = document.createElement('option'); o.value = opt.value; o.textContent = opt.label; sshSelect.appendChild(o); });
            sshSelect.value = selectedIdx >= 0 ? String(selectedIdx) : NEW_VALUE;
        };
        const sshStatusText = document.createElement('div'); sshStatusText.className = 'ui-settings-group__hint'; sshStatusText.style.marginTop = '6px';
        let connectSshBtn, disconnectSshBtn;
        const connectSsh = async () => {
            const entry = readSshForm();
            if (!entry.envKey || !entry.host || !entry.username || !entry.password) { notify('info', 'SSH', 'Env key, host, user, and password are required.'); return; }
            if (!window.ahmadIDE?.sshConnect) { notify('error', 'SSH', 'SSH backend is not available.'); return; }
            const btnLabel = connectSshBtn?.querySelector('.ui-btn__label');
            const originalLabel = btnLabel?.textContent || 'Connect SSH';
            if (connectSshBtn) connectSshBtn.disabled = true;
            if (btnLabel) btnLabel.textContent = 'Connecting...';
            try {
                const res = await window.ahmadIDE.sshConnect({ host: entry.host, port: entry.port, username: entry.username, password: entry.password, envKey: entry.envKey });
                if (!res?.ok) throw new Error(res?.error || 'SSH connect failed');
                if (res.sessionId) localStorage.setItem('ahmadIDE:sshSessionId', String(res.sessionId));
                saveDefaultSsh({ envKey: entry.envKey, host: entry.host, port: entry.port, username: entry.username });
                setStatus(`SSH: ${entry.host}`, 'success'); await refreshRoutines();
            } catch (err) {
                notify('error', 'SSH', String(err?.message || err || 'SSH connect failed'));
            } finally {
                if (connectSshBtn) connectSshBtn.disabled = false;
                if (btnLabel) btnLabel.textContent = originalLabel;
                passInput.value = ''; updateConnectionUi();
            }
        };
        const disconnectSsh = async () => {
            const sessionId = (() => { try { return String(localStorage.getItem('ahmadIDE:sshSessionId') || '').trim(); } catch (_) { return ''; } })();
            if (sessionId && window.ahmadIDE?.sshDisconnect) { try { await window.ahmadIDE.sshDisconnect(sessionId); } catch (_) { } }
            try { localStorage.removeItem('ahmadIDE:sshSessionId'); } catch (_) { }
            try { await window.ahmadIDE?.setConnection?.('docker', { docker: { containerId: null } }); } catch (_) { }
            setStatus('Ready', 'subtle'); await refreshRoutines(); updateConnectionUi();
        };
        fillSshForm(sshProfiles[activeIndex] || {});
        sshGroup.appendChild(makeRow('Saved Profiles', sshSelect, 'ssh profiles list'));
        sshGroup.appendChild(makeRow('Env Key', envKeyInput, 'ssh env key'));
        sshGroup.appendChild(makeRow('Host', hostInput, 'ssh host'));
        sshGroup.appendChild(makeRow('Port', portInput, 'ssh port'));
        sshGroup.appendChild(makeRow('User', userInput, 'ssh user'));
        sshGroup.appendChild(makeRow('Password', passInput, 'ssh password'));
        const sshActionsRow = document.createElement('div'); sshActionsRow.style.display = 'flex'; sshActionsRow.style.flexWrap = 'wrap'; sshActionsRow.style.gap = '8px';
        const saveSshBtn = createButton({
            label: 'Save Profile', variant: 'primary',
            onClick: () => {
                const entry = readSshForm();
                if (!entry.envKey || !entry.host || !entry.username) { notify('info', 'SSH Profile', 'Env key, host, and user are required.'); return; }
                const existingIdx = sshProfiles.findIndex((p) => String(p?.envKey || '').toLowerCase() === entry.envKey.toLowerCase());
                const payload = { envKey: entry.envKey, host: entry.host, port: entry.port, username: entry.username };
                if (existingIdx >= 0) { sshProfiles[existingIdx] = { ...sshProfiles[existingIdx], ...payload }; activeIndex = existingIdx; }
                else { sshProfiles.push(payload); activeIndex = sshProfiles.length - 1; }
                saveSshProfiles(sshProfiles); refreshSshSelect(activeIndex); notify('success', 'SSH Profile', 'Profile saved.');
            }
        });
        const newSshBtn = createButton({ label: 'New', variant: 'ghost', onClick: () => { activeIndex = -1; refreshSshSelect(activeIndex); fillSshForm({ port: 22 }); } });
        const removeSshBtn = createButton({
            label: 'Remove', variant: 'danger',
            onClick: () => {
                if (activeIndex < 0) { notify('info', 'SSH Profile', 'Select a saved profile to remove.'); return; }
                const removed = sshProfiles.splice(activeIndex, 1); saveSshProfiles(sshProfiles); activeIndex = sshProfiles.length ? 0 : -1; refreshSshSelect(activeIndex); fillSshForm(sshProfiles[activeIndex] || { port: 22 });
                if (removed?.[0]?.envKey && defaultSsh?.envKey === removed[0].envKey) { saveDefaultSsh({}); defaultSsh = null; }
                notify('success', 'SSH Profile', 'Profile removed.');
            }
        });
        const defaultSshBtn = createButton({
            label: 'Set As Default', variant: 'ghost',
            onClick: () => {
                const entry = readSshForm();
                if (!entry.envKey) { notify('info', 'SSH Profile', 'Enter an env key before setting default.'); return; }
                saveDefaultSsh({ envKey: entry.envKey, host: entry.host, port: entry.port, username: entry.username }); defaultSsh = entry; notify('success', 'SSH Profile', 'Default profile saved.');
            }
        });
        connectSshBtn = createButton({ label: 'Connect SSH', variant: 'primary', onClick: connectSsh });
        disconnectSshBtn = createButton({ label: 'Disconnect SSH', variant: 'danger', onClick: disconnectSsh });
        const sshActionsWrap = document.createElement('div'); sshActionsWrap.style.display = 'flex'; sshActionsWrap.style.flexDirection = 'column'; sshActionsWrap.style.gap = '6px';
        sshActionsRow.appendChild(saveSshBtn); sshActionsRow.appendChild(newSshBtn); sshActionsRow.appendChild(removeSshBtn); sshActionsRow.appendChild(defaultSshBtn); sshActionsRow.appendChild(connectSshBtn); sshActionsRow.appendChild(disconnectSshBtn);
        sshActionsWrap.appendChild(sshActionsRow); sshActionsWrap.appendChild(sshStatusText);
        sshGroup.appendChild(makeRow('Actions', sshActionsWrap, 'ssh actions save remove default connect'));
        root.appendChild(sshGroup);
        const dockerGroup = document.createElement('div'); dockerGroup.className = 'ui-settings-group'; dockerGroup.dataset.filterText = 'docker env key ydb gld routines containers';
        const dockerTitle = document.createElement('div'); dockerTitle.className = 'ui-settings-group__title'; dockerTitle.textContent = 'Docker Defaults';
        const dockerHint = document.createElement('div'); dockerHint.className = 'ui-settings-group__hint'; dockerHint.textContent = 'Defaults used when connecting to Docker containers.';
        dockerGroup.appendChild(dockerTitle); dockerGroup.appendChild(dockerHint);
        const dockerConfig = loadDockerConfig();
        const dockerEnvKeyInput = createInput({ value: String(dockerConfig?.envKey || ''), placeholder: 'auto' });
        const dockerYdbPathInput = createInput({ value: String(dockerConfig?.ydbPath || ''), placeholder: '/data/yottadb' });
        const dockerGldPathInput = createInput({ value: String(dockerConfig?.gldPath || ''), placeholder: '/data' });
        const dockerRoutinesPathInput = createInput({ value: String(dockerConfig?.routinesPath || ''), placeholder: '/data/routines' });
        dockerGroup.appendChild(makeRow('Env Key', dockerEnvKeyInput, 'docker env key'));
        dockerGroup.appendChild(makeRow('YDB Path', dockerYdbPathInput, 'docker ydb path'));
        dockerGroup.appendChild(makeRow('GLD Path', dockerGldPathInput, 'docker gld path'));
        dockerGroup.appendChild(makeRow('Routines Path', dockerRoutinesPathInput, 'docker routines path'));
        let dockerContainers = loadDockerCache();
        const dockerSelect = createSelect({ options: [{ value: '', label: 'Select container...' }], value: '' });
        const dockerListStatus = document.createElement('div'); dockerListStatus.className = 'ui-settings-group__hint';
        const dockerConnectStatus = document.createElement('div'); dockerConnectStatus.className = 'ui-settings-group__hint'; dockerConnectStatus.style.marginTop = '6px';
        // [NEW] Default Container Logic
        const defaultContainerSelect = createSelect({ options: [{ value: '', label: 'None' }], value: '' });

        const fillDockerSelect = (list) => {
            dockerSelect.innerHTML = '';
            defaultContainerSelect.innerHTML = '';

            const opts = (list && list.length) ? list : [];
            const noneOpt = document.createElement('option'); noneOpt.value = ''; noneOpt.textContent = 'None';
            defaultContainerSelect.appendChild(noneOpt);

            if (!opts.length) {
                const o = document.createElement('option'); o.value = ''; o.textContent = 'No containers found'; dockerSelect.appendChild(o);
                dockerListStatus.textContent = 'No Docker containers available.';
                return;
            }

            opts.forEach((c) => {
                const o = document.createElement('option');
                o.value = c.id || c.containerId || '';
                o.textContent = c.name || c.id || 'Container';
                dockerSelect.appendChild(o);

                const d = document.createElement('option');
                d.value = c.id || c.containerId || '';
                d.textContent = c.name || c.id || 'Container';
                defaultContainerSelect.appendChild(d);
            });

            dockerListStatus.textContent = `${opts.length} container${opts.length === 1 ? '' : 's'} found.`;

            // Set current values
            if (dockerConfig.defaultContainerId) {
                defaultContainerSelect.value = dockerConfig.defaultContainerId;
            }
        };

        fillDockerSelect(dockerContainers);
        dockerGroup.appendChild(makeRow('Containers', dockerSelect, 'docker containers list'));
        dockerGroup.appendChild(makeRow('Default Container', defaultContainerSelect, 'docker default container'));
        dockerGroup.appendChild(makeRow('List Status', dockerListStatus, 'docker containers status'));
        let dockerRefreshBtn, dockerConnectBtn, dockerDisconnectBtn;
        const refreshDockerList = async () => {
            if (!window.ahmadIDE?.listDocker) { notify('error', 'Docker', 'Docker backend is not available.'); return; }
            const btnLabel = dockerRefreshBtn?.querySelector('.ui-btn__label');
            const originalLabel = btnLabel?.textContent || 'Refresh Containers';
            if (dockerRefreshBtn) dockerRefreshBtn.disabled = true;
            if (btnLabel) btnLabel.textContent = 'Loading...';
            dockerListStatus.textContent = 'Loading containers...';
            try {
                const res = await window.ahmadIDE.listDocker(); if (!res?.ok) throw new Error(res?.error || 'Docker list failed');
                dockerContainers = res.containers || []; saveDockerCache(dockerContainers); fillDockerSelect(dockerContainers);
            } catch (err) { dockerListStatus.textContent = 'Failed to load containers.'; notify('error', 'Docker', String(err?.message || err || 'Docker list failed')); }
            finally {
                if (dockerRefreshBtn) dockerRefreshBtn.disabled = false;
                if (btnLabel) btnLabel.textContent = originalLabel;
            }
        };
        const connectDocker = async () => {
            const containerId = dockerSelect.value;
            if (!containerId) { notify('info', 'Docker', 'Select a container before connecting.'); return; }
            const envKey = dockerEnvKeyInput.value.trim();
            const ydbPath = dockerYdbPathInput.value.trim();
            const gldPath = dockerGldPathInput.value.trim();
            const routinesPath = dockerRoutinesPathInput.value.trim();
            const payloadConfig = { envKey, ydbPath, gldPath, routinesPath };
            const btnLabel = dockerConnectBtn.querySelector('.ui-btn__label');
            const originalLabel = btnLabel?.textContent || 'Connect Docker';
            dockerConnectBtn.disabled = true;
            if (btnLabel) btnLabel.textContent = 'Connecting...';
            try {
                // Validate the container is running (prevents "connected" UI when Docker isn't reachable).
                const listRes = await window.ahmadIDE.listDocker();
                if (!listRes?.ok) {
                    const msg = listRes?.message || listRes?.error || listRes?.stderr || 'Docker is not available';
                    throw new Error(msg);
                }
                const isRunning = (listRes.containers || []).some((c) => {
                    const id = String(c?.id || c?.containerId || '').trim();
                    if (!id) return false;
                    return id === containerId || id.startsWith(containerId) || containerId.startsWith(id);
                });
                if (!isRunning) {
                    throw new Error('Selected container is not running. Refresh the list and try again.');
                }

                await window.ahmadIDE.setConnection('docker', { docker: { containerId, ...payloadConfig } });
                localStorage.setItem('ahmadIDE:lastContainerId', String(containerId));

                const savedConfig = {};
                if (envKey) savedConfig.envKey = envKey;
                if (ydbPath) savedConfig.ydbPath = ydbPath;
                if (gldPath) savedConfig.gldPath = gldPath;
                if (routinesPath) savedConfig.routinesPath = routinesPath;
                saveDockerConfig(savedConfig);
                const name = dockerContainers.find((c) => (c.id || c.containerId) === containerId)?.name || containerId;
                setStatus(`Docker: ${name}`, 'success'); await refreshRoutines();
            } catch (err) { notify('error', 'Docker', String(err?.message || err || 'Docker connect failed')); } finally {
                dockerConnectBtn.disabled = false;
                if (btnLabel) btnLabel.textContent = originalLabel;
                updateConnectionUi();
            }
        };
        const disconnectDocker = async () => {
            try { await window.ahmadIDE?.setConnection?.('docker', { docker: { containerId: null } }); } catch (_) { }
            setStatus('Ready', 'subtle'); await refreshRoutines(); updateConnectionUi();
        };
        const dockerActionsRow = document.createElement('div'); dockerActionsRow.style.display = 'flex'; dockerActionsRow.style.flexWrap = 'wrap'; dockerActionsRow.style.gap = '8px';
        const dockerActionsWrap = document.createElement('div'); dockerActionsWrap.style.display = 'flex'; dockerActionsWrap.style.flexDirection = 'column'; dockerActionsWrap.style.gap = '6px';
        dockerRefreshBtn = createButton({ label: 'Refresh Containers', variant: 'ghost', onClick: refreshDockerList });
        dockerConnectBtn = createButton({ label: 'Connect Docker', variant: 'primary', onClick: connectDocker });
        dockerDisconnectBtn = createButton({ label: 'Disconnect Docker', variant: 'danger', onClick: disconnectDocker });
        const dockerSaveBtn = createButton({
            label: 'Save Docker Defaults', variant: 'ghost',
            onClick: () => {
                const envKey = dockerEnvKeyInput.value.trim();
                const ydbPath = dockerYdbPathInput.value.trim();
                const gldPath = dockerGldPathInput.value.trim();
                const routinesPath = dockerRoutinesPathInput.value.trim();

                const next = {
                    defaultContainerId: defaultContainerSelect.value || null,
                    defaultContainerName: defaultContainerSelect.options[defaultContainerSelect.selectedIndex]?.textContent || ''
                };
                if (envKey) next.envKey = envKey;
                if (ydbPath) next.ydbPath = ydbPath;
                if (gldPath) next.gldPath = gldPath;
                if (routinesPath) next.routinesPath = routinesPath;

                saveDockerConfig(next);
                notify('success', 'Docker Defaults', 'Defaults saved.');
            }
        });
        dockerActionsRow.appendChild(dockerRefreshBtn); dockerActionsRow.appendChild(dockerConnectBtn); dockerActionsRow.appendChild(dockerDisconnectBtn); dockerActionsRow.appendChild(dockerSaveBtn);
        dockerActionsWrap.appendChild(dockerActionsRow); dockerActionsWrap.appendChild(dockerConnectStatus);
        dockerGroup.appendChild(makeRow('Actions', dockerActionsWrap, 'docker save defaults refresh connect'));
        root.appendChild(dockerGroup);

        // Release Connection (for Compare with Release feature)
        const releaseGroup = document.createElement('div');
        releaseGroup.className = 'ui-settings-group';
        releaseGroup.dataset.filterText = 'release connection ssh compare routines';
        const releaseTitle = document.createElement('div');
        releaseTitle.className = 'ui-settings-group__title';
        releaseTitle.textContent = 'Release Connection';
        const releaseHint = document.createElement('div');
        releaseHint.className = 'ui-settings-group__hint';
        releaseHint.textContent = 'SSH connection to release server for comparing routines. Independent from main SSH connection.';
        releaseGroup.appendChild(releaseTitle);
        releaseGroup.appendChild(releaseHint);

        const releaseApi = window.AhmadIDEModules?.features?.releaseConnection;
        const releaseConfig = releaseApi?.loadConnection?.() || {};

        const releaseHostInput = createInput({ value: String(releaseConfig?.host || ''), placeholder: '192.168.1.100' });
        const releasePortInput = createInput({ value: String(releaseConfig?.port || 22), type: 'number', placeholder: '22' });
        const releaseUserInput = createInput({ value: String(releaseConfig?.username || ''), placeholder: 'username' });
        const releasePassInput = createInput({ value: '', type: 'password', placeholder: 'Password (stored in keychain)' });
        const releaseStatus = document.createElement('div');
        releaseStatus.className = 'ui-settings-group__hint';
        releaseStatus.style.marginTop = '6px';
        releaseStatus.textContent = releaseConfig?.host ? 'Configured' : 'Not configured';

        releaseGroup.appendChild(makeRow('Host', releaseHostInput, 'release host ip address'));
        releaseGroup.appendChild(makeRow('Port', releasePortInput, 'release port'));
        releaseGroup.appendChild(makeRow('Username', releaseUserInput, 'release username'));
        releaseGroup.appendChild(makeRow('Password', releasePassInput, 'release password keychain'));

        const releaseActionsRow = document.createElement('div');
        releaseActionsRow.style.display = 'flex';
        releaseActionsRow.style.flexWrap = 'wrap';
        releaseActionsRow.style.gap = '8px';

        let releaseConnectBtn, releaseDisconnectBtn;

        const releaseTestBtn = createButton({
            label: 'Test Connection',
            variant: 'ghost',
            onClick: async () => {
                const config = {
                    host: releaseHostInput.value.trim(),
                    port: parseInt(releasePortInput.value, 10) || 22,
                    username: releaseUserInput.value.trim(),
                    password: releasePassInput.value
                };

                if (!config.host || !config.username) {
                    notify('info', 'Release Connection', 'Host and username are required');
                    return;
                }

                if (!config.password && !releaseConfig?.host) {
                    notify('info', 'Release Connection', 'Password is required for first connection');
                    return;
                }

                if (!config.password && releaseConfig?.host) {
                    config.password = await releaseApi?.getPasswordSecurely?.();
                }

                const btnLabel = releaseTestBtn.querySelector('.ui-btn__label');
                const originalLabel = btnLabel?.textContent || 'Test Connection';
                releaseTestBtn.disabled = true;
                if (btnLabel) btnLabel.textContent = 'Testing...';
                releaseStatus.textContent = 'Testing connection...';

                try {
                    const result = await releaseApi?.testConnection?.(config);
                    if (result?.success) {
                        releaseStatus.textContent = '✓ ' + (result.message || 'Connection successful');
                        notify('success', 'Release Connection', 'Connection successful');
                    } else {
                        releaseStatus.textContent = '✗ ' + (result?.message || 'Connection failed');
                        notify('error', 'Release Connection', result?.message || 'Connection failed');
                    }
                } catch (err) {
                    releaseStatus.textContent = '✗ ' + String(err?.message || err || 'Connection failed');
                    notify('error', 'Release Connection', String(err?.message || err || 'Connection failed'));
                } finally {
                    releaseTestBtn.disabled = false;
                    if (btnLabel) btnLabel.textContent = originalLabel;
                }
            }
        });

        const releaseSaveBtn = createButton({
            label: 'Save',
            variant: 'primary',
            onClick: async () => {
                const config = {
                    id: 'release',
                    name: 'Release Connection',
                    host: releaseHostInput.value.trim(),
                    port: parseInt(releasePortInput.value, 10) || 22,
                    username: releaseUserInput.value.trim(),
                    password: releasePassInput.value
                };

                if (!config.host || !config.username) {
                    notify('info', 'Release Connection', 'Host and username are required');
                    return;
                }

                try {
                    const saved = await releaseApi?.saveConnection?.(config);
                    if (saved) {
                        releaseStatus.textContent = 'Saved successfully';
                        notify('success', 'Release Connection', 'Connection settings saved');
                        releasePassInput.value = '';
                    } else {
                        throw new Error('Failed to save');
                    }
                } catch (err) {
                    releaseStatus.textContent = '✗ Failed to save';
                    notify('error', 'Release Connection', String(err?.message || err || 'Failed to save'));
                }
            }
        });

        const releaseDeleteBtn = createButton({
            label: 'Delete',
            variant: 'danger',
            onClick: async () => {
                if (!releaseConfig?.host) {
                    notify('info', 'Release Connection', 'No connection to delete');
                    return;
                }

                if (!confirm('Delete release connection settings?')) return;

                try {
                    await releaseApi?.deleteConnection?.();
                    releaseHostInput.value = '';
                    releasePortInput.value = '22';
                    releaseUserInput.value = '';
                    releasePassInput.value = '';
                    releaseStatus.textContent = 'Not configured';

                    // Disconnect if connected
                    const sessionId = localStorage.getItem('ahmadIDE:releaseSessionId');
                    if (sessionId) {
                        disconnectRelease();
                    }

                    notify('success', 'Release Connection', 'Connection deleted');
                } catch (err) {
                    notify('error', 'Release Connection', String(err?.message || err || 'Failed to delete'));
                }
            }
        });

        // [NEW] Latency Check
        const checkLatency = async (sessionId) => {
            if (!sessionId) return null;
            const start = performance.now();
            try {
                // Execute a simple echo command
                await window.bridge?.invoke('ssh:exec', { sessionId, command: 'echo 1' });
                const end = performance.now();
                return Math.round(end - start);
            } catch (e) {
                return null;
            }
        };

        const toggleReleaseConnection = async () => {
            const btnLabel = releaseConnectBtn.querySelector('.ui-btn__label');
            // Check if currently connected
            const currentSessionId = localStorage.getItem('ahmadIDE:releaseSessionId');

            if (currentSessionId) {
                // DISCONNECT LOGIC
                if (btnLabel) btnLabel.textContent = 'Disconnecting...';
                releaseConnectBtn.disabled = true;

                try {
                    await window.bridge?.invoke('ssh:disconnect', { sessionId: currentSessionId });
                } catch (e) {
                    console.error('Failed to disconnect release:', e);
                }

                localStorage.removeItem('ahmadIDE:releaseSessionId');
                releaseStatus.innerHTML = 'Disconnected'; // Use innerHTML to reset properly
                notify('info', 'Release Connection', 'Disconnected');

                if (btnLabel) btnLabel.textContent = 'Connect Release';
                releaseConnectBtn.classList.remove('ui-btn--danger');
                releaseConnectBtn.classList.add('ui-btn--primary');
                releaseConnectBtn.disabled = false;

            } else {
                // CONNECT LOGIC
                const config = {
                    host: releaseHostInput.value.trim(),
                    port: parseInt(releasePortInput.value, 10) || 22,
                    username: releaseUserInput.value.trim(),
                    password: releasePassInput.value
                };

                if (!config.host || !config.username) {
                    notify('info', 'Release Connection', 'Host and username are required');
                    return;
                }

                if (!config.password && !releaseConfig?.host) {
                    notify('info', 'Release Connection', 'Password required for first connection');
                    return;
                }

                if (!config.password && releaseConfig?.host) {
                    config.password = await releaseApi?.getPasswordSecurely?.();
                }

                if (btnLabel) btnLabel.textContent = 'Connecting...';
                releaseConnectBtn.disabled = true;
                releaseStatus.textContent = 'Connecting to release server...';

                try {
                    const response = await window.bridge?.invoke('ssh:connect', {
                        host: config.host,
                        port: config.port,
                        username: config.username,
                        password: config.password
                    });

                    if (!response || !response.ok || !response.sessionId) {
                        throw new Error(response?.error || 'Connection failed');
                    }

                    localStorage.setItem('ahmadIDE:releaseSessionId', response.sessionId);

                    // Check latency immediately
                    const latency = await checkLatency(response.sessionId);
                    const latencyText = latency !== null ? ` (${latency}ms)` : '';

                    releaseStatus.innerHTML = `✓ Connected to release server <span style="color:var(--text-secondary);font-size:0.9em;margin-left:6px;">${latencyText}</span>`;
                    notify('success', 'Release Connection', 'Connected successfully' + latencyText);

                    if (btnLabel) btnLabel.textContent = 'Disconnect Release';
                    releaseConnectBtn.classList.remove('ui-btn--primary');
                    releaseConnectBtn.classList.add('ui-btn--danger');

                } catch (err) {
                    releaseStatus.textContent = '✗ ' + String(err?.message || err || 'Connection failed');
                    notify('error', 'Release Connection', String(err?.message || err || 'Connection failed'));
                    if (btnLabel) btnLabel.textContent = 'Connect Release';
                } finally {
                    releaseConnectBtn.disabled = false;
                }
            }
        };

        // Initialize button state based on existing session
        const initSessionId = localStorage.getItem('ahmadIDE:releaseSessionId');
        const initLabel = initSessionId ? 'Disconnect Release' : 'Connect Release';
        const initVariant = initSessionId ? 'danger' : 'primary';

        releaseConnectBtn = createButton({ label: initLabel, variant: initVariant, onClick: toggleReleaseConnection });

        // If already connected, maybe check latency again or just show connected
        if (initSessionId) {
            releaseStatus.textContent = '✓ Connected (session active)';
        }

        releaseActionsRow.appendChild(releaseTestBtn);
        releaseActionsRow.appendChild(releaseSaveBtn);
        if (releaseConfig?.host) {
            releaseActionsRow.appendChild(releaseDeleteBtn);
        }
        releaseActionsRow.appendChild(releaseConnectBtn);
        // releaseDisconnectBtn removed (consolidated)

        const releaseActionsWrap = document.createElement('div');

        releaseActionsWrap.style.display = 'flex';
        releaseActionsWrap.style.flexDirection = 'column';
        releaseActionsWrap.style.gap = '6px';
        releaseActionsWrap.appendChild(releaseActionsRow);
        releaseActionsWrap.appendChild(releaseStatus);

        releaseGroup.appendChild(makeRow('Actions', releaseActionsWrap, 'release test save delete'));
        root.appendChild(releaseGroup);

        const updateConnectionUi = () => {
            const state = parseConnectionState(readStatusText()); statusPill.textContent = state.raw || 'Ready';
            const isSsh = state.connected && state.type === 'ssh'; const isDocker = state.connected && state.type === 'docker';
            connectSshBtn.style.display = isSsh ? 'none' : ''; disconnectSshBtn.style.display = isSsh ? '' : 'none';
            dockerConnectBtn.style.display = isDocker ? 'none' : ''; dockerDisconnectBtn.style.display = isDocker ? '' : 'none';
            sshStatusText.textContent = isSsh ? `Connected: ${state.label || state.raw}` : 'Not connected';
            dockerConnectStatus.textContent = isDocker ? `Connected: ${state.label || state.raw}` : 'Not connected';
        };
        updateConnectionUi();
        if (!dockerContainers.length) refreshDockerList();
        return root;
    }
    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.features = window.AhmadIDEModules.features || {};
        window.AhmadIDEModules.features.settings = window.AhmadIDEModules.features.settings || {};
        window.AhmadIDEModules.features.settings.sections = window.AhmadIDEModules.features.settings.sections || {};
        window.AhmadIDEModules.features.settings.sections.renderConnectionsSection = renderConnectionsSection;
    }
})();
