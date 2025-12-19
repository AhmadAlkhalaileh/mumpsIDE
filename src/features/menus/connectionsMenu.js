(() => {
    /**
     * Connections Context Menu
     * Shows when clicking the "Connections" widget in status bar
     * Uses unified menu system
     */
    function createConnectionsMenu() {
        const menuController = window.AhmadIDEModules?.ui?.menu?.controller
            || window.AhmadIDEModules?.ui?.menu?.createMenuController?.({});
        const dialogRegistry = window.AhmadIDEModules?.app?.dialogRegistry;
        if (!menuController) {
            console.error('menuController not available');
            return null;
        }
        let lastOpen = null;
        let dockerAutoRefreshed = false;
        const notify = (type, title, message) => {
            try {
                if (typeof showToast === 'function') {
                    showToast(type, title, message);
                    return;
                }
            } catch (_) { }
            try { alert(`${title}\n\n${message}`); } catch (_) { }
        };
        const readJson = (key, fallback) => {
            try {
                const raw = localStorage.getItem(key);
                return raw ? JSON.parse(raw) : fallback;
            } catch (_) {
                return fallback;
            }
        };
        const writeJson = (key, value) => {
            try {
                localStorage.setItem(key, JSON.stringify(value));
            } catch (_) { }
        };
        const refreshRoutines = async () => {
            try {
                await window.AhmadIDEModules?.app?.refreshRoutines?.();
            } catch (_) { }
        };
        const setStatus = (text, severity = 'info') => {
            try {
                if (typeof window.setConnStatus === 'function') {
                    window.setConnStatus(text, severity);
                    return;
                }
            } catch (_) { }
            const pill = document.getElementById('connStatus');
            if (!pill) return;
            pill.textContent = text;
            if (severity === 'subtle') {
                pill.style.background = '';
                pill.style.color = '';
                return;
            }
            pill.style.background = severity === 'error'
                ? 'rgba(248,113,113,0.18)'
                : 'rgba(14,165,233,0.12)';
            pill.style.color = severity === 'error'
                ? '#fecdd3'
                : '#38bdf8';
        };
        const openConnectionsPanel = () => {
            const btn = document.getElementById('toggleConnections');
            if (btn) {
                btn.dispatchEvent(new MouseEvent('click', { bubbles: true, cancelable: true, shiftKey: true }));
                return true;
            }
            notify('info', 'Connections', 'Connections panel is not available.');
            return false;
        };
        const openConnectionsSettings = () => {
            try {
                if (dialogRegistry?.open?.('settings', { sectionId: 'connections' })) return true;
                if (dialogRegistry?.show?.('settings', { sectionId: 'connections' })) return true;
            } catch (_) { }
            return false;
        };
        const inferConnectionStatusFromStatusBar = () => {
            const pill = document.getElementById('connStatus');
            const raw = String(pill?.textContent || '').trim();
            if (!raw || raw.toLowerCase() === 'ready') {
                return { connected: false, type: null, profile: null, label: 'Ready' };
            }
            let type = null;
            let name = raw;
            const m = raw.match(/^\s*(ssh|docker)\s*[:\-]\s*(.+)$/i);
            if (m) {
                type = m[1].toLowerCase();
                name = String(m[2] || '').trim() || raw;
            } else {
                if (/\bssh\b/i.test(raw)) type = 'ssh';
                if (/\bdocker\b/i.test(raw)) type = 'docker';
            }
            return { connected: true, type, profile: { name }, label: raw };
        };
        const loadSshProfiles = () => {
            const list = readJson('ahmadIDE:sshList', []);
            const out = Array.isArray(list) ? list.filter(Boolean) : [];
            const last = readJson('ahmadIDE:ssh', null);
            if (last && last.envKey) {
                const exists = out.some((p) => String(p?.envKey || '').toLowerCase() === String(last.envKey || '').toLowerCase());
                if (!exists) out.push(last);
            }
            return out.map((p) => ({ ...p, type: 'ssh' }));
        };
        const loadDockerCache = () => {
            const list = readJson('ahmadIDE:dockerContainers', []);
            return Array.isArray(list) ? list.filter(Boolean) : [];
        };
        const saveDockerCache = (list) => writeJson('ahmadIDE:dockerContainers', list || []);
        const loadDockerConfig = () => readJson('ahmadIDE:dockerConfig', {});
        const promptPassword = async (profile) => {
            const prompt = window.AhmadIDEModules?.ui?.showPrompt;
            const name = `${profile?.username || ''}@${profile?.host || ''}`.trim() || 'SSH host';
            if (typeof prompt === 'function') {
                return await prompt({
                    title: 'SSH Password',
                    message: `Enter password for ${name}`,
                    placeholder: 'Password'
                });
            }
            try { return window.prompt(`SSH password for ${name}`, '') || ''; } catch (_) { return ''; }
        };
        const connectSsh = async (profile) => {
            const host = String(profile?.host || '').trim();
            const port = Number(profile?.port || 22) || 22;
            const username = String(profile?.user || profile?.username || '').trim();
            const envKey = String(profile?.envKey || 'cc').trim() || 'cc';
            if (!host || !username) {
                notify('info', 'SSH', 'Host and user are required.');
                return;
            }
            let password = String(profile?.password || '').trim();
            if (!password) password = await promptPassword(profile);
            if (!password) return;
            if (!window.ahmadIDE?.sshConnect) {
                notify('error', 'SSH', 'SSH backend is not available.');
                return;
            }
            try {
                const res = await window.ahmadIDE.sshConnect({ host, port, username, password, envKey });
                if (!res?.ok) throw new Error(res?.error || 'SSH connect failed');
                if (res.sessionId) localStorage.setItem('ahmadIDE:sshSessionId', String(res.sessionId));
                localStorage.setItem('ahmadIDE:ssh', JSON.stringify({ envKey, host, port, username }));
                await window.ahmadIDE.setConnection('ssh', { ssh: { host, port, username, password, envKey } });
                setStatus(`SSH: ${profile.name || host}`, 'success');
                await refreshRoutines();
            } catch (e) {
                notify('error', 'SSH', String(e?.message || e || 'SSH connect failed'));
            }
        };
        const connectDocker = async (container) => {
            const containerId = String(container?.id || container?.containerId || '').trim();
            if (!containerId) {
                notify('info', 'Docker', 'Select a container first.');
                return;
            }
            try {
                const config = loadDockerConfig();
                await window.ahmadIDE.setConnection('docker', { docker: { containerId, ...config } });
                localStorage.setItem('ahmadIDE:lastContainerId', containerId);
                const label = container?.name || containerId;
                setStatus(`Docker: ${label}`, 'success');
                await refreshRoutines();
            } catch (e) {
                notify('error', 'Docker', String(e?.message || e || 'Docker connect failed'));
            }
        };
        const connectDockerUniversal = async () => {
            try {
                await window.ahmadIDE.setConnection('docker', { docker: { containerId: null } });
                setStatus('Docker (local)', 'info');
                await refreshRoutines();
            } catch (e) {
                notify('error', 'Docker', String(e?.message || e || 'Docker connect failed'));
            }
        };
        const refreshDockerList = async () => {
            if (!window.ahmadIDE?.listDocker) {
                notify('error', 'Docker', 'Docker backend is not available.');
                return [];
            }
            try {
                await window.ahmadIDE.setConnection('docker');
                const res = await window.ahmadIDE.listDocker();
                if (!res?.ok) throw new Error(res?.error || 'Docker list failed');
                const list = res.containers || [];
                saveDockerCache(list);
                return list;
            } catch (e) {
                notify('error', 'Docker', String(e?.message || e || 'Docker list failed'));
                return [];
            }
        };
        const disconnectActive = async (status) => {
            const type = String(status?.type || '').toLowerCase();
            if (type === 'ssh') {
                const sessionId = (() => {
                    try { return String(localStorage.getItem('ahmadIDE:sshSessionId') || '').trim(); } catch (_) { return ''; }
                })();
                if (sessionId && window.ahmadIDE?.sshDisconnect) {
                    try { await window.ahmadIDE.sshDisconnect(sessionId); } catch (_) { }
                }
                try { localStorage.removeItem('ahmadIDE:sshSessionId'); } catch (_) { }
            }
            try { await window.ahmadIDE?.setConnection?.('docker', { docker: { containerId: null } }); } catch (_) { }
            setStatus('Ready', 'subtle');
            await refreshRoutines();
        };
        const reopenMenu = () => {
            if (!lastOpen) return;
            if (lastOpen.type === 'point') showAtPoint(lastOpen.x, lastOpen.y);
            if (lastOpen.type === 'anchor') showForAnchor(lastOpen.anchorEl);
        };
        const buildMenu = () => {
            const status = inferConnectionStatusFromStatusBar();
            const sshProfiles = loadSshProfiles();
            const dockerContainers = loadDockerCache();
            if (!dockerContainers.length && !dockerAutoRefreshed) {
                dockerAutoRefreshed = true;
                refreshDockerList().then(() => setTimeout(reopenMenu, 0));
            }
            const items = [];
            if (status.connected) {
                items.push({
                    id: 'conn-active-header',
                    label: `Connected: ${status.label}`,
                    icon: 'link',
                    disabled: true
                });
                items.push({ type: 'separator' });
            }
            items.push({
                id: 'conn-open-panel',
                label: 'Connection Settings...',
                icon: 'settings',
                onSelect: () => {
                    if (!openConnectionsSettings()) {
                        notify('info', 'Settings', 'Settings dialog is not available.');
                    }
                }
            });
            if (status.connected) {
                items.push({
                    id: 'conn-disconnect',
                    label: 'Disconnect',
                    icon: 'stop',
                    onSelect: () => disconnectActive(status)
                });
            }
            items.push({ type: 'separator' });
            if (sshProfiles.length) {
                items.push({
                    id: 'conn-ssh-profiles',
                    label: 'SSH Profiles',
                    icon: 'link',
                    submenu: sshProfiles.map((profile, idx) => ({
                        id: `conn-ssh-${idx}`,
                        label: `${String(profile.envKey || 'SSH').toUpperCase()}: ${profile.username || ''}@${profile.host || ''}`.replace('@@', '@'),
                        type: 'checkbox',
                        checked: status.connected && status.type === 'ssh' && String(status.label || '').toLowerCase().includes(String(profile.host || '').toLowerCase()),
                        onSelect: () => connectSsh(profile)
                    }))
                });
            } else {
                items.push({ id: 'conn-ssh-none', label: 'SSH Profiles', icon: 'link', disabled: true });
            }
            items.push({
                id: 'conn-docker',
                label: 'Docker Containers',
                icon: 'terminal',
                submenu: [
                    {
                        id: 'conn-docker-refresh',
                        label: 'Refresh Docker List',
                        icon: 'refresh',
                        onSelect: async () => {
                            await refreshDockerList();
                            setTimeout(reopenMenu, 0);
                        }
                    },
                    { type: 'separator' },
                    ...(dockerContainers.length
                        ? dockerContainers.map((c, idx) => ({
                            id: `conn-docker-${idx}`,
                            label: c.name || c.id || 'Container',
                            type: 'checkbox',
                            checked: status.connected && status.type === 'docker' && String(status.label || '').toLowerCase().includes(String(c.name || '').toLowerCase()),
                            onSelect: () => connectDocker(c)
                        }))
                        : [{ id: 'conn-docker-empty', label: 'No containers found', disabled: true }]),
                    { type: 'separator' },
                    { id: 'conn-docker-local', label: 'Use Local Docker', icon: 'terminal', onSelect: () => connectDockerUniversal() }
                ]
            });
            return items;
        };
        const showAtPoint = (x, y) => {
            lastOpen = { type: 'point', x, y };
            const items = buildMenu();
            menuController.openAtPoint({ x, y, items, ctx: {}, onAction: async () => { } });
        };
        const showForAnchor = (anchorEl) => {
            lastOpen = { type: 'anchor', anchorEl };
            const items = buildMenu();
            menuController.openAtElement({ anchorEl, items, ctx: {}, onAction: async () => { } });
        };
        return { show: showAtPoint, showAtPoint, showForAnchor, buildMenu };
    }
    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.features = window.AhmadIDEModules.features || {};
        window.AhmadIDEModules.features.menus = window.AhmadIDEModules.features.menus || {};
        window.AhmadIDEModules.features.menus.createConnectionsMenu = createConnectionsMenu;
    }
})();
