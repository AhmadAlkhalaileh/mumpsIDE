(() => {
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
        } catch (_) {
            // ignore
        }
    };

    const createSvg = (markup) => {
        const tpl = document.createElement('template');
        tpl.innerHTML = String(markup || '').trim();
        return tpl.content.firstElementChild || document.createElement('span');
    };

    const ICONS = {
        chevronRight: '<svg viewBox="0 0 16 16" fill="none"><path d="M6 3.5 10 8 6 12.5" stroke="currentColor" stroke-width="1.6" stroke-linecap="round" stroke-linejoin="round"/></svg>',
        folder: '<svg viewBox="0 0 16 16" fill="none"><path d="M3 5h4l1 1h5c.6 0 1 .4 1 1v4.8c0 .7-.6 1.2-1.2 1.2H3.9c-.7 0-1.3-.6-1.3-1.3V6.2C2.6 5.5 3.2 5 4 5z" stroke="currentColor" stroke-width="1.4" stroke-linejoin="round"/></svg>',
        link: '<svg viewBox="0 0 16 16" fill="none"><path d="M6.3 9.7 4.7 11.3a2.2 2.2 0 0 1-3.1 0 2.2 2.2 0 0 1 0-3.1l1.7-1.7" stroke="currentColor" stroke-width="1.4" stroke-linecap="round"/><path d="M9.7 6.3 11.3 4.7a2.2 2.2 0 0 1 3.1 0 2.2 2.2 0 0 1 0 3.1l-1.7 1.7" stroke="currentColor" stroke-width="1.4" stroke-linecap="round"/><path d="M6.1 8.0h3.8" stroke="currentColor" stroke-width="1.4" stroke-linecap="round"/></svg>',
        table: '<svg viewBox="0 0 16 16" fill="none"><rect x="2.6" y="3.2" width="10.8" height="9.6" rx="1.4" stroke="currentColor" stroke-width="1.4"/><path d="M2.6 6.4h10.8M6.2 3.2v9.6M10 3.2v9.6" stroke="currentColor" stroke-width="1.2" opacity="0.9"/></svg>',
        docker: '<svg viewBox="0 0 16 16" fill="none"><path d="M3 9h10c-.3 2.7-2.2 4.2-5 4.2S3.3 11.7 3 9z" stroke="currentColor" stroke-width="1.4" stroke-linejoin="round"/><path d="M5 6h2v3H5V6zm2 0h2v3H7V6zm2 0h2v3H9V6z" fill="currentColor" opacity="0.85"/></svg>'
    };

    const setConnPill = (text, severity = 'info') => {
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

    const getActiveSshSessionId = () => {
        try {
            return String(localStorage.getItem('ahmadIDE:sshSessionId') || '').trim();
        } catch (_) {
            return '';
        }
    };

    const getSavedSshProfiles = () => {
        const list = readJson('ahmadIDE:sshList', []);
        const out = Array.isArray(list) ? list.filter(Boolean) : [];
        const last = readJson('ahmadIDE:ssh', null);
        if (last && last.envKey) {
            const exists = out.some(p =>
                String(p?.envKey || '').toLowerCase() === String(last.envKey || '').toLowerCase()
            );
            if (!exists) out.push(last);
        }
        return out
            .map((p) => ({
                envKey: String(p?.envKey || '').trim(),
                host: String(p?.host || '').trim(),
                port: Number(p?.port || 22) || 22,
                username: String(p?.username || p?.user || '').trim()
            }))
            .filter(p => p.envKey && p.host && p.username);
    };

    const openConnectionsPanel = () => {
        const btn = document.getElementById('toggleConnections');
        if (!btn) return false;
        btn.dispatchEvent(new MouseEvent('click', { bubbles: true, cancelable: true, shiftKey: true }));
        return true;
    };

    function wireServicesPanel() {
        const treeEl = document.getElementById('servicesTree');
        const emptyEl = document.getElementById('servicesEmpty');
        const detailEl = document.getElementById('servicesDetailContent');

        const addBtn = document.getElementById('servicesAddBtn');
        const newBtn = document.getElementById('servicesNewBtn');
        const viewBtn = document.getElementById('servicesViewBtn');
        const expandBtn = document.getElementById('servicesExpandAllBtn');
        const collapseBtn = document.getElementById('servicesCollapseAllBtn');

        if (!treeEl || !emptyEl || !detailEl) return;

        const state = {
            expanded: { docker: true, ssh: true },
            selectedId: null,
            docker: { loading: false, error: '', containers: [] },
            ssh: { profiles: [] }
        };

        const select = (id) => {
            state.selectedId = id;
            renderTree();
            renderDetail();
        };

        const makeRow = ({ id, indent = 0, selected = false, hasTwisty = false, twistyOpen = false, icon, iconColor, label, meta, statusDot }) => {
            const row = document.createElement('div');
            row.className = `services-tree-row${selected ? ' selected' : ''}${indent === 1 ? ' indent-1' : indent === 2 ? ' indent-2' : ''}`;
            row.setAttribute('role', 'treeitem');
            row.dataset.nodeId = id;

            const twisty = document.createElement('span');
            twisty.className = `twisty${twistyOpen ? ' open' : ''}`;
            if (hasTwisty) {
                twisty.appendChild(createSvg(ICONS.chevronRight));
                twisty.title = twistyOpen ? 'Collapse' : 'Expand';
                twisty.addEventListener('click', (e) => {
                    e.stopPropagation();
                    if (id === 'group:docker') state.expanded.docker = !state.expanded.docker;
                    if (id === 'group:ssh') state.expanded.ssh = !state.expanded.ssh;
                    renderTree();
                });
            }
            row.appendChild(twisty);

            const iconWrap = document.createElement('span');
            iconWrap.className = 'icon';
            if (iconColor) iconWrap.style.color = iconColor;
            if (icon) iconWrap.appendChild(createSvg(icon));
            row.appendChild(iconWrap);

            const labelEl = document.createElement('span');
            labelEl.className = 'label';
            labelEl.textContent = label || '';
            row.appendChild(labelEl);

            if (meta) {
                const metaEl = document.createElement('span');
                metaEl.className = 'meta';
                metaEl.textContent = meta;
                row.appendChild(metaEl);
            }

            if (statusDot) {
                const dot = document.createElement('span');
                dot.className = `status-dot${statusDot === 'ok' ? ' ok' : ''}`;
                row.appendChild(dot);
            }

            row.addEventListener('click', () => select(id));
            return row;
        };

        const renderTree = () => {
            treeEl.innerHTML = '';

            const dockerMeta = state.docker.loading
                ? 'Loading…'
                : state.docker.error
                    ? 'Error'
                    : `${state.docker.containers.length}`;

            treeEl.appendChild(makeRow({
                id: 'group:docker',
                selected: state.selectedId === 'group:docker',
                hasTwisty: true,
                twistyOpen: state.expanded.docker,
                icon: ICONS.folder,
                label: 'Docker',
                meta: dockerMeta
            }));

            if (state.expanded.docker) {
                if (state.docker.error) {
                    treeEl.appendChild(makeRow({
                        id: 'docker:error',
                        indent: 1,
                        selected: state.selectedId === 'docker:error',
                        icon: ICONS.docker,
                        label: state.docker.error,
                        meta: '',
                        statusDot: null
                    }));
                } else if (!state.docker.containers.length && !state.docker.loading) {
                    treeEl.appendChild(makeRow({
                        id: 'docker:empty',
                        indent: 1,
                        selected: state.selectedId === 'docker:empty',
                        icon: ICONS.docker,
                        label: 'No running containers',
                        meta: '',
                        statusDot: null
                    }));
                } else {
                    state.docker.containers.forEach((c) => {
                        const isRunning = /\bup\b/i.test(String(c.status || ''));
                        treeEl.appendChild(makeRow({
                            id: `docker:${c.id}`,
                            indent: 1,
                            selected: state.selectedId === `docker:${c.id}`,
                            icon: ICONS.docker,
                            label: c.name || c.id,
                            meta: '',
                            statusDot: isRunning ? 'ok' : null
                        }));
                    });
                }
            }

            const sessionId = getActiveSshSessionId();
            const sshMeta = sessionId ? 'Connected' : `${state.ssh.profiles.length}`;
            treeEl.appendChild(makeRow({
                id: 'group:ssh',
                selected: state.selectedId === 'group:ssh',
                hasTwisty: true,
                twistyOpen: state.expanded.ssh,
                icon: ICONS.folder,
                label: 'SSH',
                meta: sshMeta
            }));

            if (state.expanded.ssh) {
                if (sessionId) {
                    const last = readJson('ahmadIDE:ssh', null);
                    const label = last?.host ? `@${String(last.host)}` : 'Active session';
                    treeEl.appendChild(makeRow({
                        id: 'ssh:active',
                        indent: 1,
                        selected: state.selectedId === 'ssh:active',
                        icon: ICONS.link,
                        iconColor: '#27a5f4',
                        label,
                        meta: '',
                        statusDot: 'ok'
                    }));
                }

                if (!state.ssh.profiles.length) {
                    treeEl.appendChild(makeRow({
                        id: 'ssh:empty',
                        indent: 1,
                        selected: state.selectedId === 'ssh:empty',
                        icon: ICONS.link,
                        iconColor: '#27a5f4',
                        label: 'No saved SSH profiles',
                        meta: '',
                        statusDot: null
                    }));
                } else {
                    state.ssh.profiles.forEach((p) => {
                        treeEl.appendChild(makeRow({
                            id: `ssh:${p.envKey}`,
                            indent: 1,
                            selected: state.selectedId === `ssh:${p.envKey}`,
                            icon: ICONS.link,
                            iconColor: '#27a5f4',
                            label: `${p.envKey.toUpperCase()} @${p.host}`,
                            meta: '',
                            statusDot: null
                        }));
                    });
                }
            }
        };

        const renderDetail = () => {
            const selected = String(state.selectedId || '');
            emptyEl.classList.toggle('hidden', !!selected);
            detailEl.classList.toggle('hidden', !selected);
            if (!selected) {
                detailEl.textContent = '';
                return;
            }

            const clear = () => { detailEl.innerHTML = ''; };
            const h = (text) => {
                const el = document.createElement('div');
                el.className = 'services-detail-title';
                el.textContent = text;
                return el;
            };
            const kv = (pairs) => {
                const grid = document.createElement('div');
                grid.className = 'services-detail-kv';
                (pairs || []).forEach(([k, v]) => {
                    const key = document.createElement('div');
                    key.className = 'k';
                    key.textContent = k;
                    const val = document.createElement('div');
                    val.textContent = v;
                    grid.appendChild(key);
                    grid.appendChild(val);
                });
                return grid;
            };

            clear();

            if (selected === 'group:docker') {
                detailEl.appendChild(h('Docker'));
                detailEl.appendChild(kv([
                    ['Containers', String(state.docker.containers.length)],
                    ['Status', state.docker.loading ? 'Loading…' : (state.docker.error ? 'Error' : 'Ready')]
                ]));
                const hint = document.createElement('div');
                hint.style.cssText = 'color:rgba(233,234,236,0.55);font-size:13px;';
                hint.textContent = 'Select a container to view details.';
                detailEl.appendChild(hint);
                return;
            }

            if (selected.startsWith('docker:')) {
                const id = selected.slice('docker:'.length);
                const c = state.docker.containers.find(x => String(x.id) === id) || null;
                detailEl.appendChild(h(`Docker: ${c?.name || id}`));
                detailEl.appendChild(kv([
                    ['ID', c?.id || id],
                    ['Status', c?.status || '—']
                ]));
                const actions = document.createElement('div');
                actions.className = 'services-detail-actions';
                const useBtn = document.createElement('button');
                useBtn.className = 'btn ghost';
                useBtn.type = 'button';
                useBtn.textContent = 'Use Container';
                useBtn.addEventListener('click', async () => {
                    if (!c?.id || !window.ahmadIDE?.setConnection) return;
                    try {
                        let dockerConfig = readJson('ahmadIDE:dockerConfig', {});
                        if (!dockerConfig || typeof dockerConfig !== 'object') dockerConfig = {};
                        await window.ahmadIDE.setConnection('docker', { docker: { containerId: c.id, ...dockerConfig } });
                        try { localStorage.setItem('ahmadIDE:lastContainerId', String(c.id)); } catch (_) { }
                        setConnPill(`Docker: ${c.name || c.id}`, 'info');
                    } catch (e) {
                        setConnPill('Docker error', 'error');
                    }
                });
                actions.appendChild(useBtn);
                detailEl.appendChild(actions);
                return;
            }

            if (selected === 'group:ssh' || selected.startsWith('ssh:')) {
                const sessionId = getActiveSshSessionId();
                const active = readJson('ahmadIDE:ssh', null);
                let profile = null;
                if (selected.startsWith('ssh:') && selected !== 'ssh:active' && selected !== 'ssh:empty') {
                    const key = selected.slice('ssh:'.length);
                    profile = state.ssh.profiles.find(p => String(p.envKey) === key) || null;
                }
                if (selected === 'ssh:active' && active) profile = {
                    envKey: String(active.envKey || '').trim(),
                    host: String(active.host || ''),
                    port: Number(active.port || 22) || 22,
                    username: String(active.username || '')
                };

                detailEl.appendChild(h('SSH'));
                detailEl.appendChild(kv([
                    ['Status', sessionId ? 'Connected' : 'Not connected'],
                    ['Session', sessionId ? sessionId : '—'],
                    ['Host', profile?.host || active?.host || '—'],
                    ['User', profile?.username || active?.username || '—'],
                    ['Port', String(profile?.port || active?.port || 22)],
                    ['Env', String(profile?.envKey || active?.envKey || '—')]
                ]));

                const actions = document.createElement('div');
                actions.className = 'services-detail-actions';

                const openBtn = document.createElement('button');
                openBtn.className = 'btn ghost';
                openBtn.type = 'button';
                openBtn.textContent = 'Open Connections…';
                openBtn.addEventListener('click', () => openConnectionsPanel());
                actions.appendChild(openBtn);

                if (profile && window.ahmadIDE?.sshConnect) {
                    const connectBtn = document.createElement('button');
                    connectBtn.className = 'btn ghost';
                    connectBtn.type = 'button';
                    connectBtn.textContent = 'Connect…';
                    connectBtn.addEventListener('click', async () => {
                        const prompt = window.AhmadIDEModules?.ui?.showPrompt;
                        const name = `${profile.username}@${profile.host}:${profile.port}`;
                        const password = prompt
                            ? await prompt({ title: 'SSH Password', message: `Enter password for ${name}`, placeholder: 'Password' })
                            : (window.prompt(`SSH password for ${name}`, '') || '');
                        if (!password) return;

                        try {
                            const res = await window.ahmadIDE.sshConnect({
                                host: profile.host,
                                port: profile.port,
                                username: profile.username,
                                password,
                                envKey: profile.envKey
                            });
                            if (!res?.ok) throw new Error(res?.error || 'SSH connect failed');
                            if (res.sessionId) {
                                try { localStorage.setItem('ahmadIDE:sshSessionId', String(res.sessionId)); } catch (_) { }
                            }
                            try { localStorage.setItem('ahmadIDE:ssh', JSON.stringify({ ...profile })); } catch (_) { }
                            await window.ahmadIDE.setConnection('ssh', { ssh: { ...profile, password } });
                            setConnPill(`SSH: ${profile.host}`, 'info');

                            // Ensure profile is persisted (if user connected via detail view first).
                            const existing = readJson('ahmadIDE:sshList', []);
                            const list = Array.isArray(existing) ? existing.filter(Boolean) : [];
                            const idx = list.findIndex(p => String(p?.envKey || '').toLowerCase() === String(profile.envKey).toLowerCase());
                            if (idx >= 0) list[idx] = { ...list[idx], ...profile };
                            else list.push(profile);
                            writeJson('ahmadIDE:sshList', list);

                            await refresh();
                            select('ssh:active');
                        } catch (e) {
                            setConnPill('SSH error', 'error');
                        }
                    });
                    actions.appendChild(connectBtn);
                }

                if (sessionId && window.ahmadIDE?.sshDisconnect) {
                    const disconnectBtn = document.createElement('button');
                    disconnectBtn.className = 'btn ghost';
                    disconnectBtn.type = 'button';
                    disconnectBtn.textContent = 'Disconnect';
                    disconnectBtn.addEventListener('click', async () => {
                        try {
                            await window.ahmadIDE.sshDisconnect(sessionId);
                        } catch (_) { }
                        try { localStorage.removeItem('ahmadIDE:sshSessionId'); } catch (_) { }
                        setConnPill('Ready', 'subtle');
                        await refresh();
                        select('group:ssh');
                    });
                    actions.appendChild(disconnectBtn);
                }

                detailEl.appendChild(actions);

                const cmdRow = document.createElement('div');
                cmdRow.className = 'services-command-row';
                const input = document.createElement('input');
                input.placeholder = 'Command (SSH)';
                input.autocomplete = 'off';
                const runBtn = document.createElement('button');
                runBtn.className = 'btn ghost';
                runBtn.type = 'button';
                runBtn.textContent = 'Run';
                cmdRow.appendChild(input);
                cmdRow.appendChild(runBtn);
                detailEl.appendChild(cmdRow);

                const out = document.createElement('pre');
                out.className = 'services-output';
                out.textContent = sessionId ? 'Ready.' : 'No SSH session. Connect first.';
                detailEl.appendChild(out);

                const run = async () => {
                    const cmd = String(input.value || '').trim();
                    const sid = getActiveSshSessionId();
                    if (!cmd) {
                        out.textContent = 'Enter a command.';
                        return;
                    }
                    if (!sid) {
                        out.textContent = 'No SSH session. Connect first.';
                        return;
                    }
                    out.textContent = `Running: ${cmd}`;
                    const res = await window.ahmadIDE.sshExec?.(sid, cmd);
                    if (res?.ok) out.textContent = res.stdout || '(no output)';
                    else out.textContent = res?.error || res?.stderr || 'SSH command failed';
                };

                runBtn.addEventListener('click', run);
                input.addEventListener('keydown', (e) => {
                    if (e.key === 'Enter') run();
                });
                return;
            }
        };

        const refresh = async () => {
            state.ssh.profiles = getSavedSshProfiles();

            state.docker.loading = true;
            state.docker.error = '';
            renderTree();
            try {
                const res = await window.ahmadIDE.listDocker?.();
                if (res?.ok) {
                    state.docker.containers = Array.isArray(res.containers) ? res.containers : [];
                    state.docker.error = '';
                } else {
                    state.docker.containers = [];
                    state.docker.error = res?.error || res?.stderr || 'Docker query failed';
                }
            } catch (e) {
                state.docker.containers = [];
                state.docker.error = 'Docker query failed';
            } finally {
                state.docker.loading = false;
            }

            renderTree();
            renderDetail();
        };

        addBtn?.addEventListener('click', () => openConnectionsPanel());
        newBtn?.addEventListener('click', () => openConnectionsPanel());
        viewBtn?.addEventListener('click', () => {
            // Placeholder for view options (matches screenshot toolbar layout)
            const target = state.selectedId || 'group:ssh';
            select(target);
        });
        expandBtn?.addEventListener('click', () => {
            state.expanded.docker = true;
            state.expanded.ssh = true;
            renderTree();
        });
        collapseBtn?.addEventListener('click', () => {
            state.expanded.docker = false;
            state.expanded.ssh = false;
            renderTree();
        });

        // Initial mount
        refresh().then(() => {
            // Default selection: SSH group if session exists, else Docker group
            const sessionId = getActiveSshSessionId();
            if (!state.selectedId) select(sessionId ? 'group:ssh' : 'group:docker');
        });
    }

    function bootstrap() {
        const featureRegistry = window.AhmadIDEModules?.app?.featureRegistry;
        if (!featureRegistry || typeof featureRegistry.onMounted !== 'function') return;
        featureRegistry.onMounted('servicesPanel', () => {
            wireServicesPanel();
        });
    }

    if (document.readyState === 'loading') {
        document.addEventListener('DOMContentLoaded', bootstrap, { once: true });
    } else {
        bootstrap();
    }
})();
