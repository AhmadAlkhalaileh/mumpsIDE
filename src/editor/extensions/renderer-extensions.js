(() => {
    function createExtensionsManager({ deps } = {}) {
        const updateDebugButtonState = deps?.updateDebugButtonState || (() => { });

        const extensionsState = {
            installed: [],
            selectedId: null,
            enabled: {}
        };

        function loadExtensionState() {
            try {
                const raw = localStorage.getItem('ahmadIDE:extensions');
                if (raw) {
                    const parsed = JSON.parse(raw);
                    extensionsState.enabled = parsed.enabled || {};
                    extensionsState.selectedId = parsed.selectedId || null;
                }
            } catch (e) {
                // ignore storage errors
            }
            updateDebugButtonState();
        }

        function persistExtensionState() {
            try {
                localStorage.setItem('ahmadIDE:extensions', JSON.stringify({
                    enabled: extensionsState.enabled,
                    selectedId: extensionsState.selectedId
                }));
            } catch (e) {
                // ignore storage failures
            }
        }

        function initExtensionsData() {
            extensionsState.installed = [
                {
                    id: 'mumps-lint',
                    name: 'MUMPS Lint',
                    description: 'Runs MUMPS lint checks on save.',
                    icon: '🧹'
                },
                {
                    id: 'code-formatter',
                    name: 'Code Formatter',
                    description: 'Formats MUMPS routines using built-in formatter.',
                    icon: '✨'
                },
                {
                    id: 'ssh-tools',
                    name: 'SSH Tools',
                    description: 'Adds SSH utilities and quick commands.',
                    icon: '🔑'
                }
            ];
            extensionsState.installed.forEach(ext => {
                if (extensionsState.enabled[ext.id] === undefined) {
                    extensionsState.enabled[ext.id] = true;
                }
            });
            if (!extensionsState.selectedId && extensionsState.installed.length) {
                extensionsState.selectedId = extensionsState.installed[0].id;
            }
        }

        function renderExtensionsDetail(ext) {
            const host = document.getElementById('extensionDetail');
            if (!host) return;
            if (!ext) {
                host.innerHTML = '<div class="pane-title">Select an extension</div><div class="pane-subtitle">Details will appear here.</div>';
                return;
            }
            const enabled = !!extensionsState.enabled[ext.id];
            host.innerHTML = `
            <div class="detail-title">${ext.name}</div>
            <div class="detail-desc">${ext.description || 'No description available.'}</div>
            <div class="detail-status">Status: <strong>${enabled ? 'Enabled' : 'Disabled'}</strong></div>
            <button class="btn ${enabled ? 'ghost' : 'primary'}" id="extToggleBtn">${enabled ? 'Disable' : 'Enable'}</button>
        `;
            document.getElementById('extToggleBtn')?.addEventListener('click', () => {
                extensionsState.enabled[ext.id] = !enabled;
                persistExtensionState();
                renderExtensionsList();
            });
        }

        function renderExtensionsList() {
            const host = document.getElementById('extensionsList');
            if (!host) return;
            host.innerHTML = '';
            if (!extensionsState.installed.length) {
                host.textContent = 'No extensions installed.';
                return;
            }
            extensionsState.installed.forEach(ext => {
                const row = document.createElement('div');
                row.className = 'extension-row';
                if (extensionsState.selectedId === ext.id) row.classList.add('active');
                const icon = document.createElement('div');
                icon.className = 'extension-icon';
                icon.textContent = ext.icon || '⋯';
                const meta = document.createElement('div');
                meta.className = 'extension-meta';
                const name = document.createElement('div');
                name.className = 'extension-name';
                name.textContent = ext.name;
                const desc = document.createElement('div');
                desc.className = 'extension-desc';
                desc.textContent = ext.description || '';
                meta.appendChild(name);
                meta.appendChild(desc);
                const toggle = document.createElement('input');
                toggle.type = 'checkbox';
                toggle.className = 'extension-toggle';
                toggle.checked = !!extensionsState.enabled[ext.id];
                toggle.addEventListener('click', (e) => {
                    e.stopPropagation();
                    extensionsState.enabled[ext.id] = !extensionsState.enabled[ext.id];
                    persistExtensionState();
                    renderExtensionsDetail(ext);
                });
                row.appendChild(icon);
                row.appendChild(meta);
                row.appendChild(toggle);
                row.addEventListener('click', () => {
                    extensionsState.selectedId = ext.id;
                    persistExtensionState();
                    renderExtensionsList();
                    renderExtensionsDetail(ext);
                });
                host.appendChild(row);
            });
            const selected = extensionsState.installed.find(e => e.id === extensionsState.selectedId);
            renderExtensionsDetail(selected);
        }

        function initExtensionsView() {
            loadExtensionState();
            initExtensionsData();
            renderExtensionsList();
        }

        return {
            initExtensionsView
        };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.extensions = window.AhmadIDEModules.extensions || {};
        window.AhmadIDEModules.extensions.createExtensionsManager = createExtensionsManager;
    }
})();
