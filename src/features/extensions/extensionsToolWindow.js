(() => {
    function createExtensionsToolWindow({ deps } = {}) {
        const extensionsService = deps?.extensionsService || window.AhmadIDEModules?.services?.extensionsService;
        const primitives = deps?.primitives || window.AhmadIDEModules?.ui?.primitives;
        if (!extensionsService || !primitives) throw new Error('ExtensionsToolWindow requires services + primitives');

        const { createToggle, createButton } = primitives;

        const state = {
            selectedId: null,
            unsub: null
        };

        const getHosts = () => ({
            list: document.getElementById('extensionsList'),
            detail: document.getElementById('extensionDetail')
        });

        const renderDetail = (ext) => {
            const { detail } = getHosts();
            if (!detail) return;
            if (!ext) {
                detail.innerHTML = '<div class="pane-title">Select an extension</div><div class="pane-subtitle">Details will appear here.</div>';
                return;
            }
            detail.innerHTML = '';
            const title = document.createElement('div');
            title.className = 'detail-title';
            title.textContent = `${ext.name}  ${ext.version ? `(${ext.version})` : ''}`.trim();
            const desc = document.createElement('div');
            desc.className = 'detail-desc';
            desc.textContent = ext.description || 'No description.';
            const status = document.createElement('div');
            status.className = 'detail-status';
            status.innerHTML = `Status: <strong>${ext.enabled ? 'Enabled' : 'Disabled'}</strong>`;

            const actions = document.createElement('div');
            actions.style.display = 'flex';
            actions.style.gap = '8px';
            actions.style.marginTop = '12px';
            const btn = createButton({
                label: ext.enabled ? 'Disable' : 'Enable',
                variant: ext.enabled ? 'ghost' : 'primary',
                onClick: async () => {
                    await extensionsService.setEnabled(ext.id, !ext.enabled);
                }
            });
            actions.appendChild(btn);

            detail.appendChild(title);
            detail.appendChild(desc);
            detail.appendChild(status);
            detail.appendChild(actions);
        };

        const renderList = () => {
            const { list } = getHosts();
            if (!list) return;

            const items = extensionsService.list();
            if (!state.selectedId && items.length) state.selectedId = items[0].id;
            if (state.selectedId && !items.find((x) => x.id === state.selectedId)) state.selectedId = items[0]?.id || null;

            list.innerHTML = '';
            if (!items.length) {
                list.textContent = 'No extensions installed.';
                renderDetail(null);
                return;
            }

            items.forEach((ext) => {
                const row = document.createElement('div');
                row.className = 'extension-row' + (ext.id === state.selectedId ? ' active' : '');

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

                const toggle = createToggle({
                    label: '',
                    checked: !!ext.enabled,
                    onChange: async (_e, checked) => {
                        await extensionsService.setEnabled(ext.id, checked);
                    }
                });
                toggle.root.classList.add('extension-toggle');

                row.appendChild(meta);
                row.appendChild(toggle.root);

                row.addEventListener('click', () => {
                    state.selectedId = ext.id;
                    renderList();
                });

                list.appendChild(row);
            });

            renderDetail(items.find((x) => x.id === state.selectedId));
        };

        const mount = () => {
            const { list, detail } = getHosts();
            if (!list || !detail) return false;
            renderList();
            if (!state.unsub) state.unsub = extensionsService.onChange(() => renderList());
            return true;
        };

        return { mount };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.features = window.AhmadIDEModules.features || {};
        window.AhmadIDEModules.features.extensions = window.AhmadIDEModules.features.extensions || {};
        window.AhmadIDEModules.features.extensions.createExtensionsToolWindow = createExtensionsToolWindow;
    }
})();

