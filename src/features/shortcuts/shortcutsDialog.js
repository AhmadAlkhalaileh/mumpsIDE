(() => {
    /**
     * Shortcuts Dialog
     * Two-panel layout for viewing and editing keyboard shortcuts
     * Matches  2025.3 New UI Keymap settings
     *
     * Checklist: SHO-001 to SHO-009
     */
    function createShortcutsDialog({ deps } = {}) {
        const createDialog = deps?.createDialog || window.AhmadIDEModules?.ui?.createDialog;
        const createDialogLayout = deps?.createDialogLayout || window.AhmadIDEModules?.ui?.createDialogLayout;
        const primitives = deps?.primitives || window.AhmadIDEModules?.ui?.primitives;

        if (!createDialog || !createDialogLayout || !primitives) {
            throw new Error('ShortcutsDialog requires ui primitives');
        }

        let dialogApi = null;
        let layout = null;
        let shortcuts = {};
        let activeCategory = 'editor';

        const DEFAULT_SHORTCUTS = {
            editor: {
                'save': { key: 'Ctrl+S', description: 'Save File' },
                'find': { key: 'Ctrl+F', description: 'Find' },
                'replace': { key: 'Ctrl+H', description: 'Replace' },
                'duplicate-line': { key: 'Ctrl+D', description: 'Duplicate Line' },
                'comment': { key: 'Ctrl+/', description: 'Toggle Comment' },
                'goto-line': { key: 'Ctrl+L', description: 'Go to Line' },
                'run-code': { key: 'Ctrl+Enter', description: 'Run Code' }
            },
            terminal: {
                'toggle-terminal': { key: 'Alt+F12', description: 'Toggle Terminal' },
                'new-terminal': { key: 'Ctrl+Shift+`', description: 'New Terminal' }
            },
            navigation: {
                'toggle-sidebar': { key: 'Ctrl+B', description: 'Toggle Sidebar' },
                'search-everywhere': { key: 'Shift+Shift', description: 'Search Everywhere' },
                'goto-file': { key: 'Ctrl+N', description: 'Go to File' }
            }
        };

        const loadShortcuts = () => {
            try {
                const raw = localStorage.getItem('ahmadIDE:shortcuts');
                shortcuts = raw ? JSON.parse(raw) : JSON.parse(JSON.stringify(DEFAULT_SHORTCUTS));
            } catch (_) {
                shortcuts = JSON.parse(JSON.stringify(DEFAULT_SHORTCUTS));
            }
        };

        const saveShortcuts = () => {
            try {
                localStorage.setItem('ahmadIDE:shortcuts', JSON.stringify(shortcuts));
            } catch (_) { }
        };

        const renderShortcutsList = () => {
            if (!layout) return;
            const container = document.createElement('div');
            container.className = 'shortcuts-list';

            const categoryShortcuts = shortcuts[activeCategory] || {};
            Object.keys(categoryShortcuts).forEach((id) => {
                const sc = categoryShortcuts[id];
                const row = document.createElement('div');
                row.className = 'shortcuts-row';
                row.setAttribute('data-filter-text', `${sc.description} ${sc.key}`.toLowerCase());
                row.innerHTML = `
                    <div class="shortcuts-row__desc">${escapeHtml(sc.description)}</div>
                    <div class="shortcuts-row__key">
                        <kbd class="shortcuts-kbd">${escapeHtml(sc.key)}</kbd>
                        <button class="ui-btn ui-btn--ghost ui-btn--sm shortcuts-edit-btn" data-id="${escapeHtml(id)}">Edit</button>
                    </div>
                `;
                const editBtn = row.querySelector('.shortcuts-edit-btn');
                editBtn?.addEventListener('click', () => editShortcut(id, sc));
                container.appendChild(row);
            });

            if (Object.keys(categoryShortcuts).length === 0) {
                container.innerHTML = '<div class="shortcuts-empty">No shortcuts in this category</div>';
            }

            layout.elements.content.innerHTML = '';
            layout.elements.content.appendChild(container);
        };

        const editShortcut = async (id, sc) => {
            const { showPrompt } = window.AhmadIDEModules?.ui || {};
            if (!showPrompt) return;

            const newKey = await showPrompt({
                title: 'Edit Shortcut',
                message: `Edit shortcut for: ${sc.description}`,
                placeholder: 'e.g., Ctrl+Shift+K',
                defaultValue: sc.key
            });

            if (newKey && newKey.trim()) {
                // Check for conflicts
                let conflict = false;
                for (const cat in shortcuts) {
                    for (const sid in shortcuts[cat]) {
                        if (shortcuts[cat][sid].key === newKey.trim() && sid !== id) {
                            conflict = true;
                            break;
                        }
                    }
                    if (conflict) break;
                }

                if (conflict) {
                    const { showConfirm } = window.AhmadIDEModules?.ui || {};
                    if (showConfirm) {
                        const proceed = await showConfirm({
                            title: 'Shortcut Conflict',
                            message: `The shortcut "${newKey}" is already assigned. Reassign it anyway?`,
                            variant: 'default',
                            confirmLabel: 'Reassign',
                            cancelLabel: 'Cancel'
                        });
                        if (!proceed) return;
                    }
                }

                shortcuts[activeCategory][id].key = newKey.trim();
                saveShortcuts();
                renderShortcutsList();
            }
        };

        const resetToDefaults = async () => {
            const { showConfirm } = window.AhmadIDEModules?.ui || {};
            if (!showConfirm) return;

            const confirmed = await showConfirm({
                title: 'Reset Shortcuts',
                message: 'Reset all shortcuts to defaults? This cannot be undone.',
                variant: 'danger',
                confirmLabel: 'Reset',
                cancelLabel: 'Cancel'
            });

            if (confirmed) {
                shortcuts = JSON.parse(JSON.stringify(DEFAULT_SHORTCUTS));
                saveShortcuts();
                renderShortcutsList();
            }
        };

        const escapeHtml = (str) => String(str || '').replace(/[&<>"']/g, (m) => ({
            '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;'
        })[m]);

        const updateFooter = () => {
            if (!layout) return;
            const { createButton } = primitives;
            const resetBtn = createButton({ label: 'Reset to Defaults', variant: 'danger', onClick: resetToDefaults });
            const closeBtn = createButton({ label: 'Close', variant: 'primary', onClick: () => dialogApi?.close('close') });
            layout.setFooter([resetBtn, closeBtn]);
        };

        const ensureDialog = () => {
            if (dialogApi && layout) return;

            dialogApi = createDialog({
                ariaLabel: 'Keyboard Shortcuts',
                closeOnEscape: true,
                closeOnBackdrop: false,
                onClose: () => { }
            });

            layout = createDialogLayout({ title: 'Keyboard Shortcuts', searchPlaceholder: 'Search shortcuts' });
            layout.setItems([
                { id: 'editor', label: 'Editor' },
                { id: 'terminal', label: 'Terminal' },
                { id: 'navigation', label: 'Navigation' }
            ]);
            layout.elements.closeBtn.addEventListener('click', () => dialogApi.close('x'));

            layout.root.addEventListener('ui:dialog-nav-change', (e) => {
                activeCategory = e.detail?.id || activeCategory;
                renderShortcutsList();
            });

            layout.root.addEventListener('ui:dialog-search', (e) => {
                const query = String(e.detail?.query || '').trim().toLowerCase();
                const rows = layout.elements.content.querySelectorAll('[data-filter-text]');
                rows.forEach((el) => {
                    if (!query) {
                        el.classList.remove('hidden');
                        return;
                    }
                    const text = String(el.dataset.filterText || '');
                    el.classList.toggle('hidden', !text.includes(query));
                });
            });

            dialogApi.setContent(layout.root);
            updateFooter();
        };

        const open = () => {
            ensureDialog();
            loadShortcuts();
            renderShortcutsList();
            dialogApi.open();
        };

        return { open };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.features = window.AhmadIDEModules.features || {};
        window.AhmadIDEModules.features.shortcuts = window.AhmadIDEModules.features.shortcuts || {};
        window.AhmadIDEModules.features.shortcuts.createShortcutsDialog = createShortcutsDialog;
    }
})();
