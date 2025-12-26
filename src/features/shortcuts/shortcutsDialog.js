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

        const KEYMAP_STORAGE_KEY = 'ahmadIDE:keymap';
        const LEGACY_SHORTCUTS_KEY = 'ahmadIDE:shortcuts';

        const DEFAULT_SHORTCUTS = {
            editor: {
                'save': { key: 'Ctrl+S', description: 'Save File' },
                'save-all': { key: 'Ctrl+Shift+S', description: 'Save All' },
                'find': { key: 'Ctrl+F', description: 'Find' },
                'replace': { key: 'Ctrl+H', description: 'Replace' },
                'find-in-folder': { key: 'Ctrl+Shift+F', description: 'Find in Files' },
                'replace-in-folder': { key: 'Ctrl+Shift+R', description: 'Replace in Files' },
                'duplicate-line': { key: 'Ctrl+D', description: 'Duplicate Line' },
                'comment': { key: 'Ctrl+/', description: 'Toggle Comment' },
                'goto-line': { key: 'Ctrl+L', description: 'Go to Line' },
                'run': { key: 'Ctrl+Enter', description: 'Run' }
            },
            terminal: {
                'toggle-terminal': { key: 'Alt+F12', description: 'Toggle Terminal' },
                'terminal-new-tab': { key: 'Ctrl+Shift+T', description: 'New Terminal Tab' }
            },
            navigation: {
                'toggle-sidebar': { key: 'Ctrl+B', description: 'Toggle Sidebar' },
                'search-everywhere': { key: 'Shift+Shift', description: 'Search Everywhere' },
                'goto-file': { key: 'Ctrl+N', description: 'Go to File' }
            }
        };

        const deepClone = (obj) => {
            try {
                return JSON.parse(JSON.stringify(obj));
            } catch (_) {
                return obj;
            }
        };

        const isKeymapShape = (obj) => {
            if (!obj || typeof obj !== 'object' || Array.isArray(obj)) return false;
            const cats = Object.keys(obj);
            if (!cats.length) return false;
            let any = false;
            for (const cat of cats) {
                const group = obj[cat];
                if (!group || typeof group !== 'object' || Array.isArray(group)) continue;
                for (const id of Object.keys(group)) {
                    const sc = group[id];
                    if (!sc || typeof sc !== 'object') continue;
                    if (typeof sc.key === 'string' && sc.key.trim()) {
                        any = true;
                        break;
                    }
                }
                if (any) break;
            }
            return any;
        };

        const normalizeKeyString = (raw) => {
            const val = String(raw || '').trim();
            if (!val) return null;
            if (val.toLowerCase().replace(/\s+/g, '') === 'shift+shift') return 'Shift+Shift';

            const rawTokens = val.split('+').map(t => t.trim()).filter(Boolean);
            if (!rawTokens.length) return null;

            const mods = { ctrl: false, shift: false, alt: false, meta: false };
            let keyToken = null;
            const keyTokens = [];

            const toCanonKey = (tok) => {
                const t = String(tok || '').trim();
                if (!t) return null;
                const lower = t.toLowerCase();
                if (/^f([1-9]|1[0-2])$/.test(lower)) return lower.toUpperCase();
                if (lower === 'esc') return 'Escape';
                if (lower === 'escape') return 'Escape';
                if (lower === 'enter' || lower === 'return') return 'Enter';
                if (lower === 'space') return 'Space';
                if (lower === 'tab') return 'Tab';
                if (lower === 'backspace') return 'Backspace';
                if (lower === 'del' || lower === 'delete') return 'Delete';
                if (lower === 'up' || lower === 'arrowup') return 'ArrowUp';
                if (lower === 'down' || lower === 'arrowdown') return 'ArrowDown';
                if (lower === 'left' || lower === 'arrowleft') return 'ArrowLeft';
                if (lower === 'right' || lower === 'arrowright') return 'ArrowRight';
                if (t.length === 1) return t.toUpperCase();
                return t;
            };

            rawTokens.forEach(tok => {
                const upper = tok.toUpperCase();
                if (upper === 'CTRL' || upper === 'CMD' || upper === 'CONTROL' || upper === 'COMMAND') {
                    mods.ctrl = true;
                    return;
                }
                if (upper === 'SHIFT') {
                    mods.shift = true;
                    return;
                }
                if (upper === 'ALT' || upper === 'OPTION') {
                    mods.alt = true;
                    return;
                }
                if (upper === 'WIN' || upper === 'META' || upper === 'SUPER') {
                    mods.meta = true;
                    return;
                }
                keyTokens.push(tok);
            });

            if (keyTokens.length === 2 && keyTokens.every(t => String(t || '').trim().toLowerCase() === 'shift') && !mods.ctrl && !mods.alt && !mods.meta) {
                return 'Shift+Shift';
            }

            keyToken = toCanonKey(keyTokens[keyTokens.length - 1]);
            if (!keyToken) return null;

            const parts = [];
            if (mods.ctrl) parts.push('Ctrl');
            if (mods.shift) parts.push('Shift');
            if (mods.alt) parts.push('Alt');
            if (mods.meta) parts.push('Meta');
            parts.push(keyToken);
            return parts.join('+');
        };

        const emitKeymapChanged = () => {
            try {
                window.dispatchEvent(new CustomEvent('ahmadIDE:keymap-changed'));
            } catch (_) { }
        };

        const loadShortcuts = () => {
            try {
                const raw = localStorage.getItem(KEYMAP_STORAGE_KEY);
                if (raw) {
                    const parsed = JSON.parse(raw);
                    if (isKeymapShape(parsed)) {
                        shortcuts = parsed;
                        return;
                    }
                }
            } catch (_) {
                // ignore and fall back
            }

            // Migrate legacy key if it looks like the old dialog schema
            try {
                const legacyRaw = localStorage.getItem(LEGACY_SHORTCUTS_KEY);
                if (legacyRaw) {
                    const parsed = JSON.parse(legacyRaw);
                    if (isKeymapShape(parsed)) {
                        shortcuts = parsed;
                        try {
                            localStorage.setItem(KEYMAP_STORAGE_KEY, JSON.stringify(shortcuts));
                        } catch (_) { }
                        return;
                    }
                }
            } catch (_) { }

            shortcuts = deepClone(DEFAULT_SHORTCUTS);
        };

        const saveShortcuts = ({ emit = true } = {}) => {
            try {
                localStorage.setItem(KEYMAP_STORAGE_KEY, JSON.stringify(shortcuts));
            } catch (_) { }
            if (emit) emitKeymapChanged();
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
                        <button class="ui-btn ui-btn--ghost ui-btn--sm shortcuts-remove-btn" data-id="${escapeHtml(id)}">Remove</button>
                    </div>
                `;
                const editBtn = row.querySelector('.shortcuts-edit-btn');
                editBtn?.addEventListener('click', () => editShortcut(id, sc));
                const removeBtn = row.querySelector('.shortcuts-remove-btn');
                removeBtn?.addEventListener('click', () => removeShortcut(id, sc));
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

            const normalized = normalizeKeyString(newKey);
            if (normalized) {
                // Check for conflicts
                let conflict = false;
                for (const cat in shortcuts) {
                    for (const sid in shortcuts[cat]) {
                        if (String(shortcuts[cat][sid].key || '').toLowerCase() === normalized.toLowerCase() && sid !== id) {
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
                            message: `The shortcut "${normalized}" is already assigned. Reassign it anyway?`,
                            variant: 'default',
                            confirmLabel: 'Reassign',
                            cancelLabel: 'Cancel'
                        });
                        if (!proceed) return;
                    }
                }

                shortcuts[activeCategory][id].key = normalized;
                saveShortcuts();
                renderShortcutsList();
            }
        };

        const removeShortcut = async (id, sc) => {
            const { showConfirm } = window.AhmadIDEModules?.ui || {};
            if (showConfirm) {
                const ok = await showConfirm({
                    title: 'Remove Shortcut',
                    message: `Remove shortcut for "${sc.description}"?`,
                    variant: 'danger',
                    confirmLabel: 'Remove',
                    cancelLabel: 'Cancel'
                });
                if (!ok) return;
            }

            try {
                delete shortcuts?.[activeCategory]?.[id];
            } catch (_) { }
            saveShortcuts();
            renderShortcutsList();
        };

        const buildCommandOptions = () => {
            const known = new Map();
            const add = (action, label) => {
                const a = String(action || '').trim();
                const l = String(label || '').trim();
                if (!a) return;
                if (!known.has(a)) known.set(a, l || a);
            };

            // Defaults
            Object.keys(DEFAULT_SHORTCUTS).forEach((cat) => {
                Object.keys(DEFAULT_SHORTCUTS[cat] || {}).forEach((action) => {
                    add(action, DEFAULT_SHORTCUTS[cat]?.[action]?.description || action);
                });
            });

            // Menu registry (best-effort)
            try {
                const menuRegistry = window.AhmadIDEModules?.app?.menuRegistry;
                const menus = menuRegistry?.get?.('menubar') || [];
                const visit = (items = []) => {
                    (items || []).forEach((it) => {
                        if (!it) return;
                        if (it.action && it.label) add(it.action, it.label);
                        if (Array.isArray(it.submenu)) visit(it.submenu);
                        if (Array.isArray(it.items)) visit(it.items);
                    });
                };
                menus.forEach((m) => visit(m?.items || m?.submenu || []));
            } catch (_) { }

            // Handy extras not in menus
            add('tab-next', 'Next Tab');
            add('expand-selection', 'Expand Selection');
            add('shrink-selection', 'Shrink Selection');

            const rows = Array.from(known.entries()).map(([action, label]) => ({ action, label }));
            rows.sort((a, b) => String(a.label).localeCompare(String(b.label)));
            return rows;
        };

        const addShortcut = async () => {
            const { showConfirm } = window.AhmadIDEModules?.ui || {};

            const dlg = createDialog({
                ariaLabel: 'Add Shortcut',
                closeOnEscape: true,
                closeOnBackdrop: false,
                onClose: () => { }
            });

            const wrapper = document.createElement('div');
            wrapper.className = 'ui-dialog-layout';

            const header = document.createElement('div');
            header.className = 'ui-dialog-header';
            const headerLeft = document.createElement('div');
            headerLeft.className = 'ui-dialog-header__left';
            const titleEl = document.createElement('div');
            titleEl.className = 'ui-dialog-title';
            titleEl.textContent = 'Add Shortcut';
            headerLeft.appendChild(titleEl);
            const headerRight = document.createElement('div');
            headerRight.className = 'ui-dialog-header__right';
            const closeBtn = document.createElement('button');
            closeBtn.className = 'ui-dialog-close';
            closeBtn.type = 'button';
            closeBtn.title = 'Close';
            closeBtn.textContent = '✕';
            headerRight.appendChild(closeBtn);
            header.appendChild(headerLeft);
            header.appendChild(headerRight);

            const body = document.createElement('div');
            body.style.cssText = 'padding:var(--ui-space-6);min-width:520px;';

            const createFormGroup = (label, node) => {
                const group = document.createElement('div');
                group.style.cssText = 'margin-bottom:var(--ui-space-4);';
                const l = document.createElement('div');
                l.textContent = label;
                l.style.cssText = 'margin-bottom:var(--ui-space-2);font-weight:600;';
                group.appendChild(l);
                group.appendChild(node);
                return group;
            };

            const { createInput, createSelect, createButton } = primitives;

            const categorySelect = createSelect({
                value: activeCategory,
                options: [
                    { value: 'editor', label: 'Editor' },
                    { value: 'terminal', label: 'Terminal' },
                    { value: 'navigation', label: 'Navigation' }
                ]
            });

            const commands = buildCommandOptions();
            const commandSelect = createSelect({
                value: '',
                options: [
                    { value: '', label: 'Select a command…', disabled: true },
                    ...commands.map((c) => ({ value: c.action, label: `${c.label} (${c.action})` })),
                    { value: '__custom__', label: 'Custom action…' }
                ]
            });

            const actionInput = createInput({ placeholder: 'Action id (e.g., toggle-terminal)', value: '' });
            actionInput.style.cssText = 'width:100%;';
            const keyInput = createInput({ placeholder: 'Shortcut (e.g., Ctrl+Shift+K)', value: '' });
            keyInput.style.cssText = 'width:100%;';
            const descInput = createInput({ placeholder: 'Description (optional)', value: '' });
            descInput.style.cssText = 'width:100%;';

            const setActionFieldMode = (mode) => {
                const isCustom = mode === 'custom';
                actionInput.disabled = !isCustom;
                actionInput.style.opacity = isCustom ? '1' : '0.7';
            };

            setActionFieldMode('custom');

            commandSelect.addEventListener('change', () => {
                const sel = String(commandSelect.value || '');
                if (!sel) return;
                if (sel === '__custom__') {
                    actionInput.value = '';
                    descInput.value = '';
                    setActionFieldMode('custom');
                    actionInput.focus();
                    return;
                }
                actionInput.value = sel;
                setActionFieldMode('locked');
                const found = commands.find(c => c.action === sel);
                if (found && !descInput.value.trim()) descInput.value = found.label;
                keyInput.focus();
            });

            body.appendChild(createFormGroup('Category', categorySelect));
            body.appendChild(createFormGroup('Command', commandSelect));
            body.appendChild(createFormGroup('Action', actionInput));
            body.appendChild(createFormGroup('Shortcut', keyInput));
            body.appendChild(createFormGroup('Description', descInput));

            const footer = document.createElement('div');
            footer.className = 'ui-dialog-footer';

            const cancelBtn = createButton({ label: 'Cancel', variant: 'ghost', onClick: () => dlg.close('cancel') });
            const addBtn = createButton({
                label: 'Add',
                variant: 'primary',
                onClick: async () => {
                    const category = String(categorySelect.value || activeCategory).trim() || activeCategory;
                    const actionId = String(actionInput.value || '').trim();
                    const desc = String(descInput.value || '').trim() || actionId;
                    const normalizedKey = normalizeKeyString(keyInput.value);

                    if (!actionId) {
                        alert('Please choose or enter an action id');
                        return;
                    }
                    if (!normalizedKey) {
                        alert('Please enter a valid shortcut, e.g. Ctrl+Shift+K');
                        return;
                    }

                    // Conflicts (any category)
                    let conflictWith = null;
                    for (const cat in shortcuts) {
                        for (const sid in shortcuts[cat]) {
                            const existing = shortcuts[cat][sid];
                            if (String(existing?.key || '').toLowerCase() === normalizedKey.toLowerCase() && sid !== actionId) {
                                conflictWith = existing?.description || sid;
                                break;
                            }
                        }
                        if (conflictWith) break;
                    }
                    if (conflictWith && showConfirm) {
                        const ok = await showConfirm({
                            title: 'Shortcut Conflict',
                            message: `"${normalizedKey}" is already assigned to "${conflictWith}". Reassign anyway?`,
                            variant: 'default',
                            confirmLabel: 'Reassign',
                            cancelLabel: 'Cancel'
                        });
                        if (!ok) return;
                    }

                    shortcuts[category] = shortcuts[category] || {};
                    shortcuts[category][actionId] = { key: normalizedKey, description: desc };
                    saveShortcuts();
                    dlg.close('ok');

                    // Switch to category so the user sees the new entry.
                    if (layout && category !== activeCategory) {
                        activeCategory = category;
                        layout.setActive(category);
                    }
                    renderShortcutsList();
                }
            });

            footer.appendChild(cancelBtn);
            footer.appendChild(addBtn);

            closeBtn.addEventListener('click', () => dlg.close('x'));

            wrapper.appendChild(header);
            wrapper.appendChild(body);
            wrapper.appendChild(footer);

            dlg.setContent(wrapper);
            dlg.open();

            requestAnimationFrame(() => commandSelect?.focus?.());
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
                shortcuts = deepClone(DEFAULT_SHORTCUTS);
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
            const addBtn = createButton({ label: 'Add Shortcut…', variant: 'ghost', onClick: addShortcut });
            const resetBtn = createButton({ label: 'Reset to Defaults', variant: 'danger', onClick: resetToDefaults });
            const closeBtn = createButton({ label: 'Close', variant: 'primary', onClick: () => dialogApi?.close('close') });
            layout.setFooter([addBtn, resetBtn, closeBtn]);
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
