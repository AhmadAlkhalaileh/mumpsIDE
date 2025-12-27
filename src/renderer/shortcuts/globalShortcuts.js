(() => {
    const coreShortcutMap = {
        'ctrl+n': { label: 'Go to File', action: 'goto-file' },
        'ctrl+shift+n': { label: 'Go to File (Alt)', action: 'goto-file' },
        'ctrl+f': { label: 'Find in File', action: 'find' },
        'ctrl+h': { label: 'Replace', action: 'replace' },
        'ctrl+shift+f': { label: 'Find in Path (Current Folder)', action: 'find-in-folder' },
        'ctrl+shift+r': { label: 'Replace in Path (Current Folder)', action: 'replace-in-folder' },
        'ctrl+s': { label: 'Save', action: 'save' },
        'ctrl+shift+s': { label: 'Save All', action: 'save-all' },
        'ctrl+enter': { label: 'Run', action: 'run' },
        'ctrl+b': { label: 'Toggle Sidebar', action: 'toggle-sidebar' },
        'ctrl+l': { label: 'Go to Line', action: 'goto-line' },
        'ctrl+d': { label: 'Duplicate Line', action: 'duplicate-line' },
        'ctrl+/': { label: 'Toggle Comment', action: 'comment' },
        'ctrl+w': { label: 'Expand Selection', action: 'expand-selection' },
        'ctrl+shift+w': { label: 'Shrink Selection', action: 'shrink-selection' },
        'ctrl+tab': { label: 'Next Tab', action: 'tab-next' },
        'ctrl+shift+tab': { label: 'Previous Tab', action: 'tab-prev' },
        'ctrl+f4': { label: 'Close Tab', action: 'tab:close' },
        'ctrl+shift+t': { label: 'New Terminal Tab', action: 'terminal-new-tab' },
        'alt+f12': { label: 'Toggle Terminal', action: 'toggle-terminal' }
    };

    let lastShiftTap = 0;

    function createGlobalShortcutsBinder({ deps } = {}) {
        const closeFindReplaceDialog = deps?.closeFindReplaceDialog;
        const closeSearchEverywhere = deps?.closeSearchEverywhere;
        const runMenuAction = deps?.runMenuAction;
        const terminalConfig = deps?.terminalConfig;
        const getGlobalRoutineState = deps?.getGlobalRoutineState;
        const getGlobalTerminalState = deps?.getGlobalTerminalState;
        const getActiveEditor = deps?.getActiveEditor;
        const canSaveActiveTab = deps?.canSaveActiveTab;
        const showToast = deps?.showToast;
        const saveAllOpenTabs = deps?.saveAllOpenTabs;
        const saveRoutineFlow = deps?.saveRoutineFlow;

        function bindGlobalShortcuts() {
            if (bindGlobalShortcuts.__bound) return;
            bindGlobalShortcuts.__bound = true;

            const KEYMAP_STORAGE_KEY = 'ahmadIDE:keymap';

            const normalizeComboFromEvent = (e) => {
                const rawKey = String(e.key || '');
                const key = rawKey.toLowerCase();

                // Ignore bare modifier presses here (handled separately for Shift+Shift).
                if (key === 'shift' || key === 'control' || key === 'alt' || key === 'meta') return null;

                const parts = [];
                if (e.ctrlKey || e.metaKey) parts.push('ctrl');
                if (e.shiftKey) parts.push('shift');
                if (e.altKey) parts.push('alt');

                const keyPart = (key === ' ') ? 'space' : key;
                parts.push(keyPart);
                return parts.join('+');
            };

            const normalizeKeyStringToCombo = (str) => {
                const raw = String(str || '').trim();
                if (!raw) return null;
                const compact = raw.toLowerCase().replace(/\s+/g, '');
                if (compact === 'shift+shift') return 'shift+shift';

                const tokens = raw.split('+').map(t => t.trim()).filter(Boolean);
                if (!tokens.length) return null;

                const mods = { ctrl: false, shift: false, alt: false };
                let keyTok = null;

                for (const tok of tokens) {
                    const t = tok.toLowerCase();
                    if (t === 'ctrl' || t === 'control' || t === 'cmd' || t === 'command' || t === 'meta' || t === 'win') {
                        mods.ctrl = true;
                        continue;
                    }
                    if (t === 'shift') {
                        mods.shift = true;
                        continue;
                    }
                    if (t === 'alt' || t === 'option') {
                        mods.alt = true;
                        continue;
                    }
                    keyTok = t;
                }

                if (!keyTok) return null;

                const keyAlias = {
                    esc: 'escape',
                    escape: 'escape',
                    enter: 'enter',
                    return: 'enter',
                    space: 'space',
                    tab: 'tab',
                    backspace: 'backspace',
                    del: 'delete',
                    delete: 'delete',
                    up: 'arrowup',
                    arrowup: 'arrowup',
                    down: 'arrowdown',
                    arrowdown: 'arrowdown',
                    left: 'arrowleft',
                    arrowleft: 'arrowleft',
                    right: 'arrowright',
                    arrowright: 'arrowright'
                };

                const keyPart = keyAlias[keyTok] || keyTok;
                const parts = [];
                if (mods.ctrl) parts.push('ctrl');
                if (mods.shift) parts.push('shift');
                if (mods.alt) parts.push('alt');
                parts.push(keyPart);
                return parts.join('+');
            };

            const loadUserKeymap = () => {
                try {
                    const raw = localStorage.getItem(KEYMAP_STORAGE_KEY);
                    if (!raw) return [];
                    const parsed = JSON.parse(raw);
                    if (!parsed || typeof parsed !== 'object' || Array.isArray(parsed)) return [];
                    const out = [];
                    for (const cat of Object.keys(parsed)) {
                        const group = parsed[cat];
                        if (!group || typeof group !== 'object' || Array.isArray(group)) continue;
                        for (const actionId of Object.keys(group)) {
                            const sc = group[actionId];
                            const key = String(sc?.key || '').trim();
                            if (!key) continue;
                            out.push({ actionId, key });
                        }
                    }
                    return out;
                } catch (_) {
                    return [];
                }
            };

            let comboToAction = new Map();
            let doubleShiftAction = 'search-everywhere';

            const rebuildShortcutMap = () => {
                doubleShiftAction = 'search-everywhere';
                const m = new Map();

                // Core defaults
                Object.keys(coreShortcutMap || {}).forEach((combo) => {
                    const act = coreShortcutMap[combo]?.action;
                    if (act) m.set(combo, act);
                });

                // User overrides / additions
                const user = loadUserKeymap();
                user.forEach((it) => {
                    const combo = normalizeKeyStringToCombo(it.key);
                    if (!combo) return;
                    if (combo === 'shift+shift') {
                        doubleShiftAction = it.actionId;
                        return;
                    }
                    m.set(combo, it.actionId);
                });

                comboToAction = m;
            };

            rebuildShortcutMap();
            window.addEventListener('ahmadIDE:keymap-changed', rebuildShortcutMap);

            const isTerminalTarget = (el) => {
                if (!el) return false;
                return !!el.closest?.('#terminalToolViewport')
                    || !!el.closest?.('#terminalViewport')
                    || !!el.closest?.('.xterm');
            };

            const isMonacoTarget = (el) => {
                if (!el) return false;
                return !!el.closest?.('.monaco-editor');
            };

            const isEditableTarget = (el) => {
                if (!el) return false;
                const tag = (el.tagName || '').toLowerCase();
                const editable = el.isContentEditable;
                return editable || tag === 'input' || tag === 'textarea' || tag === 'select';
            };

            const handler = async (e) => {
                const key = String(e.key || '').toLowerCase();
                if (key !== 'shift') lastShiftTap = 0;

                // Escape closes global search dialogs (no-op if not open).
                if (key === 'escape') {
                    try { closeFindReplaceDialog?.(); } catch (_) { }
                    try { closeSearchEverywhere?.(); } catch (_) { }
                    return;
                }

                // Double-Shift (Search Everywhere by default; configurable in keymap).
                if (!e.ctrlKey && !e.metaKey && !e.altKey && key === 'shift') {
                    if (e.repeat) return;
                    const now = Date.now();
                    if (now - lastShiftTap <= 420) {
                        lastShiftTap = 0;
                        if (!doubleShiftAction) return;
                        e.preventDefault();
                        e.stopImmediatePropagation();
                        await runMenuAction(doubleShiftAction);
                        return;
                    }
                    lastShiftTap = now;
                    return;
                }

                const combo = normalizeComboFromEvent(e);
                if (!combo) return;
                const action = comboToAction.get(combo);
                if (!action) return;
                const isSave = action === 'save';
                const isSaveAll = action === 'save-all';

                // When terminal is focused, prefer terminal input like JetBrains (but keep toolwindow toggles).
                if (isTerminalTarget(e.target) && terminalConfig.overrideIdeShortcuts) {
                    const allowInTerminal = action === 'toggle-terminal' || action === 'terminal-new-tab' || action === 'terminal';
                    if (!isSave && !allowInTerminal) return;
                }

                if (isEditableTarget(e.target) && !isSave && !isMonacoTarget(e.target)) {
                    const hasModifier = e.ctrlKey || e.metaKey || e.altKey;
                    const isFunctionKey = /^f([1-9]|1[0-2])$/.test(key);
                    if (!hasModifier && !isFunctionKey) return;
                }

                // Handle Save directly to avoid conflicts with Monaco.
                if (isSave || isSaveAll) {
                    e.preventDefault();
                    e.stopImmediatePropagation();
                    const globalRoutineState = getGlobalRoutineState ? getGlobalRoutineState() : null;
                    const globalTerminalState = getGlobalTerminalState ? getGlobalTerminalState() : null;
                    const activeEditor = getActiveEditor ? getActiveEditor() : null;
                    if (globalRoutineState && globalTerminalState && activeEditor) {
                        if (!canSaveActiveTab()) {
                            showToast('info', 'Diff', 'Diff tabs are read-only');
                            return;
                        }
                        if (isSaveAll) {
                            await saveAllOpenTabs({ terminalState: globalTerminalState });
                        } else {
                            await saveRoutineFlow(activeEditor, globalRoutineState, globalTerminalState);
                        }
                    }
                    return;
                }

                e.preventDefault();
                e.stopImmediatePropagation();
                await runMenuAction(action);
            };

            window.addEventListener('keydown', handler, true);
        }

        return { bindGlobalShortcuts };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.renderer = window.AhmadIDEModules.renderer || {};
        window.AhmadIDEModules.renderer.shortcuts = window.AhmadIDEModules.renderer.shortcuts || {};
        window.AhmadIDEModules.renderer.shortcuts.createGlobalShortcutsBinder = createGlobalShortcutsBinder;
    }
})();
