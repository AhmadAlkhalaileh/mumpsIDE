(() => {
    function createShortcutPrefs({ state } = {}) {
        const registeredShortcuts = state?.registeredShortcuts || [];

        function loadShortcutPrefs() {
            try {
                const key = 'ahmadIDE:shortcutBindings';
                const raw = localStorage.getItem(key);
                if (raw) return JSON.parse(raw) || {};
            } catch (_) { }

            // Legacy migration: numeric map used by Monaco bindings only.
            try {
                const legacyRaw = localStorage.getItem('ahmadIDE:shortcuts');
                if (!legacyRaw) return {};
                const parsed = JSON.parse(legacyRaw);
                if (!parsed || typeof parsed !== 'object' || Array.isArray(parsed)) return {};
                const vals = Object.values(parsed);
                const looksNumeric = vals.length > 0 ? vals.every(v => typeof v === 'number') : false;
                if (!looksNumeric) return {};
                try {
                    localStorage.setItem('ahmadIDE:shortcutBindings', JSON.stringify(parsed));
                } catch (_) { }
                return parsed;
            } catch (_) {
                return {};
            }
        }

        function persistShortcutPrefs(map) {
            try {
                localStorage.setItem('ahmadIDE:shortcutBindings', JSON.stringify(map || {}));
            } catch (e) {
                // ignore storage failures
            }
        }

        function keyCodeFromToken(tok) {
            if (!tok) return null;
            const upper = tok.toUpperCase();
            if (upper.length === 1 && upper >= 'A' && upper <= 'Z') {
                return monaco.KeyCode['Key' + upper];
            }
            const funcMatch = upper.match(/^F([1-9]|1[0-2])$/);
            if (funcMatch) {
                return monaco.KeyCode['F' + funcMatch[1]];
            }
            if (upper === 'ENTER' || upper === 'RETURN') return monaco.KeyCode.Enter;
            return null;
        }

        function parseShortcutString(str) {
            if (!str || typeof str !== 'string') return null;
            const tokens = str.split('+').map(t => t.trim()).filter(Boolean);
            let binding = 0;
            let keyToken = null;
            tokens.forEach(tok => {
                const upper = tok.toUpperCase();
                if (upper === 'CTRL' || upper === 'CMD' || upper === 'CONTROL') {
                    binding |= monaco.KeyMod.CtrlCmd;
                } else if (upper === 'SHIFT') {
                    binding |= monaco.KeyMod.Shift;
                } else if (upper === 'ALT' || upper === 'OPTION') {
                    binding |= monaco.KeyMod.Alt;
                } else if (upper === 'WIN' || upper === 'META') {
                    binding |= monaco.KeyMod.WinCtrl;
                } else {
                    keyToken = upper;
                }
            });
            const keyCode = keyCodeFromToken(keyToken);
            if (!keyCode) return null;
            return binding | keyCode;
        }

        function applyShortcutBinding(editor, actionId, binding, handler) {
            if (!editor || !handler || !binding) return;
            editor.addCommand(binding, handler);
        }

        function describeBinding(binding) {
            if (!binding && binding !== 0) return 'Unbound';
            const parts = [];
            if (binding & monaco.KeyMod.CtrlCmd) parts.push('Ctrl');
            if (binding & monaco.KeyMod.Shift) parts.push('Shift');
            if (binding & monaco.KeyMod.Alt) parts.push('Alt');
            if (binding & monaco.KeyMod.WinCtrl) parts.push('Meta');

            const keyPart = binding & 0xff;
            const keyNames = Object.keys(monaco.KeyCode).filter(k => monaco.KeyCode[k] === keyPart);
            const keyName = keyNames.length ? keyNames[0].replace(/^Key/, '') : '';
            if (keyName) parts.push(keyName);
            return parts.join('+') || 'Unbound';
        }

        function renderShortcutsPanel() {
            const list = document.getElementById('shortcutsList');
            if (!list) return;
            list.innerHTML = '';
            if (!registeredShortcuts.length) {
                const li = document.createElement('li');
                li.textContent = 'No shortcuts registered.';
                list.appendChild(li);
                return;
            }
            registeredShortcuts.forEach(sc => {
                const li = document.createElement('li');
                li.className = 'problem-item info shortcuts-list';
                const icon = document.createElement('span');
                icon.className = 'problem-icon';
                icon.textContent = '⌨';
                const text = document.createElement('span');
                text.className = 'problem-text';
                text.textContent = `${sc.label} — ${describeBinding(sc.binding)}`;
                li.appendChild(icon);
                li.appendChild(text);
                li.onclick = () => {
                    navigator.clipboard?.writeText(`${sc.label} :: ${describeBinding(sc.binding)}`).catch(() => { });
                };
                list.appendChild(li);
            });
        }

        return {
            loadShortcutPrefs,
            persistShortcutPrefs,
            keyCodeFromToken,
            parseShortcutString,
            applyShortcutBinding,
            describeBinding,
            renderShortcutsPanel
        };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.renderer = window.AhmadIDEModules.renderer || {};
        window.AhmadIDEModules.renderer.shortcuts = window.AhmadIDEModules.renderer.shortcuts || {};
        window.AhmadIDEModules.renderer.shortcuts.createShortcutPrefs = createShortcutPrefs;
    }
})();
