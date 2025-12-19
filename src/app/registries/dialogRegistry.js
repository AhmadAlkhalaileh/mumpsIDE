(() => {
    function createDialogRegistry() {
        const dialogs = new Map(); // id -> { open, close, title }

        const register = (def) => {
            const id = String(def?.id || '').trim();
            if (!id) return false;
            if (dialogs.has(id)) return true;
            dialogs.set(id, {
                id,
                title: String(def?.title || id),
                open: typeof def?.open === 'function' ? def.open : null,
                close: typeof def?.close === 'function' ? def.close : null
            });
            return true;
        };

        const open = (id, payload) => {
            const d = dialogs.get(String(id || ''));
            if (!d?.open) return false;
            try {
                const res = d.open(payload);
                return res === false ? false : true;
            } catch (_) {
                return false;
            }
        };

        // Back-compat helpers (older modules call `show()` / `has()`).
        const show = (id, payload) => open(id, payload);

        const close = (id) => {
            const d = dialogs.get(String(id || ''));
            if (!d?.close) return false;
            try {
                const res = d.close();
                return res === false ? false : true;
            } catch (_) {
                return false;
            }
        };

        const hide = (id) => close(id);

        const has = (id) => dialogs.has(String(id || ''));

        const list = () => Array.from(dialogs.values()).map((d) => ({ id: d.id, title: d.title }));

        return { register, open, show, close, hide, has, list };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.app = window.AhmadIDEModules.app || {};
        if (!window.AhmadIDEModules.app.dialogRegistry) {
            window.AhmadIDEModules.app.dialogRegistry = createDialogRegistry();
        }
    }
})();
