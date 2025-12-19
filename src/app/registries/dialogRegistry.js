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
            if (d?.open) return d.open(payload);
            return false;
        };

        const close = (id) => {
            const d = dialogs.get(String(id || ''));
            if (d?.close) return d.close();
            return false;
        };

        const list = () => Array.from(dialogs.values()).map((d) => ({ id: d.id, title: d.title }));

        return { register, open, close, list };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.app = window.AhmadIDEModules.app || {};
        if (!window.AhmadIDEModules.app.dialogRegistry) {
            window.AhmadIDEModules.app.dialogRegistry = createDialogRegistry();
        }
    }
})();

