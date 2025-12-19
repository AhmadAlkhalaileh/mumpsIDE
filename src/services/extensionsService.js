(() => {
    const EVENT_NAME = 'ahmadIDE:extensions-changed';

    const toDisposable = (value) => {
        if (!value) return null;
        if (typeof value.dispose === 'function') return value;
        if (typeof value === 'function') return { dispose: value };
        return null;
    };

    const safeDispose = (d) => {
        try {
            d?.dispose?.();
        } catch (_) { }
    };

    function createExtensionsService({ deps } = {}) {
        const settingsService = deps?.settingsService || window.AhmadIDEModules?.services?.settingsService;
        if (!settingsService) throw new Error('extensionsService requires settingsService');

        const registry = new Map(); // id -> extension
        const active = new Map(); // id -> { ctx, disposables: [] }
        const listeners = new Set();

        const getEnabledMap = () => settingsService.get()?.extensions?.enabled || {};

        const isEnabled = (id) => {
            const ext = registry.get(id);
            const enabledMap = getEnabledMap();
            const stored = enabledMap[id];
            if (stored === undefined) return ext?.defaultEnabled !== false;
            return !!stored;
        };

        const emit = () => {
            const snapshot = list();
            listeners.forEach((cb) => {
                try { cb(snapshot); } catch (_) { }
            });
            try {
                window.dispatchEvent(new CustomEvent(EVENT_NAME, { detail: snapshot }));
            } catch (_) { }
        };

        const createContext = (ext) => {
            const subscriptions = [];
            const ctx = {
                id: ext?.id,
                subscriptions,
                onDispose: (d) => {
                    const disp = toDisposable(d);
                    if (disp) subscriptions.push(disp);
                    return disp;
                },
                addEventListener: (target, event, handler, options) => {
                    if (!target?.addEventListener) return null;
                    target.addEventListener(event, handler, options);
                    return ctx.onDispose(() => {
                        try { target.removeEventListener(event, handler, options); } catch (_) { }
                    });
                }
            };
            return ctx;
        };

        const activate = async (id) => {
            const ext = registry.get(id);
            if (!ext || active.has(id)) return false;
            const ctx = createContext(ext);
            let returned = null;
            try {
                returned = await ext.activate?.(ctx);
            } catch (_) {
                // Activation failures should not crash the IDE.
            }

            const disposables = [];
            if (Array.isArray(returned)) returned.forEach((d) => disposables.push(toDisposable(d)));
            else if (returned) disposables.push(toDisposable(returned));
            ctx.subscriptions.forEach((d) => disposables.push(toDisposable(d)));

            active.set(id, { ctx, disposables: disposables.filter(Boolean) });
            emit();
            return true;
        };

        const deactivate = async (id) => {
            const ext = registry.get(id);
            const rec = active.get(id);
            if (!ext || !rec) return false;
            active.delete(id);
            try {
                await ext.deactivate?.();
            } catch (_) { }
            (rec.disposables || []).forEach((d) => safeDispose(d));
            emit();
            return true;
        };

        const setEnabled = async (id, enabled) => {
            const desired = !!enabled;
            settingsService.update((s) => {
                s.extensions = s.extensions || {};
                s.extensions.enabled = s.extensions.enabled || {};
                s.extensions.enabled[id] = desired;
                return s;
            });
            if (desired) {
                await activate(id);
            } else {
                await deactivate(id);
            }
            emit();
        };

        const register = (ext) => {
            if (!ext?.id) return false;
            if (registry.has(ext.id)) return true;
            registry.set(ext.id, {
                id: String(ext.id),
                name: String(ext.name || ext.id),
                version: String(ext.version || '0.0.0'),
                description: String(ext.description || ''),
                defaultEnabled: ext.defaultEnabled !== false,
                contributes: ext.contributes || {},
                activate: typeof ext.activate === 'function' ? ext.activate : async () => [],
                deactivate: typeof ext.deactivate === 'function' ? ext.deactivate : async () => { }
            });
            emit();
            return true;
        };

        const registerBundled = () => {
            const bundled = window.AhmadIDEModules?.extensions?.bundled || [];
            (Array.isArray(bundled) ? bundled : []).forEach((ext) => register(ext));
        };

        const start = async () => {
            registerBundled();
            const ids = Array.from(registry.keys());
            for (const id of ids) {
                if (isEnabled(id)) {
                    // eslint-disable-next-line no-await-in-loop
                    await activate(id);
                }
            }
            emit();
        };

        const list = () => Array.from(registry.values()).map((ext) => ({
            ...ext,
            enabled: isEnabled(ext.id),
            active: active.has(ext.id)
        }));

        const onChange = (cb) => {
            if (typeof cb !== 'function') return () => { };
            listeners.add(cb);
            return () => listeners.delete(cb);
        };

        return {
            EVENT_NAME,
            start,
            register,
            list,
            onChange,
            isEnabled,
            setEnabled,
            activate,
            deactivate
        };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.services = window.AhmadIDEModules.services || {};
        if (!window.AhmadIDEModules.services.extensionsService) {
            window.AhmadIDEModules.services.extensionsService = createExtensionsService();
        }
    }
})();

