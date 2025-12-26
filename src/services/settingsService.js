(() => {
    const STORAGE_KEY = 'ahmadIDE:settings';
    const EVENT_NAME = 'ahmadIDE:settings-changed';

    const DEFAULT_SETTINGS = Object.freeze({
        fonts: {
            ui: {
                scalePercent: 100
            },
            editor: {
                family: 'ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, "Liberation Mono", "Courier New", monospace',
                sizePx: 13,
                lineHeight: 1.55,
                weight: '400',
                ligatures: true,
                customFontId: null
            },
            terminal: {
                family: 'ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, "Liberation Mono", "Courier New", monospace',
                sizePx: 13,
                customFontId: null
            }
        },
        extensions: {
            enabled: {}
        },
        debugger: {
            timeline: {
                enable: true,
                maxSnapshots: 50,
                captureOnStop: true,
                captureGlobals: false,
                maxValueLength: 200
            }
        }
    });

    const clamp = (n, min, max) => Math.max(min, Math.min(max, n));

    const safeJsonParse = (raw) => {
        try {
            return raw ? JSON.parse(raw) : null;
        } catch (_) {
            return null;
        }
    };

    const deepMerge = (base, patch) => {
        if (!patch || typeof patch !== 'object') return base;
        if (!base || typeof base !== 'object') return patch;
        const out = Array.isArray(base) ? [...base] : { ...base };
        Object.keys(patch).forEach((k) => {
            const bv = base[k];
            const pv = patch[k];
            if (pv && typeof pv === 'object' && !Array.isArray(pv) && bv && typeof bv === 'object' && !Array.isArray(bv)) {
                out[k] = deepMerge(bv, pv);
            } else {
                out[k] = pv;
            }
        });
        return out;
    };

    const structuredCloneSafe = (obj) => {
        try {
            if (typeof structuredClone === 'function') return structuredClone(obj);
        } catch (_) { }
        return JSON.parse(JSON.stringify(obj || {}));
    };

    const normalizeSettings = (raw) => {
        const merged = deepMerge(DEFAULT_SETTINGS, raw || {});
        const fonts = merged.fonts || {};
        const dbg = merged.debugger || {};
        const tl = dbg.timeline || {};

        const uiScale = clamp(Number(fonts?.ui?.scalePercent ?? 100), 70, 150);
        const editorSize = clamp(Number(fonts?.editor?.sizePx ?? 13), 10, 40);
        const editorLineHeight = clamp(Number(fonts?.editor?.lineHeight ?? 1.55), 1.1, 2.2);
        const terminalSize = clamp(Number(fonts?.terminal?.sizePx ?? 13), 10, 40);
        const tlMaxSnapshots = clamp(Number(tl?.maxSnapshots ?? 50), 1, 500);
        const tlMaxValueLength = clamp(Number(tl?.maxValueLength ?? 200), 20, 20000);

        return {
            ...merged,
            fonts: {
                ui: {
                    scalePercent: uiScale
                },
                editor: {
                    family: String(fonts?.editor?.family || DEFAULT_SETTINGS.fonts.editor.family),
                    sizePx: editorSize,
                    lineHeight: editorLineHeight,
                    weight: String(fonts?.editor?.weight ?? '400'),
                    ligatures: !!fonts?.editor?.ligatures,
                    customFontId: fonts?.editor?.customFontId || null
                },
                terminal: {
                    family: String(fonts?.terminal?.family || DEFAULT_SETTINGS.fonts.terminal.family),
                    sizePx: terminalSize,
                    customFontId: fonts?.terminal?.customFontId || null
                }
            },
            extensions: {
                enabled: (merged.extensions && merged.extensions.enabled && typeof merged.extensions.enabled === 'object')
                    ? { ...merged.extensions.enabled }
                    : {}
            },
            debugger: {
                ...dbg,
                timeline: {
                    enable: !!tl?.enable,
                    maxSnapshots: Math.round(tlMaxSnapshots),
                    captureOnStop: tl?.captureOnStop !== false,
                    captureGlobals: !!tl?.captureGlobals,
                    maxValueLength: Math.round(tlMaxValueLength)
                }
            }
        };
    };

    function createSettingsService() {
        let state = null;
        const listeners = new Set();

        const load = () => {
            if (state) return state;
            const raw = safeJsonParse(window.localStorage?.getItem(STORAGE_KEY));
            state = normalizeSettings(raw);
            return state;
        };

        const get = () => (state ? state : load());

        const save = (next, { silent = false } = {}) => {
            state = normalizeSettings(next);
            try {
                window.localStorage?.setItem(STORAGE_KEY, JSON.stringify(state));
            } catch (_) { }
            if (!silent) {
                const snapshot = get();
                listeners.forEach((cb) => {
                    try {
                        cb(snapshot);
                    } catch (_) { }
                });
                try {
                    window.dispatchEvent(new CustomEvent(EVENT_NAME, { detail: snapshot }));
                } catch (_) { }
            }
            return state;
        };

        const update = (updater, opts) => {
            const current = structuredCloneSafe(get());
            const next = typeof updater === 'function' ? updater(current) : updater;
            return save(next, opts);
        };

        const subscribe = (cb) => {
            if (typeof cb !== 'function') return () => { };
            listeners.add(cb);
            return () => listeners.delete(cb);
        };

        return {
            STORAGE_KEY,
            EVENT_NAME,
            DEFAULT_SETTINGS,
            load,
            get,
            save,
            update,
            subscribe,
            clone: structuredCloneSafe,
            normalize: normalizeSettings
        };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.services = window.AhmadIDEModules.services || {};
        if (!window.AhmadIDEModules.services.settingsService) {
            window.AhmadIDEModules.services.settingsService = createSettingsService();
        }
    }
})();
