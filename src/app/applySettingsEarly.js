(() => {
    const STORAGE_KEY = 'ahmadIDE:settings';

    const safeJsonParse = (raw) => {
        try {
            return raw ? JSON.parse(raw) : null;
        } catch (_) {
            return null;
        }
    };

    const clamp = (n, min, max) => Math.max(min, Math.min(max, n));

    const DEFAULTS = {
        ui: {
            fontFamily: 'Inter',
            fontSize: 13
        },
        fonts: {
            ui: { scalePercent: 100 },
            editor: {
                family: 'Fira Code, ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, "Liberation Mono", "Courier New", monospace',
                sizePx: 14,
                lineHeight: 1.6,
                weight: '400',
                ligatures: true
            },
            terminal: {
                family: 'Fira Code, ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, "Liberation Mono", "Courier New", monospace',
                sizePx: 13
            }
        }
    };

    const uniqFamilies = (arr) => {
        const out = [];
        const seen = new Set();
        (arr || []).forEach((v) => {
            const s = String(v || '').trim();
            if (!s) return;
            const k = s.toLowerCase();
            if (seen.has(k)) return;
            seen.add(k);
            out.push(s);
        });
        return out;
    };

    const buildFontStack = (primary, fallbacks) => {
        const raw = String(primary || '').trim();
        if (!raw) return uniqFamilies(fallbacks || []).join(', ');
        if (raw.includes(',')) return raw;
        return uniqFamilies([raw, ...(fallbacks || [])]).join(', ');
    };

    const UI_FALLBACK = [
        'Inter',
        'Segoe UI',
        'SF Pro Display',
        'system-ui',
        'sans-serif'
    ];

    const CODE_FALLBACK = [
        'Cascadia Mono',
        'Fira Code',
        'Source Code Pro',
        'IBM Plex Mono',
        'Ubuntu Mono',
        'DejaVu Sans Mono',
        'Menlo',
        'Monaco',
        'Consolas',
        'Liberation Mono',
        'Courier New',
        'ui-monospace',
        'SFMono-Regular',
        'monospace'
    ];

    const deepMerge = (base, patch) => {
        if (!patch || typeof patch !== 'object') return base;
        const out = { ...base };
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

    const readSettings = () => {
        const raw = safeJsonParse(window.localStorage?.getItem(STORAGE_KEY));
        const merged = deepMerge(DEFAULTS, raw || {});
        const rawUi = (raw && typeof raw === 'object' && raw.ui && typeof raw.ui === 'object') ? raw.ui : null;
        const uiScale = clamp(Number(merged?.fonts?.ui?.scalePercent ?? 100), 70, 150);
        const editorSize = clamp(Number(merged?.fonts?.editor?.sizePx ?? 13), 10, 40);
        const editorLineHeight = clamp(Number(merged?.fonts?.editor?.lineHeight ?? 1.55), 1.1, 2.2);
        const terminalSize = clamp(Number(merged?.fonts?.terminal?.sizePx ?? 13), 10, 40);
        return {
            ui: {
                fontFamily: String(rawUi?.fontFamily || merged?.ui?.fontFamily || DEFAULTS.ui.fontFamily),
                fontSize: (rawUi && rawUi.fontSize != null) ? (Number(rawUi.fontSize) || null) : null
            },
            fonts: {
                ui: { scalePercent: uiScale },
                editor: {
                    family: String(merged?.fonts?.editor?.family || DEFAULTS.fonts.editor.family),
                    sizePx: editorSize,
                    lineHeight: editorLineHeight,
                    weight: String(merged?.fonts?.editor?.weight ?? '400'),
                    ligatures: !!merged?.fonts?.editor?.ligatures
                },
                terminal: {
                    family: String(merged?.fonts?.terminal?.family || DEFAULTS.fonts.terminal.family),
                    sizePx: terminalSize
                }
            }
        };
    };

    const applyCssVars = (settings) => {
        const root = document.documentElement;
        if (!root) return;

        // UI Font
        const uiFontFamily = String(settings?.ui?.fontFamily || DEFAULTS.ui.fontFamily);
        if (uiFontFamily) {
            root.style.setProperty('--font-ui', buildFontStack(uiFontFamily, UI_FALLBACK));
        }
        const uiScale = Number(settings?.fonts?.ui?.scalePercent ?? 100) || 100;
        const uiSizeExplicit = Number(settings?.ui?.fontSize);
        const uiSize = (Number.isFinite(uiSizeExplicit) && uiSizeExplicit > 0)
            ? Math.max(10, Math.round(uiSizeExplicit))
            : Math.max(10, Math.round(13 * (uiScale / 100)));
        root.style.setProperty('--font-size-ui', `${uiSize}px`);

        const editorFamily = String(settings?.fonts?.editor?.family || DEFAULTS.fonts.editor.family).trim();
        const editorSize = Number(settings?.fonts?.editor?.sizePx ?? 13) || 13;
        const editorLineHeight = Number(settings?.fonts?.editor?.lineHeight ?? 1.55) || 1.55;
        const editorWeight = String(settings?.fonts?.editor?.weight ?? '400');
        const editorLigatures = !!settings?.fonts?.editor?.ligatures;
        root.style.setProperty('--font-code', buildFontStack(editorFamily, CODE_FALLBACK));
        root.style.setProperty('--font-size-code', `${editorSize}px`);
        root.style.setProperty('--line-height-code', String(editorLineHeight));
        root.style.setProperty('--font-weight-code', editorWeight);
        root.style.setProperty('--font-ligatures-code', editorLigatures ? '1' : '0');

        const terminalFamily = String(settings?.fonts?.terminal?.family || editorFamily).trim();
        const terminalSize = Number(settings?.fonts?.terminal?.sizePx ?? 13) || 13;
        root.style.setProperty('--font-terminal', buildFontStack(terminalFamily, CODE_FALLBACK));
        root.style.setProperty('--font-size-terminal', `${terminalSize}px`);
    };

    try {
        applyCssVars(readSettings());
    } catch (_) {
        // never block startup
    }
})();
