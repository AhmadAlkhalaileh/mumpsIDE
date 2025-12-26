(() => {
    const DB_NAME = 'ahmadIDE:fonts';
    const DB_VERSION = 1;
    const STORE = 'files';

    const FALLBACK_EDITOR_FONTS = [
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
        'monospace'
    ];

    const openDb = () => new Promise((resolve, reject) => {
        try {
            const req = indexedDB.open(DB_NAME, DB_VERSION);
            req.onupgradeneeded = () => {
                const db = req.result;
                if (!db.objectStoreNames.contains(STORE)) {
                    db.createObjectStore(STORE, { keyPath: 'id' });
                }
            };
            req.onsuccess = () => resolve(req.result);
            req.onerror = () => reject(req.error);
        } catch (e) {
            reject(e);
        }
    });

    const idbGet = async (id) => {
        if (!id) return null;
        const db = await openDb();
        return new Promise((resolve) => {
            const tx = db.transaction(STORE, 'readonly');
            const store = tx.objectStore(STORE);
            const req = store.get(id);
            req.onsuccess = () => resolve(req.result || null);
            req.onerror = () => resolve(null);
        });
    };

    const idbPut = async (record) => {
        const db = await openDb();
        return new Promise((resolve, reject) => {
            const tx = db.transaction(STORE, 'readwrite');
            tx.oncomplete = () => resolve(true);
            tx.onerror = () => reject(tx.error);
            tx.objectStore(STORE).put(record);
        });
    };

    const idbDelete = async (id) => {
        if (!id) return false;
        const db = await openDb();
        return new Promise((resolve) => {
            const tx = db.transaction(STORE, 'readwrite');
            tx.oncomplete = () => resolve(true);
            tx.onerror = () => resolve(false);
            tx.objectStore(STORE).delete(id);
        });
    };

    const guessFormat = (fileName = '') => {
        const lower = String(fileName).toLowerCase();
        if (lower.endsWith('.woff2')) return 'woff2';
        if (lower.endsWith('.woff')) return 'woff';
        if (lower.endsWith('.otf')) return 'opentype';
        if (lower.endsWith('.ttf')) return 'truetype';
        return '';
    };

    const sanitizeFamily = (name) => String(name || '').replace(/["']/g, '').trim();
    const baseNameNoExt = (fileName) => String(fileName || '').replace(/\.[a-z0-9]+$/i, '').trim();

    const createStyleTag = (cssText) => {
        const tag = document.createElement('style');
        tag.type = 'text/css';
        tag.textContent = cssText;
        document.head.appendChild(tag);
        return tag;
    };

    function createFontService() {
        const installedCache = { ts: 0, list: null, pending: null };
        const registered = new Map(); // fontId -> { url, styleEl, family }

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
            const merged = uniqFamilies([raw, ...(fallbacks || [])]);
            return merged.join(', ');
        };

        const UI_FALLBACK = [
            'Inter',
            'Segoe UI',
            'SF Pro Display',
            'system-ui',
            'sans-serif'
        ];

        const CODE_FALLBACK_TAIL = [
            'ui-monospace',
            'SFMono-Regular',
            'Menlo',
            'Monaco',
            'Consolas',
            'Liberation Mono',
            'Courier New',
            'monospace'
        ];

        const registerFontFaceFromBlob = async ({ id, family, blob, fileName }) => {
            const safeFamily = sanitizeFamily(family || baseNameNoExt(fileName) || 'Custom Font');
            if (!safeFamily || !blob) return null;

            const existing = registered.get(id);
            if (existing?.family === safeFamily) return safeFamily;

            if (existing) {
                try { URL.revokeObjectURL(existing.url); } catch (_) { }
                try { existing.styleEl?.remove(); } catch (_) { }
                registered.delete(id);
            }

            const url = URL.createObjectURL(blob);
            const fmt = guessFormat(fileName);
            const src = fmt ? `url("${url}") format("${fmt}")` : `url("${url}")`;
            const css = `
@font-face {
  font-family: "${safeFamily}";
  src: ${src};
  font-display: swap;
}
            `.trim();
            const styleEl = createStyleTag(css);
            registered.set(id, { url, styleEl, family: safeFamily });

            try {
                // Ensure the font is actually loaded before consumers measure.
                if (document.fonts && document.fonts.load) {
                    const p = document.fonts.load(`16px "${safeFamily}"`);
                    await Promise.race([
                        p,
                        new Promise((resolve) => setTimeout(resolve, 500))
                    ]);
                }
            } catch (_) { }

            return safeFamily;
        };

        const storeCustomFontFile = async (file, { family, kind } = {}) => {
            if (!file) throw new Error('No font file selected');
            const safeFamily = sanitizeFamily(family || baseNameNoExt(file.name));
            const id = (typeof crypto !== 'undefined' && crypto.randomUUID) ? crypto.randomUUID() : `font_${Date.now()}_${Math.random().toString(16).slice(2)}`;
            const blob = file instanceof Blob ? file : new Blob([file]);
            const record = {
                id,
                kind: String(kind || ''),
                family: safeFamily || baseNameNoExt(file.name) || 'Custom Font',
                fileName: String(file.name || 'font'),
                mime: String(file.type || ''),
                blob,
                createdAt: Date.now()
            };
            await idbPut(record);
            await registerFontFaceFromBlob(record);
            return { id: record.id, family: record.family, fileName: record.fileName, kind: record.kind };
        };

        const deleteCustomFontFile = async (id) => {
            if (!id) return false;
            const existing = registered.get(id);
            if (existing) {
                try { URL.revokeObjectURL(existing.url); } catch (_) { }
                try { existing.styleEl?.remove(); } catch (_) { }
                registered.delete(id);
            }
            return idbDelete(id);
        };

        const ensureCustomFontsLoaded = async (settings) => {
            const editorId = settings?.fonts?.editor?.customFontId || null;
            const terminalId = settings?.fonts?.terminal?.customFontId || null;
            const ids = [editorId, terminalId].filter(Boolean);
            if (!ids.length) return;
            for (const id of ids) {
                if (registered.has(id)) continue;
                const rec = await idbGet(id);
                if (rec?.blob) {
                    await registerFontFaceFromBlob(rec);
                }
            }
        };

        const applyFontsToDocument = (settings) => {
            const root = document.documentElement;
            if (!root) return;

            const uiFamilyRaw = String(settings?.ui?.fontFamily || '').trim();
            const uiSizePx = Number(settings?.ui?.fontSize);

            if (uiFamilyRaw) {
                root.style.setProperty('--font-ui', buildFontStack(uiFamilyRaw, UI_FALLBACK));
            } else {
                try { root.style.removeProperty('--font-ui'); } catch (_) { }
            }

            if (Number.isFinite(uiSizePx) && uiSizePx > 0) {
                root.style.setProperty('--font-size-ui', `${Math.max(10, Math.round(uiSizePx))}px`);
            } else {
                const uiScale = Number(settings?.fonts?.ui?.scalePercent ?? 100) || 100;
                const uiSizeBase = 13;
                const uiSize = Math.max(10, Math.round(uiSizeBase * (uiScale / 100)));
                root.style.setProperty('--font-size-ui', `${uiSize}px`);
            }

            const editorFamily = String(settings?.fonts?.editor?.family || '').trim();
            const editorSize = Number(settings?.fonts?.editor?.sizePx ?? 13) || 13;
            const editorLineHeight = Number(settings?.fonts?.editor?.lineHeight ?? 1.55) || 1.55;
            const editorWeight = String(settings?.fonts?.editor?.weight ?? '400');
            const editorLigatures = !!settings?.fonts?.editor?.ligatures;
            const codeFallback = uniqFamilies([...FALLBACK_EDITOR_FONTS, ...CODE_FALLBACK_TAIL]);
            root.style.setProperty('--font-code', buildFontStack(editorFamily, codeFallback));
            root.style.setProperty('--font-size-code', `${editorSize}px`);
            root.style.setProperty('--line-height-code', String(editorLineHeight));
            root.style.setProperty('--font-weight-code', editorWeight);
            root.style.setProperty('--font-ligatures-code', editorLigatures ? '1' : '0');

            const terminalFamily = String(settings?.fonts?.terminal?.family || '').trim() || editorFamily;
            const terminalSize = Number(settings?.fonts?.terminal?.sizePx ?? 13) || 13;
            root.style.setProperty('--font-terminal', buildFontStack(terminalFamily, codeFallback));
            root.style.setProperty('--font-size-terminal', `${terminalSize}px`);
        };

        const listInstalledFonts = async () => {
            const now = Date.now();
            if (installedCache.list && now - installedCache.ts < 10_000) return installedCache.list;
            if (installedCache.pending) return installedCache.pending;

            installedCache.pending = (async () => {
                const families = new Set(FALLBACK_EDITOR_FONTS);
                try {
                    if (typeof window.queryLocalFonts === 'function') {
                        const localFonts = await window.queryLocalFonts();
                        (localFonts || []).forEach((f) => {
                            const fam = sanitizeFamily(f?.family);
                            if (fam) families.add(fam);
                        });
                    }
                } catch (_) {
                    // permissions / unsupported
                }

                const list = Array.from(families)
                    .filter(Boolean)
                    .sort((a, b) => a.localeCompare(b));

                installedCache.ts = now;
                installedCache.list = list;
                return list;
            })();

            try {
                return await installedCache.pending;
            } finally {
                installedCache.pending = null;
            }
        };

        const registerDynamicFont = async ({ url, fileName }) => {
            const safeFamily = sanitizeFamily(baseNameNoExt(fileName));
            if (!safeFamily || !url) return;

            const existing = registered.get(safeFamily); // Use family as ID for simplicity
            if (existing) return;

            const fmt = guessFormat(fileName);
            const src = fmt ? `url("${url}") format("${fmt}")` : `url("${url}")`;
            const css = `
 @font-face {
   font-family: "${safeFamily}";
   src: ${src};
   font-display: swap;
 }
             `.trim();
            createStyleTag(css);
            registered.set(safeFamily, { url, family: safeFamily, isDynamic: true });

            // Add to fallback list so it appears in dropdowns
            if (!FALLBACK_EDITOR_FONTS.includes(safeFamily)) {
                FALLBACK_EDITOR_FONTS.push(safeFamily);
            }
        };

        return {
            listInstalledFonts,
            applyFontsToDocument,
            ensureCustomFontsLoaded,
            storeCustomFontFile,
            deleteCustomFontFile,
            registerDynamicFont
        };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.services = window.AhmadIDEModules.services || {};

        if (!window.AhmadIDEModules.services.fontService) {
            const svc = createFontService();
            window.AhmadIDEModules.services.fontService = svc;

            // Initial scan for dynamic fonts
            if (typeof window.AhmadIDE?.scanFontsDir === 'function') {
                (async () => {
                    try {
                        const dynamicFonts = await window.AhmadIDE.scanFontsDir();
                        if (Array.isArray(dynamicFonts)) {
                            for (const font of dynamicFonts) {
                                await svc.registerDynamicFont(font);
                            }
                        }
                    } catch (e) {
                        console.error('Dynamic font scan failed:', e);
                    }
                })();
            }
        }
    }
})();
