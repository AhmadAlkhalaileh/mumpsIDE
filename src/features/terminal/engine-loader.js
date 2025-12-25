(() => {
    function defaultLoadScript(src) {
        return new Promise((resolve, reject) => {
            const existing = document.querySelector(`script[data-src="${src}"]`);
            if (existing) {
                if (existing.dataset.loaded === 'true') {
                    resolve();
                    return;
                }
                existing.addEventListener('load', () => resolve());
                existing.addEventListener('error', () => reject(new Error(`Failed to load ${src}`)));
                return;
            }

            // xterm is UMD and will register as AMD if `define.amd` exists (Monaco),
            // which prevents `window.Terminal` from being populated.
            //
            // IMPORTANT: Do NOT remove `window.define` entirely because Monaco's AMD modules
            // depend on it while loading. Instead, temporarily disable only the AMD flag.
            const shouldDisableAmd = /\/xterm(?:\.min)?\.js$/i.test(src) || /\/xterm\.js(\?|#|$)/i.test(src);
            const originalDefine = shouldDisableAmd ? window.define : undefined;
            const originalAmdFlag = shouldDisableAmd && originalDefine ? originalDefine.amd : undefined;
            if (shouldDisableAmd && originalDefine && originalAmdFlag) {
                try { originalDefine.amd = undefined; } catch (_) { }
            }
            const restoreAmd = () => {
                if (!shouldDisableAmd) return;
                if (!originalDefine) return;
                try {
                    if (typeof originalAmdFlag !== 'undefined') {
                        originalDefine.amd = originalAmdFlag;
                    }
                } catch (_) { }
            };

            const s = document.createElement('script');
            s.src = src;
            s.async = true;
            s.dataset.src = src;
            s.onload = () => {
                restoreAmd();
                s.dataset.loaded = 'true';
                resolve();
            };
            s.onerror = () => {
                restoreAmd();
                reject(new Error(`Failed to load ${src}`));
            };
            document.head.appendChild(s);
        });
    }

    function createTerminalEngineLoader({ deps } = {}) {
        const sources = Array.isArray(deps?.sources) ? deps.sources : [];
        const getTerminal = deps?.getTerminal || (() => window.Terminal);
        const setFallbackMode = deps?.setFallbackMode || (() => { });
        const loadScript = deps?.loadScript || defaultLoadScript;

        let terminalEnginePromise = null;

        async function ensureTerminalEngine() {
            if (getTerminal()) {
                setFallbackMode(false);
                return getTerminal();
            }
            if (terminalEnginePromise) return terminalEnginePromise;

            terminalEnginePromise = (async () => {
                let lastErr = null;
                for (const src of sources) {
                    try {
                        await loadScript(src);
                        if (getTerminal()) {
                            setFallbackMode(false);
                            return getTerminal();
                        }
                    } catch (err) {
                        lastErr = err;
                    }
                }
                throw new Error(`Terminal engine unavailable. Install xterm locally. ${lastErr ? lastErr.message : ''}`);
            })();

            try {
                const term = await terminalEnginePromise;
                setFallbackMode(false);
                return term;
            } catch (err) {
                terminalEnginePromise = null;
                setFallbackMode(true);
                throw err;
            }
        }

        return { ensureTerminalEngine };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.features = window.AhmadIDEModules.features || {};
        window.AhmadIDEModules.features.terminal = window.AhmadIDEModules.features.terminal || {};
        window.AhmadIDEModules.features.terminal.createTerminalEngineLoader = createTerminalEngineLoader;
    }
})();
