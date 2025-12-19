(() => {
    function createRafCoalescer(fn) {
        let raf = 0;
        let pending = false;
        let lastArgs = null;

        const flush = () => {
            raf = 0;
            if (!pending) return;
            pending = false;
            try {
                fn.apply(null, lastArgs || []);
            } catch (_) {
                // ignore scheduler errors
            } finally {
                lastArgs = null;
            }
        };

        return (...args) => {
            lastArgs = args;
            pending = true;
            if (raf) return;
            raf = (typeof requestAnimationFrame === 'function')
                ? requestAnimationFrame(flush)
                : setTimeout(flush, 16);
        };
    }

    function installMonacoCreatePatch(monaco) {
        if (!monaco?.editor?.create || monaco.editor.create.__midePatched) return;

        const originalCreate = monaco.editor.create.bind(monaco.editor);
        monaco.editor.create = function patchedCreate(dom, options, override) {
            const editor = originalCreate(dom, options, override);
            try {
                if (editor && typeof editor.layout === 'function' && !editor.layout.__midePatched) {
                    const originalLayout = editor.layout.bind(editor);
                    const scheduleLayout = createRafCoalescer((dimension) => originalLayout(dimension));

                    const wrappedLayout = (dimension) => scheduleLayout(dimension);
                    wrappedLayout.__midePatched = true;
                    editor.layout = wrappedLayout;

                    // Expose a stable scheduler for other modules (no coupling to renderer.js locals)
                    window.MIDE = window.MIDE || {};
                    window.MIDE.editor = editor;
                    window.MIDE.scheduleEditorLayout = (reason) => {
                        try {
                            window.MIDE.lastLayoutReason = reason || 'unknown';
                        } catch (_) { }
                        scheduleLayout();
                    };

                    // Best-effort: schedule a layout after tool window toggles (class changes)
                    const scheduleSoon = () => window.MIDE.scheduleEditorLayout('tool-window-toggle');
                    const onClickCapture = (e) => {
                        const t = e.target;
                        if (!t?.closest) return;
                        if (
                            t.closest('.tool-window-stripe-btn') ||
                            t.closest('.hide-panel-btn') ||
                            t.closest('#hideProjectBtn')
                        ) {
                            scheduleSoon();
                        }
                    };
                    document.addEventListener('click', onClickCapture, true);

                    // Also watch the editor host for size changes (covers split/panel toggles)
                    const host = document.getElementById('editor');
                    if (host && typeof ResizeObserver !== 'undefined') {
                        const ro = new ResizeObserver(() => window.MIDE.scheduleEditorLayout('editor-resize'));
                        ro.observe(host);
                    }
                }
            } catch (_) {
                // ignore patch errors
            }
            return editor;
        };
        monaco.editor.create.__midePatched = true;
        monaco.editor.create.__mideOriginal = originalCreate;
    }

    function tryInstall() {
        if (window.monaco?.editor?.create) {
            installMonacoCreatePatch(window.monaco);
        }
    }

    // Wrap AMD require so we patch Monaco immediately before renderer.js creates the editor.
    function wrapRequire() {
        const req = window.require;
        if (typeof req !== 'function' || req.__mideWrapped) return false;

        function wrappedRequire(deps, cb, err) {
            if (Array.isArray(deps) && deps.includes('vs/editor/editor.main') && typeof cb === 'function') {
                const wrappedCb = (...args) => {
                    tryInstall();
                    return cb(...args);
                };
                return req.call(this, deps, wrappedCb, err);
            }
            return req.call(this, deps, cb, err);
        }

        // Preserve require.config and any properties Monaco expects.
        Object.keys(req).forEach((k) => {
            try { wrappedRequire[k] = req[k]; } catch (_) { }
        });
        wrappedRequire.__mideWrapped = true;
        window.require = wrappedRequire;
        return true;
    }

    // Install now if possible; otherwise poll briefly (Monaco loader is injected dynamically).
    if (!wrapRequire()) {
        let tries = 0;
        const timer = setInterval(() => {
            tries += 1;
            if (wrapRequire() || tries > 40) {
                clearInterval(timer);
            }
        }, 25);
    }
})();

