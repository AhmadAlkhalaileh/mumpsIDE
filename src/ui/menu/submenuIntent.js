(() => {
    // Submenu intention: openDelay + closeDelay + grace zone/tunnel.
    function createSubmenuIntent(opts = {}) {
        const clamp = (v, min, max, fallback) => {
            const n = Number(v);
            if (!Number.isFinite(n)) return fallback;
            return Math.max(min, Math.min(max, n));
        };

        // Phase 3A spec: openDelay 80–140ms, closeDelay 250–400ms.
        const openDelayMs = clamp(opts.openDelayMs ?? 120, 80, 140, 120);
        const closeDelayMs = clamp(opts.closeDelayMs ?? 320, 250, 400, 320);
        const gracePx = Math.max(0, Number(opts.gracePx ?? 16) || 16);

        return {
            openDelayMs,
            closeDelayMs,
            gracePx
        };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.ui = window.AhmadIDEModules.ui || {};
        window.AhmadIDEModules.ui.menu = window.AhmadIDEModules.ui.menu || {};
        window.AhmadIDEModules.ui.menu.createSubmenuIntent = createSubmenuIntent;
    }
})();
