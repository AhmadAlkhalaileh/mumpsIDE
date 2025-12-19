(() => {
    const STORAGE_KEY = 'ahmadIDE:perfMode';

    function readSetting() {
        try {
            const raw = window.localStorage?.getItem(STORAGE_KEY);
            if (!raw) return 'auto';
            if (raw === '1') return 'on';
            if (raw === '0') return 'off';
            const normalized = String(raw).toLowerCase().trim();
            if (normalized === 'on' || normalized === 'off' || normalized === 'auto') return normalized;
        } catch (_) {
            // ignore
        }
        return 'auto';
    }

    function getRuntimeHints() {
        const runtime = window.ahmadIDE?.runtime || {};
        return {
            isSnap: !!runtime.isSnap,
            enableGpuRequested: !!runtime.enableGpuRequested,
            forceSoftwareRendering: !!runtime.forceSoftwareRendering
        };
    }

    function computeAutoEnabled() {
        const hints = getRuntimeHints();
        if (hints.forceSoftwareRendering) return true;
        if (hints.isSnap && !hints.enableGpuRequested) return true;
        // Safe default: Linux builds often run under software rendering or weak GPU setups.
        // Users can always disable via Settings → Performance Mode.
        try {
            const ua = navigator.userAgent || '';
            if (/\bLinux\b/i.test(ua)) return true;
        } catch (_) {
            // ignore
        }
        return false;
    }

    function applyPerfClass(enabled) {
        const on = !!enabled;
        const root = document.documentElement;
        if (root) {
            root.classList.toggle('perf-mode', on);
            root.dataset.perfMode = on ? 'on' : 'off';
        }

        const applyBody = () => {
            const body = document.body;
            if (!body) return false;
            body.classList.toggle('perf-mode', on);
            body.dataset.perfMode = on ? 'on' : 'off';
            return true;
        };

        if (!applyBody()) {
            // Ensure <body> receives the class as soon as it exists (for CSS selectors like body.perf-mode).
            if (document.readyState === 'loading') {
                document.addEventListener('DOMContentLoaded', applyBody, { once: true });
            } else {
                setTimeout(applyBody, 0);
            }
        }
    }

    const setting = readSetting();
    const enabled = setting === 'on' ? true : setting === 'off' ? false : computeAutoEnabled();
    applyPerfClass(enabled);
})();
