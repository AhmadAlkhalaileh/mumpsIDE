(() => {
    const STORAGE_KEY = 'ahmadIDE:perfMode';
    const EVENT_NAME = 'mide:perf-mode-change';

    function createPerfModeManager() {
        const getRuntimeHints = () => {
            const runtime = window.ahmadIDE?.runtime || {};
            return {
                isSnap: !!runtime.isSnap,
                enableGpuRequested: !!runtime.enableGpuRequested,
                forceSoftwareRendering: !!runtime.forceSoftwareRendering
            };
        };

        const readSetting = () => {
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
        };

        const writeSetting = (value) => {
            try {
                if (!value || value === 'auto') {
                    window.localStorage?.removeItem(STORAGE_KEY);
                    return;
                }
                window.localStorage?.setItem(STORAGE_KEY, value);
            } catch (_) {
                // ignore
            }
        };

        const computeAutoEnabled = () => {
            const hints = getRuntimeHints();
            if (hints.forceSoftwareRendering) return true;
            if (hints.isSnap && !hints.enableGpuRequested) return true;
            // Safe default: enable perf mode on Linux unless explicitly disabled.
            try {
                const ua = navigator.userAgent || '';
                if (/\bLinux\b/i.test(ua)) return true;
            } catch (_) {
                // ignore
            }
            return false;
        };

        const isEnabled = () => {
            const setting = readSetting();
            if (setting === 'on') return true;
            if (setting === 'off') return false;
            return computeAutoEnabled();
        };

        const apply = () => {
            const enabled = isEnabled();
            document.documentElement?.classList?.toggle('perf-mode', enabled);
            if (document.documentElement) document.documentElement.dataset.perfMode = enabled ? 'on' : 'off';
            document.body?.classList?.toggle('perf-mode', enabled);
            if (document.body) document.body.dataset.perfMode = enabled ? 'on' : 'off';
            try {
                window.dispatchEvent(new CustomEvent(EVENT_NAME, { detail: { enabled } }));
            } catch (_) {
                // ignore
            }
            return enabled;
        };

        const setSetting = (value) => {
            const normalized = String(value || 'auto').toLowerCase().trim();
            if (!['on', 'off', 'auto'].includes(normalized)) return apply();
            writeSetting(normalized);
            return apply();
        };

        const ensureSettingsCard = () => {
            const panel = document.getElementById('settingsPanel');
            if (!panel) return null;
            const grid = panel.querySelector('.connections-grid');
            if (!grid) return null;

            let card = document.getElementById('perfModeSettingsCard');
            if (card) return card;

            card = document.createElement('div');
            card.className = 'connection-card';
            card.id = 'perfModeSettingsCard';
            card.innerHTML = `
                <div class="pane-title">Performance</div>
                <div class="pane-subtitle">Reduce visual effects for speed</div>
                <label class="perf-mode-toggle-row">
                    <input type="checkbox" id="perfModeToggle">
                    <span>Performance Mode (reduce effects)</span>
                </label>
                <div class="pane-subtitle" id="perfModeHint"></div>
                <button class="btn ghost" id="perfModeAutoBtn" title="Use Auto mode">Use Auto</button>
            `;

            const first = grid.querySelector('.connection-card');
            if (first && first.parentElement === grid) {
                grid.insertBefore(card, first.nextSibling);
            } else {
                grid.appendChild(card);
            }
            return card;
        };

        const wireSettingsToggle = () => {
            const card = ensureSettingsCard();
            if (!card) return false;
            const toggle = card.querySelector('#perfModeToggle');
            const hint = card.querySelector('#perfModeHint');
            const autoBtn = card.querySelector('#perfModeAutoBtn');
            if (!toggle) return false;

            const refreshUi = () => {
                const setting = readSetting();
                const enabled = isEnabled();
                toggle.checked = enabled;
                const hints = getRuntimeHints();
                const autoActive = setting === 'auto' && enabled;
                const autoReason = hints.forceSoftwareRendering
                    ? 'Auto: software rendering detected'
                    : (hints.isSnap && !hints.enableGpuRequested)
                        ? 'Auto: Snap build (GPU disabled by default)'
                        : 'Auto: not active';
                if (hint) {
                    hint.textContent = setting === 'auto'
                        ? autoReason
                        : `Manual: ${setting.toUpperCase()}`;
                }
                if (autoBtn) autoBtn.style.display = setting === 'auto' ? 'none' : 'inline-flex';
                card.dataset.perfMode = enabled ? 'on' : 'off';
                card.dataset.perfModeSetting = setting;
                card.dataset.perfModeAuto = autoActive ? '1' : '0';
            };

            if (!toggle.dataset.wired) {
                toggle.addEventListener('change', () => {
                    setSetting(toggle.checked ? 'on' : 'off');
                    refreshUi();
                });
                autoBtn?.addEventListener('click', () => {
                    setSetting('auto');
                    refreshUi();
                });
                toggle.dataset.wired = '1';
            }

            refreshUi();
            return true;
        };

        return {
            readSetting,
            setSetting,
            isEnabled,
            apply,
            wireSettingsToggle
        };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.app = window.AhmadIDEModules.app || {};
        window.AhmadIDEModules.app.createPerfModeManager = createPerfModeManager;
        // Install a singleton so other modules can rely on it without manual wiring.
        if (!window.AhmadIDEModules.app.perfMode) {
            window.AhmadIDEModules.app.perfMode = createPerfModeManager();
            // Apply immediately (class may already be set by perf-mode-early.js; this keeps it consistent)
            window.AhmadIDEModules.app.perfMode.apply();
            // Wire settings toggle once the DOM exists.
            const wire = () => window.AhmadIDEModules.app.perfMode.wireSettingsToggle();
            // Settings panel may be lazy-mounted.
            try {
                window.AhmadIDEModules?.app?.featureRegistry?.onMounted?.('settingsPanel', wire);
            } catch (_) { }
            if (document.readyState === 'loading') {
                document.addEventListener('DOMContentLoaded', wire, { once: true });
            } else {
                setTimeout(wire, 0);
            }
        }
    }
})();
