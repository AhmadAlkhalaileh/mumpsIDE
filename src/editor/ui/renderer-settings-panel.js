(() => {
    function createSettingsPanelManager({ deps } = {}) {
        const toggleDevTools = deps?.toggleDevTools || (async () => {
            if (window.ahmadIDE && window.ahmadIDE.toggleDevTools) {
                await window.ahmadIDE.toggleDevTools();
            }
        });

        function openSettingsPanel() {
            try {
                window.AhmadIDEModules?.app?.featureRegistry?.ensureById?.('settingsPanel');
            } catch (_) { }
            const panel = document.getElementById('settingsPanel');
            const overlay = document.getElementById('settingsOverlay');
            panel?.classList.remove('hidden');
            overlay?.classList.remove('hidden');
            wireSettingsPanel();
        }

        function closeSettingsPanel() {
            document.getElementById('settingsPanel')?.classList.add('hidden');
            document.getElementById('settingsOverlay')?.classList.add('hidden');
        }

        function wireSettingsPanel() {
            // Settings panel content may be lazy-mounted.
            if (!document.getElementById('closeSettingsBtn')) {
                if (!wireSettingsPanel.__lazyHooked) {
                    const fr = window.AhmadIDEModules?.app?.featureRegistry;
                    fr?.onMounted?.('settingsPanel', () => wireSettingsPanel());
                    wireSettingsPanel.__lazyHooked = true;
                }
                return;
            }
            const panel = document.getElementById('settingsPanel');
            if (panel?.dataset?.wired === '1') return;
            if (panel) panel.dataset.wired = '1';

            document.getElementById('closeSettingsBtn')?.addEventListener('click', closeSettingsPanel);
            document.getElementById('settingsOverlay')?.addEventListener('click', closeSettingsPanel);

            // DevTools toggle
            document.getElementById('toggleDevTools')?.addEventListener('click', async () => {
                await toggleDevTools();
            });
        }

        return {
            openSettingsPanel,
            closeSettingsPanel,
            wireSettingsPanel
        };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.ui = window.AhmadIDEModules.ui || {};
        window.AhmadIDEModules.ui.createSettingsPanelManager = createSettingsPanelManager;
    }
})();
