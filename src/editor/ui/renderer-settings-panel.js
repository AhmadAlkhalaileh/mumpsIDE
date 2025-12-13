(() => {
    function createSettingsPanelManager({ deps } = {}) {
        const toggleDevTools = deps?.toggleDevTools || (async () => {
            if (window.ahmadIDE && window.ahmadIDE.toggleDevTools) {
                await window.ahmadIDE.toggleDevTools();
            }
        });

        function openSettingsPanel() {
            const panel = document.getElementById('settingsPanel');
            const overlay = document.getElementById('settingsOverlay');
            panel?.classList.remove('hidden');
            overlay?.classList.remove('hidden');
        }

        function closeSettingsPanel() {
            document.getElementById('settingsPanel')?.classList.add('hidden');
            document.getElementById('settingsOverlay')?.classList.add('hidden');
        }

        function wireSettingsPanel() {
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
