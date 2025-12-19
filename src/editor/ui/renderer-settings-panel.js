(() => {
    function createSettingsPanelManager({ deps } = {}) {
        const createSettingsDialog = deps?.createSettingsDialog || window.AhmadIDEModules?.features?.settings?.createSettingsDialog;
        const settingsService = deps?.settingsService || window.AhmadIDEModules?.services?.settingsService;
        const fontService = deps?.fontService || window.AhmadIDEModules?.services?.fontService;
        const extensionsService = deps?.extensionsService || window.AhmadIDEModules?.services?.extensionsService;
        const dialogRegistry = deps?.dialogRegistry || window.AhmadIDEModules?.app?.dialogRegistry;

        if (!createSettingsDialog) {
            throw new Error('Settings dialog module missing: src/features/settings/settingsDialog.js');
        }

        const dialog = createSettingsDialog({
            deps: { settingsService, fontService, extensionsService }
        });

        try {
            dialogRegistry?.register?.({
                id: 'settings',
                title: 'Settings',
                open: (payload) => dialog.open(payload)
            });
        } catch (_) { }

        function openSettingsPanel() {
            if (dialogRegistry?.open?.('settings')) return;
            dialog.open();
        }

        function closeSettingsPanel() {
            // No-op: dialog owns close UX (Cancel/OK/Esc).
        }

        function wireSettingsPanel() {
            // Legacy stub. Settings is now a modal dialog.
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
