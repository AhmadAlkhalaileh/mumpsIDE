(() => {
    function createExtensionsManager({ deps } = {}) {
        function initExtensionsView() {
            const createExtensionsToolWindow = window.AhmadIDEModules?.features?.extensions?.createExtensionsToolWindow;
            if (!createExtensionsToolWindow) {
                throw new Error('Extensions UI missing: src/features/extensions/extensionsToolWindow.js');
            }
            const tool = createExtensionsToolWindow();
            if (tool.mount()) return;
            const featureRegistry = window.AhmadIDEModules?.app?.featureRegistry;
            featureRegistry?.onMounted?.('extensionsPanel', () => tool.mount());
        }

        return {
            initExtensionsView
        };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.extensions = window.AhmadIDEModules.extensions || {};
        window.AhmadIDEModules.extensions.createExtensionsManager = createExtensionsManager;
    }
})();
