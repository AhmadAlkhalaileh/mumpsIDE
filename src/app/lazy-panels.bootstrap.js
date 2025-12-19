(() => {
    function safeArray(value) {
        return Array.isArray(value) ? value : [];
    }

    function registerPanelTemplates(featureRegistry, panelTemplates) {
        Object.keys(panelTemplates || {}).forEach((id) => {
            featureRegistry.register(id, { html: panelTemplates[id] });
        });
    }

    function mountPanels(featureRegistry, ids) {
        safeArray(ids).forEach((id) => {
            try {
                featureRegistry.ensureById(id);
            } catch (_) {
                // ignore mount failures
            }
        });
    }

    function mountOnFirstShow(featureRegistry, panelId) {
        const el = document.getElementById(panelId);
        if (!el) return;

        // Already visible (or already mounted)
        if (!el.classList.contains('hidden')) {
            featureRegistry.ensureById(panelId);
            return;
        }

        const obs = new MutationObserver(() => {
            // Mount synchronously as soon as it becomes visible
            if (!el.classList.contains('hidden')) {
                featureRegistry.ensureById(panelId);
                obs.disconnect();
            }
        });

        obs.observe(el, { attributes: true, attributeFilter: ['class'] });
    }

    function bootstrap() {
        if (bootstrap.__done) return;
        bootstrap.__done = true;
        const featureRegistry = window.AhmadIDEModules?.app?.featureRegistry;
        const panelTemplates = window.AhmadIDEModules?.app?.panelTemplates;
        if (!featureRegistry || !panelTemplates) return;

        registerPanelTemplates(featureRegistry, panelTemplates);

        // Lazy-mount heavy panels on first show (or immediately if already visible).
        [
            'terminalPanel',
            'debugPanel',
            'problemsPanel',
            'gitToolPanel',
            'extensionsPanel',
            'servicesPanel',
            'settingsPanel',
            'connectionsPanel',
            'newProjectPanel',
            'findDialog',
            'searchEverywhereDialog'
        ].forEach((id) => mountOnFirstShow(featureRegistry, id));
    }

    // These scripts are loaded at the end of <body>; run immediately so panels can be
    // mounted before renderer.js tries to wire/init them (no DOMContentLoaded delay).
    bootstrap();
    if (document.readyState === 'loading') {
        document.addEventListener('DOMContentLoaded', bootstrap, { once: true });
    }
})();
