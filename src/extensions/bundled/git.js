(() => {
    const ext = {
        id: 'git',
        name: 'Git (Built-in)',
        version: '0.1.0',
        description: 'Provides Git integrations (UI in Phase 3 Step 7).',
        defaultEnabled: true,
        contributes: {
            toolWindows: [
                { id: 'git', title: 'Git' }
            ]
        },
        activate: async () => {
            // UI is intentionally implemented later (Phase 3 Step 7).
            return [];
        },
        deactivate: async () => { }
    };

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.extensions = window.AhmadIDEModules.extensions || {};
        window.AhmadIDEModules.extensions.bundled = window.AhmadIDEModules.extensions.bundled || [];
        window.AhmadIDEModules.extensions.bundled.push(ext);
        try {
            window.AhmadIDEModules?.services?.extensionsService?.register?.(ext);
        } catch (_) { }
    }
})();

