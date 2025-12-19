(() => {
    const ext = {
        id: 'mumps-tools',
        name: 'MUMPS Tools (Built-in)',
        version: '0.1.0',
        description: 'Language tooling helpers for MUMPS (navigation, formatting, lint hooks).',
        defaultEnabled: true,
        contributes: {
            settings: [
                { id: 'mumps', title: 'MUMPS' }
            ]
        },
        activate: async () => {
            // Core MUMPS features are wired in core renderer for now.
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

