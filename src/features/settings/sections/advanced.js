(() => {
    function renderAdvancedSection(ctx) {
        const { primitives } = ctx;
        const { createButton } = primitives;

        const root = document.createElement('div');

        const group = document.createElement('div');
        group.className = 'ui-settings-group';
        group.innerHTML = `
            <div class="ui-settings-group__title">Advanced</div>
            <div class="ui-settings-group__hint">Developer utilities</div>
        `;

        const row = document.createElement('div');
        row.className = 'ui-settings-row';
        row.dataset.filterText = 'devtools developer tools';
        row.innerHTML = `<div class="ui-settings-row__label">Developer Tools</div>`;
        const control = document.createElement('div');
        control.className = 'ui-settings-row__control';

        const toggle = createButton({
            label: 'Toggle DevTools',
            variant: 'primary',
            onClick: async () => {
                try {
                    await window.ahmadIDE?.toggleDevTools?.();
                } catch (_) { }
            }
        });
        control.appendChild(toggle);
        row.appendChild(control);
        group.appendChild(row);

        root.appendChild(group);
        return root;
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.features = window.AhmadIDEModules.features || {};
        window.AhmadIDEModules.features.settings = window.AhmadIDEModules.features.settings || {};
        window.AhmadIDEModules.features.settings.sections = window.AhmadIDEModules.features.settings.sections || {};
        window.AhmadIDEModules.features.settings.sections.renderAdvancedSection = renderAdvancedSection;
    }
})();

