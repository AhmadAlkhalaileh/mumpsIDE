(() => {
    function renderAppearanceSection(ctx) {
        const { draft, primitives, onDraftChange } = ctx;
        const { createInput } = primitives;

        const root = document.createElement('div');

        const group = document.createElement('div');
        group.className = 'ui-settings-group';
        group.innerHTML = `
            <div class="ui-settings-group__title">Appearance</div>
            <div class="ui-settings-group__hint">Adjust UI scale (optional)</div>
        `;

        const row = document.createElement('div');
        row.className = 'ui-settings-row';
        row.dataset.filterText = 'ui scale font size appearance';

        const label = document.createElement('div');
        label.className = 'ui-settings-row__label';
        label.textContent = 'UI scale (%)';

        const control = document.createElement('div');
        control.className = 'ui-settings-row__control';
        const input = createInput({
            type: 'number',
            value: String(draft?.fonts?.ui?.scalePercent ?? 100),
            onInput: () => {
                const n = Number(input.value);
                onDraftChange((d) => {
                    d.fonts = d.fonts || {};
                    d.fonts.ui = d.fonts.ui || {};
                    d.fonts.ui.scalePercent = Number.isFinite(n) ? n : 100;
                    return d;
                });
            }
        });
        input.min = '70';
        input.max = '150';
        input.step = '5';
        input.style.maxWidth = '120px';

        control.appendChild(input);

        row.appendChild(label);
        row.appendChild(control);
        group.appendChild(row);

        const hint = document.createElement('div');
        hint.className = 'ui-settings-group__hint';
        hint.style.marginTop = '8px';
        hint.textContent = 'Applies to most UI text after Apply/OK.';
        group.appendChild(hint);

        root.appendChild(group);
        return root;
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.features = window.AhmadIDEModules.features || {};
        window.AhmadIDEModules.features.settings = window.AhmadIDEModules.features.settings || {};
        window.AhmadIDEModules.features.settings.sections = window.AhmadIDEModules.features.settings.sections || {};
        window.AhmadIDEModules.features.settings.sections.renderAppearanceSection = renderAppearanceSection;
    }
})();

