(() => {
    /**
     * UI Fonts Settings Section
     * Controls for interface font family and scale
     */
    function renderUIFontsGroup(ctx) {
        const { draft, primitives, onDraftChange } = ctx;
        const { createSelect } = primitives;

        const group = document.createElement('div');
        group.className = 'ui-settings-group';
        group.setAttribute('data-filter-text', 'ui font interface font scale size');

        const title = document.createElement('div');
        title.className = 'ui-settings-group__title';
        title.textContent = 'Interface';

        const hint = document.createElement('div');
        hint.className = 'ui-settings-group__hint';
        hint.textContent = 'Controls for the IDE user interface font and scaling';

        group.appendChild(title);
        group.appendChild(hint);

        // UI Font Family
        const fontFamilyRow = document.createElement('div');
        fontFamilyRow.className = 'ui-settings-row';
        fontFamilyRow.setAttribute('data-filter-text', 'ui font family interface');

        const fontLabel = document.createElement('div');
        fontLabel.className = 'ui-settings-row__label';
        fontLabel.textContent = 'Font Family';

        const fontControl = document.createElement('div');
        fontControl.className = 'ui-settings-row__control';

        const fontSelect = createSelect({
            options: [
                { value: 'Inter', label: 'Inter (Default)' },
                { value: 'system-ui', label: 'System UI' },
                { value: '-apple-system', label: 'Apple System' },
                { value: 'Segoe UI', label: 'Segoe UI' },
                { value: 'Roboto', label: 'Roboto' },
                { value: 'Ubuntu', label: 'Ubuntu' },
                { value: 'sans-serif', label: 'Generic Sans-Serif' }
            ],
            value: draft?.ui?.fontFamily || 'Inter',
            onChange: (val) => {
                onDraftChange((d) => {
                    d.ui = d.ui || {};
                    d.ui.fontFamily = val;
                    return d;
                });
            }
        });

        fontControl.appendChild(fontSelect);
        fontFamilyRow.appendChild(fontLabel);
        fontFamilyRow.appendChild(fontControl);
        group.appendChild(fontFamilyRow);

        // UI Font Size (Scale)
        const scaleRow = document.createElement('div');
        scaleRow.className = 'ui-settings-row';
        scaleRow.setAttribute('data-filter-text', 'ui scale size zoom');

        const scaleLabel = document.createElement('div');
        scaleLabel.className = 'ui-settings-row__label';
        scaleLabel.textContent = 'UI Scale';

        const scaleControl = document.createElement('div');
        scaleControl.className = 'ui-settings-row__control';

        const scaleSelect = createSelect({
            options: [
                { value: '11', label: '11px (Small)' },
                { value: '12', label: '12px' },
                { value: '13', label: '13px (Default)' },
                { value: '14', label: '14px' },
                { value: '15', label: '15px' },
                { value: '16', label: '16px (Large)' }
            ],
            value: String(draft?.ui?.fontSize || 13),
            onChange: (val) => {
                onDraftChange((d) => {
                    d.ui = d.ui || {};
                    d.ui.fontSize = parseInt(val, 10);
                    return d;
                });
            }
        });

        scaleControl.appendChild(scaleSelect);
        scaleRow.appendChild(scaleLabel);
        scaleRow.appendChild(scaleControl);
        group.appendChild(scaleRow);

        // Preview
        const preview = document.createElement('div');
        preview.className = 'ui-settings-preview';
        preview.style.cssText = 'margin-top:var(--ui-space-4);';
        const previewText = document.createElement('div');
        previewText.id = 'uiFontPreview';
        previewText.textContent = 'The quick brown fox jumps over the lazy dog';
        preview.appendChild(previewText);
        group.appendChild(preview);

        const applyPreviewStyles = () => {
            const previewEl = document.getElementById('uiFontPreview');
            if (!previewEl) return;
            const family = draft?.ui?.fontFamily || 'Inter';
            const size = draft?.ui?.fontSize || 13;
            previewEl.style.fontFamily = family === 'Inter' ? 'var(--font-ui)' : family;
            previewEl.style.fontSize = `${size}px`;
        };

        return { group, applyPreviewStyles };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.features = window.AhmadIDEModules.features || {};
        window.AhmadIDEModules.features.settings = window.AhmadIDEModules.features.settings || {};
        window.AhmadIDEModules.features.settings.sections = window.AhmadIDEModules.features.settings.sections || {};
        window.AhmadIDEModules.features.settings.sections.renderUIFontsGroup = renderUIFontsGroup;
    }
})();
