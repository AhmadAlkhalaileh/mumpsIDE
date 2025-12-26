(() => {
    /**
     * UI Fonts Settings Section
     * Controls for interface font family and scale
     */
    function renderUIFontsGroup(ctx) {
        const { draft, primitives, fontService, onDraftChange } = ctx;
        const shared = window.AhmadIDEModules?.features?.settings?.sections?.fontsShared;
        if (!shared) throw new Error('fontsShared missing: src/features/settings/sections/fonts.shared.js');

        const RECOMMENDED_UI_FONTS = [
            'Inter',
            'system-ui',
            '-apple-system',
            'Segoe UI',
            'SF Pro Display',
            'Roboto',
            'Ubuntu',
            'sans-serif'
        ];

        const group = document.createElement('div');
        group.className = 'ui-settings-group ui-fonts-group';
        group.setAttribute('data-filter-text', 'ui font interface scale size');

        const header = document.createElement('div');
        header.className = 'ui-fonts-group__header';

        const title = document.createElement('div');
        title.className = 'ui-settings-group__title';
        title.textContent = 'Interface';

        const hint = document.createElement('div');
        hint.className = 'ui-settings-group__hint';
        hint.textContent = 'Controls for the IDE user interface font and scale';

        header.appendChild(title);
        header.appendChild(hint);
        group.appendChild(header);

        const grid = document.createElement('div');
        grid.className = 'ui-fonts-grid';
        const controlsCol = document.createElement('div');
        controlsCol.className = 'ui-fonts-grid__controls';
        const previewCol = document.createElement('div');
        previewCol.className = 'ui-fonts-grid__preview';
        grid.appendChild(controlsCol);
        grid.appendChild(previewCol);
        group.appendChild(grid);

        const subsection = (t, h) => {
            const wrap = document.createElement('div');
            wrap.className = 'ui-settings-subsection';
            const tt = document.createElement('div');
            tt.className = 'ui-settings-subsection__title';
            tt.textContent = t;
            wrap.appendChild(tt);
            if (h) {
                const hh = document.createElement('div');
                hh.className = 'ui-settings-subsection__hint';
                hh.textContent = h;
                wrap.appendChild(hh);
            }
            return wrap;
        };

        // UI Font Family
        controlsCol.appendChild(subsection('Family', 'Pick a UI font (installed fonts are loaded automatically)'));
        const fontFamilyRow = document.createElement('div');
        fontFamilyRow.className = 'ui-settings-row';
        fontFamilyRow.setAttribute('data-filter-text', 'ui font family interface');

        const fontLabel = document.createElement('div');
        fontLabel.className = 'ui-settings-row__label';
        fontLabel.textContent = 'Font family';

        const { host: familyHost } = shared.createFontFamilyControl({
            primitives,
            fontService,
            value: draft?.ui?.fontFamily || 'Inter',
            customFontId: null,
            suggestedFonts: RECOMMENDED_UI_FONTS,
            allowFontFiles: false,
            onValue: (v) => onDraftChange((d) => {
                d.ui = d.ui || {};
                d.ui.fontFamily = v;
                return d;
            })
        });

        fontFamilyRow.appendChild(fontLabel);
        fontFamilyRow.appendChild(familyHost);
        controlsCol.appendChild(fontFamilyRow);

        // UI Scale
        controlsCol.appendChild(subsection('Scale', 'Adjust overall UI text size'));
        const scaleRow = document.createElement('div');
        scaleRow.className = 'ui-settings-row';
        scaleRow.setAttribute('data-filter-text', 'ui scale size zoom');

        const scaleLabel = document.createElement('div');
        scaleLabel.className = 'ui-settings-row__label';
        scaleLabel.textContent = 'UI scale (%)';

        const scaleControl = document.createElement('div');
        scaleControl.className = 'ui-settings-row__control';

        const range = document.createElement('input');
        range.type = 'range';
        range.className = 'ui-range';
        range.min = '70';
        range.max = '150';
        range.step = '5';
        range.value = String(draft?.fonts?.ui?.scalePercent ?? 100);

        const number = primitives.createInput({
            type: 'number',
            value: String(draft?.fonts?.ui?.scalePercent ?? 100),
            onInput: () => {
                const n = Number(number.value);
                const next = Number.isFinite(n) ? n : 100;
                range.value = String(next);
                onDraftChange((d) => {
                    d.fonts = d.fonts || {};
                    d.fonts.ui = d.fonts.ui || {};
                    d.fonts.ui.scalePercent = next;
                    return d;
                });
            }
        });
        number.min = '70';
        number.max = '150';
        number.step = '5';
        number.style.maxWidth = '110px';

        range.addEventListener('input', () => {
            number.value = range.value;
            const n = Number(range.value);
            onDraftChange((d) => {
                d.fonts = d.fonts || {};
                d.fonts.ui = d.fonts.ui || {};
                d.fonts.ui.scalePercent = Number.isFinite(n) ? n : 100;
                return d;
            });
        });

        scaleControl.appendChild(range);
        scaleControl.appendChild(number);
        scaleRow.appendChild(scaleLabel);
        scaleRow.appendChild(scaleControl);
        controlsCol.appendChild(scaleRow);

        // Preview
        const preview = document.createElement('div');
        preview.className = 'ui-settings-preview ui-fonts-preview';
        preview.style.cssText = 'display:flex;flex-direction:column;gap:8px;';

        const row1 = document.createElement('div');
        row1.style.cssText = 'font-weight:600;';
        row1.textContent = 'Settings  •  Fonts';

        const row2 = document.createElement('div');
        row2.style.cssText = 'opacity:0.85;';
        row2.textContent = 'Preview: Interface text, buttons, and labels';

        const row3 = document.createElement('div');
        row3.style.cssText = 'opacity:0.75;';
        row3.textContent = 'The quick brown fox jumps over the lazy dog 0123456789';

        preview.appendChild(row1);
        preview.appendChild(row2);
        preview.appendChild(row3);
        previewCol.appendChild(preview);

        const applyPreviewStyles = () => {
            const family = String(draft?.ui?.fontFamily || 'Inter');
            const scale = Number(draft?.fonts?.ui?.scalePercent ?? 100) || 100;
            const base = 13;
            const size = Math.max(10, Math.round(base * (scale / 100)));
            preview.style.fontFamily = family;
            preview.style.fontSize = `${size}px`;
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
