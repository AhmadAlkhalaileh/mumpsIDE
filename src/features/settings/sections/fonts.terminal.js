(() => {
    function renderTerminalFontsGroup(ctx) {
        const { draft, primitives, fontService, onDraftChange } = ctx;
        const shared = window.AhmadIDEModules?.features?.settings?.sections?.fontsShared;
        if (!shared) throw new Error('fontsShared missing: src/features/settings/sections/fonts.shared.js');

        const RECOMMENDED_CODE_FONTS = [
            'Cascadia Mono',
            'Fira Code',
            'JetBrains Mono',
            'Source Code Pro',
            'IBM Plex Mono',
            'Ubuntu Mono',
            'DejaVu Sans Mono',
            'Menlo',
            'Monaco',
            'Consolas',
            'Liberation Mono',
            'Courier New',
            'monospace'
        ];

        const group = document.createElement('div');
        group.className = 'ui-settings-group ui-fonts-group';
        group.dataset.filterText = 'terminal font xterm shell console';

        const header = document.createElement('div');
        header.className = 'ui-fonts-group__header';
        const title = document.createElement('div');
        title.className = 'ui-settings-group__title';
        title.textContent = 'Terminal';
        const hint = document.createElement('div');
        hint.className = 'ui-settings-group__hint';
        hint.textContent = 'Used by the built-in terminal (xterm)';
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

        // Terminal: family
        {
            controlsCol.appendChild(subsection('Family', 'Pick a font or upload your own font file'));
            const row = document.createElement('div');
            row.className = 'ui-settings-row';
            row.dataset.filterText = 'terminal font family typeface';
            const label = document.createElement('div');
            label.className = 'ui-settings-row__label';
            label.textContent = 'Font family';

            const { host, removeBtn } = shared.createFontFamilyControl({
                primitives,
                fontService,
                value: draft?.fonts?.terminal?.family,
                customFontId: draft?.fonts?.terminal?.customFontId,
                suggestedFonts: RECOMMENDED_CODE_FONTS,
                onValue: (v) => onDraftChange((d) => { d.fonts.terminal.family = v; return d; }),
                onUploadFontFile: async (file) => {
                    const saved = await fontService.storeCustomFontFile(file, { kind: 'terminal' });
                    onDraftChange((d) => {
                        d.fonts.terminal.customFontId = saved.id;
                        d.fonts.terminal.family = `${saved.family}, ${d.fonts.terminal.family || ''}`.replace(/,\s*$/, '');
                        return d;
                    });
                    removeBtn.disabled = false;
                },
                onRemoveFontFile: async () => {
                    const id = draft?.fonts?.terminal?.customFontId;
                    if (id) await fontService.deleteCustomFontFile(id);
                    onDraftChange((d) => {
                        d.fonts.terminal.customFontId = null;
                        const current = String(d.fonts.terminal.family || '');
                        const removeFamily = String(draft?.fonts?.terminal?.family || '').split(',')[0].trim();
                        if (removeFamily) {
                            const parts = current.split(',').map(s => s.trim()).filter(Boolean);
                            const cleaned = parts.filter((p) => p.toLowerCase() !== removeFamily.toLowerCase());
                            d.fonts.terminal.family = cleaned.join(', ');
                        }
                        return d;
                    });
                    removeBtn.disabled = true;
                }
            });

            row.appendChild(label);
            row.appendChild(host);
            controlsCol.appendChild(row);
        }

        // Terminal: size
        controlsCol.appendChild(subsection('Typography', 'Adjust terminal size'));

        controlsCol.appendChild(shared.createNumberRow({
            primitives,
            labelText: 'Font size (px)',
            filterText: 'terminal font size',
            value: draft?.fonts?.terminal?.sizePx ?? 13,
            min: 10,
            max: 40,
            step: 1,
            onValue: (n) => onDraftChange((d) => { d.fonts.terminal.sizePx = n; return d; })
        }).row);

        const preview = document.createElement('div');
        preview.className = 'ui-settings-preview ui-fonts-preview';
        preview.dataset.filterText = 'preview terminal';
        const pre = document.createElement('pre');
        pre.textContent = '$ echo \"Hello\"\\nHello\\n$';
        preview.appendChild(pre);
        previewCol.appendChild(preview);

        const applyPreviewStyles = () => {
            const ef = String(draft?.fonts?.editor?.family || '');
            const tf = String(draft?.fonts?.terminal?.family || ef);
            const ts = Number(draft?.fonts?.terminal?.sizePx ?? 13) || 13;
            pre.style.fontFamily = tf;
            pre.style.fontSize = `${ts}px`;
            pre.style.lineHeight = '1.25';
        };

        return { group, applyPreviewStyles };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.features = window.AhmadIDEModules.features || {};
        window.AhmadIDEModules.features.settings = window.AhmadIDEModules.features.settings || {};
        window.AhmadIDEModules.features.settings.sections = window.AhmadIDEModules.features.settings.sections || {};
        window.AhmadIDEModules.features.settings.sections.renderTerminalFontsGroup = renderTerminalFontsGroup;
    }
})();
