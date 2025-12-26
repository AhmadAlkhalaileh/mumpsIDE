(() => {
    function renderEditorFontsGroup(ctx) {
        const { draft, primitives, fontService, onDraftChange } = ctx;
        const { createSelect, createToggle } = primitives;
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
        group.dataset.filterText = 'editor font monaco code ligatures line height weight';

        const header = document.createElement('div');
        header.className = 'ui-fonts-group__header';

        const title = document.createElement('div');
        title.className = 'ui-settings-group__title';
        title.textContent = 'Editor';

        const hint = document.createElement('div');
        hint.className = 'ui-settings-group__hint';
        hint.textContent = 'Used by Monaco editor (code view)';

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

        // Editor: family
        {
            controlsCol.appendChild(subsection('Family', 'Pick a font or upload your own font file'));
            const row = document.createElement('div');
            row.className = 'ui-settings-row';
            row.dataset.filterText = 'editor font family typeface';
            const label = document.createElement('div');
            label.className = 'ui-settings-row__label';
            label.textContent = 'Font family';

            const { host, removeBtn } = shared.createFontFamilyControl({
                primitives,
                fontService,
                value: draft?.fonts?.editor?.family,
                customFontId: draft?.fonts?.editor?.customFontId,
                suggestedFonts: RECOMMENDED_CODE_FONTS,
                onValue: (v) => onDraftChange((d) => { d.fonts.editor.family = v; return d; }),
                onUploadFontFile: async (file) => {
                    const saved = await fontService.storeCustomFontFile(file, { kind: 'editor' });
                    onDraftChange((d) => {
                        d.fonts.editor.customFontId = saved.id;
                        d.fonts.editor.family = `${saved.family}, ${d.fonts.editor.family || ''}`.replace(/,\s*$/, '');
                        return d;
                    });
                    removeBtn.disabled = false;
                },
                onRemoveFontFile: async () => {
                    const id = draft?.fonts?.editor?.customFontId;
                    if (id) await fontService.deleteCustomFontFile(id);
                    onDraftChange((d) => {
                        d.fonts.editor.customFontId = null;
                        const current = String(d.fonts.editor.family || '');
                        const removeFamily = String(draft?.fonts?.editor?.family || '').split(',')[0].trim();
                        if (removeFamily) {
                            const parts = current.split(',').map(s => s.trim()).filter(Boolean);
                            const cleaned = parts.filter((p) => p.toLowerCase() !== removeFamily.toLowerCase());
                            d.fonts.editor.family = cleaned.join(', ');
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

        // Editor: size / line height
        controlsCol.appendChild(subsection('Typography', 'Tweak size, spacing, and rendering'));

        controlsCol.appendChild(shared.createNumberRow({
            primitives,
            labelText: 'Font size (px)',
            filterText: 'editor font size',
            value: draft?.fonts?.editor?.sizePx ?? 13,
            min: 10,
            max: 40,
            step: 1,
            onValue: (n) => onDraftChange((d) => { d.fonts.editor.sizePx = n; return d; })
        }).row);

        controlsCol.appendChild(shared.createNumberRow({
            primitives,
            labelText: 'Line height (x)',
            filterText: 'editor line height',
            value: draft?.fonts?.editor?.lineHeight ?? 1.55,
            min: 1.1,
            max: 2.2,
            step: 0.05,
            onValue: (n) => onDraftChange((d) => { d.fonts.editor.lineHeight = n; return d; })
        }).row);

        // Editor: weight
        {
            const row = document.createElement('div');
            row.className = 'ui-settings-row';
            row.dataset.filterText = 'editor font weight';
            row.innerHTML = `<div class="ui-settings-row__label">Font weight</div>`;
            const control = document.createElement('div');
            control.className = 'ui-settings-row__control';
            const weights = ['300', '400', '500', '600', '700'];
            const weightSelect = createSelect({
                value: String(draft?.fonts?.editor?.weight ?? '400'),
                options: weights.map((w) => ({ value: w, label: w }))
            });
            weightSelect.addEventListener('change', () => onDraftChange((d) => { d.fonts.editor.weight = weightSelect.value; return d; }));
            control.appendChild(weightSelect);
            row.appendChild(control);
            controlsCol.appendChild(row);
        }

        // Editor: ligatures
        {
            const row = document.createElement('div');
            row.className = 'ui-settings-row';
            row.dataset.filterText = 'editor ligatures';
            row.innerHTML = `<div class="ui-settings-row__label">Ligatures</div>`;
            const toggle = createToggle({
                label: 'Enable font ligatures',
                checked: !!draft?.fonts?.editor?.ligatures,
                onChange: (_e, checked) => onDraftChange((d) => { d.fonts.editor.ligatures = checked; return d; })
            });
            const control = document.createElement('div');
            control.className = 'ui-settings-row__control';
            control.appendChild(toggle.root);
            row.appendChild(control);
            controlsCol.appendChild(row);
        }

        const preview = document.createElement('div');
        preview.className = 'ui-settings-preview ui-fonts-preview';
        preview.dataset.filterText = 'preview editor';
        const pre = document.createElement('pre');
        pre.textContent = [
            'HELLO ; sample routine',
            '    SET X=1',
            '    WRITE \"Hello from Settings\",!',
            '    QUIT'
        ].join('\n');
        preview.appendChild(pre);
        previewCol.appendChild(preview);

        const applyPreviewStyles = () => {
            const ef = String(draft?.fonts?.editor?.family || '');
            const es = Number(draft?.fonts?.editor?.sizePx ?? 13) || 13;
            const lh = Number(draft?.fonts?.editor?.lineHeight ?? 1.55) || 1.55;
            const ew = String(draft?.fonts?.editor?.weight ?? '400');
            pre.style.fontFamily = ef;
            pre.style.fontSize = `${es}px`;
            pre.style.lineHeight = String(lh);
            pre.style.fontWeight = ew;
            pre.style.fontVariantLigatures = draft?.fonts?.editor?.ligatures ? 'normal' : 'none';
        };

        return { group, applyPreviewStyles };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.features = window.AhmadIDEModules.features || {};
        window.AhmadIDEModules.features.settings = window.AhmadIDEModules.features.settings || {};
        window.AhmadIDEModules.features.settings.sections = window.AhmadIDEModules.features.settings.sections || {};
        window.AhmadIDEModules.features.settings.sections.renderEditorFontsGroup = renderEditorFontsGroup;
    }
})();
