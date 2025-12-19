(() => {
    function renderTerminalFontsGroup(ctx) {
        const { draft, primitives, fontService, onDraftChange } = ctx;
        const shared = window.AhmadIDEModules?.features?.settings?.sections?.fontsShared;
        if (!shared) throw new Error('fontsShared missing: src/features/settings/sections/fonts.shared.js');

        const group = document.createElement('div');
        group.className = 'ui-settings-group';
        group.innerHTML = `
            <div class="ui-settings-group__title">Terminal Font</div>
            <div class="ui-settings-group__hint">Affects xterm terminal rendering</div>
        `;

        // Terminal: family
        {
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
                onValue: (v) => onDraftChange((d) => { d.fonts.terminal.family = v; return d; }),
                onUploadFontFile: async (file) => {
                    const saved = await fontService.storeCustomFontFile(file, { kind: 'terminal' });
                    onDraftChange((d) => {
                        d.fonts.terminal.customFontId = saved.id;
                        d.fonts.terminal.family = `"${saved.family}", ${d.fonts.terminal.family || ''}`.replace(/,\s*$/, '');
                        return d;
                    });
                    removeBtn.disabled = false;
                },
                onRemoveFontFile: async () => {
                    const id = draft?.fonts?.terminal?.customFontId;
                    if (id) await fontService.deleteCustomFontFile(id);
                    onDraftChange((d) => { d.fonts.terminal.customFontId = null; return d; });
                    removeBtn.disabled = true;
                }
            });

            row.appendChild(label);
            row.appendChild(host);
            group.appendChild(row);
        }

        // Terminal: size
        group.appendChild(shared.createNumberRow({
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
        preview.className = 'ui-settings-preview';
        preview.dataset.filterText = 'preview terminal';
        const pre = document.createElement('pre');
        pre.textContent = '$ echo \"Hello\"\\nHello\\n$';
        preview.appendChild(pre);
        group.appendChild(preview);

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

