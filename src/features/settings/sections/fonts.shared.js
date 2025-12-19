(() => {
    const FONT_FILE_ACCEPT = '.woff2,.woff,.ttf,.otf';

    const asNumber = (v, fallback) => {
        const n = Number(v);
        return Number.isFinite(n) ? n : fallback;
    };

    function createNumberRow({ primitives, labelText, filterText, value, min, max, step, onValue }) {
        const { createInput } = primitives;
        const row = document.createElement('div');
        row.className = 'ui-settings-row';
        row.dataset.filterText = filterText;
        const label = document.createElement('div');
        label.className = 'ui-settings-row__label';
        label.textContent = labelText;
        const control = document.createElement('div');
        control.className = 'ui-settings-row__control';
        const input = createInput({
            type: 'number',
            value: String(value),
            onInput: () => onValue(asNumber(input.value, value))
        });
        input.min = String(min);
        input.max = String(max);
        input.step = String(step);
        input.style.maxWidth = '120px';
        control.appendChild(input);
        row.appendChild(label);
        row.appendChild(control);
        return { row, input };
    }

    function createFontFamilyControl({
        primitives,
        fontService,
        value,
        placeholder = 'Font family…',
        customFontId,
        onValue,
        onUploadFontFile,
        onRemoveFontFile
    }) {
        const { createInput, createButton } = primitives;

        const host = document.createElement('div');
        host.className = 'ui-settings-row__control';

        const input = createInput({
            value: value || '',
            placeholder,
            onInput: () => onValue(input.value)
        });
        input.style.flex = '1';

        const datalistId = `fontlist_${Math.random().toString(16).slice(2)}`;
        const datalist = document.createElement('datalist');
        datalist.id = datalistId;
        input.setAttribute('list', datalistId);

        const refreshBtn = createButton({
            label: 'List fonts',
            variant: 'ghost',
            size: 'sm',
            onClick: async () => {
                const list = await fontService.listInstalledFonts();
                datalist.innerHTML = '';
                list.forEach((fam) => {
                    const opt = document.createElement('option');
                    opt.value = fam;
                    datalist.appendChild(opt);
                });
            }
        });

        const fileInput = document.createElement('input');
        fileInput.type = 'file';
        fileInput.accept = FONT_FILE_ACCEPT;
        fileInput.className = 'hidden';
        fileInput.addEventListener('change', async () => {
            const file = fileInput.files?.[0];
            fileInput.value = '';
            if (!file) return;
            await onUploadFontFile?.(file);
        });

        const uploadBtn = createButton({
            label: 'Add font file…',
            variant: 'ghost',
            size: 'sm',
            onClick: () => fileInput.click()
        });

        const removeBtn = createButton({
            label: 'Remove',
            variant: 'ghost',
            size: 'sm',
            disabled: !customFontId,
            onClick: () => onRemoveFontFile?.()
        });

        host.appendChild(input);
        host.appendChild(refreshBtn);
        host.appendChild(uploadBtn);
        host.appendChild(removeBtn);
        host.appendChild(datalist);
        host.appendChild(fileInput);

        return { host, input, removeBtn };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.features = window.AhmadIDEModules.features || {};
        window.AhmadIDEModules.features.settings = window.AhmadIDEModules.features.settings || {};
        window.AhmadIDEModules.features.settings.sections = window.AhmadIDEModules.features.settings.sections || {};
        window.AhmadIDEModules.features.settings.sections.fontsShared = {
            asNumber,
            createNumberRow,
            createFontFamilyControl
        };
    }
})();

