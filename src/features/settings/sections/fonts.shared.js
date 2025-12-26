(() => {
    const FONT_FILE_ACCEPT = '.woff2,.woff,.ttf,.otf';

    const asNumber = (v, fallback) => {
        const n = Number(v);
        return Number.isFinite(n) ? n : fallback;
    };

    const uniq = (arr) => {
        const out = [];
        const seen = new Set();
        (arr || []).forEach((v) => {
            const s = String(v || '').trim();
            if (!s) return;
            const key = s.toLowerCase();
            if (seen.has(key)) return;
            seen.add(key);
            out.push(s);
        });
        return out;
    };

    const extractPrimaryFamily = (fontFamilyValue) => {
        const raw = String(fontFamilyValue || '').trim();
        if (!raw) return '';
        const first = raw.split(',')[0].trim();
        return first.replace(/^["']|["']$/g, '').trim();
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
        placeholder = 'Select a font…',
        customFontId,
        suggestedFonts = [],
        allowFontFiles = true,
        onValue,
        onUploadFontFile,
        onRemoveFontFile
    }) {
        const { createInput, createButton } = primitives;

        const host = document.createElement('div');
        host.className = 'ui-settings-row__control ui-font-control';

        const picker = document.createElement('div');
        picker.className = 'ui-font-picker';

        const select = document.createElement('select');
        select.className = 'ui-select ui-font-picker__select';
        select.setAttribute('aria-label', placeholder || 'Font family');

        const advancedToggle = createButton({
            label: 'Advanced…',
            variant: 'ghost',
            size: 'sm',
            onClick: () => {
                advancedWrap.classList.toggle('hidden');
                if (!advancedWrap.classList.contains('hidden')) {
                    requestAnimationFrame(() => advancedInput.focus());
                }
            }
        });

        const populateSelect = (fontList, currentValue) => {
            const currentRaw = String(currentValue || '').trim();
            const currentPrimary = extractPrimaryFamily(currentRaw);

            const installed = uniq(fontList || []).sort((a, b) => a.localeCompare(b));
            const suggested = uniq(suggestedFonts || []);

            const hasPrimary = (arr, name) => arr.some((x) => x.toLowerCase() === String(name || '').toLowerCase());

            select.innerHTML = '';

            if (!installed.length && !suggested.length && currentPrimary) {
                const opt = document.createElement('option');
                opt.value = currentPrimary;
                opt.textContent = currentPrimary;
                select.appendChild(opt);
            }

            const maybeAddGroup = (label) => {
                const g = document.createElement('optgroup');
                g.label = label;
                select.appendChild(g);
                return g;
            };

            // Current (when not found in lists)
            if (currentPrimary && !hasPrimary(installed, currentPrimary) && !hasPrimary(suggested, currentPrimary)) {
                const g = maybeAddGroup('Current');
                const opt = document.createElement('option');
                opt.value = currentPrimary;
                opt.textContent = currentPrimary;
                g.appendChild(opt);
            }

            if (suggested.length) {
                const g = maybeAddGroup('Recommended');
                suggested.forEach((fam) => {
                    const opt = document.createElement('option');
                    opt.value = fam;
                    opt.textContent = fam;
                    g.appendChild(opt);
                });
            }

            if (installed.length) {
                const g = maybeAddGroup('Installed');
                installed.forEach((fam) => {
                    if (hasPrimary(suggested, fam)) return;
                    const opt = document.createElement('option');
                    opt.value = fam;
                    opt.textContent = fam;
                    g.appendChild(opt);
                });
            }

            const gAdv = maybeAddGroup('Advanced');
            const adv = document.createElement('option');
            adv.value = '__custom__';
            adv.textContent = 'Custom font stack…';
            gAdv.appendChild(adv);

            // Keep selection stable.
            const desired = currentPrimary || '__custom__';
            select.value = hasPrimary([...installed, ...suggested], desired) ? desired : (currentPrimary || '__custom__');
        };

        let fileInput = null;
        let uploadBtn = null;
        let removeBtn = null;

        if (allowFontFiles) {
            fileInput = document.createElement('input');
            fileInput.type = 'file';
            fileInput.accept = FONT_FILE_ACCEPT;
            fileInput.className = 'hidden';
            fileInput.addEventListener('change', async () => {
                const file = fileInput.files?.[0];
                fileInput.value = '';
                if (!file) return;
                await onUploadFontFile?.(file);
            });

            uploadBtn = createButton({
                label: 'Add font file…',
                variant: 'ghost',
                size: 'sm',
                onClick: () => fileInput.click()
            });

            removeBtn = createButton({
                label: 'Remove',
                variant: 'ghost',
                size: 'sm',
                disabled: !customFontId,
                onClick: () => onRemoveFontFile?.()
            });
        }

        const advancedWrap = document.createElement('div');
        advancedWrap.className = 'ui-font-picker__advanced hidden';
        const advancedInput = createInput({
            value: String(value || ''),
            placeholder: 'CSS font-family stack, e.g. Fira Code, ui-monospace, monospace',
            onInput: () => {
                onValue(advancedInput.value);
                const primary = extractPrimaryFamily(advancedInput.value);
                if (primary) {
                    // Ensure the current value remains selectable.
                    if (select.value !== primary && Array.from(select.options || []).some(o => o.value === primary)) {
                        select.value = primary;
                    }
                } else {
                    select.value = '__custom__';
                }
            }
        });
        advancedInput.className = 'ui-input ui-font-picker__advancedInput';
        advancedWrap.appendChild(advancedInput);

        const syncAdvancedFromSelect = () => {
            const sel = String(select.value || '').trim();
            if (!sel || sel === '__custom__') {
                return;
            }
            const raw = String(advancedInput.value || '').trim();
            const rest = raw.includes(',') ? raw.split(',').slice(1).join(',').trim() : '';
            const next = rest ? `${sel}, ${rest}` : sel;
            advancedInput.value = next;
            onValue(next);
        };

        select.addEventListener('change', () => {
            if (select.value === '__custom__') {
                advancedWrap.classList.remove('hidden');
                requestAnimationFrame(() => advancedInput.focus());
                return;
            }
            syncAdvancedFromSelect();
        });

        picker.appendChild(select);
        picker.appendChild(advancedToggle);
        if (uploadBtn) picker.appendChild(uploadBtn);
        if (removeBtn) picker.appendChild(removeBtn);

        host.appendChild(picker);
        host.appendChild(advancedWrap);
        if (fileInput) host.appendChild(fileInput);

        // Initial populate (fast) + async installed fonts load (no "List fonts" button).
        populateSelect([], advancedInput.value);
        try {
            Promise.resolve(fontService?.listInstalledFonts?.()).then((list) => {
                populateSelect(list || [], advancedInput.value);
            });
        } catch (_) { }

        return { host, select, advancedInput, removeBtn, populateSelect };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.features = window.AhmadIDEModules.features || {};
        window.AhmadIDEModules.features.settings = window.AhmadIDEModules.features.settings || {};
        window.AhmadIDEModules.features.settings.sections = window.AhmadIDEModules.features.settings.sections || {};
        window.AhmadIDEModules.features.settings.sections.fontsShared = {
            asNumber,
            createNumberRow,
            extractPrimaryFamily,
            createFontFamilyControl
        };
    }
})();
