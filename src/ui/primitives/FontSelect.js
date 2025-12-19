(() => {
    /**
     * FontSelect - Searchable font family dropdown with preview
     * PhpStorm-like font picker with pinned bundled fonts + system fonts
     */
    function createFontSelect({ value = '', onChange, showImport = false } = {}) {
        const container = document.createElement('div');
        container.className = 'ui-font-select';
        container.style.cssText = 'position:relative;flex:1;min-width:0;';

        const input = document.createElement('input');
        input.className = 'ui-input';
        input.type = 'text';
        input.placeholder = 'Select font...';
        input.value = value || '';
        input.autocomplete = 'off';
        input.style.cssText = 'width:100%;cursor:pointer;';

        const dropdown = document.createElement('div');
        dropdown.className = 'ui-font-select__dropdown';
        dropdown.style.cssText = `
            position:absolute;
            top:calc(100% + 4px);
            left:0;
            right:0;
            max-height:320px;
            overflow-y:auto;
            background:rgba(60, 63, 65, 0.98);
            border:1px solid var(--ui-border-strong);
            border-radius:var(--ui-radius-2);
            box-shadow:0 8px 24px rgba(0,0,0,0.45);
            z-index:1000;
            display:none;
        `;

        let isOpen = false;
        let fonts = [];
        let filteredFonts = [];

        const loadFonts = async () => {
            const fontService = window.AhmadIDEModules?.services?.fontService;
            if (!fontService) return [];

            const bundled = [
                { family: 'Inter', type: 'bundled', preview: 'Inter — UI Font' },
                { family: 'Fira Code', type: 'bundled', preview: 'Fira Code — Monospace' }
            ];

            const systemFonts = await (fontService.listInstalledFonts?.() || Promise.resolve([]));
            const system = systemFonts.map(f => ({ family: f, type: 'system', preview: f }));

            return [...bundled, ...system];
        };

        const renderDropdown = () => {
            dropdown.innerHTML = '';

            if (filteredFonts.length === 0) {
                const empty = document.createElement('div');
                empty.style.cssText = 'padding:var(--ui-space-4);text-align:center;color:rgba(255,255,255,0.5);font-size:12px;';
                empty.textContent = 'No fonts found';
                dropdown.appendChild(empty);
                return;
            }

            let lastType = null;
            filteredFonts.forEach((font) => {
                // Add section header
                if (font.type !== lastType) {
                    const header = document.createElement('div');
                    header.style.cssText = 'padding:6px 10px;font-size:11px;font-weight:600;color:rgba(255,255,255,0.5);text-transform:uppercase;letter-spacing:0.5px;';
                    header.textContent = font.type === 'bundled' ? 'Bundled' : font.type === 'custom' ? 'Custom' : 'System';
                    dropdown.appendChild(header);
                    lastType = font.type;
                }

                const item = document.createElement('button');
                item.type = 'button';
                item.className = 'ui-font-select__item';
                item.style.cssText = `
                    width:100%;
                    text-align:left;
                    padding:8px 12px;
                    border:none;
                    background:transparent;
                    color:var(--text);
                    cursor:pointer;
                    font-size:13px;
                    display:block;
                `;
                item.textContent = font.family;
                item.style.fontFamily = font.type === 'bundled' || font.type === 'custom' ? font.family : 'inherit';

                item.addEventListener('mouseenter', () => {
                    item.style.background = 'rgba(74, 158, 255, 0.12)';
                });
                item.addEventListener('mouseleave', () => {
                    item.style.background = 'transparent';
                });
                item.addEventListener('click', (e) => {
                    e.stopPropagation();
                    input.value = font.family;
                    close();
                    if (onChange) onChange(font.family);
                });

                dropdown.appendChild(item);
            });

            // Add import option
            if (showImport) {
                const importBtn = document.createElement('button');
                importBtn.type = 'button';
                importBtn.style.cssText = `
                    width:100%;
                    text-align:left;
                    padding:8px 12px;
                    border-top:1px solid var(--ui-border-subtle);
                    background:transparent;
                    color:rgba(74, 158, 255, 0.9);
                    cursor:pointer;
                    font-size:12px;
                    font-weight:500;
                `;
                importBtn.textContent = '+ Import Font File...';
                importBtn.addEventListener('click', async (e) => {
                    e.stopPropagation();
                    close();
                    await importFontFile();
                });
                dropdown.appendChild(importBtn);
            }
        };

        const filter = (query) => {
            const q = String(query || '').trim().toLowerCase();
            if (!q) {
                filteredFonts = fonts;
            } else {
                filteredFonts = fonts.filter(f => f.family.toLowerCase().includes(q));
            }
            renderDropdown();
        };

        const open = async () => {
            if (isOpen) return;
            isOpen = true;
            fonts = await loadFonts();
            filter(input.value);
            dropdown.style.display = 'block';
        };

        const close = () => {
            if (!isOpen) return;
            isOpen = false;
            dropdown.style.display = 'none';
        };

        const importFontFile = async () => {
            const fileInput = document.createElement('input');
            fileInput.type = 'file';
            fileInput.accept = '.ttf,.otf,.woff,.woff2';
            fileInput.addEventListener('change', async (e) => {
                const file = e.target.files?.[0];
                if (!file) return;

                const fontService = window.AhmadIDEModules?.services?.fontService;
                if (!fontService?.storeCustomFontFile) {
                    alert('Font import not available');
                    return;
                }

                try {
                    const result = await fontService.storeCustomFontFile(file, { kind: 'custom' });
                    input.value = result.family;
                    if (onChange) onChange(result.family);
                    // Reload fonts to include the new one
                    fonts = await loadFonts();
                } catch (err) {
                    alert(`Font import failed: ${err.message}`);
                }
            });
            fileInput.click();
        };

        input.addEventListener('focus', () => open());
        input.addEventListener('input', () => filter(input.value));
        input.addEventListener('keydown', (e) => {
            if (e.key === 'Escape') {
                e.preventDefault();
                close();
            }
        });

        // Close on outside click
        const closeOnOutside = (e) => {
            if (!container.contains(e.target)) {
                close();
            }
        };
        document.addEventListener('mousedown', closeOnOutside);

        container.appendChild(input);
        container.appendChild(dropdown);

        // Cleanup
        const destroy = () => {
            document.removeEventListener('mousedown', closeOnOutside);
        };

        return { element: container, setValue: (val) => { input.value = val; }, destroy };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.ui = window.AhmadIDEModules.ui || {};
        window.AhmadIDEModules.ui.primitives = window.AhmadIDEModules.ui.primitives || {};
        window.AhmadIDEModules.ui.primitives.createFontSelect = createFontSelect;
    }
})();
