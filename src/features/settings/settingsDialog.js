(() => {
    const applyFilter = (root, query) => {
        const q = String(query || '').trim().toLowerCase();
        const rows = root.querySelectorAll('[data-filter-text]');
        rows.forEach((el) => {
            if (!q) {
                el.classList.remove('hidden');
                return;
            }
            const hay = String(el.dataset.filterText || '').toLowerCase();
            el.classList.toggle('hidden', !hay.includes(q));
        });
    };

    function createSettingsDialog({ deps } = {}) {
        const settingsService = deps?.settingsService || window.AhmadIDEModules?.services?.settingsService;
        const fontService = deps?.fontService || window.AhmadIDEModules?.services?.fontService;
        const extensionsService = deps?.extensionsService || window.AhmadIDEModules?.services?.extensionsService;

        const createDialog = deps?.createDialog || window.AhmadIDEModules?.ui?.createDialog;
        const createDialogLayout = deps?.createDialogLayout || window.AhmadIDEModules?.ui?.createDialogLayout;
        const primitives = deps?.primitives || window.AhmadIDEModules?.ui?.primitives;

        const renderAppearanceSection = window.AhmadIDEModules?.features?.settings?.sections?.renderAppearanceSection;
        const renderFontsSection = window.AhmadIDEModules?.features?.settings?.sections?.renderFontsSection;
        const renderAdvancedSection = window.AhmadIDEModules?.features?.settings?.sections?.renderAdvancedSection;

        if (!settingsService || !fontService || !extensionsService) throw new Error('SettingsDialog requires services');
        if (!createDialog || !createDialogLayout || !primitives) throw new Error('SettingsDialog requires ui primitives');

        let dialogApi = null;
        let layout = null;
        let draft = null;
        let dirty = false;
        let activeSectionId = 'appearance';
        let lastSearch = '';
        let fontsSectionApi = null;

        const markDirty = () => {
            dirty = true;
            updateFooter();
        };

        const onDraftChange = (mutator) => {
            if (!draft) return;
            const next = typeof mutator === 'function' ? mutator(draft) : mutator;
            draft = next || draft;
            markDirty();
            fontsSectionApi?.applyPreviewStyles?.();
        };

        const apply = async () => {
            if (!draft) return;
            // Ensure custom fonts are registered before Monaco/terminal re-measure.
            await fontService.ensureCustomFontsLoaded(draft);
            fontService.applyFontsToDocument(draft);
            settingsService.save(draft);
            dirty = false;
            updateFooter();
        };

        const updateFooter = () => {
            if (!layout) return;
            const { createButton } = primitives;
            const btnApply = createButton({ label: 'Apply', variant: 'ghost', disabled: !dirty, onClick: () => apply() });
            const btnOk = createButton({
                label: 'OK',
                variant: 'primary',
                onClick: async () => {
                    await apply();
                    dialogApi?.close('ok');
                }
            });
            const btnCancel = createButton({ label: 'Cancel', variant: 'ghost', onClick: () => dialogApi?.close('cancel') });
            layout.setFooter([btnCancel, btnApply, btnOk]);
        };

        const renderSection = () => {
            if (!layout) return;
            layout.elements.content.innerHTML = '';
            fontsSectionApi = null;

            const sectionCtx = { draft, primitives, fontService, extensionsService, onDraftChange };
            let node = null;
            if (activeSectionId === 'appearance') node = renderAppearanceSection?.(sectionCtx);
            else if (activeSectionId === 'fonts') {
                const res = renderFontsSection?.(sectionCtx);
                if (res && res.root) {
                    fontsSectionApi = res;
                    node = res.root;
                } else {
                    node = res;
                }
            } else if (activeSectionId === 'advanced') node = renderAdvancedSection?.(sectionCtx);

            if (!node) {
                node = document.createElement('div');
                node.textContent = 'Not implemented.';
            }
            layout.elements.content.appendChild(node);
            applyFilter(layout.elements.content, lastSearch);
        };

        const ensureDialog = () => {
            if (dialogApi && layout) return;

            dialogApi = createDialog({
                ariaLabel: 'Settings',
                closeOnEscape: true,
                closeOnBackdrop: false,
                onClose: () => { }
            });

            layout = createDialogLayout({ title: 'Settings', searchPlaceholder: 'Search settings' });
            layout.setItems([
                { id: 'appearance', label: 'Appearance' },
                { id: 'fonts', label: 'Fonts' },
                { id: 'advanced', label: 'Advanced' }
            ]);
            layout.elements.closeBtn.addEventListener('click', () => dialogApi.close('x'));

            layout.root.addEventListener('ui:dialog-nav-change', (e) => {
                activeSectionId = e.detail?.id || activeSectionId;
                renderSection();
            });
            layout.root.addEventListener('ui:dialog-search', (e) => {
                lastSearch = e.detail?.query || '';
                applyFilter(layout.elements.content, lastSearch);
            });

            layout.root.addEventListener('keydown', (e) => {
                if (e.key !== 'Enter') return;
                const tag = (e.target && e.target.tagName) ? e.target.tagName.toUpperCase() : '';
                if (tag === 'TEXTAREA') return;
                if (tag === 'INPUT' || tag === 'SELECT' || tag === 'BUTTON') {
                    // Let controls handle Enter naturally.
                    return;
                }
                e.preventDefault();
                layout.elements.footer.querySelector('button.ui-btn--primary')?.click?.();
            });

            dialogApi.setContent(layout.root);
            updateFooter();
        };

        const open = () => {
            ensureDialog();
            draft = settingsService.clone(settingsService.get());
            dirty = false;
            updateFooter();
            renderSection();
            dialogApi.open();
            requestAnimationFrame(() => layout.elements.searchInput.focus());
        };

        return { open, apply };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.features = window.AhmadIDEModules.features || {};
        window.AhmadIDEModules.features.settings = window.AhmadIDEModules.features.settings || {};
        window.AhmadIDEModules.features.settings.createSettingsDialog = createSettingsDialog;
    }
})();
