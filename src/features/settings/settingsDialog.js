(() => {
    const loadScript = (src) => {
        const existing = document.querySelector(`script[data-src="${src}"]`) || document.querySelector(`script[src="${src}"]`);
        if (existing) return;
        const s = document.createElement('script');
        s.src = src;
        s.async = false;
        s.dataset.src = src;
        document.head.appendChild(s);
    };
    if (typeof document !== 'undefined') {
        loadScript('./src/features/settings/sections/connections.js');
        loadScript('./src/features/settings/sections/versionControl.js');
    }
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
        const renderConnectionsSectionFn = () => window.AhmadIDEModules?.features?.settings?.sections?.renderConnectionsSection;
        const renderVersionControlSectionFn = () => window.AhmadIDEModules?.features?.settings?.sections?.renderVersionControlSection;
        if (!settingsService || !fontService || !extensionsService) throw new Error('SettingsDialog requires services');
        if (!createDialog || !createDialogLayout || !primitives) throw new Error('SettingsDialog requires ui primitives');
        let dialogApi = null;
        let layout = null;
        let draft = null;
        let dirty = false;
        let activeSectionId = 'appearance';
        let lastSearch = '';
        let fontsSectionApi = null;
        const ensureConnectionsSection = () => {
            const hasRenderer = () => typeof window.AhmadIDEModules?.features?.settings?.sections?.renderConnectionsSection === 'function';
            return Promise.resolve(hasRenderer());
        };
        const ensureVersionControlSection = () => {
            const hasRenderer = () => typeof window.AhmadIDEModules?.features?.settings?.sections?.renderVersionControlSection === 'function';
            return Promise.resolve(hasRenderer());
        };
        const renderConnectionsSection = (sectionCtx) => {
            const fn = renderConnectionsSectionFn();
            if (typeof fn === 'function') {
                return fn(sectionCtx);
            }
            const fallback = document.createElement('div');
            fallback.textContent = 'Connections section unavailable.';
            return fallback;
        };
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
            try {
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
                else if (activeSectionId === 'connections') node = renderConnectionsSection(sectionCtx);
                else if (activeSectionId === 'version-control') {
                    const fn = renderVersionControlSectionFn();
                    node = typeof fn === 'function' ? fn(sectionCtx) : null;
                }
            } catch (_) {
                node = null;
            }
            if (!node) {
                node = document.createElement('div');
                node.textContent = activeSectionId === 'connections'
                    ? 'Connections section unavailable.'
                    : (activeSectionId === 'version-control'
                        ? 'Version Control section unavailable.'
                        : 'Section unavailable.');
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
                { id: 'connections', label: 'Connections' },
                { id: 'version-control', label: 'Version Control' },
                { id: 'advanced', label: 'Advanced' }
            ]);
            layout.elements.closeBtn.addEventListener('click', () => dialogApi.close('x'));
            layout.root.addEventListener('ui:dialog-nav-change', (e) => {
                activeSectionId = e.detail?.id || activeSectionId;
                if (activeSectionId === 'connections') {
                    ensureConnectionsSection().then((ok) => {
                        if (ok && activeSectionId === 'connections') renderSection();
                    });
                } else if (activeSectionId === 'version-control') {
                    ensureVersionControlSection().then((ok) => {
                        if (ok && activeSectionId === 'version-control') renderSection();
                    });
                } else {
                    renderSection();
                }
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
        const open = (payload) => {
            ensureDialog();
            ensureConnectionsSection();
            ensureVersionControlSection();
            draft = settingsService.clone(settingsService.get());
            dirty = false;
            updateFooter();
            lastSearch = '';
            if (layout?.elements?.searchInput) layout.elements.searchInput.value = '';
            const requestedSection = String(payload?.sectionId || '').trim();
            if (requestedSection && ['appearance', 'fonts', 'connections', 'version-control', 'advanced'].includes(requestedSection)) {
                activeSectionId = requestedSection;
                if (requestedSection === 'connections') {
                    dialogApi.open();
                    ensureConnectionsSection().then((ok) => {
                        if (ok && activeSectionId === 'connections') {
                            if (layout.getActive() !== requestedSection) {
                                layout.setActive(requestedSection);
                            } else {
                                renderSection();
                            }
                        }
                    });
                    requestAnimationFrame(() => layout.elements.searchInput.focus());
                    return;
                }
                if (requestedSection === 'version-control') {
                    dialogApi.open();
                    ensureVersionControlSection().then((ok) => {
                        if (ok && activeSectionId === 'version-control') {
                            if (layout.getActive() !== requestedSection) {
                                layout.setActive(requestedSection);
                            } else {
                                renderSection();
                            }
                        }
                    });
                    requestAnimationFrame(() => layout.elements.searchInput.focus());
                    return;
                }
                if (layout.getActive() !== requestedSection) {
                    layout.setActive(requestedSection);
                } else {
                    renderSection();
                }
            } else {
                activeSectionId = layout.getActive() || activeSectionId;
                renderSection();
            }
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
