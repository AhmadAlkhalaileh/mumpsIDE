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
    const renderMumpsSection = ({ draft, primitives, onDraftChange }) => {
        const root = document.createElement('div');

        // Helper to get deeply nested values
        const getVal = (path, def) => path.split('.').reduce((obj, key) => (obj && obj[key] !== undefined) ? obj[key] : undefined, draft) ?? def;

        // Helper to set deeply nested values
        const setVal = (path, val) => {
            onDraftChange((d) => {
                const keys = path.split('.');
                let curr = d;
                for (let i = 0; i < keys.length - 1; i++) {
                    if (!curr[keys[i]]) curr[keys[i]] = {};
                    curr = curr[keys[i]];
                }
                curr[keys[keys.length - 1]] = val;
                return d;
            });
        };

        // --- Routine Header Template ---
        const group1 = document.createElement('div');
        group1.className = 'ui-settings-group';
        group1.innerHTML = `
            <div class="ui-settings-group__title">Routine Header Template</div>
            <div class="ui-settings-group__hint">Define the header inserted when creating new routines.<br>Variables: \${TAG}, \${DATE}, \${USER}, \${ROUTINE}. Enter exactly as you want it to appear (multi-line supported).</div>
        `;
        const row1 = document.createElement('div');
        row1.className = 'ui-settings-row';
        row1.style.gridTemplateColumns = '1fr';  // Full width for textarea
        row1.dataset.filterText = 'routine header template mumps snippet entry';
        const control1 = document.createElement('div');
        control1.className = 'ui-settings-row__control';
        control1.style.width = '100%';

        // Load: convert array to string if needed
        const currentSnippet = getVal('mumps.snippets.entry-point.body');
        const routineVal = Array.isArray(currentSnippet) ? currentSnippet.join('\n') : (currentSnippet || '');
        const routineArea = document.createElement('textarea');
        routineArea.className = 'ui-input';
        routineArea.value = routineVal;
        routineArea.placeholder = `Example:
\${TAG} ; \${ROUTINE} - \${DESCRIPTION}
    ; Created: \${DATE}
    ; Author: \${USER}
    QUIT`;
        routineArea.rows = 12;
        routineArea.style.width = '100%';
        routineArea.style.minHeight = '200px';
        routineArea.style.fontFamily = 'monospace';
        routineArea.style.whiteSpace = 'pre';
        routineArea.style.resize = 'vertical';
        routineArea.addEventListener('input', () => {
            let existing = { ...getVal('mumps.snippets.entry-point', {}) };
            // Store as string to preserve exact formatting
            existing.body = routineArea.value;
            setVal('mumps.snippets.entry-point', existing);
        });
        control1.appendChild(routineArea);
        row1.appendChild(control1);
        group1.appendChild(row1);
        root.appendChild(group1);

        // --- User Name ---
        const group4 = document.createElement('div');
        group4.className = 'ui-settings-group';
        group4.innerHTML = `
            <div class="ui-settings-group__title">Author Name</div>
            <div class="ui-settings-group__hint">Your name used in \${USER} variable when creating routines.</div>
        `;
        const row4 = document.createElement('div');
        row4.className = 'ui-settings-row';
        row4.dataset.filterText = 'author name user mumps';
        const label4 = document.createElement('div');
        label4.className = 'ui-settings-row__label';
        label4.textContent = 'Author Name';
        const control4 = document.createElement('div');
        control4.className = 'ui-settings-row__control';

        const userNameInput = document.createElement('input');
        userNameInput.type = 'text';
        userNameInput.className = 'ui-input';
        userNameInput.value = getVal('mumps.userName', '');
        userNameInput.placeholder = 'Developer';
        userNameInput.style.width = '200px';
        userNameInput.addEventListener('input', () => {
            setVal('mumps.userName', userNameInput.value);
        });
        control4.appendChild(userNameInput);
        row4.appendChild(label4);
        row4.appendChild(control4);
        group4.appendChild(row4);
        root.appendChild(group4);

        // --- Auto Tag Header Template ---
        const groupTagHeader = document.createElement('div');
        groupTagHeader.className = 'ui-settings-group';
        groupTagHeader.innerHTML = `
            <div class="ui-settings-group__title">Auto Tag Header</div>
            <div class="ui-settings-group__hint">
                Generate a documentation header above the selected tag.<br>
                Right-click on a tag name and choose "Generate Tag Header".<br>
                Variables: \${TAG}, \${DESCRIPTION}, \${PARAMS_IN}, \${OUT}, \${RETURN}, \${NOTES}, \${AUTHOR}.
            </div>
        `;

        // Author override (defaults to Author Name above if empty)
        const tagHeaderAuthorRow = document.createElement('div');
        tagHeaderAuthorRow.className = 'ui-settings-row';
        tagHeaderAuthorRow.dataset.filterText = 'auto tag header author name mumps';
        const tagHeaderAuthorLabel = document.createElement('div');
        tagHeaderAuthorLabel.className = 'ui-settings-row__label';
        tagHeaderAuthorLabel.textContent = 'Tag Header Author';
        const tagHeaderAuthorControl = document.createElement('div');
        tagHeaderAuthorControl.className = 'ui-settings-row__control';

        const tagHeaderAuthorInput = document.createElement('input');
        tagHeaderAuthorInput.type = 'text';
        tagHeaderAuthorInput.className = 'ui-input';
        tagHeaderAuthorInput.value = getVal('mumps.tagHeader.author', '');
        tagHeaderAuthorInput.placeholder = 'Leave empty to use Author Name';
        tagHeaderAuthorInput.style.width = '280px';
        tagHeaderAuthorInput.addEventListener('input', () => {
            setVal('mumps.tagHeader.author', tagHeaderAuthorInput.value);
        });
        tagHeaderAuthorControl.appendChild(tagHeaderAuthorInput);
        tagHeaderAuthorRow.appendChild(tagHeaderAuthorLabel);
        tagHeaderAuthorRow.appendChild(tagHeaderAuthorControl);
        groupTagHeader.appendChild(tagHeaderAuthorRow);

        // Template
        const tagHeaderTemplateRow = document.createElement('div');
        tagHeaderTemplateRow.className = 'ui-settings-row';
        tagHeaderTemplateRow.style.gridTemplateColumns = '1fr';
        tagHeaderTemplateRow.dataset.filterText = 'auto tag header template mumps';
        const tagHeaderTemplateControl = document.createElement('div');
        tagHeaderTemplateControl.className = 'ui-settings-row__control';
        tagHeaderTemplateControl.style.width = '100%';

        const tagHeaderTemplateArea = document.createElement('textarea');
        tagHeaderTemplateArea.className = 'ui-input';
        tagHeaderTemplateArea.value = getVal('mumps.tagHeader.template', '');
        tagHeaderTemplateArea.placeholder = 'Leave empty to use default template...';
        tagHeaderTemplateArea.rows = 12;
        tagHeaderTemplateArea.style.width = '100%';
        tagHeaderTemplateArea.style.minHeight = '200px';
        tagHeaderTemplateArea.style.fontFamily = 'monospace';
        tagHeaderTemplateArea.style.whiteSpace = 'pre';
        tagHeaderTemplateArea.style.resize = 'vertical';
        tagHeaderTemplateArea.addEventListener('input', () => {
            setVal('mumps.tagHeader.template', tagHeaderTemplateArea.value);
        });

        tagHeaderTemplateControl.appendChild(tagHeaderTemplateArea);
        tagHeaderTemplateRow.appendChild(tagHeaderTemplateControl);
        groupTagHeader.appendChild(tagHeaderTemplateRow);
        root.appendChild(groupTagHeader);

        // --- Tag Snippet Template ---
        const group5 = document.createElement('div');
        group5.className = 'ui-settings-group';
        group5.innerHTML = `
            <div class="ui-settings-group__title">Tag Snippet Template</div>
            <div class="ui-settings-group__hint">Define the snippet inserted when using "mtag" or "new-label".<br>Variables: \${TAG}, \${DATE}, \${USER}, \${DESCRIPTION}.</div>
        `;
        const row5 = document.createElement('div');
        row5.className = 'ui-settings-row';
        row5.style.gridTemplateColumns = '1fr';  // Full width for textarea
        row5.dataset.filterText = 'tag snippet template mumps mtag label';
        const control5 = document.createElement('div');
        control5.className = 'ui-settings-row__control';
        control5.style.width = '100%';

        const currentTagSnippet = getVal('mumps.snippets.mytag.body') || getVal('mumps.snippets.routine-tag.body');
        const tagVal = Array.isArray(currentTagSnippet) ? currentTagSnippet.join('\n') : (currentTagSnippet || '');
        const tagArea = document.createElement('textarea');
        tagArea.className = 'ui-input';
        tagArea.value = tagVal;
        tagArea.placeholder = `Example:
;; Tag Header
\${TAG} ; \${DESCRIPTION}
    ; Author: \${USER}
    QUIT`;
        tagArea.rows = 8;
        tagArea.style.width = '100%';
        tagArea.style.minHeight = '120px';
        tagArea.style.fontFamily = 'monospace';
        tagArea.style.whiteSpace = 'pre';
        tagArea.style.resize = 'vertical';
        tagArea.addEventListener('input', () => {
            // Use 'mytag' key to avoid issues with hyphenated 'routine-tag' key
            let existing = { ...getVal('mumps.snippets.mytag', {}) };
            existing.body = tagArea.value;
            setVal('mumps.snippets.mytag', existing);
        });
        control5.appendChild(tagArea);
        row5.appendChild(control5);
        group5.appendChild(row5);
        root.appendChild(group5);

        // --- FileMan API Assistance ---
        const group6 = document.createElement('div');
        group6.className = 'ui-settings-group';
        group6.innerHTML = `
            <div class="ui-settings-group__title">FileMan API Assistance</div>
            <div class="ui-settings-group__hint">First-class snippets and hover docs for VA FileMan APIs (HardHats).</div>
        `;

        const filemanSnippetsRow = document.createElement('div');
        filemanSnippetsRow.className = 'ui-settings-row';
        filemanSnippetsRow.dataset.filterText = 'fileman api snippets hardhats dic die diq find gets list update file';
        const filemanSnippetsLabel = document.createElement('div');
        filemanSnippetsLabel.className = 'ui-settings-row__label';
        filemanSnippetsLabel.textContent = 'FileMan API Snippets';
        const filemanSnippetsControl = document.createElement('div');
        filemanSnippetsControl.className = 'ui-settings-row__control';
        const filemanSnippetsToggle = primitives.createToggle({
            label: '',
            checked: getVal('mumps.fileman.snippetsEnabled', true) !== false,
            onChange: (_e, checked) => setVal('mumps.fileman.snippetsEnabled', !!checked)
        });
        filemanSnippetsControl.appendChild(filemanSnippetsToggle.root);
        filemanSnippetsRow.appendChild(filemanSnippetsLabel);
        filemanSnippetsRow.appendChild(filemanSnippetsControl);
        group6.appendChild(filemanSnippetsRow);

        const filemanHoverRow = document.createElement('div');
        filemanHoverRow.className = 'ui-settings-row';
        filemanHoverRow.dataset.filterText = 'fileman api hover docs documentation hardhats dic die diq find gets list update file';
        const filemanHoverLabel = document.createElement('div');
        filemanHoverLabel.className = 'ui-settings-row__label';
        filemanHoverLabel.textContent = 'FileMan Hover Docs';
        const filemanHoverControl = document.createElement('div');
        filemanHoverControl.className = 'ui-settings-row__control';
        const filemanHoverToggle = primitives.createToggle({
            label: '',
            checked: getVal('mumps.fileman.hoverDocsEnabled', true) !== false,
            onChange: (_e, checked) => setVal('mumps.fileman.hoverDocsEnabled', !!checked)
        });
        filemanHoverControl.appendChild(filemanHoverToggle.root);
        filemanHoverRow.appendChild(filemanHoverLabel);
        filemanHoverRow.appendChild(filemanHoverControl);
        group6.appendChild(filemanHoverRow);

        root.appendChild(group6);

        return root;
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
        const renderMumpsSectionFn = () => renderMumpsSection;
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
        const ensureMumpsSection = () => {
            return Promise.resolve(true);
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
                } else if (activeSectionId === 'mumps') {
                    const fn = renderMumpsSectionFn();
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
                { id: 'mumps', label: 'MUMPS' },
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
                } else if (activeSectionId === 'mumps') {
                    ensureMumpsSection().then((ok) => {
                        if (ok && activeSectionId === 'mumps') renderSection();
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
            ensureMumpsSection();
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
