(() => {
    /**
     * Search Everywhere Dialog
     * Palette-style universal search dialog
     * Matches PhpStorm 2025.3 New UI Search Everywhere (Shift+Shift)
     *
     * Checklist: SEA-001 to SEA-008
     */
    function createSearchEverywhereDialog({ deps } = {}) {
        const createDialog = deps?.createDialog || window.AhmadIDEModules?.ui?.createDialog;
        const primitives = deps?.primitives || window.AhmadIDEModules?.ui?.primitives;

        if (!createDialog || !primitives) {
            throw new Error('SearchEverywhereDialog requires ui primitives');
        }

        let dialogApi = null;
        let activeTab = 'all';
        let results = [];

        const buildContent = () => {
            const container = document.createElement('div');
            container.style.cssText = 'padding:0;min-width:600px;min-height:400px;display:flex;flex-direction:column;';

            // Search input
            const searchRow = document.createElement('div');
            searchRow.style.cssText = 'padding:var(--ui-space-4);border-bottom:1px solid var(--ui-border-subtle);';
            const { createInput } = primitives;
            const searchInput = createInput({ id: 'searchEverywhereInput', placeholder: 'Search everywhere...', value: '' });
            searchInput.style.width = '100%';
            searchInput.addEventListener('input', (e) => performSearch(e.target.value));
            searchRow.appendChild(searchInput);

            // Tab filters
            const tabsRow = document.createElement('div');
            tabsRow.style.cssText = 'display:flex;gap:0;padding:var(--ui-space-2) var(--ui-space-4);border-bottom:1px solid var(--ui-border-subtle);';
            ['All', 'Files', 'Symbols', 'Actions'].forEach((label) => {
                const tab = document.createElement('button');
                tab.className = 'ui-btn ui-btn--ghost ui-btn--sm';
                tab.textContent = label;
                tab.dataset.tab = label.toLowerCase();
                if (label.toLowerCase() === activeTab) {
                    tab.style.background = 'rgba(74, 158, 255, 0.16)';
                }
                tab.addEventListener('click', () => {
                    activeTab = label.toLowerCase();
                    tabsRow.querySelectorAll('button').forEach((b) => {
                        b.style.background = b.dataset.tab === activeTab ? 'rgba(74, 158, 255, 0.16)' : '';
                    });
                    renderResults();
                });
                tabsRow.appendChild(tab);
            });

            // Results list
            const resultsList = document.createElement('div');
            resultsList.id = 'searchEverywhereResults';
            resultsList.style.cssText = 'flex:1;overflow:auto;padding:var(--ui-space-2);';

            container.appendChild(searchRow);
            container.appendChild(tabsRow);
            container.appendChild(resultsList);

            return container;
        };

        const performSearch = (query) => {
            const q = String(query || '').trim().toLowerCase();
            if (!q) {
                results = [];
                renderResults();
                return;
            }

            // Mock search results
            results = [
                { type: 'file', name: 'main.m', path: '/project/main.m', score: 0.9 },
                { type: 'file', name: 'utils.m', path: '/project/utils.m', score: 0.7 },
                { type: 'symbol', name: 'SAVEROUTINE', routine: 'main.m', line: 45, score: 0.8 },
                { type: 'action', name: 'Open Settings', action: 'settings', score: 0.6 }
            ].filter((r) => r.name.toLowerCase().includes(q));

            if (activeTab !== 'all') {
                results = results.filter((r) => r.type === (activeTab === 'symbols' ? 'symbol' : activeTab.slice(0, -1)));
            }

            results.sort((a, b) => b.score - a.score);
            renderResults();
        };

        const renderResults = () => {
            const container = document.getElementById('searchEverywhereResults');
            if (!container) return;

            container.innerHTML = '';

            if (results.length === 0) {
                container.innerHTML = '<div style="padding:var(--ui-space-6);text-align:center;color:rgba(255,255,255,0.5);">No results</div>';
                return;
            }

            results.forEach((result, idx) => {
                const item = document.createElement('button');
                item.className = 'search-everywhere-item';
                item.style.cssText = `
                    width:100%;
                    text-align:left;
                    padding:var(--ui-space-2) var(--ui-space-3);
                    border-radius:var(--ui-radius-1);
                    background:transparent;
                    border:1px solid transparent;
                    cursor:pointer;
                    display:flex;
                    align-items:center;
                    gap:var(--ui-space-3);
                    margin-bottom:2px;
                `;
                item.dataset.index = idx;

                const icon = document.createElement('div');
                icon.style.cssText = 'width:16px;height:16px;';
                icon.textContent = { file: '📄', symbol: '🔣', action: '⚡' }[result.type] || '•';

                const details = document.createElement('div');
                details.style.flex = '1';
                const name = document.createElement('div');
                name.textContent = result.name;
                name.style.cssText = 'font-weight:500;';
                const path = document.createElement('div');
                path.textContent = result.path || result.routine || result.action || '';
                path.style.cssText = 'font-size:11px;color:rgba(255,255,255,0.5);';
                details.appendChild(name);
                details.appendChild(path);

                item.appendChild(icon);
                item.appendChild(details);

                item.addEventListener('mouseenter', () => {
                    item.style.background = 'rgba(74, 158, 255, 0.12)';
                });
                item.addEventListener('mouseleave', () => {
                    item.style.background = '';
                });
                item.addEventListener('click', () => {
                    console.log('Selected:', result);
                    dialogApi?.close('selected');
                });

                container.appendChild(item);
            });
        };

        const ensureDialog = () => {
            if (dialogApi) return;

            dialogApi = createDialog({
                ariaLabel: 'Search Everywhere',
                closeOnEscape: true,
                closeOnBackdrop: true,
                onClose: () => { }
            });

            const wrapper = document.createElement('div');
            wrapper.className = 'ui-dialog';
            wrapper.style.cssText = 'width:min(700px, 90vw);height:min(500px, 70vh);display:flex;flex-direction:column;';

            wrapper.appendChild(buildContent());

            dialogApi.setContent(wrapper);
        };

        const open = () => {
            ensureDialog();
            results = [];
            activeTab = 'all';
            dialogApi.open();
            requestAnimationFrame(() => {
                document.getElementById('searchEverywhereInput')?.focus();
            });
        };

        return { open };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.features = window.AhmadIDEModules.features || {};
        window.AhmadIDEModules.features.search = window.AhmadIDEModules.features.search || {};
        window.AhmadIDEModules.features.search.createSearchEverywhereDialog = createSearchEverywhereDialog;
    }
})();
