(() => {
    /**
     * Search Everywhere Dialog
     * Palette-style universal search dialog
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
        let allRoutines = []; // Cache of all indexed routines

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
            ['All', 'localr', 'routines'].forEach((label) => {
                const tab = document.createElement('button');
                tab.className = 'ui-btn ui-btn--ghost ui-btn--sm';
                tab.textContent = label === 'all' ? 'All' : label;
                tab.dataset.tab = label.toLowerCase();
                if (label.toLowerCase() === activeTab) {
                    tab.style.background = 'var(--accent-soft)';
                }
                tab.addEventListener('click', () => {
                    activeTab = label.toLowerCase();
                    tabsRow.querySelectorAll('button').forEach((b) => {
                        b.style.background = b.dataset.tab === activeTab ? 'var(--accent-soft)' : '';
                    });
                    const input = document.getElementById('searchEverywhereInput');
                    performSearch(input?.value || '');
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

            // Filter routines based on active tab
            let filteredRoutines = allRoutines;
            if (activeTab === 'localr') {
                filteredRoutines = allRoutines.filter(r => r.path.toLowerCase().startsWith('localr/'));
            } else if (activeTab === 'routines') {
                filteredRoutines = allRoutines.filter(r => r.path.toLowerCase().startsWith('routines/'));
            }

            if (!q) {
                // Show all routines in current tab (limit to first 100)
                results = filteredRoutines.slice(0, 100).map(r => ({
                    type: 'file',
                    name: r.name,
                    path: r.path,
                    folder: r.folder,
                    score: 1
                }));
                renderResults();
                return;
            }

            // Search and score results
            results = filteredRoutines
                .map(r => {
                    const name = r.name.toLowerCase();
                    const path = r.path.toLowerCase();
                    const idx = name.indexOf(q);
                    const pathIdx = path.indexOf(q);

                    if (idx === -1 && pathIdx === -1) return null;

                    // Score: prefer exact match at start, then anywhere in name, then in path
                    let score = 0;
                    if (idx === 0) score = 100;
                    else if (idx > 0) score = 50 - idx;
                    else if (pathIdx >= 0) score = 25 - pathIdx;

                    return {
                        type: 'file',
                        name: r.name,
                        path: r.path,
                        folder: r.folder,
                        score
                    };
                })
                .filter(r => r !== null)
                .sort((a, b) => b.score - a.score)
                .slice(0, 60);

            renderResults();
        };

        const renderResults = () => {
            const container = document.getElementById('searchEverywhereResults');
            if (!container) return;

            container.innerHTML = '';

            if (results.length === 0) {
                const emptyMsg = allRoutines.length === 0
                    ? 'Indexing routines...'
                    : 'No results';
                container.innerHTML = `<div style="padding:var(--ui-space-6);text-align:center;color:var(--muted);">${emptyMsg}</div>`;
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
                item.dataset.path = result.path;

                const icon = document.createElement('div');
                icon.style.cssText = 'width:16px;height:16px;font-size:12px;';
                icon.textContent = '📄';

                const details = document.createElement('div');
                details.style.flex = '1';
                const name = document.createElement('div');
                name.textContent = result.name;
                name.style.cssText = 'font-weight:500;';
                const path = document.createElement('div');
                path.textContent = result.path;
                path.style.cssText = 'font-size:11px;color:var(--muted);';
                details.appendChild(name);
                details.appendChild(path);

                item.appendChild(icon);
                item.appendChild(details);

                item.addEventListener('mouseenter', () => {
                    item.style.background = 'var(--hover-bg)';
                });
                item.addEventListener('mouseleave', () => {
                    item.style.background = '';
                });
                item.addEventListener('click', async () => {
                    console.log('Selected:', result.path);
                    // Use the global search manager to open the routine
                    const searchMgr = window.searchManager;
                    if (searchMgr && typeof searchMgr.openSearchEverywhereResult === 'function') {
                        await searchMgr.openSearchEverywhereResult(result.path);
                        dialogApi?.close('selected');
                    } else {
                        console.error('searchManager.openSearchEverywhereResult not available');
                    }
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
            wrapper.style.cssText = 'width:min(700px, 90vw);height:min(500px, 70vh);display:flex;flex-direction:column;';

            wrapper.appendChild(buildContent());

            dialogApi.setContent(wrapper);
        };

        const open = async () => {
            ensureDialog();
            results = [];
            activeTab = 'all';
            dialogApi.open();

            // Index routines if not already done
            if (allRoutines.length === 0) {
                const container = document.getElementById('searchEverywhereResults');
                if (container) container.innerHTML = '<div style="padding:var(--ui-space-6);text-align:center;color:var(--muted);">Indexing routines...</div>';

                try {
                    const allRoutinesSet = new Set();
                    const folders = ['localr', 'routines'];

                    for (const folder of folders) {
                        try {
                            const res = await window.ahmadIDE.listRoutines(`${folder}/`);
                            if (res?.ok && Array.isArray(res.routines)) {
                                res.routines.forEach(r => allRoutinesSet.add(r));
                            }
                        } catch (err) {
                            console.warn(`Failed to list routines in ${folder}:`, err);
                        }
                    }

                    // Fallback: also try listing without folder prefix
                    try {
                        const res = await window.ahmadIDE.listRoutines('');
                        if (res?.ok && Array.isArray(res.routines)) {
                            res.routines.forEach(r => allRoutinesSet.add(r));
                        }
                    } catch (err) {
                        console.warn('Failed to list all routines:', err);
                    }

                    allRoutines = Array.from(allRoutinesSet).map(r => ({
                        path: r,
                        name: r.split('/').pop(),
                        folder: r.split('/')[0] || ''
                    }));
                } catch (err) {
                    console.error('Failed to index routines:', err);
                }

                performSearch(''); // Show initial results
            }

            requestAnimationFrame(() => {
                const input = document.getElementById('searchEverywhereInput');
                if (input) {
                    input.value = '';
                    input.focus();
                }
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
