(() => {
    function wireSearchBindings({
        updateFindScopeLabels,
        findReplaceState,
        toggleFindMode,
        executeFindReplacePreview,
        closeFindReplaceDialog,
        confirmAndReplaceAll,
        searchDebounce,
        renderSearchEverywhereResults,
        searchEverywhereState,
        openSearchEverywhereResult,
        closeSearchEverywhere
    } = {}) {
        updateFindScopeLabels();
        document.getElementById('findReplaceToggleBtn')?.addEventListener('click', () => {
            const nextMode = findReplaceState.mode === 'replace' ? 'find' : 'replace';
            toggleFindMode(nextMode);
            executeFindReplacePreview(false);
        });
        document.getElementById('closeFindDialog')?.addEventListener('click', closeFindReplaceDialog);
        // Legacy overlay removed - dialog handles backdrop clicks
        document.getElementById('replaceAllBtn')?.addEventListener('click', confirmAndReplaceAll);
        document.getElementById('findQueryInput')?.addEventListener('input', () => searchDebounce(() => executeFindReplacePreview(false), 200));
        document.getElementById('findQueryInput')?.addEventListener('keydown', (e) => {
            if (e.key === 'Enter') executeFindReplacePreview(false);
            if (e.key === 'Escape') closeFindReplaceDialog();
        });
        document.getElementById('replaceQueryInput')?.addEventListener('input', () => {
            if (findReplaceState.mode === 'replace') {
                searchDebounce(() => executeFindReplacePreview(false), 280);
            }
        });
        document.getElementById('replaceQueryInput')?.addEventListener('keydown', (e) => {
            if (e.key === 'Enter') executeFindReplacePreview(e.ctrlKey || e.metaKey);
            if (e.key === 'Escape') closeFindReplaceDialog();
        });
        ['findCaseOption', 'findWholeOption', 'findRegexOption'].forEach(id => {
            document.getElementById(id)?.addEventListener('change', () => executeFindReplacePreview(false));
        });
        document.getElementById('findDialog')?.addEventListener('keydown', (e) => {
            if (e.key === 'Enter') {
                if (findReplaceState.mode === 'replace' && e.ctrlKey) {
                    confirmAndReplaceAll();
                } else {
                    executeFindReplacePreview(false);
                }
            }
            if (e.key === 'Escape') closeFindReplaceDialog();
        });

        // Legacy overlay removed - dialog handles backdrop clicks
        const searchEverywhereInput = document.getElementById('searchEverywhereInput');
        searchEverywhereInput?.addEventListener('input', () => renderSearchEverywhereResults(searchEverywhereInput.value));
        searchEverywhereInput?.addEventListener('keydown', async (e) => {
            const resultsHost = document.getElementById('searchEverywhereResults');
            const items = resultsHost?.querySelectorAll('.search-everywhere-item') || [];
            if (e.key === 'ArrowDown') {
                e.preventDefault();
                searchEverywhereState.selectedIndex = Math.min(items.length - 1, searchEverywhereState.selectedIndex + 1);
                renderSearchEverywhereResults(searchEverywhereInput.value);
            } else if (e.key === 'ArrowUp') {
                e.preventDefault();
                searchEverywhereState.selectedIndex = Math.max(0, searchEverywhereState.selectedIndex - 1);
                renderSearchEverywhereResults(searchEverywhereInput.value);
            } else if (e.key === 'Enter') {
                e.preventDefault();
                const refreshedItems = document.getElementById('searchEverywhereResults')?.querySelectorAll('.search-everywhere-item') || [];
                const active = refreshedItems[searchEverywhereState.selectedIndex];
                const path = active?.dataset?.path;
                if (path) openSearchEverywhereResult(path);
            } else if (e.key === 'Escape') {
                closeSearchEverywhere();
            }
        });
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.renderer = window.AhmadIDEModules.renderer || {};
        window.AhmadIDEModules.renderer.search = window.AhmadIDEModules.renderer.search || {};
        window.AhmadIDEModules.renderer.search.wireSearchBindings = wireSearchBindings;
    }
})();
