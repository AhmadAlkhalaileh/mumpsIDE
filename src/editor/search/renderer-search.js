(() => {
    const createNoopLogger = () => ({
        debug: () => { },
        info: () => { },
        warn: () => { },
        error: () => { },
        isEnabled: () => false
    });

    function createSearchManager({ state, deps } = {}) {
        if (!state?.findReplaceState || !state?.searchEverywhereState || !state?.routinesCacheRef) {
            throw new Error('createSearchManager requires { state.findReplaceState, state.searchEverywhereState, state.routinesCacheRef }');
        }

        const logger = deps?.logger || createNoopLogger();
        const showToast = deps?.showToast || (() => { });
        const getCurrentProject = deps?.getCurrentProject || (() => null);
        const getActiveEditor = deps?.getActiveEditor || (() => null);
        const getRoutineState = deps?.getRoutineState || (() => null);
        const getGlobalTerminalState = deps?.getGlobalTerminalState || (() => null);
        const normalizeRoutineTarget = deps?.normalizeRoutineTarget;
        const getActiveRoutine = deps?.getActiveRoutine;
        const loadRoutineByName = deps?.loadRoutineByName;
        const revealLine = deps?.revealLine || (() => { });
        const findOpenTab = deps?.findOpenTab;
        const tabModels = deps?.tabModels;
        const getActiveTabId = deps?.getActiveTabId || (() => null);
        const renderTabs = deps?.renderTabs || (() => { });

        if (!normalizeRoutineTarget || typeof normalizeRoutineTarget !== 'function') {
            throw new Error('createSearchManager requires deps.normalizeRoutineTarget');
        }
        if (!getActiveRoutine || typeof getActiveRoutine !== 'function') {
            throw new Error('createSearchManager requires deps.getActiveRoutine');
        }
        if (!loadRoutineByName || typeof loadRoutineByName !== 'function') {
            throw new Error('createSearchManager requires deps.loadRoutineByName');
        }
        if (!findOpenTab || typeof findOpenTab !== 'function') {
            throw new Error('createSearchManager requires deps.findOpenTab');
        }
        if (!tabModels) {
            throw new Error('createSearchManager requires deps.tabModels');
        }

        const findReplaceState = state.findReplaceState;
        const searchEverywhereState = state.searchEverywhereState;
        const routinesCacheRef = state.routinesCacheRef;

        const escapeRegex = (str = '') => str.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
        const escapeHtml = (str = '') => str
            .replace(/&/g, '&amp;')
            .replace(/</g, '&lt;')
            .replace(/>/g, '&gt;')
            .replace(/"/g, '&quot;')
            .replace(/'/g, '&#39;');

        // Promise helper to avoid hanging UI operations forever
        const withTimeout = (promise, ms, label = 'Operation') => {
            return new Promise((resolve, reject) => {
                let settled = false;
                const timer = setTimeout(() => {
                    if (settled) return;
                    settled = true;
                    reject(new Error(`${label} timed out after ${ms}ms`));
                }, ms);
                promise.then((res) => {
                    if (settled) return;
                    settled = true;
                    clearTimeout(timer);
                    resolve(res);
                }).catch((err) => {
                    if (settled) return;
                    settled = true;
                    clearTimeout(timer);
                    reject(err);
                });
            });
        };

        const getSelectedText = () => {
            const activeEditor = getActiveEditor();
            if (!activeEditor) return '';
            const sel = activeEditor.getSelection();
            const model = activeEditor.getModel();
            if (sel && model) {
                return model.getValueInRange(sel).trim();
            }
            return '';
        };

        const searchDebounce = (() => {
            let t = null;
            return (fn, ms = 220) => {
                clearTimeout(t);
                t = setTimeout(fn, ms);
            };
        })();

        const ensurePanelMounted = (containerId) => {
            const fr = window.AhmadIDEModules?.app?.featureRegistry;
            if (!fr || typeof fr.ensureById !== 'function') return false;
            try {
                return fr.ensureById(containerId);
            } catch (_) {
                return false;
            }
        };

        let findReplaceDomWired = false;
        const wireFindReplaceDomOnce = () => {
            if (findReplaceDomWired) return;
            const {
                dialog,
                toggleBtn,
                replaceAllBtn,
                findInput,
                replaceInput,
                caseOption,
                wholeOption,
                regexOption
            } = getFindReplaceElements();

            if (!dialog || !toggleBtn || !findInput) return;

            toggleBtn.addEventListener('click', () => {
                const nextMode = findReplaceState.mode === 'replace' ? 'find' : 'replace';
                toggleFindMode(nextMode);
                executeFindReplacePreview(false);
            });

            document.getElementById('closeFindDialog')?.addEventListener('click', closeFindReplaceDialog);
            replaceAllBtn?.addEventListener('click', confirmAndReplaceAll);

            findInput.addEventListener('input', () => searchDebounce(() => executeFindReplacePreview(false), 200));
            findInput.addEventListener('keydown', (e) => {
                if (e.key === 'Enter') executeFindReplacePreview(false);
                if (e.key === 'Escape') closeFindReplaceDialog();
            });

            replaceInput?.addEventListener('input', () => {
                if (findReplaceState.mode === 'replace') {
                    searchDebounce(() => executeFindReplacePreview(false), 280);
                }
            });
            replaceInput?.addEventListener('keydown', (e) => {
                if (e.key === 'Enter') executeFindReplacePreview(e.ctrlKey || e.metaKey);
                if (e.key === 'Escape') closeFindReplaceDialog();
            });

            [caseOption, wholeOption, regexOption].forEach((el) => {
                el?.addEventListener('change', () => executeFindReplacePreview(false));
            });

            dialog.addEventListener('keydown', (e) => {
                if (e.key === 'Enter') {
                    if (findReplaceState.mode === 'replace' && e.ctrlKey) {
                        confirmAndReplaceAll();
                    } else {
                        executeFindReplacePreview(false);
                    }
                }
                if (e.key === 'Escape') closeFindReplaceDialog();
            });

            findReplaceDomWired = true;
        };

        let searchEverywhereDomWired = false;
        const wireSearchEverywhereDomOnce = () => {
            if (searchEverywhereDomWired) return;
            const { input } = getSearchEverywhereElements();
            if (!input) return;
            input.addEventListener('input', () => renderSearchEverywhereResults(input.value));
            input.addEventListener('keydown', (e) => {
                const resultsHost = getSearchEverywhereElements().resultsHost;
                const items = resultsHost?.querySelectorAll('.search-everywhere-item') || [];
                if (e.key === 'ArrowDown') {
                    e.preventDefault();
                    searchEverywhereState.selectedIndex = Math.min(items.length - 1, searchEverywhereState.selectedIndex + 1);
                    renderSearchEverywhereResults(input.value);
                } else if (e.key === 'ArrowUp') {
                    e.preventDefault();
                    searchEverywhereState.selectedIndex = Math.max(0, searchEverywhereState.selectedIndex - 1);
                    renderSearchEverywhereResults(input.value);
                } else if (e.key === 'Enter') {
                    e.preventDefault();
                    const refreshedItems = getSearchEverywhereElements().resultsHost?.querySelectorAll('.search-everywhere-item') || [];
                    const active = refreshedItems[searchEverywhereState.selectedIndex];
                    const path = active?.dataset?.path;
                    if (path) openSearchEverywhereResult(path);
                } else if (e.key === 'Escape') {
                    closeSearchEverywhere();
                }
            });
            searchEverywhereDomWired = true;
        };

        const getFindReplaceElements = () => ({
            overlay: document.getElementById('findOverlay'),
            dialog: document.getElementById('findDialog'),
            title: document.getElementById('findDialogTitle'),
            scopeLabel: document.getElementById('findScopeLabel'),
            scopePath: document.getElementById('findScopePath'),
            modePill: document.getElementById('findModePill'),
            toggleBtn: document.getElementById('findReplaceToggleBtn'),
            replaceRow: document.getElementById('replaceRow'),
            replaceAllBtn: document.getElementById('replaceAllBtn'),
            findInput: document.getElementById('findQueryInput'),
            replaceInput: document.getElementById('replaceQueryInput'),
            resultsHost: document.getElementById('findResults'),
            caseOption: document.getElementById('findCaseOption'),
            wholeOption: document.getElementById('findWholeOption'),
            regexOption: document.getElementById('findRegexOption')
        });

        const getSearchEverywhereElements = () => ({
            overlay: document.getElementById('searchEverywhereOverlay'),
            dialog: document.getElementById('searchEverywhereDialog'),
            input: document.getElementById('searchEverywhereInput'),
            resultsHost: document.getElementById('searchEverywhereResults')
        });

        const getScopeInfo = () => {
            const active = getActiveRoutine();
            const normalized = normalizeRoutineTarget(active);
            const folder = normalized.folder || (normalized.path?.includes('/') ? normalized.path.split('/')[0] : null) || 'localr';
            const root = (getCurrentProject()?.routinesPath || '').replace(/\\/g, '/');
            const scopePath = root && folder ? `${root}/${folder}` : (folder || 'UNKNOWN – NEED DESIGN DECISION: localR / routines paths');
            return { folder, scopePath };
        };

        async function listScopedRoutines(folder) {
            try {
                const query = folder ? `${folder}/` : '';
                const res = await withTimeout(window.ahmadIDE.listRoutines(query), 8000, 'List routines');
                if (res?.ok && Array.isArray(res.routines)) {
                    routinesCacheRef.value = res.routines || routinesCacheRef.value;
                }
            } catch (e) {
                // fall through to cache
            }
            const prefix = folder ? `${folder.toLowerCase()}/` : '';
            const cache = routinesCacheRef.value || [];
            return cache.filter(r => !prefix || r.toLowerCase().startsWith(prefix));
        }

        const buildSearchRegex = (term, opts = {}) => {
            if (opts.regex) {
                try {
                    return new RegExp(term, opts.matchCase ? 'g' : 'gi');
                } catch (err) {
                    return null;
                }
            }
            const safe = escapeRegex(term);
            const pattern = opts.wholeWords ? `\\b${safe}\\b` : safe;
            return new RegExp(pattern, opts.matchCase ? 'g' : 'gi');
        };

        const renderFindResults = (hits = [], term = '', mode = 'find') => {
            const host = getFindReplaceElements().resultsHost;
            if (!host) return;
            if (!hits.length) {
                host.textContent = 'No matches found.';
                return;
            }
            host.innerHTML = '';
            const limit = hits.slice(0, 400);
            const markRegex = term ? buildSearchRegex(term, findReplaceState.options) : null;
            limit.forEach(hit => {
                const row = document.createElement('div');
                row.className = 'search-hit';
                const header = document.createElement('div');
                header.className = 'search-hit-header';
                const title = document.createElement('span');
                title.textContent = `${hit.file}:${hit.line}`;
                title.className = 'search-hit-file';
                const chip = document.createElement('span');
                chip.className = 'pill subtle';
                chip.textContent = hit.kind || (mode === 'replace' ? 'Replace' : 'Match');
                header.appendChild(chip);
                header.appendChild(title);

                const snippet = document.createElement('div');
                snippet.className = 'search-hit-snippet';
                if (hit.snippet && markRegex) {
                    snippet.innerHTML = escapeHtml(hit.snippet).replace(markRegex, (m) => `<span class="search-hit-mark">${m}</span>`);
                } else {
                    snippet.textContent = hit.snippet || '';
                }

                row.appendChild(header);
                row.appendChild(snippet);
                row.onclick = async () => {
                    const routineState = getRoutineState();
                    const activeEditor = getActiveEditor();
                    await loadRoutineByName(hit.file, routineState, activeEditor, routinesCacheRef.value, getGlobalTerminalState());
                    revealLine(hit.line);
                    closeFindReplaceDialog();
                };
                host.appendChild(row);
            });

            if (hits.length > limit.length) {
                const more = document.createElement('div');
                more.className = 'search-hit-file';
                more.textContent = `Showing first ${limit.length} of ${hits.length} matches. Refine your query to narrow results.`;
                host.appendChild(more);
            }
        };

        const updateFindScopeLabels = () => {
            const { scopePath, folder } = getScopeInfo();
            const { scopeLabel, scopePath: scopePathEl } = getFindReplaceElements();
            findReplaceState.scopeFolder = folder;
            if (scopeLabel) scopeLabel.textContent = `Scope: ${scopePath}`;
            if (scopePathEl) scopePathEl.textContent = `Scope: ${scopePath}`;
        };

        const toggleFindMode = (mode) => {
            findReplaceState.mode = mode;
            const {
                dialog,
                replaceRow,
                replaceAllBtn,
                toggleBtn,
                modePill: pill,
                title
            } = getFindReplaceElements();
            if (dialog) dialog.dataset.mode = mode;
            if (replaceRow) replaceRow.style.display = mode === 'replace' ? 'flex' : 'none';
            if (replaceAllBtn) replaceAllBtn.style.display = mode === 'replace' ? 'inline-flex' : 'none';
            if (toggleBtn) toggleBtn.textContent = mode === 'replace' ? 'Switch to Find' : 'Switch to Replace';
            if (pill) pill.textContent = mode === 'replace' ? 'Replace' : 'Find';
            if (title) title.textContent = mode === 'replace' ? 'Replace in Files' : 'Find in Files';
        };

        const executeFindReplacePreview = async (applyReplace = false) => {
            const { findInput, replaceInput, resultsHost: host, caseOption, wholeOption, regexOption } = getFindReplaceElements();
            const term = (findInput?.value || '').trim();
            const replaceTerm = (replaceInput?.value || '').trim();
            if (!host) return;

            const fail = (msg) => {
                host.textContent = msg;
                showToast('error', 'Find', msg);
            };

            try {
                if (!term) {
                    host.textContent = 'Enter text to search.';
                    return;
                }

                const options = {
                    matchCase: !!caseOption?.checked,
                    wholeWords: !!wholeOption?.checked,
                    regex: !!regexOption?.checked
                };
                findReplaceState.options = options;
                const regex = buildSearchRegex(term, options);
                if (!regex) {
                    host.textContent = 'Invalid regular expression.';
                    return;
                }

                const token = ++findReplaceState.token;
                const { folder } = getScopeInfo();
                host.textContent = applyReplace ? 'Replacing across files…' : 'Searching…';
                logger.info('SEARCH_FIND_IN_PATH_QUERY', { term, replaceTerm: applyReplace ? replaceTerm : undefined, scopeFolder: folder, options, mode: findReplaceState.mode });

                // Fast path: ask backend to grep all routines in scope
                if (!applyReplace && window.ahmadIDE.searchRoutines) {
                    try {
                        const fast = await withTimeout(
                            window.ahmadIDE.searchRoutines(term, { folder, ...options }),
                            10000,
                            'Search routines (fast)'
                        );
                        if (fast?.ok) {
                            const hits = fast.hits || [];
                            renderFindResults(hits, term, findReplaceState.mode);
                            logger.info('SEARCH_FIND_IN_PATH_RESULTS', { term, count: hits.length, mode: findReplaceState.mode });
                            return;
                        }
                        console.warn('Fast search failed, falling back to slow scan:', fast?.error);
                    } catch (err) {
                        console.warn('Fast search errored, falling back:', err);
                    }
                }

                const routines = await listScopedRoutines(folder);
                if (!routines.length) {
                    host.textContent = `No routines found in scope "${folder || 'UNKNOWN – NEED DESIGN DECISION: localR / routines paths'}".`;
                    return;
                }

                const total = routines.length;
                let processed = 0;
                host.textContent = applyReplace
                    ? `Replacing across files… (0/${total})`
                    : `Searching… (0/${total})`;
                let hits = [];
                let replaceCount = 0;
                let failed = false;

                const searchOne = async (routine) => {
                    if (token !== findReplaceState.token || failed) return;
                    let read = null;
                    try {
                        read = await withTimeout(window.ahmadIDE.readRoutine(routine), 8000, `Read ${routine}`);
                    } catch (err) {
                        console.error('Find/Replace read failed:', err);
                        failed = true;
                        fail(`Search failed on ${routine}: ${err?.message || err}`);
                        return;
                    }
                    if (!read?.ok) return;

                    const code = read.code || '';
                    const lines = code.split(/\r?\n/);
                    let mutated = code;
                    lines.forEach((line, idx) => {
                        regex.lastIndex = 0;
                        if (regex.test(line)) {
                            hits.push({ file: routine, line: idx + 1, snippet: line });
                        }
                    });

                    if (applyReplace && findReplaceState.mode === 'replace' && replaceTerm) {
                        mutated = code.replace(regex, replaceTerm);
                        if (mutated !== code) {
                            replaceCount += 1;
                            try {
                                await window.ahmadIDE.saveRoutine(routine, mutated);
                            } catch (err) {
                                console.error('Replace failed to save', err);
                            }
                            const openTab = findOpenTab(routine, { exact: true });
                            if (openTab) {
                                openTab.content = mutated;
                                const model = tabModels.get(openTab.id);
                                if (model) {
                                    model.setValue(mutated);
                                    openTab.isDirty = false;
                                    renderTabs();
                                } else if (getActiveTabId() === openTab.id && getActiveEditor()) {
                                    getActiveEditor().setValue(mutated);
                                }
                            }
                        }
                    }
                };

                // Concurrency-limited search to speed up large projects
                const limit = Math.min(12, Math.max(2, navigator.hardwareConcurrency || 4));
                let cursor = 0;
                const worker = async () => {
                    while (cursor < total && token === findReplaceState.token && !failed) {
                        const idx = cursor++;
                        const routine = routines[idx];
                        await searchOne(routine);
                        processed += 1;
                        if (!failed && token === findReplaceState.token) {
                            if (!applyReplace && processed % 20 === 0) {
                                host.textContent = `Searching… (${processed}/${total})`;
                            } else if (applyReplace && processed % 20 === 0) {
                                host.textContent = `Replacing across files… (${processed}/${total})`;
                            }
                            if (!applyReplace && hits.length && hits.length % 50 === 0) {
                                renderFindResults(hits, term, findReplaceState.mode);
                            }
                        }
                    }
                };
                const workers = Array.from({ length: limit }, () => worker());
                await Promise.all(workers);

                if (token !== findReplaceState.token || failed) return;
                renderFindResults(hits, term, findReplaceState.mode);
                logger.info('SEARCH_FIND_IN_PATH_RESULTS', { term, count: hits.length, mode: findReplaceState.mode });
                if (applyReplace && host) {
                    const summary = document.createElement('div');
                    summary.className = 'search-hit-file';
                    summary.textContent = replaceCount
                        ? `Applied replacements in ${replaceCount} file(s).`
                        : 'No replacements applied.';
                    host.appendChild(summary);
                }
            } catch (err) {
                console.error('Find/Replace failed:', err);
                fail(`Search failed: ${err?.message || err}`);
            }
        };

        function openFindReplaceDialog(mode = 'find', prefill = '') {
            ensurePanelMounted('findDialog');
            wireFindReplaceDomOnce();
            toggleFindMode(mode);
            updateFindScopeLabels();
            const { overlay, dialog, findInput, replaceInput } = getFindReplaceElements();
            overlay?.classList.remove('hidden');
            dialog?.classList.remove('hidden');
            logger.info('SEARCH_DIALOG_OPEN', { mode, prefill });
            if (findInput) {
                findInput.value = prefill || findInput.value;
                findInput.focus();
                if (prefill) findInput.select();
            }
            if (mode === 'replace' && replaceInput && !replaceInput.value) {
                replaceInput.value = '';
            }
            executeFindReplacePreview();
        }

        function closeFindReplaceDialog() {
            const { overlay, dialog } = getFindReplaceElements();
            overlay?.classList.add('hidden');
            dialog?.classList.add('hidden');
            findReplaceState.token += 1; // cancel inflight searches
        }

        const confirmAndReplaceAll = () => {
            const { findInput, replaceInput } = getFindReplaceElements();
            const term = (findInput?.value || '').trim();
            if (!term) return;
            const replaceTerm = (replaceInput?.value || '').trim();
            if (!replaceTerm) {
                showToast('error', 'Replace', 'Enter replacement text before replacing.');
                return;
            }
            const ok = window.confirm(`Replace all occurrences of "${term}" in the current folder?`);
            if (!ok) return;
            executeFindReplacePreview(true);
        };

        const openSearchEverywhereResult = async (path) => {
            if (!path) return;
            try {
                logger.info('SEARCH_EVERYWHERE_OPEN_RESULT', { path });
                const routineState = getRoutineState();
                const activeEditor = getActiveEditor();
                const ok = await loadRoutineByName(path, routineState, activeEditor, routinesCacheRef.value, getGlobalTerminalState());
                if (ok !== false) closeSearchEverywhere();
            } catch (err) {
                showToast('error', 'Search Everywhere', err?.message || 'Could not open selection.');
                logger.error('SEARCH_EVERYWHERE_OPEN_ERROR', { path, message: err?.message, stack: err?.stack });
            }
        };

        const openSearchEverywhere = async (prefill = '') => {
            ensurePanelMounted('searchEverywhereDialog');
            wireSearchEverywhereDomOnce();
            const { overlay, dialog, input, resultsHost } = getSearchEverywhereElements();
            overlay?.classList.remove('hidden');
            dialog?.classList.remove('hidden');
            searchEverywhereState.open = true;
            searchEverywhereState.selectedIndex = 0;
            logger.info('SEARCH_EVERYWHERE_OPEN', { prefill });
            if (input) {
                input.value = prefill || '';
                input.focus();
                if (prefill) input.select();
            }
            if (resultsHost) resultsHost.textContent = 'Indexing project files…';
            if (!searchEverywhereState.index.length) {
                // Search across ALL folders: localr, routines, and any others
                const allRoutines = new Set();
                const folders = ['localr', 'routines'];

                for (const folder of folders) {
                    try {
                        const res = await withTimeout(
                            window.ahmadIDE.listRoutines(`${folder}/`),
                            8000,
                            `List routines in ${folder}`
                        );
                        if (res?.ok && Array.isArray(res.routines)) {
                            res.routines.forEach(r => allRoutines.add(r));
                        }
                    } catch (err) {
                        console.warn(`Failed to list routines in ${folder}:`, err);
                    }
                }

                // Fallback: also try listing without folder prefix to catch any missed routines
                try {
                    const res = await withTimeout(
                        window.ahmadIDE.listRoutines(''),
                        8000,
                        'List all routines'
                    );
                    if (res?.ok && Array.isArray(res.routines)) {
                        res.routines.forEach(r => allRoutines.add(r));
                    }
                } catch (err) {
                    console.warn('Failed to list all routines:', err);
                }

                if (allRoutines.size > 0) {
                    const routinesArray = Array.from(allRoutines);
                    routinesCacheRef.value = routinesArray;
                    searchEverywhereState.index = routinesArray.map(r => ({
                        path: r,
                        name: r.split('/').pop(),
                        folder: r.split('/')[0] || ''
                    }));
                } else if (resultsHost) {
                    resultsHost.textContent = 'Unable to index project files.';
                    return;
                }
            }
            renderSearchEverywhereResults(input?.value || '');
        };

        const closeSearchEverywhere = () => {
            const { overlay, dialog } = getSearchEverywhereElements();
            overlay?.classList.add('hidden');
            dialog?.classList.add('hidden');
            searchEverywhereState.open = false;
        };

        const renderSearchEverywhereResults = (query = '') => {
            const host = getSearchEverywhereElements().resultsHost;
            if (!host) return;
            const q = (query || '').toLowerCase();
            logger.debug('SEARCH_EVERYWHERE_QUERY', { query });
            const scored = searchEverywhereState.index
                .map(item => {
                    const name = (item.name || '').toLowerCase();
                    const path = (item.path || '').toLowerCase();
                    const idx = name.indexOf(q);
                    const pathIdx = path.indexOf(q);
                    const hit = q ? Math.min(idx === -1 ? 9999 : idx, pathIdx === -1 ? 9999 : pathIdx) : 0;
                    return { item, score: hit };
                })
                .filter(entry => q ? entry.score < 9999 : true)
                .sort((a, b) => a.score - b.score || a.item.name.localeCompare(b.item.name));

            const results = scored.slice(0, 60).map(s => s.item);
            host.innerHTML = '';
            if (!results.length) {
                searchEverywhereState.selectedIndex = 0;
                host.textContent = q ? 'No matches.' : 'No files indexed yet.';
                logger.info('SEARCH_EVERYWHERE_RESULTS', { query, count: 0 });
                return;
            }
            logger.info('SEARCH_EVERYWHERE_RESULTS', { query, count: results.length });
            searchEverywhereState.selectedIndex = Math.max(0, Math.min(searchEverywhereState.selectedIndex, results.length - 1));

            results.forEach((res, idx) => {
                const row = document.createElement('div');
                row.className = 'search-everywhere-item' + (idx === searchEverywhereState.selectedIndex ? ' active' : '');
                row.dataset.path = res.path;
                const title = document.createElement('span');
                title.textContent = res.name;
                const path = document.createElement('span');
                path.className = 'search-everywhere-path';
                path.textContent = res.path;
                row.appendChild(title);
                row.appendChild(path);
                row.onclick = () => openSearchEverywhereResult(res.path);
                host.appendChild(row);
            });
        };

        return {
            getSelectedText,
            searchDebounce,
            openFindReplaceDialog,
            closeFindReplaceDialog,
            executeFindReplacePreview,
            updateFindScopeLabels,
            toggleFindMode,
            confirmAndReplaceAll,
            openSearchEverywhereResult,
            openSearchEverywhere,
            closeSearchEverywhere,
            renderSearchEverywhereResults
        };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.search = window.AhmadIDEModules.search || {};
        window.AhmadIDEModules.search.createSearchManager = createSearchManager;
    }
})();
