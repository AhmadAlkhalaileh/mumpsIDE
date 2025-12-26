(() => {
    const ACCESS_ORDER = Object.freeze(['READ', 'WRITE', 'KILL', 'MERGE', 'UNKNOWN']);

    const createNoopLogger = () => ({
        debug: () => { },
        info: () => { },
        warn: () => { },
        error: () => { },
        isEnabled: () => false
    });

    function createGlobalImpactManager({ deps } = {}) {
        const logger = deps?.logger || createNoopLogger();
        const showToast = deps?.showToast || (() => { });
        const ensureBottomPanel = deps?.ensureBottomPanel || null;
        const toggleToolWindowPanel = deps?.toggleToolWindowPanel || (() => { });
        const getMonaco = deps?.getMonaco || (() => (typeof monaco !== 'undefined' ? monaco : null));
        const getActiveEditor = deps?.getActiveEditor || (() => null);
        const getProjectRoot = deps?.getProjectRoot || (() => (typeof window !== 'undefined' ? window.currentProject?.projectPath : ''));
        const openRoutine = deps?.openRoutine || (async () => ({ ok: false, error: 'Open routine unavailable' }));
        const runDirectCommand = deps?.runDirectCommand || (async () => ({ ok: false, error: 'Direct Mode unavailable' }));
        const getConnection = deps?.getConnection || (async () => {
            try {
                return await window.ahmadIDE?.getConnection?.();
            } catch (_) {
                return null;
            }
        });

        const indexService = deps?.indexService || window.AhmadIDEModules?.services?.globalReferenceIndexService || null;
        if (!indexService) {
            throw new Error('GlobalImpactManager requires globalReferenceIndexService');
        }

        const state = {
            projectRoot: '',
            queryRaw: '',
            queryNorm: '',
            lastResults: null,
            accessFilter: new Set(ACCESS_ORDER),
            collapsedFiles: new Set(),
            indexing: false,
            progress: null,
            lastIndexMs: null,
            runtimeAvailable: false,
            _wired: false,
            _domReady: false
        };

        const dom = {
            root: null,
            input: null,
            suggestions: null,
            results: null,
            metaLeft: null,
            metaRight: null,
            exportBtn: null,
            exportFormat: null,
            refreshBtn: null,
            searchBtn: null,
            runtimeBadge: null,
            directBtn: null,
            filterChips: []
        };

        let unsubscribeIndex = null;
        let _refreshTimer = null;
        let _suggestTimer = null;
        let _domReadyResolve = null;
        const _domReadyPromise = new Promise((resolve) => { _domReadyResolve = resolve; });

        const normalizeGlobalInput = (value) => {
            const raw = String(value || '').trim();
            if (!raw) return '';
            const withCaret = raw.startsWith('^') ? raw : '^' + raw;
            const m = withCaret.match(/^\^([A-Za-z%][A-Za-z0-9]*)/);
            return m ? ('^' + m[1]).toUpperCase() : '';
        };

        const escapeHtml = (value) => String(value ?? '')
            .replace(/&/g, '&amp;')
            .replace(/</g, '&lt;')
            .replace(/>/g, '&gt;')
            .replace(/"/g, '&quot;')
            .replace(/'/g, '&#39;');

        const escapeCsv = (value) => {
            const s = String(value ?? '');
            if (/[",\n\r]/.test(s)) return `"${s.replace(/"/g, '""')}"`;
            return s;
        };

        const slugify = (value) => String(value || '')
            .replace(/[^A-Za-z0-9._-]+/g, '_')
            .replace(/^_+|_+$/g, '')
            .slice(0, 64) || 'export';

        const setMeta = (left, right) => {
            if (dom.metaLeft) dom.metaLeft.textContent = String(left || '');
            if (dom.metaRight) dom.metaRight.textContent = String(right || '');
        };

        const computeRuntimeAvailable = async () => {
            const conn = await getConnection();
            const type = String(conn?.type || '').trim();
            if (type === 'docker') return !!conn?.connectionId;
            if (type === 'ssh') return !!String(conn?.config?.host || '').trim();
            return false;
        };

        const updateRuntimeUi = async () => {
            const available = await computeRuntimeAvailable();
            state.runtimeAvailable = available;
            if (!dom.runtimeBadge || !dom.directBtn) return;
            dom.runtimeBadge.classList.toggle('hidden', !available);
            dom.runtimeBadge.classList.toggle('active', available);
            dom.directBtn.disabled = !available || !state.queryNorm;
        };

        const isPanelVisible = () => {
            const panel = document.getElementById('globalImpactPanel');
            return !!panel && !panel.classList.contains('hidden');
        };

        const openPanel = async ({ focusSearch = true } = {}) => {
            if (typeof ensureBottomPanel === 'function') {
                ensureBottomPanel('globalImpactPanel');
            } else {
                toggleToolWindowPanel('globalImpactPanel', 'bottom');
            }

            await _domReadyPromise;
            await ensureIndexed({ force: false });
            await updateRuntimeUi();

            if (focusSearch) {
                requestAnimationFrame(() => {
                    try {
                        dom.input?.focus?.();
                        dom.input?.select?.();
                    } catch (_) { }
                });
            }
        };

        const ensureIndexed = async ({ force = false } = {}) => {
            const root = String(getProjectRoot() || '').trim();
            if (!root) {
                // No local project open. Still allow connected-environment search (Docker/SSH) via indexService.queryAsync.
                setMeta('Scope: environment', '');
                return { ok: true, scope: 'environment' };
            }

            const rootChanged = root !== state.projectRoot;
            state.projectRoot = root;

            try {
                // Indexer is cached; avoid reindexing unless forced or project root changes.
                if (rootChanged || force || !indexService.isReady()) {
                    setMeta('Index: scanning…', '');
                    const res = await indexService.startIndexing({ projectRoot: root, force: true });
                    if (!res?.ok) {
                        const msg = String(res?.error || (res?.canceled ? 'Index canceled' : 'Index failed')).trim();
                        setMeta('Index: error', msg);
                        showToast('error', 'Global Impact', msg);
                        return { ok: false, error: msg };
                    }
                }
                return { ok: true };
            } catch (err) {
                const msg = String(err?.message || err || 'Index failed');
                setMeta('Index: error', msg);
                showToast('error', 'Global Impact', msg);
                return { ok: false, error: msg };
            }
        };

        const setQuery = async (value, { run = true } = {}) => {
            state.queryRaw = String(value || '').trim();
            state.queryNorm = normalizeGlobalInput(state.queryRaw);
            if (dom.input && dom.input.value !== state.queryRaw) dom.input.value = state.queryRaw;
            if (dom.directBtn) dom.directBtn.disabled = !state.runtimeAvailable || !state.queryNorm;

            if (!run) return;
            await runQuery({ focusIfEmpty: false });
        };

        const toOpenRoutineKey = (fileRel) => {
            const rel = String(fileRel || '').replace(/\\/g, '/').replace(/^\/+/, '');
            // ReadRoutine() only supports folder/routine.m; if a path is nested under localr/ or routines/,
            // drop that prefix so nested folders can still be addressed as "SUBDIR/ROUTINE.m".
            if (/^(localr|routines)\/.+\/.+\.m$/i.test(rel)) {
                return rel.replace(/^(localr|routines)\//i, '');
            }
            return rel;
        };

        const navigateToRef = async (ref) => {
            const fileRel = ref?.fileRel || '';
            const openKey = toOpenRoutineKey(fileRel);
            const line = Number(ref?.line || 0) || 1;
            const startColumn = Number(ref?.range?.startColumn || 1) || 1;
            const endColumn = Number(ref?.range?.endColumn || (startColumn + 1)) || (startColumn + 1);

            const res = await openRoutine(openKey);
            if (res?.ok === false && res?.error) {
                showToast('error', 'Open', String(res.error));
                return;
            }

            const editor = getActiveEditor();
            const monacoRef = getMonaco();
            if (!editor || !monacoRef?.Range) return;

            const model = editor.getModel?.();
            const safeLine = model ? Math.max(1, Math.min(model.getLineCount(), line)) : line;
            const maxCol = model ? (model.getLineMaxColumn(safeLine) || 1) : 1;
            const safeStart = Math.max(1, Math.min(maxCol, startColumn));
            const safeEnd = Math.max(safeStart, Math.min(maxCol, endColumn));
            const range = new monacoRef.Range(safeLine, safeStart, safeLine, safeEnd);

            try {
                editor.revealRangeInCenter(range);
                editor.setSelection(range);
                editor.focus();
            } catch (_) { }
        };

        const buildEmptyState = (message) => {
            const host = dom.results;
            if (!host) return;
            host.innerHTML = '';
            const empty = document.createElement('div');
            empty.className = 'ps-tree-empty';
            empty.textContent = String(message || '');
            host.appendChild(empty);
        };

        const appendHighlighted = (host, text, needleUpper) => {
            const raw = String(text ?? '');
            const needle = String(needleUpper || '').trim();
            if (!needle) {
                host.textContent = raw;
                return;
            }

            const upper = raw.toUpperCase();
            const n = needle.toUpperCase();
            let i = 0;
            while (true) {
                const idx = upper.indexOf(n, i);
                if (idx === -1) break;
                if (idx > i) host.appendChild(document.createTextNode(raw.slice(i, idx)));
                const mark = document.createElement('span');
                mark.className = 'ps-gi-mark';
                mark.textContent = raw.slice(idx, idx + needle.length);
                host.appendChild(mark);
                i = idx + needle.length;
            }
            if (i < raw.length) host.appendChild(document.createTextNode(raw.slice(i)));
        };

        const renderResults = (data) => {
            const host = dom.results;
            if (!host) return;
            host.innerHTML = '';

            const total = Number(data?.total || 0) || 0;
            if (!total) {
                const msg = state.queryNorm ? `No references found for ${state.queryNorm}` : 'Type a global name like ^DIC to see where it is used.';
                buildEmptyState(msg);
                return;
            }

            const frag = document.createDocumentFragment();

            (data.files || []).forEach((fileGroup) => {
                const fileRel = String(fileGroup?.fileRel || '');
                const count = Number(fileGroup?.count || 0) || 0;
                const collapsed = state.collapsedFiles.has(fileRel);

                const wrap = document.createElement('div');
                wrap.className = 'ps-gi-file';
                wrap.dataset.fileRel = fileRel;

                const header = document.createElement('div');
                header.className = 'ps-gi-file-header';
                header.dataset.role = 'file-header';
                header.dataset.fileRel = fileRel;

                const chevron = document.createElement('span');
                chevron.setAttribute('data-ui-icon', collapsed ? 'chevron-right' : 'chevron-down');
                chevron.setAttribute('data-ui-icon-size', '12');
                chevron.setAttribute('aria-hidden', 'true');
                header.appendChild(chevron);

                const fileText = document.createElement('div');
                fileText.className = 'ps-gi-file-path';
                fileText.textContent = fileRel || '(unknown file)';
                header.appendChild(fileText);

                const meta = document.createElement('div');
                meta.className = 'ps-gi-file-meta';
                meta.textContent = `${count}×`;
                header.appendChild(meta);

                wrap.appendChild(header);

                const hits = document.createElement('div');
                hits.dataset.role = 'file-hits';
                hits.dataset.fileRel = fileRel;
                hits.classList.toggle('hidden', collapsed);

                (fileGroup.refs || []).forEach((ref) => {
                    const hit = document.createElement('div');
                    hit.className = 'ps-gi-hit';
                    hit.dataset.role = 'hit';
                    hit.dataset.fileRel = fileRel;
                    hit.dataset.line = String(ref?.line || '');
                    hit.dataset.start = String(ref?.range?.startColumn || '');
                    hit.dataset.end = String(ref?.range?.endColumn || '');
                    hit.dataset.access = String(ref?.access || '');

                    const metaLeft = document.createElement('div');
                    metaLeft.className = 'ps-gi-hit-meta';

                    const lineEl = document.createElement('div');
                    lineEl.className = 'ps-gi-hit-line';
                    lineEl.textContent = `L${ref?.line || ''}`;
                    metaLeft.appendChild(lineEl);

                    const access = document.createElement('div');
                    access.className = 'ps-gi-access';
                    access.dataset.access = String(ref?.access || 'UNKNOWN').toUpperCase();
                    access.textContent = access.dataset.access;
                    metaLeft.appendChild(access);

                    const snippet = document.createElement('div');
                    snippet.className = 'ps-gi-hit-snippet';
                    appendHighlighted(snippet, ref?.snippet || '', state.queryNorm);

                    hit.appendChild(metaLeft);
                    hit.appendChild(snippet);

                    hits.appendChild(hit);
                });

                wrap.appendChild(hits);
                frag.appendChild(wrap);
            });

            host.appendChild(frag);
        };

        const runQuery = async ({ focusIfEmpty = true, skipIndexCheck = false } = {}) => {
            if (!skipIndexCheck) {
                await ensureIndexed({ force: false });
            }

            const q = normalizeGlobalInput(dom.input?.value || state.queryRaw);
            state.queryRaw = dom.input?.value || state.queryRaw;
            state.queryNorm = q;
            if (!q) {
                state.lastResults = null;
                buildEmptyState('Type a global name like ^DIC to see where it is used.');
                if (focusIfEmpty) {
                    try { dom.input?.focus?.(); } catch (_) { }
                }
                setMeta(state.projectRoot ? (state.indexing ? 'Index: scanning…' : 'Index: ready') : 'Scope: environment', '');
                if (dom.directBtn) dom.directBtn.disabled = !state.runtimeAvailable;
                return;
            }

            const access = Array.from(state.accessFilter || []).filter(Boolean);
            let data;
            try {
                if (typeof indexService.queryAsync === 'function') {
                    data = await indexService.queryAsync(q, { access });
                } else {
                    data = indexService.query(q, { access });
                }
            } catch (err) {
                const msg = String(err?.message || err || 'Query failed').trim();
                setMeta('Index: error', msg);
                buildEmptyState(msg);
                showToast('error', 'Global Impact', msg);
                return;
            }

            if (data?.error) {
                const msg = String(data.error || 'Query failed').trim();
                setMeta('Index: error', msg);
                buildEmptyState(msg);
                showToast('error', 'Global Impact', msg);
                return;
            }
            state.lastResults = data;

            const total = Number(data?.total || 0) || 0;
            const filesCount = Array.isArray(data?.files) ? data.files.length : 0;
            setMeta(
                state.projectRoot ? (state.indexing ? `Index: scanning…` : `Index: ready`) : `Scope: environment`,
                `${q} · ${total} reference${total === 1 ? '' : 's'} · ${filesCount} file${filesCount === 1 ? '' : 's'}`
            );

            renderResults(data);
            if (dom.directBtn) dom.directBtn.disabled = !state.runtimeAvailable || !state.queryNorm;
        };

        const scheduleQueryRefresh = () => {
            if (_refreshTimer) return;
            _refreshTimer = setTimeout(() => {
                _refreshTimer = null;
                if (!isPanelVisible()) return;
                if (!state.queryNorm) return;
                runQuery({ focusIfEmpty: false, skipIndexCheck: true }).catch(() => { });
            }, 120);
        };

        const renderSuggestions = (items) => {
            if (!dom.suggestions) return;
            dom.suggestions.innerHTML = '';
            if (!items.length) {
                dom.suggestions.classList.add('hidden');
                return;
            }
            const frag = document.createDocumentFragment();
            items.forEach((it) => {
                const btn = document.createElement('button');
                btn.type = 'button';
                btn.className = 'ps-gi-suggest-item';
                btn.dataset.role = 'suggest';
                btn.dataset.global = it.globalUpper || it.global || '';
                const left = document.createElement('span');
                left.textContent = it.globalUpper || it.global || '';
                const right = document.createElement('span');
                right.className = 'ps-gi-suggest-count';
                right.textContent = `${Number(it.count || 0) || 0}`;
                btn.appendChild(left);
                btn.appendChild(right);
                frag.appendChild(btn);
            });
            dom.suggestions.appendChild(frag);
            dom.suggestions.classList.remove('hidden');
        };

        const scheduleSuggest = () => {
            if (_suggestTimer) clearTimeout(_suggestTimer);
            _suggestTimer = setTimeout(() => {
                _suggestTimer = null;
                if (!dom.input || !dom.suggestions) return;
                const prefix = normalizeGlobalInput(dom.input.value || '');
                if (!prefix) {
                    dom.suggestions.classList.add('hidden');
                    return;
                }
                const items = indexService.listGlobals({ prefix, limit: 12 }) || [];
                renderSuggestions(items);
            }, 60);
        };

        const exportResults = () => {
            const format = String(dom.exportFormat?.value || 'json').toLowerCase();
            const data = state.lastResults;
            if (!data?.globalUpper || !Array.isArray(data?.files)) {
                showToast('info', 'Export', 'No results to export');
                return;
            }

            const now = new Date();
            const stamp = now.toISOString().replace(/[:.]/g, '-');
            const baseName = slugify(`${data.globalUpper}_${stamp}`);
            const access = Array.from(state.accessFilter || []);

            if (format === 'csv') {
                const rows = [];
                rows.push(['global', 'file', 'line', 'startColumn', 'endColumn', 'access', 'snippet'].map(escapeCsv).join(','));
                data.files.forEach((f) => {
                    (f.refs || []).forEach((r) => {
                        rows.push([
                            data.globalUpper,
                            f.fileRel || '',
                            r.line || '',
                            r.range?.startColumn || '',
                            r.range?.endColumn || '',
                            r.access || '',
                            (r.snippet || '').trimEnd()
                        ].map(escapeCsv).join(','));
                    });
                });
                const blob = new Blob([rows.join('\n')], { type: 'text/csv;charset=utf-8' });
                downloadBlob(blob, `${baseName}.csv`);
                showToast('success', 'Export', `Saved ${baseName}.csv`);
                return;
            }

            const payload = {
                global: data.globalUpper,
                generatedAt: now.toISOString(),
                filters: { access },
                total: data.total || 0,
                files: data.files
            };
            const blob = new Blob([JSON.stringify(payload, null, 2)], { type: 'application/json;charset=utf-8' });
            downloadBlob(blob, `${baseName}.json`);
            showToast('success', 'Export', `Saved ${baseName}.json`);
        };

        const downloadBlob = (blob, filename) => {
            const url = URL.createObjectURL(blob);
            const a = document.createElement('a');
            a.href = url;
            a.download = filename;
            document.body.appendChild(a);
            a.click();
            a.remove();
            setTimeout(() => {
                try { URL.revokeObjectURL(url); } catch (_) { }
            }, 1000);
        };

        const runDirectModeInspect = async () => {
            const g = state.queryNorm;
            if (!g) return;
            const code = [
                `WRITE "DATA: ",$DATA(${g}),!`,
                `WRITE "FIRST: ",$ORDER(${g}("")),!`,
                `WRITE "LAST: ",$ORDER(${g}(""),-1),!`
            ].join('\n');

            const res = await runDirectCommand(code, { title: `Inspect ${g}` });
            if (res?.ok) return;
            const msg = String(res?.error || res?.stderr || 'Direct Mode failed').trim();
            if (msg) showToast('error', 'Direct Mode', msg);
        };

        const toggleAccessFilter = async (access) => {
            const key = String(access || '').toUpperCase();
            if (!ACCESS_ORDER.includes(key)) return;
            if (state.accessFilter.has(key)) state.accessFilter.delete(key);
            else state.accessFilter.add(key);

            dom.filterChips.forEach((btn) => {
                const a = String(btn?.dataset?.access || '').toUpperCase();
                btn.classList.toggle('active', !!a && state.accessFilter.has(a));
            });

            await runQuery({ focusIfEmpty: false, skipIndexCheck: true });
        };

        const extractGlobalAtCursor = (editor) => {
            if (!editor) return null;
            const model = editor.getModel?.();
            const pos = editor.getPosition?.();
            if (!model || !pos) return null;
            if (typeof model.getLanguageId === 'function' && model.getLanguageId() !== 'mumps') return null;

            const line = model.getLineContent(pos.lineNumber) || '';
            const column0 = Math.max(0, (pos.column || 1) - 1);

            const rx = /\^([A-Za-z%][A-Za-z0-9]*)/g;
            let m;
            while ((m = rx.exec(line))) {
                const start = m.index;
                const end = start + m[0].length;
                if (column0 >= start && column0 <= end) {
                    return ('^' + (m[1] || '')).toUpperCase();
                }
            }
            return null;
        };

        const openFromEditor = async (editor) => {
            const g = extractGlobalAtCursor(editor || getActiveEditor());
            await openPanel({ focusSearch: !g });
            if (g) {
                await setQuery(g, { run: true });
            }
        };

        const openForGlobal = async (globalName) => {
            const g = normalizeGlobalInput(globalName);
            await openPanel({ focusSearch: !g });
            if (g) {
                await setQuery(g, { run: true });
            }
        };

        const wireDom = () => {
            if (state._wired) return;

            const root = document.getElementById('globalImpactRoot');
            if (!root) return;

            dom.root = root;
            dom.input = document.getElementById('globalImpactSearchInput');
            dom.suggestions = document.getElementById('globalImpactSuggestions');
            dom.results = document.getElementById('globalImpactResults');
            dom.metaLeft = document.getElementById('globalImpactMetaLeft');
            dom.metaRight = document.getElementById('globalImpactMetaRight');
            dom.exportBtn = document.getElementById('globalImpactExportBtn');
            dom.exportFormat = document.getElementById('globalImpactExportFormat');
            dom.refreshBtn = document.getElementById('globalImpactRefreshIndexBtn');
            dom.searchBtn = document.getElementById('globalImpactSearchBtn');
            dom.runtimeBadge = document.getElementById('globalImpactRuntimeBadge');
            dom.directBtn = document.getElementById('globalImpactDirectModeBtn');
            dom.filterChips = Array.from(root.querySelectorAll('.ps-gi-chip'));

            if (!dom.input || !dom.suggestions || !dom.results) return;

            state._wired = true;
            state._domReady = true;
            _domReadyResolve?.();

            // Default: all filters enabled
            dom.filterChips.forEach((btn) => {
                const a = String(btn?.dataset?.access || '').toUpperCase();
                btn.classList.toggle('active', !!a && state.accessFilter.has(a));
            });

            // Input + suggestions
            dom.input.addEventListener('input', () => scheduleSuggest());
            dom.input.addEventListener('focus', () => scheduleSuggest());
            dom.input.addEventListener('keydown', (e) => {
                if (e.key === 'Enter') {
                    e.preventDefault();
                    dom.suggestions?.classList.add('hidden');
                    runQuery({ focusIfEmpty: false }).catch(() => { });
                }
                if (e.key === 'Escape') {
                    dom.suggestions?.classList.add('hidden');
                }
            });

            document.addEventListener('click', (e) => {
                if (!dom.suggestions || dom.suggestions.classList.contains('hidden')) return;
                if (e.target === dom.input) return;
                if (dom.suggestions.contains(e.target)) return;
                dom.suggestions.classList.add('hidden');
            }, true);

            dom.suggestions.addEventListener('click', (e) => {
                const btn = e.target?.closest?.('[data-role="suggest"]');
                const g = btn?.dataset?.global;
                if (!g) return;
                dom.suggestions.classList.add('hidden');
                setQuery(g, { run: true }).catch(() => { });
            });

            dom.searchBtn?.addEventListener('click', () => {
                dom.suggestions?.classList.add('hidden');
                runQuery({ focusIfEmpty: false }).catch(() => { });
            });

            dom.refreshBtn?.addEventListener('click', async () => {
                await ensureIndexed({ force: true });
                if (state.queryNorm) await runQuery({ focusIfEmpty: false });
            });

            dom.exportBtn?.addEventListener('click', () => exportResults());

            dom.directBtn?.addEventListener('click', () => runDirectModeInspect().catch(() => { }));

            // Filters
            root.addEventListener('click', (e) => {
                const chip = e.target?.closest?.('.ps-gi-chip');
                const access = chip?.dataset?.access;
                if (!access) return;
                toggleAccessFilter(access).catch(() => { });
            });

            // Results click delegation
            dom.results.addEventListener('click', (e) => {
                const header = e.target?.closest?.('[data-role="file-header"]');
                if (header) {
                    const fileRel = String(header?.dataset?.fileRel || '');
                    if (fileRel) {
                        if (state.collapsedFiles.has(fileRel)) state.collapsedFiles.delete(fileRel);
                        else state.collapsedFiles.add(fileRel);
                        scheduleQueryRefresh();
                    }
                    return;
                }

                const hit = e.target?.closest?.('[data-role="hit"]');
                if (!hit) return;

                const fileRel = String(hit.dataset?.fileRel || '');
                const line = Number(hit.dataset?.line || 1) || 1;
                const startColumn = Number(hit.dataset?.start || 1) || 1;
                const endColumn = Number(hit.dataset?.end || (startColumn + 1)) || (startColumn + 1);
                const access = String(hit.dataset?.access || 'UNKNOWN');
                navigateToRef({
                    fileRel,
                    line,
                    range: { startColumn, endColumn },
                    access
                }).catch(() => { });
            });

            // React to activation
            window.addEventListener('ahmadIDE:toolwindow-activated', (e) => {
                const panelId = e?.detail?.panelId;
                if (panelId !== 'globalImpactPanel') return;
                updateRuntimeUi().catch(() => { });
                // No automatic query refresh here (avoid jank); user can refresh index or re-run.
            });

            // Index service subscription (UI live updates)
            if (!unsubscribeIndex && typeof indexService.subscribe === 'function') {
                unsubscribeIndex = indexService.subscribe((evt) => {
                    const type = String(evt?.type || '');
                    if (type === 'index-start') {
                        state.indexing = true;
                        state.progress = null;
                        setMeta('Index: scanning…', state.queryNorm ? dom.metaRight?.textContent : '');
                        return;
                    }
                    if (type === 'progress') {
                        state.progress = { done: Number(evt?.done || 0) || 0, total: Number(evt?.total || 0) || 0 };
                        if (isPanelVisible()) {
                            const right = state.queryNorm ? dom.metaRight?.textContent : '';
                            setMeta('Index: scanning…', right || '');
                        }
                        return;
                    }
                    if (type === 'index-done') {
                        state.indexing = false;
                        state.lastIndexMs = Number(evt?.ms || 0) || null;
                        if (isPanelVisible()) scheduleQueryRefresh();
                        return;
                    }
                    if (type === 'file-updated' || type === 'file-removed') {
                        if (isPanelVisible()) scheduleQueryRefresh();
                    }
                });
            }
        };

        const wireGlobalImpactPanel = () => {
            const fr = window.AhmadIDEModules?.app?.featureRegistry;
            fr?.onMounted?.('globalImpactPanel', wireDom);
            if (fr?.isMounted?.('globalImpactPanel')) wireDom();
            return { openPanel, openForGlobal, openFromEditor, setQuery, getState: () => ({ ...state }) };
        };

        return { wireGlobalImpactPanel };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.globalImpact = window.AhmadIDEModules.globalImpact || {};
        window.AhmadIDEModules.globalImpact.createGlobalImpactManager = createGlobalImpactManager;
    }
})();
