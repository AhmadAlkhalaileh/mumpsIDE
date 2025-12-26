(() => {
    const createNoopLogger = () => ({
        debug: () => { },
        info: () => { },
        warn: () => { },
        error: () => { }
    });

    const activateDebugTab = (tabId) => {
        const target = String(tabId || '').trim();
        if (!target) return;
        const tabs = Array.from(document.querySelectorAll('.debug-tab'));
        const tab = tabs.find((t) => t.getAttribute('data-tab') === target) || null;
        if (tab && typeof tab.click === 'function') {
            tab.click();
            return;
        }
        // Fallback (should be rare): toggle classes directly.
        const panes = document.querySelectorAll('.debug-tabpane');
        tabs.forEach((t) => t.classList.toggle('active', t.getAttribute('data-tab') === target));
        panes.forEach((p) => p.classList.toggle('active', p.id === target));
    };

    const formatTime = (ts) => {
        try {
            const d = new Date(Number(ts || 0));
            if (!Number.isFinite(d.getTime())) return '';
            return d.toLocaleTimeString([], { hour: '2-digit', minute: '2-digit', second: '2-digit' });
        } catch (_) {
            return '';
        }
    };

    const escapeHtml = (value) => String(value ?? '')
        .replace(/&/g, '&amp;')
        .replace(/</g, '&lt;')
        .replace(/>/g, '&gt;')
        .replace(/"/g, '&quot;')
        .replace(/'/g, '&#39;');

    const safeText = (v) => (v && typeof v === 'object' && typeof v.text === 'string') ? v.text : String(v ?? '');

    const ensureTimelineStyles = () => {
        try {
            const links = Array.from(document.querySelectorAll('link[rel="stylesheet"]'));
            const already = links.some((l) => String(l.getAttribute('href') || l.href || '').includes('debug-timeline.css'));
            if (already) return;
            const link = document.createElement('link');
            link.rel = 'stylesheet';
            link.href = './styles/debug-timeline.css';
            link.dataset.feature = 'debug-timeline';
            document.head.appendChild(link);
        } catch (_) { }
    };

    function createDebugTimelineUiManager({ deps } = {}) {
        const logger = deps?.logger || createNoopLogger();
        const timelineService = deps?.timelineService || window.AhmadIDEModules?.services?.debugTimelineService || null;
        const diffService = deps?.diffService || window.AhmadIDEModules?.services?.debugTimelineDiffService || null;

        if (!timelineService || !diffService) {
            logger.warn('DEBUG_TIMELINE_MISSING_DEPS', { timeline: !!timelineService, diff: !!diffService });
        }

        const state = {
            sessionId: null,
            snapshots: [],
            selectedSeq: null,
            compareSeq: null,
            changedOnly: true,
            search: '',
            expandedArrays: new Set(),
            lastStatus: ''
        };

        const dom = {
            root: null,
            captureBtn: null,
            exportBtn: null,
            clearBtn: null,
            list: null,
            listMeta: null,
            meta: null,
            compareSelect: null,
            locals: null,
            globals: null,
            changedOnlyChk: null,
            searchInput: null,
            status: null,
            watchInput: null,
            watchAddBtn: null,
            watchList: null,
            watchHint: null
        };

        let unsub = null;
        let _wired = false;

        const getConfig = () => {
            try { return timelineService?.getConfig?.() || null; } catch (_) { return null; }
        };

        const setStatus = (msg) => {
            state.lastStatus = String(msg || '');
            if (dom.status) dom.status.textContent = state.lastStatus;
        };

        const getSnapshots = () => {
            if (!state.sessionId) return [];
            try { return timelineService?.getSnapshots?.(state.sessionId) || []; } catch (_) { return []; }
        };

        const getWatchList = () => {
            if (!state.sessionId) return [];
            try { return timelineService?.getWatchedGlobals?.(state.sessionId) || []; } catch (_) { return []; }
        };

        const updateButtons = () => {
            const cfg = getConfig();
            const enabled = !!cfg?.enable;
            const hasSession = !!state.sessionId;
            const hasSnaps = state.snapshots.length > 0;
            if (dom.captureBtn) dom.captureBtn.disabled = !(enabled && hasSession);
            if (dom.exportBtn) dom.exportBtn.disabled = !(enabled && hasSession && hasSnaps);
            if (dom.clearBtn) dom.clearBtn.disabled = !(hasSession && hasSnaps);
        };

        const ensureSelected = () => {
            if (!state.snapshots.length) {
                state.selectedSeq = null;
                state.compareSeq = null;
                return;
            }
            const exists = state.selectedSeq != null && state.snapshots.some(s => s.seq === state.selectedSeq);
            if (!exists) state.selectedSeq = state.snapshots[state.snapshots.length - 1].seq;
            if (state.compareSeq != null && state.compareSeq !== '__none__') {
                const ok = state.snapshots.some(s => s.seq === state.compareSeq);
                if (!ok) state.compareSeq = null;
            }
        };

        const getSnapshotBySeq = (seq) => state.snapshots.find(s => s.seq === seq) || null;

        const computeDefaultCompareSeq = () => {
            if (state.selectedSeq == null) return '__none__';
            const prev = state.snapshots
                .filter(s => s.seq < state.selectedSeq)
                .sort((a, b) => b.seq - a.seq)[0];
            return prev ? prev.seq : '__none__';
        };

        const renderList = () => {
            if (!dom.list) return;
            dom.list.textContent = '';

            if (!timelineService || !diffService) {
                dom.list.innerHTML = `<div class="dbg-timeline-empty">Timeline unavailable (missing services).</div>`;
                return;
            }

            if (!state.sessionId) {
                dom.list.innerHTML = `<div class="dbg-timeline-empty">No active debug session.</div>`;
                return;
            }

            if (!state.snapshots.length) {
                dom.list.innerHTML = `<div class="dbg-timeline-empty">No snapshots yet. Step / break to capture, or use “Capture Snapshot”.</div>`;
                return;
            }

            const frag = document.createDocumentFragment();
            state.snapshots.forEach((snap) => {
                const item = document.createElement('div');
                item.className = 'dbg-timeline-item';
                item.classList.toggle('active', snap.seq === state.selectedSeq);

                const reason = String(snap.reason || '').toLowerCase();
                const dot = document.createElement('div');
                dot.className = 'dbg-timeline-item__dot';
                if (reason.includes('break')) dot.classList.add('reason-breakpoint');
                else if (reason.includes('step')) dot.classList.add('reason-step');
                else if (reason.includes('manual')) dot.classList.add('reason-manual');

                const main = document.createElement('div');
                main.className = 'dbg-timeline-item__main';
                const loc = document.createElement('div');
                loc.className = 'dbg-timeline-item__loc';
                const routine = String(snap.location?.routine || '');
                const tag = String(snap.location?.tag || '');
                const entry = tag ? `${tag}^${routine || 'Unknown'}` : (routine || 'Unknown');
                const line = snap.location?.line != null ? `:${snap.location.line}` : '';
                loc.textContent = `${entry}${line}`;

                const meta = document.createElement('div');
                meta.className = 'dbg-timeline-item__meta';
                const stackLen = Array.isArray(snap.stack) ? snap.stack.length : 0;
                meta.textContent = `#${snap.seq} · ${formatTime(snap.ts)} · ${snap.reason || 'Stop'} · stack ${stackLen}`;

                main.appendChild(loc);
                main.appendChild(meta);

                item.appendChild(dot);
                item.appendChild(main);

                item.addEventListener('click', () => {
                    state.selectedSeq = snap.seq;
                    state.compareSeq = null;
                    renderAll({ navigate: true });
                });

                frag.appendChild(item);
            });
            dom.list.appendChild(frag);
        };

        const renderMeta = () => {
            if (!dom.meta) return;
            dom.meta.textContent = '';

            const snap = state.selectedSeq != null ? getSnapshotBySeq(state.selectedSeq) : null;
            if (!snap) {
                dom.meta.innerHTML = `<div class="dbg-timeline-empty">Select a snapshot.</div>`;
                return;
            }

            const left = document.createElement('div');
            left.className = 'dbg-timeline-meta__left';
            const title = document.createElement('div');
            title.className = 'dbg-timeline-meta__title';
            const routine = String(snap.location?.routine || '');
            const tag = String(snap.location?.tag || '');
            const entry = tag ? `${tag}^${routine || 'Unknown'}` : (routine || 'Unknown');
            const line = snap.location?.line != null ? `:${snap.location.line}` : '';
            title.textContent = `${entry}${line}`;
            const sub = document.createElement('div');
            sub.className = 'dbg-timeline-meta__sub';
            sub.textContent = `${snap.reason || 'Stop'} · ${new Date(snap.ts).toLocaleString()}`;
            left.appendChild(title);
            left.appendChild(sub);

            const right = document.createElement('div');
            right.className = 'dbg-timeline-meta__right';

            const mkBadge = (text) => {
                const b = document.createElement('span');
                b.className = 'dbg-timeline-badge';
                b.textContent = String(text || '');
                return b;
            };

            right.appendChild(mkBadge(`#${snap.seq}`));
            right.appendChild(mkBadge(`${snap.localsMeta?.totalLocals ?? Object.keys(snap.locals || {}).length} locals`));
            right.appendChild(mkBadge(`${Object.keys(snap.watchedGlobals || {}).length} globals`));
            right.appendChild(mkBadge(`${Array.isArray(snap.stack) ? snap.stack.length : 0} stack`));

            dom.meta.appendChild(left);
            dom.meta.appendChild(right);
        };

        const renderCompareSelect = () => {
            if (!dom.compareSelect) return;
            dom.compareSelect.textContent = '';

            const snap = state.selectedSeq != null ? getSnapshotBySeq(state.selectedSeq) : null;
            if (!snap) {
                dom.compareSelect.disabled = true;
                return;
            }

            const opts = [];
            opts.push({ value: '__none__', label: 'None (show values)' });

            const candidates = state.snapshots
                .filter(s => s.seq < snap.seq)
                .sort((a, b) => b.seq - a.seq);
            candidates.forEach((s) => {
                const routine = String(s.location?.routine || '');
                const tag = String(s.location?.tag || '');
                const entry = tag ? `${tag}^${routine || 'Unknown'}` : (routine || 'Unknown');
                const line = s.location?.line != null ? `:${s.location.line}` : '';
                opts.push({ value: String(s.seq), label: `#${s.seq} · ${formatTime(s.ts)} · ${entry}${line}` });
            });

            const cfgDefault = state.compareSeq != null ? String(state.compareSeq) : String(computeDefaultCompareSeq());
            const selected = opts.some(o => o.value === cfgDefault) ? cfgDefault : '__none__';
            state.compareSeq = selected === '__none__' ? '__none__' : Number(selected);

            for (const o of opts) {
                const opt = document.createElement('option');
                opt.value = o.value;
                opt.textContent = o.label;
                dom.compareSelect.appendChild(opt);
            }
            dom.compareSelect.value = selected;
            dom.compareSelect.disabled = false;

            if (!dom.compareSelect.dataset.wired) {
                dom.compareSelect.dataset.wired = '1';
                dom.compareSelect.addEventListener('change', () => {
                    const v = dom.compareSelect.value;
                    state.compareSeq = v === '__none__' ? '__none__' : Number(v);
                    renderDetails();
                });
            }
        };

        const matchSearch = (hay, q) => {
            if (!q) return true;
            return String(hay || '').toLowerCase().includes(q);
        };

        const shouldShowRow = (row, q) => {
            if (!row) return false;
            const status = String(row.status || '');
            const UNCHANGED = diffService?.STATUS?.UNCHANGED || 'unchanged';
            if (state.changedOnly && status === UNCHANGED) return false;

            if (!q) return true;

            if (row.type === 'local') {
                if (matchSearch(row.name, q)) return true;
                if (matchSearch(row.beforeText, q) || matchSearch(row.afterText, q)) return true;
                    if (Array.isArray(row.children)) {
                        return row.children.some((c) => (
                            matchSearch(`${row.name}${c.name}`, q) ||
                            matchSearch(safeText(c.before), q) ||
                            matchSearch(safeText(c.after), q)
                        ));
                    }
                }

            if (row.type === 'global') {
                if (matchSearch(row.expr, q)) return true;
                if (matchSearch(safeText(row.before?.value), q)) return true;
                if (matchSearch(safeText(row.after?.value), q)) return true;
                if (matchSearch(row.before?.error, q) || matchSearch(row.after?.error, q)) return true;
            }

            return false;
        };

        const openConsoleAndRun = (command) => {
            const cmd = String(command || '').trim();
            if (!cmd) return;
            try { activateDebugTab('tab-console'); } catch (_) { }
            const input = document.getElementById('debugConsoleInput');
            const send = document.getElementById('debugConsoleSend');
            if (input) input.value = cmd;
            if (send && typeof send.click === 'function') {
                send.click();
                return;
            }
            setStatus('Debug console is not ready yet.');
        };

        const renderValueCell = (valueObj, { inspectCommand } = {}) => {
            const wrap = document.createElement('span');
            const text = safeText(valueObj);
            wrap.textContent = text;
            const truncated = !!valueObj?.truncated;
            if (!truncated || !inspectCommand) return wrap;

            const btn = document.createElement('button');
            btn.className = 'ui-btn ui-btn--sm ui-btn--ghost';
            btn.style.height = '22px';
            btn.style.padding = '0 8px';
            btn.textContent = 'Inspect';
            btn.title = 'Run inspect command in Debug Console';
            btn.addEventListener('click', (e) => {
                e.preventDefault();
                e.stopPropagation();
                openConsoleAndRun(inspectCommand);
            });

            const container = document.createElement('span');
            container.style.display = 'inline-flex';
            container.style.gap = '8px';
            container.style.alignItems = 'center';
            container.appendChild(wrap);
            container.appendChild(btn);
            return container;
        };

        const renderDetails = () => {
            renderMeta();
            renderCompareSelect();

            if (!dom.locals || !dom.globals) return;
            dom.locals.textContent = '';
            dom.globals.textContent = '';

            if (!timelineService || !diffService || typeof diffService.diffSnapshots !== 'function') {
                dom.locals.innerHTML = `<div class="dbg-timeline-empty">Timeline unavailable (missing services).</div>`;
                dom.globals.innerHTML = `<div class="dbg-timeline-empty">Timeline unavailable (missing services).</div>`;
                return;
            }

            const snap = state.selectedSeq != null ? getSnapshotBySeq(state.selectedSeq) : null;
            if (!snap) {
                dom.locals.innerHTML = `<div class="dbg-timeline-empty">No snapshot selected.</div>`;
                dom.globals.innerHTML = `<div class="dbg-timeline-empty">No snapshot selected.</div>`;
                return;
            }

            const before = (state.compareSeq && state.compareSeq !== '__none__')
                ? getSnapshotBySeq(state.compareSeq)
                : null;

            const diff = diffService.diffSnapshots(before, snap);
            const q = String(state.search || '').trim().toLowerCase();

            // Locals
            const lRows = diff.locals.rows.filter((r) => shouldShowRow(r, q));
            if (!lRows.length) {
                dom.locals.innerHTML = `<div class="dbg-timeline-empty">${state.changedOnly ? 'No changes.' : 'No locals.'}</div>`;
            } else {
                const frag = document.createDocumentFragment();
                lRows.forEach((r) => {
                    if (r.kind === 'array' && Array.isArray(r.children)) {
                        const row = document.createElement('div');
                        row.className = `dbg-timeline-row status-${r.status}`;

                        const nameCell = document.createElement('div');
                        nameCell.className = 'dbg-timeline-row__name';

                        const header = document.createElement('div');
                        header.className = 'dbg-timeline-array-header';

                        const toggle = document.createElement('button');
                        toggle.className = 'dbg-timeline-array-toggle';
                        const expanded = state.expandedArrays.has(r.name);
                        toggle.textContent = expanded ? '−' : '+';
                        toggle.title = expanded ? 'Collapse array' : 'Expand array';
                        toggle.addEventListener('click', (e) => {
                            e.preventDefault();
                            e.stopPropagation();
                            if (state.expandedArrays.has(r.name)) state.expandedArrays.delete(r.name);
                            else state.expandedArrays.add(r.name);
                            renderDetails();
                        });

                        const label = document.createElement('span');
                        label.textContent = r.name;
                        header.appendChild(toggle);
                        header.appendChild(label);
                        nameCell.appendChild(header);

                        const valueCell = document.createElement('div');
                        valueCell.className = 'dbg-timeline-row__value';
                        const beforeTxt = document.createElement('div');
                        beforeTxt.className = 'dbg-timeline-row__before';
                        beforeTxt.textContent = r.meta?.beforeTruncated ? 'Array (truncated)' : 'Array';
                        const arrow = document.createElement('div');
                        arrow.className = 'dbg-timeline-row__arrow';
                        arrow.textContent = '→';
                        const afterTxt = document.createElement('div');
                        afterTxt.className = 'dbg-timeline-row__after';
                        afterTxt.textContent = r.meta?.afterTruncated ? 'Array (truncated)' : 'Array';
                        valueCell.appendChild(beforeTxt);
                        valueCell.appendChild(arrow);
                        valueCell.appendChild(afterTxt);

                        row.appendChild(nameCell);
                        row.appendChild(valueCell);
                        frag.appendChild(row);

                        if (expanded) {
                            const childWrap = document.createElement('div');
                            childWrap.className = 'dbg-timeline-array-children';
                            const childRows = r.children
                                .filter((c) => {
                                    const UNCHANGED = diffService?.STATUS?.UNCHANGED || 'unchanged';
                                    if (state.changedOnly && c.status === UNCHANGED) return false;
                                    if (!q) return true;
                                    return matchSearch(`${r.name}${c.name}`, q) ||
                                        matchSearch(safeText(c.before), q) ||
                                        matchSearch(safeText(c.after), q);
                                });

                            if (!childRows.length) {
                                const empty = document.createElement('div');
                                empty.className = 'dbg-timeline-empty';
                                empty.textContent = state.changedOnly ? 'No changed elements.' : 'No elements.';
                                childWrap.appendChild(empty);
                            } else {
                                childRows.forEach((c) => {
                                    const childRow = document.createElement('div');
                                    childRow.className = `dbg-timeline-row status-${c.status}`;
                                    childRow.style.gridTemplateColumns = '220px 1fr';

                                    const cn = document.createElement('div');
                                    cn.className = 'dbg-timeline-row__name';
                                    cn.textContent = `${r.name}${c.name}`;

                                    const cv = document.createElement('div');
                                    cv.className = 'dbg-timeline-row__value';

                                    const cb = document.createElement('div');
                                    cb.className = 'dbg-timeline-row__before';
                                    const ca = document.createElement('div');
                                    ca.className = 'dbg-timeline-row__after';
                                    const arrow2 = document.createElement('div');
                                    arrow2.className = 'dbg-timeline-row__arrow';
                                    arrow2.textContent = '→';

                                    const inspectCmd = `${r.name}${c.name}`.includes('(')
                                        ? `WRITE $GET(${r.name}${c.name}),!`
                                        : `WRITE ${r.name},!`;

                                    cb.appendChild(renderValueCell(c.before, { inspectCommand: inspectCmd }));
                                    ca.appendChild(renderValueCell(c.after, { inspectCommand: inspectCmd }));

                                    cv.appendChild(cb);
                                    cv.appendChild(arrow2);
                                    cv.appendChild(ca);
                                    childRow.appendChild(cn);
                                    childRow.appendChild(cv);
                                    childWrap.appendChild(childRow);
                                });
                            }
                            frag.appendChild(childWrap);
                        }
                        return;
                    }

                    const row = document.createElement('div');
                    row.className = `dbg-timeline-row status-${r.status}`;

                    const name = document.createElement('div');
                    name.className = 'dbg-timeline-row__name';
                    name.textContent = r.name;

                    const val = document.createElement('div');
                    val.className = 'dbg-timeline-row__value';

                    const beforeCell = document.createElement('div');
                    beforeCell.className = 'dbg-timeline-row__before';
                    const afterCell = document.createElement('div');
                    afterCell.className = 'dbg-timeline-row__after';
                    const arrow = document.createElement('div');
                    arrow.className = 'dbg-timeline-row__arrow';
                    arrow.textContent = '→';

                    const inspectCmd = `WRITE ${r.name},!`;
                    beforeCell.appendChild(renderValueCell(r.before?.value, { inspectCommand: inspectCmd }));
                    afterCell.appendChild(renderValueCell(r.after?.value, { inspectCommand: inspectCmd }));

                    val.appendChild(beforeCell);
                    val.appendChild(arrow);
                    val.appendChild(afterCell);

                    row.appendChild(name);
                    row.appendChild(val);
                    frag.appendChild(row);
                });
                dom.locals.appendChild(frag);
            }

            // Globals
            const gRows = diff.globals.rows.filter((r) => shouldShowRow(r, q));
            if (!gRows.length) {
                dom.globals.innerHTML = `<div class="dbg-timeline-empty">${state.changedOnly ? 'No changes.' : 'No globals.'}</div>`;
            } else {
                const frag = document.createDocumentFragment();
                gRows.forEach((r) => {
                    const row = document.createElement('div');
                    row.className = `dbg-timeline-row status-${r.status}`;
                    row.style.gridTemplateColumns = '180px 1fr';

                    const name = document.createElement('div');
                    name.className = 'dbg-timeline-row__name';
                    name.textContent = r.expr;

                    const val = document.createElement('div');
                    val.className = 'dbg-timeline-row__value';

                    const beforeCell = document.createElement('div');
                    beforeCell.className = 'dbg-timeline-row__before';
                    const afterCell = document.createElement('div');
                    afterCell.className = 'dbg-timeline-row__after';
                    const arrow = document.createElement('div');
                    arrow.className = 'dbg-timeline-row__arrow';
                    arrow.textContent = '→';

                    const inspectCmd = `WRITE $GET(${r.expr}),!`;
                    // Handle both watched globals (with error field) and captured globals (with value directly)
                    if (r.before?.error) {
                        beforeCell.textContent = `! ${r.before.error}`;
                    } else if (r.before) {
                        // For captured globals, before/after are entries with kind/value
                        const beforeVal = r.before.kind === 'scalar' ? r.before.value : r.before.value;
                        beforeCell.appendChild(renderValueCell(beforeVal, { inspectCommand: inspectCmd }));
                    } else {
                        beforeCell.textContent = '—';
                    }
                    if (r.after?.error) {
                        afterCell.textContent = `! ${r.after.error}`;
                    } else if (r.after) {
                        const afterVal = r.after.kind === 'scalar' ? r.after.value : r.after.value;
                        afterCell.appendChild(renderValueCell(afterVal, { inspectCommand: inspectCmd }));
                    } else {
                        afterCell.textContent = '—';
                    }

                    val.appendChild(beforeCell);
                    val.appendChild(arrow);
                    val.appendChild(afterCell);

                    row.appendChild(name);
                    row.appendChild(val);
                    frag.appendChild(row);
                });
                dom.globals.appendChild(frag);
            }
        };

        const renderWatches = () => {
            if (!dom.watchList || !dom.watchHint) return;
            dom.watchList.textContent = '';

            const cfg = getConfig();
            const enabled = !!cfg?.enable;

            if (!enabled) {
                dom.watchHint.textContent = 'Timeline is disabled. Enable it in Settings → Advanced.';
                return;
            }

            if (!state.sessionId) {
                dom.watchHint.textContent = 'Start a debug session to watch globals.';
                return;
            }

            if (!cfg?.captureGlobals) {
                dom.watchHint.textContent = 'Enable “Capture watched globals” in Settings to include watched nodes in snapshots.';
            } else {
                dom.watchHint.textContent = 'Watched nodes are read using $GET() at each stop (best-effort).';
            }

            const watches = getWatchList();
            if (!watches.length) return;

            const frag = document.createDocumentFragment();
            watches.forEach((expr) => {
                const chip = document.createElement('span');
                chip.className = 'dbg-timeline-watch-chip';
                chip.textContent = expr;

                const rm = document.createElement('button');
                rm.className = 'dbg-timeline-watch-remove';
                rm.type = 'button';
                rm.textContent = '×';
                rm.title = 'Remove watch';
                rm.addEventListener('click', (e) => {
                    e.preventDefault();
                    e.stopPropagation();
                    const next = getWatchList().filter(x => x !== expr);
                    timelineService?.setWatchedGlobals?.(state.sessionId, next);
                });

                chip.appendChild(rm);
                frag.appendChild(chip);
            });
            dom.watchList.appendChild(frag);
        };

        const renderDetailsAndStatus = () => {
            const cfg = getConfig();
            const enabled = !!cfg?.enable;
            if (!enabled) {
                setStatus('Timeline is disabled. Enable it in Settings → Advanced → Debugger Timeline.');
            } else if (!state.sessionId) {
                setStatus('No active debug session.');
            } else if (!state.snapshots.length) {
                setStatus('Waiting for a stop event…');
            } else {
                setStatus(state.lastStatus || '');
            }

            if (dom.listMeta) {
                dom.listMeta.textContent = state.snapshots.length ? `${state.snapshots.length} snapshots` : '';
            }
            updateButtons();
            renderWatches();
            renderDetails();
        };

        const renderAll = ({ navigate = false } = {}) => {
            state.snapshots = getSnapshots();
            ensureSelected();
            renderList();
            renderDetailsAndStatus();

            if (navigate && state.selectedSeq != null) {
                const snap = getSnapshotBySeq(state.selectedSeq);
                const routine = String(snap?.location?.routine || '').trim();
                const line = Number(snap?.location?.line || 0) || null;
                if (routine && line) {
                    try {
                        window.dispatchEvent(new CustomEvent('ahmadIDE:debugTimelineNavigate', { detail: { routine, line } }));
                    } catch (_) { }
                }
            }
        };

        const wireDom = () => {
            if (_wired) return;
            dom.root = document.getElementById('dbgTimelineRoot');
            if (!dom.root) return;
            ensureTimelineStyles();

            _wired = true;
            dom.captureBtn = document.getElementById('dbgTimelineCaptureBtn');
            dom.exportBtn = document.getElementById('dbgTimelineExportBtn');
            dom.clearBtn = document.getElementById('dbgTimelineClearBtn');
            dom.list = document.getElementById('dbgTimelineList');
            dom.listMeta = document.getElementById('dbgTimelineListMeta');
            dom.meta = document.getElementById('dbgTimelineMeta');
            dom.compareSelect = document.getElementById('dbgTimelineCompareSelect');
            dom.locals = document.getElementById('dbgTimelineLocals');
            dom.globals = document.getElementById('dbgTimelineGlobals');
            dom.changedOnlyChk = document.getElementById('dbgTimelineChangedOnlyChk');
            dom.searchInput = document.getElementById('dbgTimelineSearchInput');
            dom.status = document.getElementById('dbgTimelineStatus');
            dom.watchInput = document.getElementById('dbgTimelineWatchInput');
            dom.watchAddBtn = document.getElementById('dbgTimelineWatchAddBtn');
            dom.watchList = document.getElementById('dbgTimelineWatchList');
            dom.watchHint = document.getElementById('dbgTimelineWatchHint');

            if (dom.changedOnlyChk && !dom.changedOnlyChk.dataset.wired) {
                dom.changedOnlyChk.dataset.wired = '1';
                dom.changedOnlyChk.addEventListener('change', () => {
                    state.changedOnly = !!dom.changedOnlyChk.checked;
                    renderDetails();
                });
            }

            if (dom.searchInput && !dom.searchInput.dataset.wired) {
                dom.searchInput.dataset.wired = '1';
                dom.searchInput.addEventListener('input', () => {
                    state.search = dom.searchInput.value || '';
                    renderDetails();
                });
            }

            if (dom.captureBtn) {
                dom.captureBtn.addEventListener('click', () => {
                    if (!state.sessionId) return;
                    try {
                        window.dispatchEvent(new CustomEvent('ahmadIDE:debugTimelineCapture', { detail: { sessionId: state.sessionId } }));
                    } catch (_) { }
                });
            }

            if (dom.clearBtn) {
                dom.clearBtn.addEventListener('click', () => {
                    if (!state.sessionId) return;
                    timelineService?.clearSession?.(state.sessionId);
                });
            }

            if (dom.exportBtn) {
                dom.exportBtn.addEventListener('click', () => {
                    if (!state.sessionId) return;
                    const res = timelineService?.exportSession?.(state.sessionId);
                    if (!res?.ok || !res?.payload) {
                        setStatus(String(res?.error || 'Export failed'));
                        return;
                    }
                    const blob = new Blob([JSON.stringify(res.payload, null, 2)], { type: 'application/json;charset=utf-8' });
                    const stamp = new Date().toISOString().replace(/[:.]/g, '-');
                    const filename = `debug-timeline_${stamp}.json`;
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
                });
            }

            const addWatch = () => {
                if (!state.sessionId || !dom.watchInput) return;
                const raw = String(dom.watchInput.value || '').trim();
                if (!raw) return;
                const next = getWatchList().concat([raw]);
                const res = timelineService?.setWatchedGlobals?.(state.sessionId, next);
                if (!res?.ok) {
                    setStatus(String(res?.error || 'Failed to add watch'));
                    return;
                }
                if (Array.isArray(res?.invalid) && res.invalid.length) {
                    setStatus(String(res.invalid[0]?.error || 'Invalid global reference'));
                    return;
                }
                dom.watchInput.value = '';
                renderWatches();
            };

            if (dom.watchAddBtn) dom.watchAddBtn.addEventListener('click', addWatch);
            if (dom.watchInput) {
                dom.watchInput.addEventListener('keydown', (e) => {
                    if (e.key === 'Enter') {
                        e.preventDefault();
                        addWatch();
                    }
                });
            }

            if (!unsub && timelineService?.subscribe) {
                unsub = timelineService.subscribe((evt) => {
                    const type = String(evt?.type || '');
                    if (type === 'config') {
                        renderDetailsAndStatus();
                        return;
                    }
                    if (type === 'session-start') {
                        state.sessionId = String(evt.sessionId || '').trim() || state.sessionId;
                        renderAll({ navigate: false });
                        return;
                    }
                    if (type === 'session-end') {
                        renderDetailsAndStatus();
                        return;
                    }
                    if (type === 'session-cleared') {
                        renderAll({ navigate: false });
                        return;
                    }
                    if (type === 'snapshot-added') {
                        if (evt.sessionId && String(evt.sessionId) === state.sessionId) {
                            state.lastStatus = '';
                            renderAll({ navigate: false });
                            return;
                        }
                    }
                    if (type === 'snapshot-error') {
                        setStatus(String(evt.error || 'Snapshot failed'));
                    }
                    if (type === 'watched-globals-changed') {
                        if (evt.sessionId && String(evt.sessionId) === state.sessionId) renderWatches();
                    }
                });
            }

            // If the UI mounted after a session started, pick up the current active session.
            try {
                const active = timelineService?.getActiveSessionId?.();
                if (active && !state.sessionId) state.sessionId = String(active);
            } catch (_) { }

            renderAll({ navigate: false });
        };

        const wire = () => {
            const fr = window.AhmadIDEModules?.app?.featureRegistry;
            fr?.onMounted?.('debugPanel', wireDom);
            if (fr?.isMounted?.('debugPanel')) wireDom();
        };

        return { wire };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.debugTimeline = window.AhmadIDEModules.debugTimeline || {};
        window.AhmadIDEModules.debugTimeline.createDebugTimelineUiManager = createDebugTimelineUiManager;
    }
})();
