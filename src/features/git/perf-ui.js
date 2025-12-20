(() => {
    function createDiffRenderer(opts = {}) {
        const leftId = opts.leftId || 'gitDiffLeft';
        const rightId = opts.rightId || 'gitDiffRight';
        const MAX_RENDER_LINES = Math.max(500, Number(opts.maxLines || 6000));

        let token = 0;

        const schedule = (cb) => {
            if (window.requestIdleCallback) return window.requestIdleCallback(cb, { timeout: 80 });
            return setTimeout(cb, 0);
        };

        const makeLine = (kind, lineno, text) => {
            const span = document.createElement('span');
            span.className = `diff-line ${kind}`;
            if (kind !== 'diff-hunk') {
                const ln = document.createElement('span');
                ln.className = 'lineno';
                ln.textContent = lineno ? String(lineno) : '';
                span.appendChild(ln);
            }
            span.appendChild(document.createTextNode(text || ''));
            return span;
        };

        const render = (diffText) => {
            const current = ++token;
            const left = document.getElementById(leftId);
            const right = document.getElementById(rightId);
            if (!left || !right) return;
            left.textContent = '';
            right.textContent = '';

            const raw = diffText || '';
            if (!raw.trim()) {
                left.textContent = 'No left changes';
                right.textContent = 'No right changes';
                return;
            }

            const lines = raw.split('\n');
            const total = lines.length;
            const clipped = total > MAX_RENDER_LINES;
            const limit = clipped ? MAX_RENDER_LINES : total;

            let idx = 0;
            let leftNo = 0;
            let rightNo = 0;

            const renderChunk = (deadline) => {
                if (current !== token) return;
                const fragL = document.createDocumentFragment();
                const fragR = document.createDocumentFragment();
                const start = performance.now();
                const budgetMs = 10;

                while (idx < limit) {
                    if (current !== token) return;
                    if (deadline && typeof deadline.timeRemaining === 'function') {
                        if (deadline.timeRemaining() < 2) break;
                    } else if ((performance.now() - start) > budgetMs) {
                        break;
                    }

                    const line = lines[idx++];
                    if (line.startsWith('+++') || line.startsWith('---')) continue;

                    if (line.startsWith('@@')) {
                        fragL.appendChild(makeLine('diff-hunk', '', line));
                        fragR.appendChild(makeLine('diff-hunk', '', line));
                        const match = line.match(/-([0-9]+)/);
                        const matchR = line.match(/\+([0-9]+)/);
                        leftNo = match ? parseInt(match[1], 10) - 1 : leftNo;
                        rightNo = matchR ? parseInt(matchR[1], 10) - 1 : rightNo;
                        continue;
                    }

                    if (line.startsWith('+')) {
                        rightNo += 1;
                        fragL.appendChild(makeLine('diff-context', '', ''));
                        fragR.appendChild(makeLine('diff-add', rightNo, line));
                        continue;
                    }

                    if (line.startsWith('-')) {
                        leftNo += 1;
                        fragL.appendChild(makeLine('diff-del', leftNo, line));
                        fragR.appendChild(makeLine('diff-context', '', ''));
                        continue;
                    }

                    leftNo += 1;
                    rightNo += 1;
                    fragL.appendChild(makeLine('diff-context', leftNo, line));
                    fragR.appendChild(makeLine('diff-context', rightNo, line));
                }

                left.appendChild(fragL);
                right.appendChild(fragR);

                if (idx < limit) {
                    schedule(renderChunk);
                } else if (clipped) {
                    if (current !== token) return;
                    const msg = `[Diff truncated: showing first ${MAX_RENDER_LINES} of ${total} lines]`;
                    left.appendChild(makeLine('diff-hunk', '', msg));
                    right.appendChild(makeLine('diff-hunk', '', msg));
                }
            };

            schedule(renderChunk);
        };

        return { render };
    }

    function createChangesRenderer(opts = {}) {
        const unstagedId = opts.unstagedId || 'gitChangesUnstaged';
        const stagedId = opts.stagedId || 'gitChangesStaged';
        const gitSelected = opts.gitSelected || { staged: new Set(), unstaged: new Set() };
        const createVirtualList = opts.createVirtualList || null;
        const onSelectionChange = typeof opts.onSelectionChange === 'function' ? opts.onSelectionChange : null;

        let unstagedVirtual = null;
        let stagedVirtual = null;

        const destroyVirtual = (which) => {
            try { which?.destroy?.(); } catch (_) { }
        };

        const makeRowFactory = (staged) => (ent) => {
            const row = document.createElement('div');
            row.className = 'git-change-row' + (staged ? ' staged' : '');
            row.dataset.path = ent.path;
            row.dataset.staged = staged ? '1' : '0';
            const checkbox = document.createElement('input');
            checkbox.type = 'checkbox';
            checkbox.checked = staged ? gitSelected.staged.has(ent.path) : gitSelected.unstaged.has(ent.path);
            row.classList.toggle('selected', checkbox.checked);
            checkbox.addEventListener('change', () => {
                const targetSet = staged ? gitSelected.staged : gitSelected.unstaged;
                checkbox.checked ? targetSet.add(ent.path) : targetSet.delete(ent.path);
                row.classList.toggle('selected', checkbox.checked);
                onSelectionChange?.();
            });
            const status = document.createElement('span');
            status.className = 'git-change-status';
            status.textContent = ent.status;
            const path = document.createElement('span');
            path.className = 'git-change-path';
            path.textContent = ent.path;
            row.appendChild(checkbox);
            row.appendChild(status);
            row.appendChild(path);

            // Click anywhere on row toggles checkbox (Storm/JetBrains rhythm)
            row.addEventListener('click', (e) => {
                if (e.target === checkbox) return;
                checkbox.checked = !checkbox.checked;
                checkbox.dispatchEvent(new Event('change', { bubbles: true }));
            });
            return row;
        };

        const renderList = (host, list, staged) => {
            if (!host) return;
            if (!list.length) {
                if (staged) {
                    destroyVirtual(stagedVirtual);
                    stagedVirtual = null;
                } else {
                    destroyVirtual(unstagedVirtual);
                    unstagedVirtual = null;
                }
                host.innerHTML = '';
                host.textContent = staged ? 'No staged files.' : 'No local changes.';
                return;
            }

            if (!createVirtualList) {
                host.innerHTML = '';
                list.slice(0, 400).forEach(ent => host.appendChild(makeRowFactory(staged)(ent)));
                if (list.length > 400) {
                    const msg = document.createElement('div');
                    msg.className = 'pane-subtitle';
                    msg.textContent = `Showing first 400 of ${list.length} files (virtual list unavailable).`;
                    host.appendChild(msg);
                }
                return;
            }

            const rowHeight = 22;
            const renderRow = makeRowFactory(staged);
            if (staged) {
                if (stagedVirtual && host.childElementCount === 0 && host.textContent.trim()) {
                    destroyVirtual(stagedVirtual);
                    stagedVirtual = null;
                    host.textContent = "";
                }
                if (!stagedVirtual) stagedVirtual = createVirtualList(host, { rowHeight, renderRow, overscan: 8 });
                stagedVirtual.setItems(list);
            } else {
                if (unstagedVirtual && host.childElementCount === 0 && host.textContent.trim()) {
                    destroyVirtual(unstagedVirtual);
                    unstagedVirtual = null;
                    host.textContent = "";
                }
                if (!unstagedVirtual) unstagedVirtual = createVirtualList(host, { rowHeight, renderRow, overscan: 8 });
                unstagedVirtual.setItems(list);
            }
        };

        const render = (entries) => {
            const stagedList = (entries || []).filter(e => e.staged);
            const unstagedList = (entries || []).filter(e => !e.staged);

            const unstagedHost = document.getElementById(unstagedId);
            const stagedHost = document.getElementById(stagedId);
            renderList(unstagedHost, unstagedList, false);
            renderList(stagedHost, stagedList, true);
        };

        return { render };
    }

    class GraphModel {
        constructor(commits, opts = {}) {
            const list = Array.isArray(commits) ? commits : [];
            this.rowHeight = Math.max(12, Number(opts.rowHeight || 22));
            this.laneGap = Math.max(8, Number(opts.laneGap || 10));
            this.laneInset = Math.max(0, Number(opts.laneInset || 4));

            const n = list.length;
            this.rowCount = n;
            this.maxLanes = 0;

            this.rowLane = new Int32Array(n);
            this.rowFlags = new Uint8Array(n); // bit0 = merge commit

            this.activeOffsets = new Int32Array(n + 1);
            this.mergeOffsets = new Int32Array(n + 1);
            this.joinOffsets = new Int32Array(n + 1);

            const activeFlat = [];
            const mergeFlat = [];
            const joinFlat = [];

            const lanes = []; // array of expected hashes (can include duplicates); null means free slot

            const findLaneSlots = (hash) => {
                const indices = [];
                for (let i = 0; i < lanes.length; i += 1) {
                    if (lanes[i] === hash) indices.push(i);
                }
                return indices;
            };

            const allocLane = (hash) => {
                let idx = lanes.indexOf(null);
                if (idx === -1) {
                    idx = lanes.length;
                    lanes.push(hash);
                    return idx;
                }
                lanes[idx] = hash;
                return idx;
            };

            const trimTrailing = () => {
                while (lanes.length && lanes[lanes.length - 1] == null) lanes.pop();
            };

            for (let row = 0; row < n; row += 1) {
                const c = list[row] || {};
                const hash = String(c.hash || '').trim();
                const parents = Array.isArray(c.parents) ? c.parents.map((p) => String(p || '').trim()).filter(Boolean) : [];

                const slots = hash ? findLaneSlots(hash) : [];
                let lane = slots.length ? slots[0] : -1;
                const joinLanes = slots.length > 1 ? slots.slice(1) : [];

                if (lane === -1) {
                    if (hash) lane = allocLane(hash);
                    else lane = allocLane(`__EMPTY__:${row}`);
                }

                this.rowLane[row] = lane;
                this.rowFlags[row] = parents.length > 1 ? 1 : 0;

                this.joinOffsets[row] = joinFlat.length;
                if (joinLanes.length) joinFlat.push(...joinLanes);
                this.joinOffsets[row + 1] = joinFlat.length;

                // Apply joins: those lanes end at this commit.
                joinLanes.forEach((idx) => { lanes[idx] = null; });

                // Update primary lane to first parent (or end).
                lanes[lane] = parents[0] || null;

                // Additional parents create/attach lanes + merge connectors.
                this.mergeOffsets[row] = mergeFlat.length;
                if (parents.length > 1) {
                    for (let pi = 1; pi < parents.length; pi += 1) {
                        const pHash = parents[pi];
                        if (!pHash) continue;
                        const existing = findLaneSlots(pHash);
                        const pLane = existing.length ? existing[0] : allocLane(pHash);
                        mergeFlat.push(pLane);
                    }
                }
                this.mergeOffsets[row + 1] = mergeFlat.length;

                trimTrailing();
                this.maxLanes = Math.max(this.maxLanes, lanes.length);

                // Active lanes (after update) draw vertical segments to next row.
                this.activeOffsets[row] = activeFlat.length;
                for (let li = 0; li < lanes.length; li += 1) {
                    if (lanes[li] != null) activeFlat.push(li);
                }
                this.activeOffsets[row + 1] = activeFlat.length;
            }

            this.activeFlat = Int32Array.from(activeFlat);
            this.mergeFlat = Int32Array.from(mergeFlat);
            this.joinFlat = Int32Array.from(joinFlat);

            this.graphWidth = (this.laneInset * 2) + (Math.max(1, this.maxLanes) * this.laneGap);
        }

        getActiveLanes(row) {
            const start = this.activeOffsets[row] || 0;
            const end = this.activeOffsets[row + 1] || start;
            return this.activeFlat.subarray(start, end);
        }

        getMergeLanes(row) {
            const start = this.mergeOffsets[row] || 0;
            const end = this.mergeOffsets[row + 1] || start;
            return this.mergeFlat.subarray(start, end);
        }

        getJoinLanes(row) {
            const start = this.joinOffsets[row] || 0;
            const end = this.joinOffsets[row + 1] || start;
            return this.joinFlat.subarray(start, end);
        }
    }

    function createGraphRenderer(opts = {}) {
        const host = opts.host;
        if (!host) throw new Error('createGraphRenderer requires host');

        const getSelectedHash = typeof opts.getSelectedHash === 'function' ? opts.getSelectedHash : (() => '');
        const getItems = typeof opts.getItems === 'function' ? opts.getItems : (() => []);

        let model = null;
        let destroyed = false;
        let raf = 0;
        let overlay = null;
        let canvas = null;
        let ctx = null;
        let resizeObs = null;

        const palette = [
            'rgba(90, 169, 255, 0.65)',
            'rgba(255, 176, 95, 0.65)',
            'rgba(170, 120, 255, 0.65)',
            'rgba(90, 210, 140, 0.65)',
            'rgba(255, 122, 214, 0.65)',
            'rgba(255, 210, 110, 0.65)',
            'rgba(90, 210, 220, 0.65)',
            'rgba(255, 120, 120, 0.65)'
        ];

        const laneColor = (laneIdx) => palette[Math.abs(Number(laneIdx) || 0) % palette.length];

        const ensureOverlay = () => {
            if (overlay && canvas && ctx) return;
            overlay = host.querySelector?.(':scope > .git-graph-overlay') || null;
            if (!overlay) {
                overlay = document.createElement('div');
                overlay.className = 'git-graph-overlay';
                host.appendChild(overlay);
            }
            canvas = overlay.querySelector('canvas');
            if (!canvas) {
                canvas = document.createElement('canvas');
                canvas.setAttribute('aria-hidden', 'true');
                overlay.appendChild(canvas);
            }
            ctx = canvas.getContext('2d');
        };

        const parsePx = (value) => {
            const n = parseFloat(String(value || '').replace('px', ''));
            return Number.isFinite(n) ? n : 0;
        };

        const measureLayout = () => {
            if (!model || !overlay || !canvas) return;
            const contentEl = host.querySelector('.virtual-list-items') || host;
            const contentStyle = getComputedStyle(contentEl);
            const padLeft = parsePx(contentStyle.paddingLeft);
            const padTop = parsePx(contentStyle.paddingTop);

            const sampleRow = host.querySelector('.git-log-row');
            const rowPadLeft = sampleRow ? parsePx(getComputedStyle(sampleRow).paddingLeft) : 6;

            // Use model width, but allow CSS override (e.g. for theme tweaks).
            const cssGraphWidth = parsePx(getComputedStyle(host).getPropertyValue('--git-graph-width'));
            const graphWidth = cssGraphWidth > 0 ? cssGraphWidth : model.graphWidth;

            overlay.style.left = '0px';
            overlay.style.top = '0px';
            overlay.style.bottom = '0px';
            overlay.style.width = `${padLeft + rowPadLeft + graphWidth}px`;

            const cssW = Math.max(1, overlay.clientWidth || 1);
            const cssH = Math.max(1, host.clientHeight || 1);
            const dpr = Math.max(1, window.devicePixelRatio || 1);
            const nextW = Math.floor(cssW * dpr);
            const nextH = Math.floor(cssH * dpr);
            if (canvas.width !== nextW || canvas.height !== nextH) {
                canvas.width = nextW;
                canvas.height = nextH;
                canvas.style.width = `${cssW}px`;
                canvas.style.height = `${cssH}px`;
                ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
            }

            return { padLeft, padTop, rowPadLeft, graphWidth, cssW, cssH };
        };

        const drawLine = (x1, y1, x2, y2) => {
            const crisp = (n) => Math.round(Number(n) || 0) + 0.5;
            ctx.beginPath();
            ctx.moveTo(crisp(x1), crisp(y1));
            ctx.lineTo(crisp(x2), crisp(y2));
            ctx.stroke();
        };

        const draw = () => {
            if (destroyed) return;
            if (!ctx || !canvas) return;
            const layout = measureLayout();
            if (!layout || !model) return;

            const { padLeft, padTop, rowPadLeft, graphWidth, cssH } = layout;
            const items = getItems() || [];
            const n = model.rowCount || items.length || 0;
            if (!n) {
                ctx.clearRect(0, 0, canvas.width, canvas.height);
                return;
            }

            const rowH = model.rowHeight;
            const laneGap = model.laneGap;
            const laneInset = model.laneInset;
            const scrollTop = host.scrollTop || 0;

            const overscan = 8;
            const clamp = (v, min, max) => Math.max(min, Math.min(max, v));
            const start = clamp(Math.floor((scrollTop - padTop) / rowH) - overscan, 0, Math.max(0, n - 1));
            const end = clamp(Math.ceil((scrollTop + cssH - padTop) / rowH) + overscan, 0, n);

            const laneCenterX = (laneIdx) => {
                const center = laneInset + (laneGap / 2) + (Number(laneIdx) * laneGap);
                return padLeft + rowPadLeft + center;
            };

            ctx.clearRect(0, 0, canvas.width, canvas.height);
            ctx.lineWidth = 1;
            ctx.lineCap = 'round';

            const selectedHash = String(getSelectedHash() || '');

            // Lines first
            for (let i = start; i < end; i += 1) {
                const lane = model.rowLane[i];
                const y = padTop + (i * rowH) - scrollTop + (rowH / 2);
                if (y < -rowH || y > cssH + rowH) continue;

                const xNode = laneCenterX(lane);

                // Joins: lanes that collapse into this commit.
                const joins = model.getJoinLanes(i);
                if (joins && joins.length) {
                    ctx.strokeStyle = laneColor(lane);
                    for (let j = 0; j < joins.length; j += 1) {
                        const xJ = laneCenterX(joins[j]);
                        drawLine(xJ, y, xNode, y);
                    }
                }

                // Merge parents: connect to other lanes at this row.
                const merges = model.getMergeLanes(i);
                if (merges && merges.length) {
                    for (let m = 0; m < merges.length; m += 1) {
                        const targetLane = merges[m];
                        ctx.strokeStyle = laneColor(targetLane);
                        drawLine(xNode, y, laneCenterX(targetLane), y);
                    }
                }

                // Active vertical segments to next row.
                if (i < n - 1) {
                    const active = model.getActiveLanes(i);
                    for (let a = 0; a < active.length; a += 1) {
                        const laneIdx = active[a];
                        ctx.strokeStyle = laneColor(laneIdx);
                        const x = laneCenterX(laneIdx);
                        drawLine(x, y, x, y + rowH);
                    }
                }
            }

            // Nodes on top
            for (let i = start; i < end; i += 1) {
                const item = items[i] || {};
                const hash = String(item.hash || '').trim();
                const lane = model.rowLane[i];
                const y = padTop + (i * rowH) - scrollTop + (rowH / 2);
                if (y < -rowH || y > cssH + rowH) continue;
                const x = laneCenterX(lane);

                const isSelected = !!(hash && selectedHash && hash === selectedHash);
                const isMerge = (model.rowFlags[i] & 1) === 1;

                const r = isMerge ? 3.6 : 3.1;
                ctx.beginPath();
                ctx.arc(x, y, r, 0, Math.PI * 2);
                ctx.fillStyle = isSelected ? 'rgba(74, 158, 255, 0.95)' : laneColor(lane);
                ctx.fill();
                ctx.lineWidth = 1;
                ctx.strokeStyle = 'rgba(20, 20, 20, 0.8)';
                ctx.stroke();
            }
        };

        const schedule = () => {
            if (destroyed) return;
            if (raf) return;
            raf = (typeof requestAnimationFrame === 'function')
                ? requestAnimationFrame(() => {
                    raf = 0;
                    draw();
                })
                : setTimeout(() => {
                    raf = 0;
                    draw();
                }, 16);
        };

        const onScroll = () => schedule();
        host.addEventListener('scroll', onScroll, { passive: true });

        if (typeof ResizeObserver !== 'undefined') {
            resizeObs = new ResizeObserver(() => schedule());
            resizeObs.observe(host);
        } else {
            window.addEventListener('resize', onScroll, { passive: true });
        }

        const setModel = (nextModel) => {
            ensureOverlay();
            model = nextModel;
            schedule();
        };

        const refresh = () => schedule();

        const destroy = () => {
            destroyed = true;
            try { host.removeEventListener('scroll', onScroll); } catch (_) { }
            try { if (resizeObs) resizeObs.disconnect(); } catch (_) { }
            try { window.removeEventListener('resize', onScroll); } catch (_) { }
            try { if (raf && typeof cancelAnimationFrame === 'function') cancelAnimationFrame(raf); } catch (_) { }
            raf = 0;
            try { overlay?.remove?.(); } catch (_) { }
            overlay = null;
            canvas = null;
            ctx = null;
        };

        return { setModel, refresh, destroy };
    }

    function createHistoryRenderer(opts = {}) {
        const hostId = opts.hostId || 'gitHistoryList';
        const createVirtualList = opts.createVirtualList || null;
        const selectedCommitRef = opts.selectedCommitRef || { hash: null };
        const onSelectCommit = typeof opts.onSelectCommit === 'function' ? opts.onSelectCommit : null;

        let historyVirtual = null;
        let graphRenderer = null;
        let graphModel = null;
        let lastItemsRef = null;
        let currentItems = [];

        const formatDate = (iso) => {
            const raw = String(iso || '').trim();
            if (!raw) return '';
            const d = new Date(raw);
            if (!Number.isFinite(d.getTime())) return raw.slice(0, 10);
            const hh = String(d.getHours()).padStart(2, '0');
            const mm = String(d.getMinutes()).padStart(2, '0');
            const yyyy = String(d.getFullYear());
            const mo = String(d.getMonth() + 1).padStart(2, '0');
            const da = String(d.getDate()).padStart(2, '0');
            return `${hh}:${mm} ${yyyy}/${mo}/${da}`;
        };

        const parseDecorations = (refs) => {
            const raw = String(refs || '').trim();
            if (!raw) return [];
            const parts = raw.split(',').map((s) => s.trim()).filter(Boolean);
            const out = [];
            for (const part of parts) {
                const arrowIdx = part.indexOf('->');
                if (arrowIdx !== -1) {
                    const right = String(part.slice(arrowIdx + 2)).trim();
                    if (right) out.push({ type: 'head', label: right });
                    continue;
                }
                const tagMatch = part.match(/^tag:\s*(.+)$/i);
                if (tagMatch) {
                    const label = String(tagMatch[1] || '').trim();
                    if (label) out.push({ type: 'tag', label });
                    continue;
                }
                const first = part.split('/')[0];
                const isRemote = !!first && ['origin', 'upstream'].includes(first);
                out.push({ type: isRemote ? 'remote' : 'branch', label: part });
            }
            // Stable de-dupe
            const seen = new Set();
            return out.filter((d) => {
                const key = `${d.type}:${d.label}`;
                if (seen.has(key)) return false;
                seen.add(key);
                return true;
            });
        };

        const destroyVirtual = () => {
            try { historyVirtual?.destroy?.(); } catch (_) { }
            historyVirtual = null;
        };

        const destroyGraph = () => {
            try { graphRenderer?.destroy?.(); } catch (_) { }
            graphRenderer = null;
            graphModel = null;
            lastItemsRef = null;
            currentItems = [];
        };

        const render = (items) => {
            const host = document.getElementById(hostId);
            if (!host) return;

            const list = Array.isArray(items) ? items : [];
            currentItems = list;
            if (!list.length) {
                destroyGraph();
                destroyVirtual();
                host.innerHTML = '';
                host.textContent = 'No history yet.';
                return;
            }

            const rowHeight = 22;

            const makeRow = (item) => {
                const isObj = item && typeof item === 'object';
                const hash = isObj ? (item.hash || '') : '';
                const shortHash = isObj ? (item.shortHash || (hash ? String(hash).slice(0, 7) : '')) : '';
                const subject = isObj ? (item.subject || '') : String(item || '');
                const author = isObj ? (item.author || '') : '';
                const dateIso = isObj ? (item.date || '') : '';
                const date = dateIso ? formatDate(dateIso) : '';
                const decorations = isObj ? parseDecorations(item.refs) : [];

                const row = document.createElement('div');
                row.className = 'git-log-row';
                if (hash) row.dataset.hash = hash;
                row.classList.toggle('selected', Boolean(hash && selectedCommitRef?.hash === hash));
                if (hash && onSelectCommit) {
                    row.addEventListener('click', () => onSelectCommit(item));
                }
                row.setAttribute('role', 'row');
                row.tabIndex = 0;
                row.addEventListener('dblclick', () => {
                    if (hash && onSelectCommit) onSelectCommit(item);
                });
                row.addEventListener('keydown', (e) => {
                    if (e.key === 'Enter' || e.key === ' ') {
                        e.preventDefault();
                        if (hash && onSelectCommit) onSelectCommit(item);
                    }
                });

                const graphCol = document.createElement('div');
                graphCol.className = 'git-graph-cell';
                graphCol.setAttribute('aria-hidden', 'true');

                const msg = document.createElement('div');
                msg.className = 'git-log-msg';
                if (shortHash) {
                    const h = document.createElement('span');
                    h.className = 'git-log-hash';
                    h.textContent = shortHash;
                    msg.appendChild(h);
                }
                const s = document.createElement('span');
                s.className = 'git-log-subject';
                s.textContent = subject;
                msg.appendChild(s);
                if (decorations.length) {
                    const wrap = document.createElement('span');
                    wrap.className = 'git-log-decorations';
                    const max = 5;
                    decorations.slice(0, max).forEach((d) => {
                        const chip = document.createElement('span');
                        chip.className = `git-ref-chip git-ref-chip--${d.type}`;
                        chip.textContent = d.label;
                        chip.title = d.label;
                        wrap.appendChild(chip);
                    });
                    if (decorations.length > max) {
                        const more = document.createElement('span');
                        more.className = 'git-ref-chip git-ref-chip--more';
                        more.textContent = `+${decorations.length - max}`;
                        more.title = `${decorations.length - max} more`;
                        wrap.appendChild(more);
                    }
                    msg.appendChild(wrap);
                }

                const a = document.createElement('span');
                a.className = 'git-log-author';
                a.textContent = author;
                const d = document.createElement('span');
                d.className = 'git-log-date';
                d.textContent = date;
                if (dateIso) d.title = String(dateIso);

                row.appendChild(graphCol);
                row.appendChild(msg);
                row.appendChild(a);
                row.appendChild(d);
                return row;
            };

            if (!createVirtualList) {
                // Non-virtual fallback: still draw graph via overlay, but render all rows.
                host.innerHTML = '';
                host.style.setProperty('--git-graph-width', '56px');
                host.innerHTML = '';
                list.slice(0, 300).forEach((item) => host.appendChild(makeRow(item)));
                if (list.length > 300) {
                    const msg = document.createElement('div');
                    msg.className = 'pane-subtitle';
                    msg.textContent = `Showing first 300 of ${list.length} commits (virtual list unavailable).`;
                    host.appendChild(msg);
                }
                if (!graphRenderer) {
                    graphRenderer = createGraphRenderer({
                        host,
                        getSelectedHash: () => selectedCommitRef?.hash || '',
                        getItems: () => currentItems
                    });
                }
                if (list !== lastItemsRef) {
                    graphModel = new GraphModel(list, { rowHeight });
                    host.style.setProperty('--git-graph-width', `${Math.ceil(graphModel.graphWidth)}px`);
                    graphRenderer.setModel(graphModel);
                    lastItemsRef = list;
                } else {
                    graphRenderer.refresh();
                }
                return;
            }

            if (historyVirtual && host.childElementCount === 0 && host.textContent.trim()) {
                destroyVirtual();
                host.textContent = "";
            }

            if (!historyVirtual) historyVirtual = createVirtualList(host, { rowHeight, renderRow: makeRow, overscan: 10 });
            historyVirtual.setItems(list);

            if (!graphRenderer) {
                graphRenderer = createGraphRenderer({
                    host,
                    getSelectedHash: () => selectedCommitRef?.hash || '',
                    getItems: () => currentItems
                });
            }

            if (list !== lastItemsRef) {
                graphModel = new GraphModel(list, { rowHeight });
                host.style.setProperty('--git-graph-width', `${Math.ceil(graphModel.graphWidth)}px`);
                graphRenderer.setModel(graphModel);
                lastItemsRef = list;
            } else {
                graphRenderer.refresh();
            }
        };

        return { render };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.features = window.AhmadIDEModules.features || {};
        window.AhmadIDEModules.features.git = window.AhmadIDEModules.features.git || {};
        window.AhmadIDEModules.features.git.perfUi = {
            createDiffRenderer,
            createChangesRenderer,
            createHistoryRenderer,
            GraphModel,
            createGraphRenderer
        };
    }
})();
