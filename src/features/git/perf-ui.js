(() => {
    /**
     * Git Tool - Enhanced UI Components
     * Build #PS-253.28294.345, December 8, 2025
     */

    // ============================================
    // Diff Renderer
    // ============================================
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
                left.textContent = 'No changes';
                right.textContent = 'No changes';
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
                const budgetMs = 12;

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

    // ============================================
    // Changes Renderer
    // ============================================
    function createChangesRenderer(opts = {}) {
        const unstagedId = opts.unstagedId || 'gitChangesUnstaged';
        const stagedId = opts.stagedId || 'gitChangesStaged';
        const untrackedId = opts.untrackedId || 'gitChangesUntracked';
        const gitSelected = opts.gitSelected || { staged: new Set(), unstaged: new Set(), untracked: new Set() };
        const createVirtualList = opts.createVirtualList || null;
        const onSelectionChange = typeof opts.onSelectionChange === 'function' ? opts.onSelectionChange : null;

        let unstagedVirtual = null;
        let stagedVirtual = null;
        let untrackedVirtual = null;

        const destroyVirtual = (which) => {
            try { which?.destroy?.(); } catch (_) { }
        };

        const getStatusIcon = (status) => {
            const icons = {
                'M': 'M',
                'A': 'A',
                'D': 'D',
                'R': 'R',
                'C': 'C',
                '?': '?',
                'U': 'U'
            };
            return icons[status] || status;
        };

        const getStatusClass = (status) => {
            const classes = {
                'M': 'modified',
                'A': 'added',
                'D': 'deleted',
                'R': 'renamed',
                'C': 'copied',
                '?': 'untracked',
                'U': 'unmerged'
            };
            return classes[status] || '';
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
            status.className = `git-change-status ${getStatusClass(ent.status)}`;
            status.textContent = getStatusIcon(ent.status);
            status.dataset.status = ent.status || "M";

            const path = document.createElement('span');
            path.className = 'git-change-path';
            path.textContent = ent.path;

            row.appendChild(checkbox);
            row.appendChild(status);
            row.appendChild(path);

            row.addEventListener('click', (e) => {
                if (e.target === checkbox) return;
                checkbox.checked = !checkbox.checked;
                checkbox.dispatchEvent(new Event('change', { bubbles: true }));
            });

            return row;
        };

        const renderList = (host, list, staged, emptyMessage) => {
            if (!host) return;
            if (!list.length) {
                destroyVirtual(staged ? stagedVirtual : unstagedVirtual);
                if (staged) stagedVirtual = null;
                else unstagedVirtual = null;
                host.innerHTML = '';
                host.textContent = emptyMessage || (staged ? 'No staged files' : 'No changes');
                return;
            }

            if (!createVirtualList) {
                host.innerHTML = '';
                list.slice(0, 400).forEach(ent => host.appendChild(makeRowFactory(staged)(ent)));
                if (list.length > 400) {
                    const msg = document.createElement('div');
                    msg.className = 'pane-subtitle';
                    msg.textContent = `Showing first 400 of ${list.length} files`;
                    host.appendChild(msg);
                }
                return;
            }

            const rowHeight = 28;
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
            const stagedList = (entries || []).filter(e => e.staged && e.status !== '?');
            const unstagedList = (entries || []).filter(e => !e.staged && e.status !== '?');
            const untrackedList = (entries || []).filter(e => e.status === '?');

            const stagedHost = document.getElementById(stagedId);
            const unstagedHost = document.getElementById(unstagedId);
            const untrackedHost = document.getElementById(untrackedId);

            renderList(stagedHost, stagedList, true, 'No staged files');
            renderList(unstagedHost, unstagedList, false, 'No local changes');
            renderList(untrackedHost, untrackedList, false, 'No untracked files');

            // Update counts
            const stagedCount = document.getElementById('gitStagedCount');
            const unstagedCount = document.getElementById('gitUnstagedCount');
            const untrackedCount = document.getElementById('gitUntrackedCount');

            if (stagedCount) stagedCount.textContent = stagedList.length;
            if (unstagedCount) unstagedCount.textContent = unstagedList.length;
            if (untrackedCount) untrackedCount.textContent = untrackedList.length;
        };

        return { render };
    }

    // ============================================
    // Graph Model
    // ============================================
    class GraphModel {
        constructor(commits, opts = {}) {
            const list = Array.isArray(commits) ? commits : [];
            this.rowHeight = Math.max(20, Number(opts.rowHeight || 24));
            this.laneGap = Math.max(10, Number(opts.laneGap || 12));
            this.laneInset = Math.max(0, Number(opts.laneInset || 8));

            const n = list.length;
            this.rowCount = n;
            this.maxLanes = 0;

            this.rowLane = new Int32Array(n);
            this.rowFlags = new Uint8Array(n);

            this.activeOffsets = new Int32Array(n + 1);
            this.mergeOffsets = new Int32Array(n + 1);
            this.joinOffsets = new Int32Array(n + 1);

            const activeFlat = [];
            const mergeFlat = [];
            const joinFlat = [];

            const lanes = [];

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

                joinLanes.forEach((idx) => { lanes[idx] = null; });
                lanes[lane] = parents[0] || null;

                this.mergeOffsets[row] = mergeFlat.length;
                if (parents.length > 1) {
                    for (let pi = 1; pi < parents.length; pi += 1) {
                        const pHash = parents[pi];
                        if (!pHash) continue;
                        const pLane = allocLane(pHash);
                        mergeFlat.push(pLane);
                    }
                }
                this.mergeOffsets[row + 1] = mergeFlat.length;

                trimTrailing();
                this.maxLanes = Math.max(this.maxLanes, lanes.length);

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

    // ============================================
    // Enhanced Graph Renderer
    // ============================================
    function createGraphRenderer(opts = {}) {
        const host = opts.host;

        const getSelectedHash = typeof opts.getSelectedHash === 'function' ? opts.getSelectedHash : (() => '');
        const getItems = typeof opts.getItems === 'function' ? opts.getItems : (() => []);

        let model = null;
        let destroyed = false;
        let raf = 0;
        let overlay = null;
        let canvas = null;
        let ctx = null;
        let resizeObs = null;

        // Enhanced color palette for Git graph (IntelliJ-style colors)
        const palette = [
            '#4584b6', // Blue
            '#d19a66', // Orange  
            '#6a9955', // Green
            '#c678dd', // Purple
            '#56b6c2', // Cyan
            '#e06c75', // Red
            '#abb2bf', // Gray
            '#98c379', // Light Green
            '#dcdfe4'  // Light Gray
        ];

        const laneColor = (laneIdx) => palette[Math.abs(Number(laneIdx) || 0) % palette.length];

        // Animation state with smooth transitions
        let animationState = {
            startTime: 0,
            duration: 350,
            active: false,
            progress: 0,
            prevLanes: null,
            currentLanes: null
        };

        let hoverState = {
            row: -1,
            lane: -1
        };

        // Tooltip for commit info
        let tooltip = null;
        let tooltipTimeout = null;

        const ensureOverlay = () => {
            if (overlay && canvas && ctx) {
                console.log('[GraphRenderer] Overlay already exists');
                return;
            }

            console.log('[GraphRenderer] Creating overlay, host:', host?.id || 'unknown', 'host size:', host?.clientWidth, 'x', host?.clientHeight);

            // Check if overlay already exists
            const existingOverlay = host.querySelector ? host.querySelector('.git-graph-overlay') : null;
            if (existingOverlay) {
                console.log('[GraphRenderer] Found existing overlay');
                overlay = existingOverlay;
            } else {
                overlay = document.createElement('div');
                overlay.className = 'git-graph-overlay';
                overlay.style.position = 'absolute';
                overlay.style.left = '0';
                overlay.style.top = '0';
                overlay.style.width = '150px'; // Initial width, will be updated by measureLayout
                overlay.style.height = '100%';
                // Keep the overlay non-interactive so it doesn't block row clicks; we listen on `host` for hover.
                overlay.style.zIndex = '5';
                overlay.style.pointerEvents = 'none';
                console.log('[GraphRenderer] Created new overlay element');
                host.appendChild(overlay);
                console.log('[GraphRenderer] Overlay appended to host, child count:', host.children.length);
            }

            canvas = overlay.querySelector ? overlay.querySelector('canvas') : null;
            if (!canvas) {
                canvas = document.createElement('canvas');
                canvas.setAttribute('aria-hidden', 'true');
                // Set initial size to prevent 0x0
                canvas.width = 300;
                canvas.height = 600;
                canvas.style.width = '150px';
                canvas.style.height = '300px';
                overlay.appendChild(canvas);
                console.log('[GraphRenderer] Canvas created with initial size 300x600');
            }
            ctx = canvas.getContext('2d', { alpha: true });
            console.log('[GraphRenderer] Context obtained, canvas size:', canvas.width, 'x', canvas.height);

            // Force initial measurement if model exists
            if (model) {
                console.log('[GraphRenderer] Model exists, forcing initial measureLayout');
                requestAnimationFrame(() => {
                    measureLayout();
                    schedule();
                });
            }
        };

        const parsePx = (value) => {
            const n = parseFloat(String(value || '').replace('px', ''));
            return Number.isFinite(n) ? n : 0;
        };

        const measureLayout = () => {
            if (!model || !overlay || !canvas) {
                console.log('[GraphRenderer] measureLayout: missing requirements', { hasModel: !!model, hasOverlay: !!overlay, hasCanvas: !!canvas });
                return null;
            }

            // NOTE: `host` is a scroll container. An absolutely-positioned child would normally scroll
            // with the content. We keep the overlay pinned to the visible viewport by offsetting it
            // by the current scrollTop.
            const hostScrollTop = host.scrollTop || 0;

            // Get the items container (either from virtual list or host)
            const contentEl = host.querySelector ? host.querySelector('.virtual-list-items') : host;
            const contentStyle = contentEl ? getComputedStyle(contentEl) : {};
            const padLeft = contentEl ? parsePx(contentStyle.paddingLeft) : 0;
            const padTop = contentEl ? parsePx(contentStyle.paddingTop) : 0;

            const rowPadLeft = 8;

            const cssGraphWidth = parsePx(getComputedStyle(host).getPropertyValue('--git-graph-width'));
            const graphWidth = cssGraphWidth > 0 ? cssGraphWidth : model.graphWidth;

            // Set overlay position to cover the graph area
            overlay.style.position = 'absolute';
            overlay.style.left = '0px';
            overlay.style.top = `${hostScrollTop}px`;
            overlay.style.width = `${padLeft + rowPadLeft + graphWidth}px`;
            const overlayCssH = Math.max(300, host.clientHeight || 300);
            overlay.style.height = `${overlayCssH}px`;
            overlay.style.zIndex = '5';

            const cssW = Math.max(150, overlay.clientWidth || 150); // Minimum 150px
            const cssH = overlayCssH; // Keep canvas height consistent with overlay viewport height
            const dpr = Math.max(1, window.devicePixelRatio || 1);
            const nextW = Math.floor(cssW * dpr);
            const nextH = Math.floor(cssH * dpr);

            if (canvas.width !== nextW || canvas.height !== nextH) {
                console.log('[GraphRenderer] Resizing canvas from', canvas.width, 'x', canvas.height, 'to', nextW, 'x', nextH);
                canvas.width = nextW;
                canvas.height = nextH;
                canvas.style.width = `${cssW}px`;
                canvas.style.height = `${cssH}px`;
                ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
            }

            return { padLeft, padTop, rowPadLeft, graphWidth, cssW, cssH };
        };

        const getInterpolatedLaneX = (row, laneIdx, progress, layout) => {
            const { padLeft, rowPadLeft } = layout;
            const laneGap = model.laneGap;
            const laneInset = model.laneInset;

            let targetX = laneInset + (laneGap / 2) + (laneIdx * laneGap);

            if (animationState.active && animationState.prevLanes && progress < 1) {
                const items = getItems();
                const item = items[row];
                if (item && item.hash) {
                    const prevIdx = animationState.prevLanes.get(item.hash);
                    if (prevIdx !== undefined && prevIdx !== laneIdx) {
                        const prevX = laneInset + (laneGap / 2) + (prevIdx * laneGap);
                        // Smooth easing interpolation
                        const eased = easeInOutCubic(progress);
                        targetX = prevX + (targetX - prevX) * eased;
                    }
                }
            }

            return padLeft + rowPadLeft + targetX;
        };

        // Easing function for smooth animations
        const easeInOutCubic = (t) => {
            return t < 0.5 ? 4 * t * t * t : 1 - Math.pow(-2 * t + 2, 3) / 2;
        };

        const draw = () => {
            console.log('[GraphRenderer] draw() called');
            if (destroyed) return;
            if (!ctx || !canvas) {
                console.warn('[GraphRenderer] draw called but ctx or canvas missing');
                return;
            }

            const layout = measureLayout();
            if (!layout || !model) {
                // Don't log here as it's normal during initialization
                return;
            }

            console.log('[GraphRenderer] Drawing with layout:', layout, 'model rows:', model.rowCount);

            const { padLeft, padTop, rowPadLeft, cssH } = layout;
            const items = getItems() || [];
            const n = model.rowCount || items.length || 0;
            if (!n) {
                ctx.clearRect(0, 0, canvas.width, canvas.height);
                return;
            }

            let progress = 1;
            if (animationState.active) {
                const now = performance.now();
                progress = Math.min(1, (now - animationState.startTime) / animationState.duration);
                progress = easeInOutCubic(progress);
                if (progress >= 1) {
                    animationState.active = false;
                    progress = 1;
                }
            }

            const rowH = model.rowHeight;
            const scrollTop = host.scrollTop || 0;

            const overscan = 10;
            const clamp = (v, min, max) => Math.max(min, Math.min(v, max));
            const start = clamp(Math.floor((scrollTop - padTop) / rowH) - overscan, 0, Math.max(0, n - 1));
            const end = clamp(Math.ceil((scrollTop + cssH - padTop) / rowH) + overscan, 0, n);

            ctx.clearRect(0, 0, canvas.width, canvas.height);
            ctx.lineWidth = 2;
            ctx.lineCap = 'round';
            ctx.lineJoin = 'round';

            const selectedHash = String(getSelectedHash() || '');

            // Draw vertical lines first (behind nodes)
            for (let i = start; i < end - 1; i += 1) {
                const y = padTop + (i * rowH) - scrollTop + (rowH / 2);
                if (y < -rowH * 2 || y > cssH + rowH * 2) continue;

                const active = model.getActiveLanes(i);
                for (let a = 0; a < active.length; a += 1) {
                    const laneIdx = active[a];
                    ctx.strokeStyle = laneColor(laneIdx);
                    ctx.globalAlpha = (hoverState.lane === laneIdx) ? 1.0 : 0.55;

                    const xTop = getInterpolatedLaneX(i, laneIdx, progress, layout);
                    const xBottom = getInterpolatedLaneX(i + 1, laneIdx, progress, layout);

                    ctx.beginPath();
                    ctx.moveTo(xTop, y);
                    ctx.lineTo(xBottom, y + rowH);
                    ctx.stroke();
                }
            }

            // Draw connections (joins and merges)
            for (let i = start; i < end; i += 1) {
                const y = padTop + (i * rowH) - scrollTop + (rowH / 2);
                if (y < -rowH * 2 || y > cssH + rowH * 2) continue;

                const lane = model.rowLane[i];
                const xNode = getInterpolatedLaneX(i, lane, progress, layout);

                // Joins (children merging into current)
                const joins = model.getJoinLanes(i);
                if (joins && joins.length) {
                    for (let j = 0; j < joins.length; j += 1) {
                        const sourceLane = joins[j];
                        ctx.strokeStyle = laneColor(sourceLane);
                        ctx.globalAlpha = (hoverState.lane === sourceLane) ? 1.0 : 0.6;

                        const xSource = getInterpolatedLaneX(i - 1, sourceLane, progress, layout);
                        const cp1x = xSource;
                        const cp1y = y - rowH * 0.35;
                        const cp2x = xNode;
                        const cp2y = y - rowH * 0.08;

                        ctx.beginPath();
                        ctx.moveTo(xSource, y - rowH);
                        ctx.bezierCurveTo(cp1x, cp1y, cp2x, cp2y, xNode, y);
                        ctx.stroke();
                    }
                }

                // Merges (current merging into parents)
                const merges = model.getMergeLanes(i);
                if (merges && merges.length) {
                    for (let m = 0; m < merges.length; m += 1) {
                        const targetLane = merges[m];
                        ctx.strokeStyle = laneColor(targetLane);
                        ctx.globalAlpha = (hoverState.lane === targetLane) ? 1.0 : 0.6;

                        const xTarget = getInterpolatedLaneX(i + 1, targetLane, progress, layout);
                        const cp1x = xNode;
                        const cp1y = y + rowH * 0.08;
                        const cp2x = xTarget;
                        const cp2y = y + rowH * 0.35;

                        ctx.beginPath();
                        ctx.moveTo(xNode, y);
                        ctx.bezierCurveTo(cp1x, cp1y, cp2x, cp2y, xTarget, y + rowH);
                        ctx.stroke();
                    }
                }
            }

            // Draw nodes
            ctx.globalAlpha = 1.0;
            for (let i = start; i < end; i += 1) {
                const item = items[i] || {};
                const hash = String(item.hash || '').trim();
                const lane = model.rowLane[i];
                const y = padTop + (i * rowH) - scrollTop + (rowH / 2);
                if (y < -rowH || y > cssH + rowH) continue;
                const x = getInterpolatedLaneX(i, lane, progress, layout);

                const isSelected = !!(hash && selectedHash && hash === selectedHash);
                const isHovered = i === hoverState.row && lane === hoverState.lane;

                // Node glow effect for selected/hovered
                const r = (isSelected || isHovered) ? 6 : 4;
                const glowRadius = r + 4;

                if (isSelected || isHovered) {
                    ctx.shadowColor = laneColor(lane);
                    ctx.shadowBlur = 8;
                }

                ctx.beginPath();
                ctx.arc(x, y, r, 0, Math.PI * 2);
                ctx.fillStyle = laneColor(lane);
                ctx.fill();

                // Node border
                ctx.shadowBlur = 0;
                if (isSelected || isHovered) {
                    ctx.strokeStyle = '#ffffff';
                    ctx.lineWidth = 2;
                    ctx.stroke();
                } else {
                    ctx.strokeStyle = '#2b2d33';
                    ctx.lineWidth = 1.5;
                    ctx.stroke();
                }
            }

            if (animationState.active) schedule();
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

        const onMouseMove = (e) => {
            const layout = measureLayout();
            if (!layout || !model) return;
            const { padTop, cssW, cssH } = layout;
            const rect = host.getBoundingClientRect();
            const mouseX = e.clientX - rect.left;
            const mouseY = e.clientY - rect.top;

            if (mouseX < 0 || mouseX > cssW || mouseY < 0 || mouseY > cssH) {
                if (hoverState.row !== -1) {
                    hoverState.row = -1;
                    hoverState.lane = -1;
                    hideTooltip();
                    schedule();
                }
                return;
            }

            const rowH = model.rowHeight;
            const scrollTop = host.scrollTop || 0;
            const row = Math.floor((mouseY + scrollTop - padTop) / rowH);

            const lane = model.rowLane[row];
            if (lane !== undefined) {
                const x = getInterpolatedLaneX(row, lane, 1.0, layout);
                const y = padTop + (row * rowH) - scrollTop + (rowH / 2);
                const distNode = Math.hypot(mouseX - x, mouseY - y);

                if (distNode < 14) {
                    if (hoverState.row !== row || hoverState.lane !== lane) {
                        hoverState.row = row;
                        hoverState.lane = lane;
                        showTooltip(e, row);
                        schedule();
                    }
                    return;
                }
            }

            // Check lanes for hover
            const active = model.getActiveLanes(row);
            let closestLane = -1;
            let minDist = 12;
            for (let a = 0; a < active.length; a += 1) {
                const lIdx = active[a];
                const lx = getInterpolatedLaneX(row, lIdx, 1.0, layout);
                const d = Math.abs(mouseX - lx);
                if (d < minDist) {
                    minDist = d;
                    closestLane = lIdx;
                }
            }

            if (hoverState.row !== -1 || hoverState.lane !== closestLane) {
                hoverState.row = -1;
                hoverState.lane = closestLane;
                hideTooltip();
                schedule();
            }
        };

        const showTooltip = (e, row) => {
            const items = getItems();
            const item = items[row];
            if (!item) return;

            if (!tooltip) {
                tooltip = document.createElement('div');
                tooltip.className = 'git-graph-tooltip';
                tooltip.style.cssText = `
                    position: fixed;
                    background: var(--panel-strong);
                    border: 1px solid var(--border);
                    border-radius: 6px;
                    padding: 8px 12px;
                    font-size: 12px;
                    color: var(--text);
                    z-index: 10000;
                    pointer-events: none;
                    max-width: 280px;
                    box-shadow: 0 4px 12px rgba(0,0,0,0.3);
                    font-family: 'Inter', -apple-system, sans-serif;
                `;
                document.body.appendChild(tooltip);
            }

            const shortHash = item.shortHash || (item.hash ? item.hash.slice(0, 7) : '');
            tooltip.innerHTML = `
                <div style="font-weight: 600; color: var(--accent); margin-bottom: 4px;">${shortHash}</div>
                <div style="margin-bottom: 2px;">${item.subject || ''}</div>
                <div style="color: var(--muted); font-size: 11px;">${item.author || ''} · ${formatDate(item.date)}</div>
            `;

            tooltipTimeout = setTimeout(() => {
                tooltip.style.display = 'block';
                const x = e.clientX + 12;
                const y = e.clientY + 12;
                tooltip.style.left = x + 'px';
                tooltip.style.top = y + 'px';
            }, 300);
        };

        const hideTooltip = () => {
            if (tooltipTimeout) {
                clearTimeout(tooltipTimeout);
                tooltipTimeout = null;
            }
            if (tooltip) {
                tooltip.style.display = 'none';
            }
        };

        const formatDate = (iso) => {
            if (!iso) return '';
            const d = new Date(iso);
            return d.toLocaleDateString() + ' ' + d.toLocaleTimeString([], { hour: '2-digit', minute: '2-digit' });
        };

        const onMouseLeave = () => {
            if (hoverState.lane !== -1 || hoverState.row !== -1) {
                hoverState.row = -1;
                hoverState.lane = -1;
                hideTooltip();
                schedule();
            }
        };

        host.addEventListener('mousemove', onMouseMove);
        host.addEventListener('mouseleave', onMouseLeave);

        const setModel = (nextModel) => {
            ensureOverlay();

            console.log('[GraphRenderer] setModel called, rowCount:', nextModel?.rowCount, 'prev rowCount:', model?.rowCount);

            if (model && nextModel && model.rowCount > 0 && nextModel.rowCount > 0) {
                const prevMap = new Map();
                const items = getItems();
                for (let i = 0; i < model.rowCount; i++) {
                    const item = items[i];
                    if (item && item.hash) prevMap.set(item.hash, model.rowLane[i]);
                }
                animationState.prevLanes = prevMap;
                animationState.startTime = performance.now();
                animationState.active = true;
            }

            model = nextModel;
            console.log('[GraphRenderer] model set, calling schedule');
            schedule();
        };

        const refresh = () => schedule();

        const destroy = () => {
            destroyed = true;
            try { host.removeEventListener('scroll', onScroll); } catch (_) { }
            try { host.removeEventListener('mousemove', onMouseMove); } catch (_) { }
            try { host.removeEventListener('mouseleave', onMouseLeave); } catch (_) { }
            try { if (resizeObs) resizeObs.disconnect(); } catch (_) { }
            try { window.removeEventListener('resize', onScroll); } catch (_) { }
            try { if (raf && typeof cancelAnimationFrame === 'function') cancelAnimationFrame(raf); } catch (_) { }
            raf = 0;
            try { overlay?.remove?.(); } catch (_) { }
            overlay = null;
            canvas = null;
            ctx = null;
            hideTooltip();
            try { tooltip?.remove?.(); } catch (_) { }
            tooltip = null;
        };

        return { setModel, refresh, destroy };
    }

    // ============================================
    // History Renderer
    // ============================================
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
            const now = new Date();
            const sameYear = d.getFullYear() === now.getFullYear();
            const sameDay = d.toDateString() === now.toDateString();
            const mo = new Intl.DateTimeFormat('en', { month: 'short' }).format(d);
            const da = String(d.getDate()).padStart(2, '0');
            const time = d.toLocaleTimeString([], { hour: '2-digit', minute: '2-digit', hour12: false });
            if (sameDay) return `Today ${time}`;
            const yesterday = new Date();
            yesterday.setDate(yesterday.getDate() - 1);
            if (d.toDateString() === yesterday.toDateString()) return `Yesterday ${time}`;
            return `${da} ${mo} ${sameYear ? '' : d.getFullYear() + ' '}${time}`;
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
            console.log('[HistoryRenderer] render called, items:', items?.length);
            const host = document.getElementById(hostId);
            console.log('[HistoryRenderer] host element:', !!host, 'hostId:', hostId);
            if (!host) return;

            const list = Array.isArray(items) ? items : [];
            currentItems = list;
            console.log('[HistoryRenderer] list length:', list.length);
            if (!list.length) {
                console.log('[HistoryRenderer] No items, showing empty state');
                destroyGraph();
                destroyVirtual();
                host.innerHTML = '';
                host.innerHTML = `
                    <div class=\"git-log-empty\">
                        <svg width=\"48\" height=\"48\" viewBox=\"0 0 16 16\" fill=\"currentColor\" opacity=\"0.3\">
                            <path d=\"M8 1C4.7 1 2 3.7 2 7c0 2.4 1.4 4.5 3.5 5.5L6 14v1h1v-1l.5-.5c1.1.5 2.4.5 3.5.5h.5v-1l1-1.5C12.6 11.5 14 9.4 14 7c0-3.3-2.7-6-6-6z\"/>
                        </svg>
                        <p>No commit history yet</p>
                    </div>
                `;
                return;
            }

            const rowHeight = 36;

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
                    const max = 3;
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
                console.log('[HistoryRenderer] Using non-virtual rendering');
                host.innerHTML = '';
                host.style.setProperty('--git-graph-width', '70px');
                host.innerHTML = '';
                list.slice(0, 300).forEach((item) => host.appendChild(makeRow(item)));
                console.log('[HistoryRenderer] Added', Math.min(300, list.length), 'rows to host');
                if (list.length > 300) {
                    const msg = document.createElement('div');
                    msg.className = 'pane-subtitle';
                    msg.textContent = `Showing first 300 of ${list.length} commits.`;
                    host.appendChild(msg);
                }
                if (!graphRenderer) {
                    console.log('[HistoryRenderer] Creating graphRenderer');
                    graphRenderer = createGraphRenderer({
                        host,
                        getSelectedHash: () => selectedCommitRef?.hash || '',
                        getItems: () => currentItems
                    });
                    console.log('[HistoryRenderer] graphRenderer created:', !!graphRenderer);
                }
                if (list !== lastItemsRef) {
                    console.log('[HistoryRenderer] Creating GraphModel with', list.length, 'commits');
                    graphModel = new GraphModel(list, { rowHeight });
                    console.log('[HistoryRenderer] GraphModel created, graphWidth:', graphModel.graphWidth);
                    host.style.setProperty('--git-graph-width', `${Math.ceil(graphModel.graphWidth) + 8}px`);
                    console.log('[HistoryRenderer] Calling graphRenderer.setModel');
                    graphRenderer.setModel(graphModel);
                    lastItemsRef = list;
                } else {
                    console.log('[HistoryRenderer] Refreshing graph');
                    graphRenderer.refresh();
                }
                return;
            }

            console.log('[HistoryRenderer] Using VIRTUAL list rendering');
            if (historyVirtual && host.childElementCount === 0 && host.textContent.trim()) {
                destroyVirtual();
                host.textContent = "";
            }

            if (!historyVirtual) {
                console.log('[HistoryRenderer] Creating virtual list');
                historyVirtual = createVirtualList(host, { rowHeight, renderRow: makeRow, overscan: 10 });
            }
            historyVirtual.setItems(list);
            console.log('[HistoryRenderer] Virtual list items set:', list.length);

            if (!graphRenderer) {
                console.log('[HistoryRenderer] Creating graphRenderer for virtual list');
                graphRenderer = createGraphRenderer({
                    host,
                    getSelectedHash: () => selectedCommitRef?.hash || '',
                    getItems: () => currentItems
                });
                console.log('[HistoryRenderer] graphRenderer created for virtual list:', !!graphRenderer);
            }

            if (list !== lastItemsRef) {
                console.log('[HistoryRenderer] Creating GraphModel for virtual list, commits:', list.length);
                graphModel = new GraphModel(list, { rowHeight });
                console.log('[HistoryRenderer] GraphModel created, graphWidth:', graphModel.graphWidth);
                host.style.setProperty('--git-graph-width', `${Math.ceil(graphModel.graphWidth) + 8}px`);
                console.log('[HistoryRenderer] Calling graphRenderer.setModel for virtual list');
                graphRenderer.setModel(graphModel);
                lastItemsRef = list;
            } else {
                console.log('[HistoryRenderer] Refreshing graph for virtual list');
                graphRenderer.refresh();
            }
        };

        return { render };
    }

    // ============================================
    // Export Module
    // ============================================
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
