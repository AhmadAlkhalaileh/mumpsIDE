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

        let unstagedVirtual = null;
        let stagedVirtual = null;

        const destroyVirtual = (which) => {
            try { which?.destroy?.(); } catch (_) { }
        };

        const makeRowFactory = (staged) => (ent) => {
            const row = document.createElement('div');
            row.className = 'git-change-row' + (staged ? ' staged' : '');
            const checkbox = document.createElement('input');
            checkbox.type = 'checkbox';
            checkbox.checked = staged ? gitSelected.staged.has(ent.path) : gitSelected.unstaged.has(ent.path);
            checkbox.addEventListener('change', () => {
                const targetSet = staged ? gitSelected.staged : gitSelected.unstaged;
                checkbox.checked ? targetSet.add(ent.path) : targetSet.delete(ent.path);
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

            const rowHeight = 34;
            const renderRow = makeRowFactory(staged);
            if (staged) {
                if (!stagedVirtual) stagedVirtual = createVirtualList(host, { rowHeight, renderRow, overscan: 8 });
                stagedVirtual.setItems(list);
            } else {
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

    function createHistoryRenderer(opts = {}) {
        const hostId = opts.hostId || 'gitHistoryList';
        const createVirtualList = opts.createVirtualList || null;

        let historyVirtual = null;

        const destroyVirtual = () => {
            try { historyVirtual?.destroy?.(); } catch (_) { }
            historyVirtual = null;
        };

        const render = (lines) => {
            const host = document.getElementById(hostId);
            if (!host) return;

            const list = Array.isArray(lines) ? lines : [];
            if (!list.length) {
                destroyVirtual();
                host.innerHTML = '';
                host.textContent = 'No history yet.';
                return;
            }

            if (!createVirtualList) {
                host.innerHTML = '';
                list.slice(0, 200).forEach(line => {
                    const div = document.createElement('div');
                    div.textContent = line;
                    host.appendChild(div);
                });
                if (list.length > 200) {
                    const msg = document.createElement('div');
                    msg.className = 'pane-subtitle';
                    msg.textContent = `Showing first 200 of ${list.length} commits (virtual list unavailable).`;
                    host.appendChild(msg);
                }
                return;
            }

            const renderRow = (line) => {
                const div = document.createElement('div');
                div.className = 'git-history-row';
                div.textContent = line || '';
                return div;
            };

            if (!historyVirtual) historyVirtual = createVirtualList(host, { rowHeight: 22, renderRow, overscan: 10 });
            historyVirtual.setItems(list);
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
            createHistoryRenderer
        };
    }
})();

