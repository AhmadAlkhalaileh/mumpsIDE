(() => {
    function createDiffTabUi({ getActiveTab, isDiffTab, openDiffTab, getActiveEditor } = {}) {
        // Diff toolbar in the main editor action bar (shown only for diff tabs).
        let diffToolbarEl = null;
        let diffToolbarWired = false;

        // Split diff viewer overlay (replaces plain-text diff tabs rendering).
        let diffTabViewerEl = null;
        let diffTabRenderer = null;
        let diffTabScrollSyncing = false;

        function ensureDiffTabRenderer() {
            if (diffTabRenderer?.render) return diffTabRenderer;
            try {
                const perfUi = window.AhmadIDEModules?.features?.git?.perfUi;
                if (perfUi?.createDiffRenderer) {
                    diffTabRenderer = perfUi.createDiffRenderer({
                        leftId: 'diffTabLeft',
                        rightId: 'diffTabRight',
                        maxLines: 12000
                    });
                }
            } catch (_) { }
            return diffTabRenderer;
        }

        function ensureDiffTabViewer() {
            if (diffTabViewerEl) return diffTabViewerEl;
            const frame = document.querySelector('.editor-frame');
            if (!frame) return null;

            diffTabViewerEl = document.createElement('div');
            diffTabViewerEl.id = 'diffTabViewer';
            diffTabViewerEl.className = 'diff-tab-viewer hidden';
            diffTabViewerEl.innerHTML = `
            <div class="git-diff-grid" role="region" aria-label="Diff view">
                <div class="git-diff-pane" id="diffTabLeft" aria-label="Original"></div>
                <div class="git-diff-pane" id="diffTabRight" aria-label="Modified"></div>
            </div>
        `;
            frame.appendChild(diffTabViewerEl);

            // Keep both panes scrolled together (IntelliJ-style)
            const left = diffTabViewerEl.querySelector('#diffTabLeft');
            const right = diffTabViewerEl.querySelector('#diffTabRight');
            if (left && right) {
                const sync = (src, dst) => () => {
                    if (diffTabScrollSyncing) return;
                    diffTabScrollSyncing = true;
                    try {
                        dst.scrollTop = src.scrollTop;
                        dst.scrollLeft = src.scrollLeft;
                    } finally {
                        diffTabScrollSyncing = false;
                    }
                };
                left.addEventListener('scroll', sync(left, right), { passive: true });
                right.addEventListener('scroll', sync(right, left), { passive: true });
            }

            // Reuse the Git perf diff renderer (two-column, colored).
            ensureDiffTabRenderer();

            return diffTabViewerEl;
        }

        function syncDiffTabViewer(tab) {
            const viewer = ensureDiffTabViewer();
            const editorHost = document.getElementById('editor');
            if (!viewer || !editorHost) return;

            const show = isDiffTab(tab);
            viewer.classList.toggle('hidden', !show);
            editorHost.classList.toggle('hidden', show);

            if (!show) {
                // Editor was hidden; force layout once visible.
                const editor = getActiveEditor();
                try { editor?.layout?.(); } catch (_) { }
                return;
            }

            const left = document.getElementById('diffTabLeft');
            const right = document.getElementById('diffTabRight');
            const text = String(tab?.content || '');

            const renderer = ensureDiffTabRenderer();
            if (renderer?.render) {
                renderer.render(text);
            } else {
                // Fallback: show unified diff as plain text in both panes.
                if (left) left.textContent = text || 'No changes.\n';
                if (right) right.textContent = text || 'No changes.\n';
            }

            // Reset scroll on activation (keeps panes aligned).
            if (left) left.scrollTop = 0;
            if (right) right.scrollTop = 0;
        }

        function computeDiffAnchors(diffText) {
            const anchors = [];
            const lines = String(diffText || '').split('\n');
            for (let i = 0; i < lines.length; i += 1) {
                const line = lines[i];
                if (line.startsWith('@@') || line.startsWith('diff --git')) {
                    anchors.push(i + 1);
                }
            }
            return anchors;
        }

        function ensureDiffToolbar() {
            const host = document.querySelector('.action-bar .actions');
            if (!host) return null;
            if (!diffToolbarEl) {
                diffToolbarEl = document.createElement('div');
                diffToolbarEl.id = 'diffToolbar';
                diffToolbarEl.className = 'diff-toolbar hidden';
                diffToolbarEl.innerHTML = `
                <button class="btn ghost diff-toolbar-btn" type="button" id="diffPrevChangeBtn" title="Previous change">Prev</button>
                <button class="btn ghost diff-toolbar-btn" type="button" id="diffNextChangeBtn" title="Next change">Next</button>
                <div class="toolbar-separator" aria-hidden="true"></div>
                <button class="btn ghost diff-toolbar-btn" type="button" id="diffIgnoreWhitespaceBtn" title="Ignore whitespace">Ignore WS</button>
                <select class="diff-parent-select hidden" id="diffParentSelect" title="Compare with parent"></select>
            `;
                host.prepend(diffToolbarEl);
            }
            if (!diffToolbarWired) {
                diffToolbarWired = true;
                diffToolbarEl.querySelector('#diffPrevChangeBtn')?.addEventListener('click', () => navigateDiffChange(-1));
                diffToolbarEl.querySelector('#diffNextChangeBtn')?.addEventListener('click', () => navigateDiffChange(1));
                diffToolbarEl.querySelector('#diffIgnoreWhitespaceBtn')?.addEventListener('click', () => toggleDiffWhitespace());
                diffToolbarEl.querySelector('#diffParentSelect')?.addEventListener('change', (e) => {
                    const tab = getActiveTab();
                    if (!isDiffTab(tab)) return;
                    const meta = tab?.meta || {};
                    const parentHash = String(e?.target?.value || '').trim();
                    if (!parentHash) return;
                    openDiffTab({ ...meta, parentHash, source: 'diff-parent-select' }).catch(() => { });
                });
            }
            return diffToolbarEl;
        }

        function syncDiffToolbar(tab) {
            const el = ensureDiffToolbar();
            if (!el) return;
            if (!isDiffTab(tab)) {
                el.classList.add('hidden');
                return;
            }

            el.classList.remove('hidden');
            const ignoreBtn = el.querySelector('#diffIgnoreWhitespaceBtn');
            const parentSelect = el.querySelector('#diffParentSelect');
            const meta = tab?.meta || {};
            const ignore = !!meta.ignoreWhitespace;
            if (ignoreBtn) {
                ignoreBtn.classList.toggle('active', ignore);
                ignoreBtn.textContent = ignore ? 'Ignore WS: On' : 'Ignore WS';
            }
            if (parentSelect) {
                const parents = Array.isArray(meta.parents) ? meta.parents.filter(Boolean) : [];
                if (parents.length > 1) {
                    parentSelect.classList.remove('hidden');
                    parentSelect.innerHTML = '';
                    parents.forEach((hash, idx) => {
                        const opt = document.createElement('option');
                        opt.value = hash;
                        opt.textContent = idx === 0 ? `Parent 1 (${hash.slice(0, 7)})` : `Parent ${idx + 1} (${hash.slice(0, 7)})`;
                        parentSelect.appendChild(opt);
                    });
                    const selected = String(meta.parentHash || parents[0] || '').trim();
                    if (selected) parentSelect.value = selected;
                } else {
                    parentSelect.classList.add('hidden');
                }
            }
        }

        function navigateDiffChange(direction) {
            const tab = getActiveTab();
            if (!isDiffTab(tab)) return;

            // If the split diff viewer is active, scroll it to the next/prev hunk.
            const viewer = diffTabViewerEl;
            const leftPane = document.getElementById('diffTabLeft');
            const rightPane = document.getElementById('diffTabRight');
            if (viewer && !viewer.classList.contains('hidden') && leftPane && rightPane) {
                const anchors = Array.from(leftPane.querySelectorAll('.diff-hunk'));
                if (!anchors.length) return;
                const cur = leftPane.scrollTop;
                let targetEl = null;
                if (direction > 0) {
                    targetEl = anchors.find((el) => el.offsetTop > cur + 2) || anchors[0];
                } else {
                    for (let i = anchors.length - 1; i >= 0; i -= 1) {
                        if (anchors[i].offsetTop < cur - 2) {
                            targetEl = anchors[i];
                            break;
                        }
                    }
                    if (!targetEl) targetEl = anchors[anchors.length - 1];
                }
                if (!targetEl) return;
                const top = Math.max(0, targetEl.offsetTop - 12);
                leftPane.scrollTop = top;
                rightPane.scrollTop = top;
                return;
            }

            // Fallback: plain-text diff in Monaco editor.
            const editor = getActiveEditor();
            if (!editor) return;
            const anchors = Array.isArray(tab.diffAnchors) ? tab.diffAnchors : [];
            if (!anchors.length) return;
            const pos = editor.getPosition?.();
            const curLine = pos?.lineNumber || 1;
            let target = null;
            if (direction > 0) {
                target = anchors.find((n) => n > curLine) || anchors[0];
            } else {
                for (let i = anchors.length - 1; i >= 0; i -= 1) {
                    if (anchors[i] < curLine) {
                        target = anchors[i];
                        break;
                    }
                }
                if (!target) target = anchors[anchors.length - 1];
            }
            if (!target) return;
            editor.setPosition?.({ lineNumber: target, column: 1 });
            editor.revealLineInCenter?.(target);
            editor.focus?.();
        }

        function toggleDiffWhitespace() {
            const tab = getActiveTab();
            if (!isDiffTab(tab)) return;
            const meta = tab?.meta || {};
            openDiffTab({ ...meta, ignoreWhitespace: !meta.ignoreWhitespace, source: 'diff-ignore-ws' }).catch(() => { });
        }

        function handleEditorTabActivated(tab) {
            syncDiffToolbar(tab);
            syncDiffTabViewer(tab);
        }

        function handleEditorTabClosed(_tab) {
            // When the last tab closes there is no "tab activated" event,
            // so ensure diff UI is reset (hide split viewer + diff toolbar).
            try {
                const active = getActiveTab();
                syncDiffToolbar(active);
                syncDiffTabViewer(active);
            } catch (_) { }
        }

        return {
            computeDiffAnchors,
            syncDiffToolbar,
            syncDiffTabViewer,
            handleEditorTabActivated,
            handleEditorTabClosed
        };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.features = window.AhmadIDEModules.features || {};
        window.AhmadIDEModules.features.git = window.AhmadIDEModules.features.git || {};
        window.AhmadIDEModules.features.git.diffTabUi = {
            createDiffTabUi
        };
    }
})();
