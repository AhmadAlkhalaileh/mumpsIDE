(() => {
    function createMumpsRenamePreviewDialog({ deps } = {}) {
        const createDialog = deps?.createDialog || window.AhmadIDEModules?.ui?.createDialog;
        const primitives = deps?.primitives || window.AhmadIDEModules?.ui?.primitives;

        if (!createDialog || !primitives) {
            throw new Error('MumpsRenamePreviewDialog requires ui dialog + primitives');
        }

        const escapeHtml = (str) => String(str ?? '').replace(/[&<>"']/g, (m) => ({
            '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;'
        })[m]);

        const groupEditsByResource = (edits) => {
            const map = new Map();
            (Array.isArray(edits) ? edits : []).forEach((e) => {
                const key = e?.resource?.toString?.() || String(e?.resource || '');
                if (!map.has(key)) map.set(key, []);
                map.get(key).push(e);
            });
            return Array.from(map.entries()).map(([resource, list]) => ({ resource, edits: list }));
        };

        const applyEditsToLine = (lineText, editsForLine) => {
            const original = String(lineText ?? '');
            const sorted = (Array.isArray(editsForLine) ? editsForLine : []).slice().sort((a, b) => {
                const ra = a.textEdit.range;
                const rb = b.textEdit.range;
                return ra.startColumn - rb.startColumn;
            });

            const oldRanges = [];
            const newRanges = [];
            let out = original;
            let delta = 0;

            for (const e of sorted) {
                const r = e.textEdit.range;
                const start0 = Math.max(0, (r.startColumn - 1));
                const end0 = Math.max(start0, (r.endColumn - 1));
                oldRanges.push({ start: start0, end: end0 });

                const start = start0 + delta;
                const end = end0 + delta;
                const text = String(e.textEdit.text ?? '');
                out = out.slice(0, start) + text + out.slice(end);
                newRanges.push({ start, end: start + text.length });
                delta += text.length - (end0 - start0);
            }

            return { original, updated: out, oldRanges, newRanges };
        };

        const renderHighlighted = (text, ranges, cls) => {
            const s = String(text ?? '');
            const safeRanges = (Array.isArray(ranges) ? ranges : []).slice()
                .filter((r) => Number.isFinite(r.start) && Number.isFinite(r.end) && r.end > r.start)
                .sort((a, b) => a.start - b.start);

            let out = '';
            let pos = 0;
            for (const r of safeRanges) {
                const a = Math.max(0, Math.min(s.length, r.start));
                const b = Math.max(a, Math.min(s.length, r.end));
                if (a > pos) out += escapeHtml(s.slice(pos, a));
                out += `<span class="${cls}">${escapeHtml(s.slice(a, b))}</span>`;
                pos = b;
            }
            if (pos < s.length) out += escapeHtml(s.slice(pos));
            return out;
        };

        const buildLineRow = ({ lineNumber, before, after, beforeRanges, afterRanges }) => {
            const row = document.createElement('div');
            row.className = 'mumps-rename-preview__line';

            const ln = document.createElement('div');
            ln.className = 'mumps-rename-preview__ln';
            ln.textContent = String(lineNumber);

            const snippet = document.createElement('div');
            snippet.className = 'mumps-rename-preview__snippet';

            const preBefore = document.createElement('pre');
            preBefore.className = 'mumps-rename-preview__code';
            preBefore.innerHTML = renderHighlighted(before, beforeRanges, 'mumps-rename-preview__hl-old');

            const preAfter = document.createElement('pre');
            preAfter.className = 'mumps-rename-preview__code mumps-rename-preview__code--after';
            preAfter.innerHTML = renderHighlighted(after, afterRanges, 'mumps-rename-preview__hl-new');

            snippet.appendChild(preBefore);
            snippet.appendChild(preAfter);

            row.appendChild(ln);
            row.appendChild(snippet);
            return row;
        };

        const buildChangesView = ({ model, edits }) => {
            const container = document.createElement('div');
            container.className = 'mumps-rename-preview__changes';

            if (!model || !Array.isArray(edits) || !edits.length) {
                const empty = document.createElement('div');
                empty.className = 'mumps-rename-preview__empty';
                empty.textContent = 'No changes.';
                container.appendChild(empty);
                return container;
            }

            const perLine = new Map();
            edits.forEach((e) => {
                const r = e?.textEdit?.range;
                if (!r || r.startLineNumber !== r.endLineNumber) return;
                const ln = r.startLineNumber;
                if (!perLine.has(ln)) perLine.set(ln, []);
                perLine.get(ln).push(e);
            });

            const lineNumbers = Array.from(perLine.keys()).sort((a, b) => a - b);
            lineNumbers.forEach((ln) => {
                const lineText = model.getLineContent(ln);
                const applied = applyEditsToLine(lineText, perLine.get(ln));
                container.appendChild(buildLineRow({
                    lineNumber: ln,
                    before: applied.original,
                    after: applied.updated,
                    beforeRanges: applied.oldRanges,
                    afterRanges: applied.newRanges
                }));
            });

            return container;
        };

        const open = (opts = {}, token) => {
            const {
                title = 'Rename Preview',
                kind = 'variable',
                oldName = '',
                newName = '',
                fileLabel = 'Current file',
                model = null,
                baseEdits = [],
                compute = null,
                unsafeReasons = [],
                stringCommentMatches = 0
            } = opts;

            return new Promise((resolve) => {
                let resolved = false;
                const finish = (result) => {
                    if (resolved) return;
                    resolved = true;
                    resolve(result || { apply: false, includeStringsComments: false, allowUnsafe: false });
                };

                const dialog = createDialog({
                    ariaLabel: title,
                    closeOnEscape: true,
                    closeOnBackdrop: false,
                    onClose: () => finish({ apply: false, includeStringsComments: false, allowUnsafe: false })
                });

                const wrapper = document.createElement('div');
                wrapper.className = 'ui-dialog-layout';

                const header = document.createElement('div');
                header.className = 'ui-dialog-header';
                const headerLeft = document.createElement('div');
                headerLeft.className = 'ui-dialog-header__left';
                const titleEl = document.createElement('div');
                titleEl.className = 'ui-dialog-title';
                titleEl.textContent = title;
                headerLeft.appendChild(titleEl);

                const headerRight = document.createElement('div');
                headerRight.className = 'ui-dialog-header__right';
                const closeBtn = document.createElement('button');
                closeBtn.className = 'ui-dialog-close';
                closeBtn.type = 'button';
                closeBtn.title = 'Close';
                closeBtn.textContent = '✕';
                closeBtn.addEventListener('click', () => {
                    finish({ apply: false, includeStringsComments: state.includeStringsComments, allowUnsafe: state.allowUnsafe });
                    dialog.close('x');
                });
                headerRight.appendChild(closeBtn);

                header.appendChild(headerLeft);
                header.appendChild(headerRight);

                const body = document.createElement('div');
                body.className = 'mumps-rename-preview__body';

                const summaryCard = document.createElement('div');
                summaryCard.className = 'ui-settings-group';
                const summaryTitle = document.createElement('div');
                summaryTitle.className = 'ui-settings-group__title';
                summaryTitle.textContent = kind === 'label' ? 'Label Rename' : 'Local Variable Rename';
                const summaryHint = document.createElement('div');
                summaryHint.className = 'ui-settings-group__hint';
                summaryHint.textContent = fileLabel;

                const summaryRow = document.createElement('div');
                summaryRow.className = 'mumps-rename-preview__summaryRow';
                summaryRow.innerHTML = `<span class="mumps-rename-preview__sym">${escapeHtml(oldName)}</span><span class="mumps-rename-preview__arrow">→</span><span class="mumps-rename-preview__sym mumps-rename-preview__sym--new">${escapeHtml(newName)}</span>`;

                const togglesRow = document.createElement('div');
                togglesRow.className = 'mumps-rename-preview__toggles';

                const { createCheckbox, createButton } = primitives;

                const state = {
                    includeStringsComments: false,
                    allowUnsafe: false,
                    current: { edits: baseEdits }
                };

                const unsafeOn = Array.isArray(unsafeReasons) && unsafeReasons.length > 0;
                const stringsOn = Number(stringCommentMatches || 0) > 0;

                let unsafeCheck = null;
                if (unsafeOn) {
                    unsafeCheck = createCheckbox({
                        label: 'Unsafe rename',
                        checked: false,
                        onChange: (_e, checked) => {
                            state.allowUnsafe = !!checked;
                            applyBtn.disabled = unsafeOn && !state.allowUnsafe;
                        }
                    });
                    togglesRow.appendChild(unsafeCheck.root);
                }

                let stringsCheck = null;
                if (stringsOn) {
                    stringsCheck = createCheckbox({
                        label: `Include strings/comments (${stringCommentMatches})`,
                        checked: false,
                        onChange: (_e, checked) => {
                            state.includeStringsComments = !!checked;
                            refresh();
                        }
                    });
                    togglesRow.appendChild(stringsCheck.root);
                }

                const warnings = document.createElement('div');
                warnings.className = 'mumps-rename-preview__warnings' + (unsafeOn ? '' : ' hidden');
                if (unsafeOn) {
                    const ul = document.createElement('ul');
                    ul.className = 'mumps-rename-preview__warningsList';
                    unsafeReasons.forEach((r) => {
                        const li = document.createElement('li');
                        li.textContent = String(r || '');
                        ul.appendChild(li);
                    });
                    warnings.appendChild(ul);
                }

                summaryCard.appendChild(summaryTitle);
                summaryCard.appendChild(summaryHint);
                summaryCard.appendChild(summaryRow);
                if (togglesRow.childNodes.length) summaryCard.appendChild(togglesRow);
                if (unsafeOn) summaryCard.appendChild(warnings);

                const changesCard = document.createElement('div');
                changesCard.className = 'ui-settings-group';
                const changesTitle = document.createElement('div');
                changesTitle.className = 'ui-settings-group__title';
                changesTitle.textContent = 'Changes';
                const changesHint = document.createElement('div');
                changesHint.className = 'ui-settings-group__hint';

                const changesWrap = document.createElement('div');
                changesWrap.className = 'mumps-rename-preview__changesWrap';

                changesCard.appendChild(changesTitle);
                changesCard.appendChild(changesHint);
                changesCard.appendChild(changesWrap);

                const footer = document.createElement('div');
                footer.className = 'ui-dialog-footer';

                const cancelBtn = createButton({
                    label: 'Cancel',
                    variant: 'ghost',
                    onClick: () => {
                        finish({ apply: false, includeStringsComments: state.includeStringsComments, allowUnsafe: state.allowUnsafe });
                        dialog.close('cancel');
                    }
                });

                const applyBtn = createButton({
                    label: 'Apply',
                    variant: 'primary',
                    onClick: () => {
                        finish({ apply: true, includeStringsComments: state.includeStringsComments, allowUnsafe: state.allowUnsafe });
                        dialog.close('apply');
                    }
                });

                applyBtn.disabled = unsafeOn && !state.allowUnsafe;

                footer.appendChild(cancelBtn);
                footer.appendChild(applyBtn);

                const refresh = () => {
                    const computeFn = typeof compute === 'function' ? compute : null;
                    const res = computeFn
                        ? computeFn({ includeStringsComments: state.includeStringsComments })
                        : { edits: baseEdits };

                    const nextEdits = Array.isArray(res?.edits) ? res.edits : baseEdits;
                    state.current = { edits: nextEdits };

                    changesHint.textContent = `${nextEdits.length} edit${nextEdits.length === 1 ? '' : 's'}`;
                    changesWrap.innerHTML = '';

                    // Group by file even if single-file (future-safe)
                    const groups = groupEditsByResource(nextEdits);
                    groups.forEach((g) => {
                        const fileBox = document.createElement('div');
                        fileBox.className = 'mumps-rename-preview__file';
                        const fileHead = document.createElement('div');
                        fileHead.className = 'mumps-rename-preview__fileHead';
                        fileHead.textContent = g.resource || fileLabel;
                        fileBox.appendChild(fileHead);
                        fileBox.appendChild(buildChangesView({ model, edits: g.edits }));
                        changesWrap.appendChild(fileBox);
                    });
                };

                body.appendChild(summaryCard);
                body.appendChild(changesCard);

                wrapper.appendChild(header);
                wrapper.appendChild(body);
                wrapper.appendChild(footer);

                dialog.setContent(wrapper);
                dialog.open();

                refresh();

                try {
                    token?.onCancellationRequested?.(() => {
                        try { dialog.close('cancel'); } catch (_) { }
                        finish({ apply: false, includeStringsComments: state.includeStringsComments, allowUnsafe: state.allowUnsafe });
                    });
                } catch (_) { }
            });
        };

        return { open };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.mumpsMonaco = window.AhmadIDEModules.mumpsMonaco || {};
        window.AhmadIDEModules.mumpsMonaco.createMumpsRenamePreviewDialog = createMumpsRenamePreviewDialog;
    }

    if (typeof module !== 'undefined' && module.exports) {
        module.exports = { createMumpsRenamePreviewDialog };
    }
})();

