/**
 * MUMPS Smart Rename Preview Dialog
 * Multi-file diff preview with risk scoring for cross-routine tag rename
 */

(() => {
    function createMumpsSmartRenameDialog({ deps } = {}) {
        const createDialog = deps?.createDialog || window.AhmadIDEModules?.ui?.createDialog;
        const primitives = deps?.primitives || window.AhmadIDEModules?.ui?.primitives;

        if (!createDialog || !primitives) {
            throw new Error('MumpsSmartRenameDialog requires ui dialog + primitives');
        }

        const escapeHtml = (str) => String(str ?? '').replace(/[&<>"']/g, (m) => ({
            '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;'
        })[m]);

        /**
         * Apply edits to a line of text
         */
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

        /**
         * Render highlighted text with ranges
         */
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

        /**
         * Build a line diff row
         */
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

        /**
         * Build changes view for a single file
         */
        const buildFileChangesView = async ({ filePath, edits, readFile }) => {
            const container = document.createElement('div');
            container.className = 'mumps-rename-preview__changes';

            if (!Array.isArray(edits) || !edits.length) {
                const empty = document.createElement('div');
                empty.className = 'mumps-rename-preview__empty';
                empty.textContent = 'No changes.';
                container.appendChild(empty);
                return container;
            }

            // Read file content
            let lines = [];
            try {
                const content = await readFile(filePath);
                lines = String(content || '').split('\n');
            } catch (err) {
                const error = document.createElement('div');
                error.className = 'mumps-rename-preview__error';
                error.textContent = `Error reading file: ${err.message}`;
                container.appendChild(error);
                return container;
            }

            // Group edits by line
            const perLine = new Map();
            edits.forEach((e) => {
                const r = e?.textEdit?.range;
                if (!r || r.startLineNumber !== r.endLineNumber) return;
                const ln = r.startLineNumber;
                if (!perLine.has(ln)) perLine.set(ln, []);
                perLine.get(ln).push(e);
            });

            // Build line rows
            const lineNumbers = Array.from(perLine.keys()).sort((a, b) => a - b);
            lineNumbers.forEach((ln) => {
                if (ln < 1 || ln > lines.length) return;
                const lineText = lines[ln - 1];
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

        /**
         * Open smart rename preview dialog
         */
        const open = async (opts = {}, token) => {
            const {
                title = 'Smart Rename Tag',
                oldName = '',
                newName = '',
                fileEdits = new Map(),
                riskScore = 0,
                riskLevel = 'low',
                riskFactors = [],
                hasCollision = false,
                totalChanges = 0,
                filesAffected = 0,
                localChanges = 0,
                crossRoutineChanges = 0,
                unsafeChanges = 0,
                computeRename = null,
                readFile = null
            } = opts;

            return new Promise((resolve) => {
                let resolved = false;
                const finish = (result) => {
                    if (resolved) return;
                    resolved = true;
                    resolve(result || { apply: false, includeLocal: true, includeCrossRoutine: false, includeUnsafe: false });
                };

                const dialog = createDialog({
                    ariaLabel: title,
                    closeOnEscape: true,
                    closeOnBackdrop: false,
                    onClose: () => finish({ apply: false })
                });

                const wrapper = document.createElement('div');
                wrapper.className = 'ui-dialog-layout mumps-smart-rename-dialog';

                // Header
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
                    finish({ apply: false });
                    dialog.close('x');
                });
                headerRight.appendChild(closeBtn);

                header.appendChild(headerLeft);
                header.appendChild(headerRight);

                // Body
                const body = document.createElement('div');
                body.className = 'mumps-smart-rename-dialog__body';

                // Summary card
                const summaryCard = document.createElement('div');
                summaryCard.className = 'ui-settings-group';

                const summaryTitle = document.createElement('div');
                summaryTitle.className = 'ui-settings-group__title';
                summaryTitle.textContent = 'Smart Tag Rename';

                const summaryRow = document.createElement('div');
                summaryRow.className = 'mumps-rename-preview__summaryRow';
                summaryRow.innerHTML = `<span class="mumps-rename-preview__sym">${escapeHtml(oldName)}</span><span class="mumps-rename-preview__arrow">→</span><span class="mumps-rename-preview__sym mumps-rename-preview__sym--new">${escapeHtml(newName)}</span>`;

                // Risk score display
                const riskRow = document.createElement('div');
                riskRow.className = 'mumps-smart-rename__risk';
                const riskLevelClass = `mumps-smart-rename__risk-badge--${riskLevel}`;
                riskRow.innerHTML = `
                    <div class="mumps-smart-rename__risk-label">Risk Level:</div>
                    <div class="mumps-smart-rename__risk-badge ${riskLevelClass}">${riskLevel.toUpperCase()} (${riskScore}/100)</div>
                `;

                // Risk factors
                const factorsDiv = document.createElement('div');
                factorsDiv.className = 'mumps-smart-rename__factors';
                if (riskFactors.length > 0) {
                    const ul = document.createElement('ul');
                    ul.className = 'mumps-smart-rename__factors-list';
                    riskFactors.forEach(factor => {
                        const li = document.createElement('li');
                        li.textContent = factor;
                        ul.appendChild(li);
                    });
                    factorsDiv.appendChild(ul);
                }

                // Stats row
                const statsRow = document.createElement('div');
                statsRow.className = 'mumps-smart-rename__stats';
                statsRow.innerHTML = `
                    <div class="mumps-smart-rename__stat"><strong>${filesAffected}</strong> files</div>
                    <div class="mumps-smart-rename__stat"><strong>${totalChanges}</strong> changes</div>
                    <div class="mumps-smart-rename__stat"><strong>${localChanges}</strong> local</div>
                    <div class="mumps-smart-rename__stat"><strong>${crossRoutineChanges}</strong> cross-routine</div>
                    ${unsafeChanges > 0 ? `<div class="mumps-smart-rename__stat mumps-smart-rename__stat--warn"><strong>${unsafeChanges}</strong> unsafe</div>` : ''}
                `;

                summaryCard.appendChild(summaryTitle);
                summaryCard.appendChild(summaryRow);
                summaryCard.appendChild(riskRow);
                if (riskFactors.length > 0) summaryCard.appendChild(factorsDiv);
                summaryCard.appendChild(statsRow);

                // Collision warning
                if (hasCollision) {
                    const warningDiv = document.createElement('div');
                    warningDiv.className = 'mumps-smart-rename__collision-warning';
                    warningDiv.innerHTML = `<strong>⚠ Name Collision:</strong> Tag "${escapeHtml(newName)}" already exists. Rename blocked.`;
                    summaryCard.appendChild(warningDiv);
                }

                // Options checkboxes
                const { createCheckbox, createButton } = primitives;

                const state = {
                    includeLocal: true,
                    includeCrossRoutine: true,
                    includeUnsafe: false,
                    currentFileEdits: fileEdits
                };

                const optionsRow = document.createElement('div');
                optionsRow.className = 'mumps-smart-rename__options';

                const localCheck = createCheckbox({
                    label: `Apply local changes (${localChanges})`,
                    checked: true,
                    onChange: async (_e, checked) => {
                        state.includeLocal = !!checked;
                        if (computeRename) await refresh();
                    }
                });

                const crossRoutineCheck = createCheckbox({
                    label: `Apply safe cross-routine changes (${crossRoutineChanges})`,
                    checked: true,
                    disabled: crossRoutineChanges === 0,
                    onChange: async (_e, checked) => {
                        state.includeCrossRoutine = !!checked;
                        if (computeRename) await refresh();
                    }
                });

                const unsafeCheck = createCheckbox({
                    label: `Include unsafe/uncertain changes (${unsafeChanges})`,
                    checked: false,
                    disabled: unsafeChanges === 0,
                    onChange: async (_e, checked) => {
                        state.includeUnsafe = !!checked;
                        if (computeRename) await refresh();
                    }
                });

                optionsRow.appendChild(localCheck.root);
                optionsRow.appendChild(crossRoutineCheck.root);
                if (unsafeChanges > 0) optionsRow.appendChild(unsafeCheck.root);

                summaryCard.appendChild(optionsRow);

                // Changes card
                const changesCard = document.createElement('div');
                changesCard.className = 'ui-settings-group';
                const changesTitle = document.createElement('div');
                changesTitle.className = 'ui-settings-group__title';
                changesTitle.textContent = 'File Changes';

                const changesWrap = document.createElement('div');
                changesWrap.className = 'mumps-smart-rename__changesWrap';

                changesCard.appendChild(changesTitle);
                changesCard.appendChild(changesWrap);

                // Footer
                const footer = document.createElement('div');
                footer.className = 'ui-dialog-footer';

                const cancelBtn = createButton({
                    label: 'Cancel',
                    variant: 'ghost',
                    onClick: () => {
                        finish({ apply: false });
                        dialog.close('cancel');
                    }
                });

                const applyBtn = createButton({
                    label: 'Apply Rename',
                    variant: 'primary',
                    disabled: hasCollision,
                    onClick: () => {
                        finish({
                            apply: true,
                            includeLocal: state.includeLocal,
                            includeCrossRoutine: state.includeCrossRoutine,
                            includeUnsafe: state.includeUnsafe
                        });
                        dialog.close('apply');
                    }
                });

                footer.appendChild(cancelBtn);
                footer.appendChild(applyBtn);

                // Refresh function
                const refresh = async () => {
                    if (!computeRename) {
                        await renderFileEdits(state.currentFileEdits);
                        return;
                    }

                    // Recompute with current options
                    const result = await computeRename({
                        includeLocalChanges: state.includeLocal,
                        includeCrossRoutineChanges: state.includeCrossRoutine,
                        includeUnsafeChanges: state.includeUnsafe
                    });

                    state.currentFileEdits = result.fileEdits || new Map();
                    await renderFileEdits(state.currentFileEdits);
                };

                const renderFileEdits = async (editsMap) => {
                    changesWrap.innerHTML = '';

                    if (editsMap.size === 0) {
                        const empty = document.createElement('div');
                        empty.className = 'mumps-rename-preview__empty';
                        empty.textContent = 'No changes with current options.';
                        changesWrap.appendChild(empty);
                        return;
                    }

                    for (const [filePath, fileData] of editsMap.entries()) {
                        const fileBox = document.createElement('div');
                        fileBox.className = 'mumps-smart-rename__file';

                        const fileHead = document.createElement('div');
                        fileHead.className = 'mumps-smart-rename__fileHead';
                        const fileName = filePath.split(/[\\/]/).pop() || filePath;
                        const changeType = fileData.isLocal ? 'Local' : 'Cross-routine';
                        fileHead.innerHTML = `
                            <span class="mumps-smart-rename__fileName">${escapeHtml(fileName)}</span>
                            <span class="mumps-smart-rename__fileType">${changeType}</span>
                            <span class="mumps-smart-rename__fileCount">${fileData.edits.length} changes</span>
                        `;

                        const changesView = await buildFileChangesView({
                            filePath,
                            edits: fileData.edits,
                            readFile
                        });

                        fileBox.appendChild(fileHead);
                        fileBox.appendChild(changesView);
                        changesWrap.appendChild(fileBox);
                    }
                };

                // Assemble dialog
                body.appendChild(summaryCard);
                body.appendChild(changesCard);

                wrapper.appendChild(header);
                wrapper.appendChild(body);
                wrapper.appendChild(footer);

                dialog.setContent(wrapper);
                dialog.open();

                // Initial render
                renderFileEdits(state.currentFileEdits);

                try {
                    token?.onCancellationRequested?.(() => {
                        try { dialog.close('cancel'); } catch (_) { }
                        finish({ apply: false });
                    });
                } catch (_) { }
            });
        };

        return { open };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.mumps = window.AhmadIDEModules.mumps || {};
        window.AhmadIDEModules.mumps.createMumpsSmartRenameDialog = createMumpsSmartRenameDialog;
    }

    if (typeof module !== 'undefined' && module.exports) {
        module.exports = { createMumpsSmartRenameDialog };
    }
})();
