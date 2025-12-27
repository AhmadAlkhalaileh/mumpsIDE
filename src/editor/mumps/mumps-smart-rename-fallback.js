/**
 * Smart Rename Fallback Mode
 * Works without CallIndexer - uses only currently open Monaco models
 * Perfect for Docker/SSH/connection-based workflows
 */

(() => {
    function createSmartRenameFallback({ deps } = {}) {
        const getMonaco = deps?.getMonaco || (() => (typeof monaco !== 'undefined' ? monaco : null));
        const getAllOpenModels = deps?.getAllOpenModels || (() => {
            const monacoRef = getMonaco();
            if (!monacoRef) return [];
            return monacoRef.editor.getModels() || [];
        });

        /**
         * Build lightweight index from open Monaco models only
         */
        function buildIndexFromOpenModels() {
            const models = getAllOpenModels();
            console.log('[Smart Rename] Building index from', models.length, 'open models');
            const index = {
                tags: new Map(), // tagName -> [{routine, line, model}]
                references: new Map() // tagName -> [{routine, line, column, kind, model}]
            };

            models.forEach(model => {
                const uri = model.uri;
                const path = uri.path || uri.fsPath || uri.toString();
                const languageId = model.getLanguageId ? model.getLanguageId() : '';

                // Extract routine name from path
                let routineName = path.split(/[\\/]/).pop().replace(/\.m$/i, '');

                // If path doesn't look like a .m file, check language ID
                const isMumps = path.toLowerCase().endsWith('.m') ||
                    languageId === 'mumps' ||
                    languageId === 'MUMPS' ||
                    languageId.toLowerCase().includes('mumps');

                if (!isMumps) {
                    console.log('[Smart Rename] Skipping non-MUMPS model:', path, 'lang:', languageId);
                    return;
                }

                console.log('[Smart Rename] Indexing routine:', routineName, 'from', path);

                const lineCount = model.getLineCount();

                for (let lineNumber = 1; lineNumber <= lineCount; lineNumber++) {
                    const line = model.getLineContent(lineNumber);

                    // Find tag definitions (start of line)
                    const tagMatch = line.match(/^([A-Za-z%][A-Za-z0-9]*)/);
                    if (tagMatch) {
                        const tagName = tagMatch[1];
                        const tagUpper = tagName.toUpperCase();

                        if (!index.tags.has(tagUpper)) {
                            index.tags.set(tagUpper, []);
                        }
                        index.tags.get(tagUpper).push({
                            routine: routineName,
                            line: lineNumber,
                            model,
                            tag: tagName
                        });
                    }

                    // Find references (DO TAG, G TAG, $$TAG, DO TAG^ROU, etc.)
                    // Create fresh regex each time to avoid lastIndex issues
                    const getPatterns = () => [
                        { rx: /\b(DO|D)\s+([A-Za-z%][A-Za-z0-9]*)(\^([A-Za-z%][A-Za-z0-9]*))?/gi, kind: 'do' },
                        { rx: /\b(GOTO|G)\s+([A-Za-z%][A-Za-z0-9]*)(\^([A-Za-z%][A-Za-z0-9]*))?/gi, kind: 'goto' },
                        { rx: /\$\$([A-Za-z%][A-Za-z0-9]*)(\^([A-Za-z%][A-Za-z0-9]*))?/gi, kind: 'extrinsic' }
                    ];

                    getPatterns().forEach(({ rx, kind }) => {
                        rx.lastIndex = 0; // Reset regex state
                        let match;
                        while ((match = rx.exec(line)) !== null) {
                            let tagName, targetRoutine;

                            if (kind === 'extrinsic') {
                                tagName = match[1];
                                targetRoutine = match[3];
                            } else {
                                tagName = match[2];
                                targetRoutine = match[4];
                            }

                            if (!tagName) continue;

                            const tagUpper = tagName.toUpperCase();
                            const isLocal = !targetRoutine || targetRoutine.toUpperCase() === routineName.toUpperCase();

                            if (!index.references.has(tagUpper)) {
                                index.references.set(tagUpper, []);
                            }

                            index.references.get(tagUpper).push({
                                routine: routineName,
                                targetRoutine: targetRoutine || routineName,
                                line: lineNumber,
                                column: match.index,
                                kind,
                                isLocal,
                                model,
                                tag: tagName
                            });
                        }
                    });
                }
            });

            return index;
        }

        /**
         * Quick rename (current file only)
         */
        async function quickRenameCurrentFile(model, oldTagName, newTagName, monacoRef) {
            const edits = [];
            const routineName = getRoutineName(model);
            const oldUpper = oldTagName.toUpperCase();

            for (let lineNumber = 1; lineNumber <= model.getLineCount(); lineNumber++) {
                const line = model.getLineContent(lineNumber);

                // Tag definition
                const tagMatch = line.match(/^([A-Za-z%][A-Za-z0-9]*)/);
                if (tagMatch && tagMatch[1].toUpperCase() === oldUpper) {
                    edits.push({
                        resource: model.uri,
                        textEdit: {
                            range: new monacoRef.Range(lineNumber, 1, lineNumber, 1 + tagMatch[1].length),
                            text: newTagName
                        }
                    });
                }

                // References
                const refPatterns = [
                    /\b(DO|D)\s+([A-Za-z%][A-Za-z0-9]*)(?!\^)/gi,
                    /\b(GOTO|G)\s+([A-Za-z%][A-Za-z0-9]*)(?!\^)/gi,
                    /\$\$([A-Za-z%][A-Za-z0-9]*)(?!\^)/gi
                ];

                refPatterns.forEach(rx => {
                    let match;
                    while ((match = rx.exec(line)) !== null) {
                        const tagName = match[2] || match[1];
                        if (tagName && tagName.toUpperCase() === oldUpper) {
                            const startCol = match.index + (match[2] ? match[0].indexOf(tagName) : 2);
                            edits.push({
                                resource: model.uri,
                                textEdit: {
                                    range: new monacoRef.Range(lineNumber, startCol + 1, lineNumber, startCol + 1 + tagName.length),
                                    text: newTagName
                                }
                            });
                        }
                    }
                });
            }

            return edits;
        }

        function getRoutineName(model) {
            const uri = model?.uri;
            const path = uri?.path || uri?.fsPath || uri?.toString() || '';
            return path.split(/[\\/]/).pop().replace(/\.m$/i, '');
        }

        /**
         * Rename across ALL open Monaco models (for Docker/SSH cross-routine rename)
         * Returns edit plan grouped by routine for preview
         */
        function renameAcrossOpenModels(oldTagName, newTagName, currentModel, monacoRef) {
            const index = buildIndexFromOpenModels();
            const oldUpper = oldTagName.toUpperCase();
            const currentRoutine = getRoutineName(currentModel);

            // Directly find the tag definition in current model (don't rely on index)
            let defLine = -1;
            let defTag = null;
            for (let ln = 1; ln <= currentModel.getLineCount(); ln++) {
                const line = currentModel.getLineContent(ln);
                const tagMatch = line.match(/^([A-Za-z%][A-Za-z0-9]*)/);
                if (tagMatch && tagMatch[1].toUpperCase() === oldUpper) {
                    defLine = ln;
                    defTag = tagMatch[1];
                    break;
                }
            }

            if (defLine === -1) {
                return { edits: [], byRoutine: {}, tagNotFound: true };
            }

            // Collect all edits grouped by routine
            const byRoutine = {};
            const allEdits = [];

            // 1. Add the definition edit
            const defEdit = {
                resource: currentModel.uri,
                routine: currentRoutine,
                lineNumber: defLine,
                type: 'definition',
                textEdit: {
                    range: new monacoRef.Range(defLine, 1, defLine, 1 + defTag.length),
                    text: newTagName
                }
            };
            allEdits.push(defEdit);
            byRoutine[currentRoutine] = byRoutine[currentRoutine] || [];
            byRoutine[currentRoutine].push(defEdit);

            // 2. Find all references across all open models (from index)
            const refs = index.references.get(oldUpper) || [];
            const processedLines = new Set(); // Track processed to avoid duplicates
            processedLines.add(`${currentRoutine}:${defLine}`); // Don't re-process definition line

            refs.forEach(ref => {
                // For local references (no ^ROUTINE), only rename if in same routine
                // For cross-routine references (TAG^ROUTINE), rename if target matches current routine
                const targetRoutineUpper = (ref.targetRoutine || ref.routine).toUpperCase();

                if (ref.isLocal && ref.routine.toUpperCase() !== currentRoutine.toUpperCase()) {
                    // Local reference in different routine - skip (different tag)
                    return;
                }

                if (!ref.isLocal && targetRoutineUpper !== currentRoutine.toUpperCase()) {
                    // Cross-routine reference to different routine - skip
                    return;
                }

                const lineKey = `${ref.routine}:${ref.line}`;
                if (processedLines.has(lineKey)) return;
                processedLines.add(lineKey);

                // Find exact position in line
                const line = ref.model.getLineContent(ref.line);
                const tagStart = findTagInLine(line, ref.tag, ref.kind);

                if (tagStart === -1) return;

                const edit = {
                    resource: ref.model.uri,
                    routine: ref.routine,
                    lineNumber: ref.line,
                    type: ref.kind,
                    isCrossRoutine: ref.routine.toUpperCase() !== currentRoutine.toUpperCase(),
                    textEdit: {
                        range: new monacoRef.Range(ref.line, tagStart + 1, ref.line, tagStart + 1 + ref.tag.length),
                        text: newTagName
                    }
                };
                allEdits.push(edit);
                byRoutine[ref.routine] = byRoutine[ref.routine] || [];
                byRoutine[ref.routine].push(edit);
            });

            // 3. Also directly scan current model for local references (backup in case index missed them)
            for (let ln = 1; ln <= currentModel.getLineCount(); ln++) {
                if (ln === defLine) continue; // Skip definition line
                const lineKey = `${currentRoutine}:${ln}`;
                if (processedLines.has(lineKey)) continue;

                const line = currentModel.getLineContent(ln);
                const lineUpper = line.toUpperCase();
                const tagUpper = oldTagName.toUpperCase();

                // Check for local references: DO TAG, D TAG, GOTO TAG, G TAG, $$TAG
                const localPatterns = [
                    { search: 'DO ' + tagUpper, kind: 'do' },
                    { search: 'D ' + tagUpper, kind: 'do' },
                    { search: 'GOTO ' + tagUpper, kind: 'goto' },
                    { search: 'G ' + tagUpper, kind: 'goto' },
                    { search: '$$' + tagUpper, kind: 'extrinsic' }
                ];

                for (const { search, kind } of localPatterns) {
                    const idx = lineUpper.indexOf(search);
                    if (idx !== -1) {
                        // Make sure it's not followed by ^ (which would be cross-routine)
                        const afterTag = lineUpper.charAt(idx + search.length);
                        if (afterTag === '^') continue; // Cross-routine, will be handled by index

                        const tagStart = kind === 'extrinsic' ? idx + 2 : idx + search.length - tagUpper.length;

                        const edit = {
                            resource: currentModel.uri,
                            routine: currentRoutine,
                            lineNumber: ln,
                            type: kind,
                            isCrossRoutine: false,
                            textEdit: {
                                range: new monacoRef.Range(ln, tagStart + 1, ln, tagStart + 1 + oldTagName.length),
                                text: newTagName
                            }
                        };
                        allEdits.push(edit);
                        byRoutine[currentRoutine] = byRoutine[currentRoutine] || [];
                        byRoutine[currentRoutine].push(edit);
                        processedLines.add(lineKey);
                        break; // Only one edit per line
                    }
                }
            }

            return {
                edits: allEdits,
                byRoutine,
                totalEdits: allEdits.length,
                routinesAffected: Object.keys(byRoutine).length,
                crossRoutineCount: allEdits.filter(e => e.isCrossRoutine).length
            };
        }

        /**
         * Find tag position in line for accurate replacement
         */
        function findTagInLine(line, tagName, kind) {
            // Simple approach: find the tag name directly (case-insensitive)
            const lineUpper = line.toUpperCase();
            const tagUpper = tagName.toUpperCase();

            // Search for the tag in typical patterns
            let idx = -1;

            if (kind === 'extrinsic') {
                // Look for $$TAG
                idx = lineUpper.indexOf('$$' + tagUpper);
                if (idx !== -1) idx += 2; // Skip past $$
            } else {
                // Look for DO TAG or GOTO TAG or D TAG or G TAG
                const patterns = ['DO ' + tagUpper, 'D ' + tagUpper, 'GOTO ' + tagUpper, 'G ' + tagUpper];
                for (const pat of patterns) {
                    const pos = lineUpper.indexOf(pat);
                    if (pos !== -1) {
                        idx = pos + pat.length - tagUpper.length;
                        break;
                    }
                }
            }

            return idx;
        }

        return {
            buildIndexFromOpenModels,
            quickRenameCurrentFile,
            renameAcrossOpenModels,
            getRoutineName
        };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.mumps = window.AhmadIDEModules.mumps || {};
        window.AhmadIDEModules.mumps.createSmartRenameFallback = createSmartRenameFallback;
    }

    if (typeof module !== 'undefined' && module.exports) {
        module.exports = { createSmartRenameFallback };
    }
})();
