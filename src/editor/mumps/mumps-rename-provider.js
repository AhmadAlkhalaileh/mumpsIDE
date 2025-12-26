(() => {
    function createMumpsRenameProvider({ deps } = {}) {
        const getMonaco = deps?.getMonaco || (() => (typeof monaco !== 'undefined' ? monaco : null));
        const getPreviewDialogFactory = deps?.getPreviewDialogFactory || (() => window.AhmadIDEModules?.mumpsMonaco?.createMumpsRenamePreviewDialog);

        const isAlpha = (ch) => (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || ch === '%';
        const isDigit = (ch) => ch >= '0' && ch <= '9';
        const isNameStart = (ch) => isAlpha(ch);
        const isNameChar = (ch) => isAlpha(ch) || isDigit(ch);

        const isValidLocalName = (name) => /^[A-Za-z%][A-Za-z0-9]*$/.test(String(name || ''));
        const isValidLabelName = (name) => isValidLocalName(name);

        const segmentMumpsLine = (line) => {
            const s = String(line || '');
            const segments = [];
            let i = 0;
            let kind = 'code';
            let start = 0;

            const push = (k, a, b) => {
                if (b <= a) return;
                segments.push({ kind: k, start: a, end: b });
            };

            while (i < s.length) {
                const ch = s[i];
                if (kind === 'code') {
                    if (ch === '"') {
                        push('code', start, i);
                        kind = 'string';
                        start = i;
                        i++;
                        continue;
                    }
                    if (ch === ';') {
                        push('code', start, i);
                        push('comment', i, s.length);
                        return segments;
                    }
                    i++;
                    continue;
                }

                if (kind === 'string') {
                    if (ch === '"') {
                        if (i + 1 < s.length && s[i + 1] === '"') {
                            i += 2;
                            continue;
                        }
                        i++;
                        push('string', start, i);
                        kind = 'code';
                        start = i;
                        continue;
                    }
                    i++;
                }
            }

            push(kind, start, s.length);
            return segments;
        };

        const maskNonCode = (line, segments) => {
            const s = String(line || '');
            if (!segments?.length) return s;
            const arr = s.split('');
            for (const seg of segments) {
                if (seg.kind === 'code') continue;
                for (let i = seg.start; i < seg.end; i++) arr[i] = ' ';
            }
            return arr.join('');
        };

        const segmentKindAt = (segments, idx) => {
            if (!Array.isArray(segments)) return 'code';
            for (const seg of segments) {
                if (idx >= seg.start && idx < seg.end) return seg.kind;
            }
            return 'code';
        };

        const getModelRoutineNameUpper = (model) => {
            try {
                const uri = model?.uri;
                const p = uri?.path || uri?.fsPath || uri?.toString?.() || '';
                const base = String(p).split(/[\\/]/).pop() || '';
                const noExt = base.replace(/\.[^.]+$/g, '');
                return String(noExt || '').toUpperCase();
            } catch (_) {
                return '';
            }
        };

        const getLabelDefinition = (line) => {
            const s = String(line || '');
            if (!s || !isNameStart(s[0])) return null;
            let i = 0;
            while (i < s.length && isNameChar(s[i])) i++;
            if (i === 0) return null;
            return { name: s.slice(0, i), start: 0, end: i };
        };

        const skipParen = (lineMasked, idx) => {
            const s = String(lineMasked || '');
            const len = s.length;
            if (idx >= len || s[idx] !== '(') return idx;
            let depth = 0;
            let i = idx;
            while (i < len) {
                const ch = s[i];
                if (ch === '(') depth++;
                else if (ch === ')') {
                    depth--;
                    if (depth === 0) return i + 1;
                }
                i++;
            }
            return len;
        };

        const skipSpaces = (lineMasked, idx) => {
            const s = String(lineMasked || '');
            let i = idx;
            while (i < s.length && (s[i] === ' ' || s[i] === '\t')) i++;
            return i;
        };

        const parseEntryRefAt = (lineMasked, idx, routineNameUpper) => {
            const s = String(lineMasked || '');
            const len = s.length;
            let i = skipSpaces(s, idx);
            if (i >= len) return null;

            const ch = s[i];
            if (ch === '@') {
                const start = i;
                i++;
                i = skipSpaces(s, i);
                if (i < len && s[i] === '(') {
                    i = skipParen(s, i);
                } else {
                    while (i < len) {
                        const c = s[i];
                        if (c === ',' || c === ' ' || c === '\t') break;
                        i++;
                    }
                }
                return { nextIndex: i, computed: true, hadIndirection: true, hadComputedEntryRef: true, skippedFrom: start };
            }

            if (ch === '^') {
                i++;
                i = skipSpaces(s, i);
                const start = i;
                if (i < len && isNameStart(s[i])) {
                    i++;
                    while (i < len && isNameChar(s[i])) i++;
                    return { nextIndex: i, computed: false, hadIndirection: false, routineRange: { start, end: i } };
                }
                return { nextIndex: i, computed: false, hadIndirection: false };
            }

            if (!isNameStart(ch)) return null;

            const labelStart = i;
            i++;
            while (i < len && isNameChar(s[i])) i++;
            const labelEnd = i;
            const label = s.slice(labelStart, labelEnd);

            // Optional offset: +<expr> (skip until ^,(,),comma,whitespace)
            if (i < len && s[i] === '+') {
                i++;
                while (i < len) {
                    const c = s[i];
                    if (c === '^' || c === '(' || c === ',' || c === ' ' || c === '\t') break;
                    i++;
                }
            }

            let routine = null;
            let routineRange = null;
            if (i < len && s[i] === '^') {
                i++;
                i = skipSpaces(s, i);
                const rStart = i;
                if (i < len && isNameStart(s[i])) {
                    i++;
                    while (i < len && isNameChar(s[i])) i++;
                    routine = s.slice(rStart, i);
                    routineRange = { start: rStart, end: i };
                }
            }

            if (i < len && s[i] === '(') {
                i = skipParen(s, i);
            }

            const inThisRoutine = !routine || (routineNameUpper && String(routine).toUpperCase() === routineNameUpper);
            return {
                nextIndex: i,
                computed: false,
                hadIndirection: false,
                label,
                labelRange: { start: labelStart, end: labelEnd },
                routine,
                routineRange,
                inThisRoutine
            };
        };

        const parseEntryRefList = (lineMasked, idx, routineNameUpper) => {
            const s = String(lineMasked || '');
            const len = s.length;
            let i = idx;
            const refs = [];
            let hadIndirection = false;
            let hadComputedEntryRef = false;

            i = skipSpaces(s, i);
            while (i < len) {
                const start = i;
                const ch = s[i];
                if (!(ch === '@' || ch === '^' || isNameStart(ch))) break;
                const parsed = parseEntryRefAt(s, i, routineNameUpper);
                if (!parsed) break;
                if (parsed.hadIndirection) hadIndirection = true;
                if (parsed.hadComputedEntryRef) hadComputedEntryRef = true;
                if (parsed.labelRange) refs.push(parsed);
                i = skipSpaces(s, parsed.nextIndex);
                if (i < len && s[i] === ',') {
                    i++;
                    i = skipSpaces(s, i);
                    continue;
                }
                // Entryref list uses commas; stop once we hit non-comma.
                break;
            }

            return { refs, nextIndex: i, hadIndirection, hadComputedEntryRef };
        };

        const isBoundaryChar = (ch) => ch == null || !isNameChar(ch);

        const findWordMatchesInSegment = (line, seg, needle, { caseInsensitive = false } = {}) => {
            const s = String(line || '');
            const target = String(needle || '');
            if (!target) return [];

            const matches = [];
            const segStart = Math.max(0, Math.min(s.length, seg?.start ?? 0));
            const segEnd = Math.max(segStart, Math.min(s.length, seg?.end ?? s.length));
            if (segEnd <= segStart) return matches;

            const hay = caseInsensitive ? s.toUpperCase() : s;
            const ndl = caseInsensitive ? target.toUpperCase() : target;

            let idx = segStart;
            while (idx < segEnd) {
                const found = hay.indexOf(ndl, idx);
                if (found === -1) break;
                const end = found + target.length;
                if (end > segEnd) break;
                const prev = found > 0 ? s[found - 1] : null;
                const next = end < s.length ? s[end] : null;
                if (isBoundaryChar(prev) && isBoundaryChar(next)) {
                    matches.push({ start: found, end });
                }
                idx = Math.max(end, found + 1);
            }

            return matches;
        };

        const rangesOverlap = (a, b) => !!(a && b && a.start < b.end && b.start < a.end);
        const isInsideAny = (range, list) => Array.isArray(list) && list.some((r) => rangesOverlap(range, r));

        const scanLineForEntryRefs = (lineMasked, startIndex, routineNameUpper) => {
            const s = String(lineMasked || '');
            const len = s.length;
            let i = Math.max(0, startIndex || 0);
            const labelRefs = [];
            const unsafe = { hasXecute: false, hasIndirection: false, hasComputedEntryRef: false };

            const isBoundaryBefore = (pos) => pos <= 0 || s[pos - 1] === ' ' || s[pos - 1] === '\t' || s[pos - 1] === '.';
            const nextChar = (pos) => (pos < len ? s[pos] : '');
            const isCmdFollowChar = (ch) => ch === '' || ch === ' ' || ch === '\t' || ch === ':' || ch === '^' || ch === '@' || isNameStart(ch);

            while (i < len) {
                if (s[i] === '@') unsafe.hasIndirection = true;

                if (s[i] === '$' && i + 1 < len && s[i + 1] === '$') {
                    let j = i + 2;
                    j = skipSpaces(s, j);
                    const parsed = parseEntryRefAt(s, j, routineNameUpper);
                    if (parsed?.hadIndirection) unsafe.hasIndirection = true;
                    if (parsed?.hadComputedEntryRef) unsafe.hasComputedEntryRef = true;
                    if (parsed?.labelRange) {
                        labelRefs.push({ ...parsed, kind: 'extrinsic' });
                    }
                    i = parsed?.nextIndex ?? (i + 2);
                    continue;
                }

                if (!isBoundaryBefore(i)) {
                    i++;
                    continue;
                }

                const ch = s[i];
                const up2 = s.slice(i, i + 2).toUpperCase();
                const up4 = s.slice(i, i + 4).toUpperCase();
                const up6 = s.slice(i, i + 6).toUpperCase();

                // XECUTE / X (unsafe marker only)
                if (up6 === 'XECUTE') {
                    const after = nextChar(i + 6);
                    if (after === '' || after === ' ' || after === '\t' || after === ':') {
                        unsafe.hasXecute = true;
                    }
                } else if (ch === 'X' || ch === 'x') {
                    const after = nextChar(i + 1);
                    if (after === '' || after === ' ' || after === '\t' || after === ':') {
                        unsafe.hasXecute = true;
                    }
                }

                // DO / D
                let cmd = null;
                let cmdLen = 0;
                if (up2 === 'DO') {
                    const after = nextChar(i + 2);
                    if (isCmdFollowChar(after) && after !== '=') {
                        cmd = 'do';
                        cmdLen = 2;
                    }
                } else if (ch === 'D' || ch === 'd') {
                    const after = nextChar(i + 1);
                    if ((after === '' || after === ' ' || after === '\t' || after === ':' || after === '^' || after === '@' || isNameStart(after)) && after !== '=' && !isDigit(after)) {
                        cmd = 'do';
                        cmdLen = 1;
                    }
                }

                // GOTO / G
                if (!cmd && up4 === 'GOTO') {
                    const after = nextChar(i + 4);
                    if (isCmdFollowChar(after) && after !== '=') {
                        cmd = 'goto';
                        cmdLen = 4;
                    }
                } else if (!cmd && (ch === 'G' || ch === 'g')) {
                    const after = nextChar(i + 1);
                    if ((after === '' || after === ' ' || after === '\t' || after === ':' || after === '^' || after === '@' || isNameStart(after)) && after !== '=' && !isDigit(after)) {
                        cmd = 'goto';
                        cmdLen = 1;
                    }
                }

                if (!cmd) {
                    i++;
                    continue;
                }

                let j = i + cmdLen;
                if (j < len && s[j] === ':') {
                    j++;
                    while (j < len && s[j] !== ' ' && s[j] !== '\t') j++;
                }
                j = skipSpaces(s, j);
                const parsedList = parseEntryRefList(s, j, routineNameUpper);
                if (parsedList.hadIndirection) unsafe.hasIndirection = true;
                if (parsedList.hadComputedEntryRef) unsafe.hasComputedEntryRef = true;
                for (const ref of parsedList.refs) {
                    labelRefs.push({ ...ref, kind: cmd });
                }
                i = parsedList.nextIndex;
            }

            return { labelRefs, unsafe };
        };

        const resolveTargetAtPosition = (model, position) => {
            const word = model?.getWordAtPosition?.(position);
            if (!word?.word) return { kind: null, reason: 'No symbol found.' };
            const oldName = word.word;
            const line = model.getLineContent(position.lineNumber);
            const segments = segmentMumpsLine(line);
            const idx = Math.max(0, Math.min(line.length, position.column - 1));
            const kindAt = segmentKindAt(segments, idx);
            if (kindAt !== 'code') {
                return { kind: null, reason: 'Rename is available only in code (not comments/strings).' };
            }

            const range = {
                startLineNumber: position.lineNumber,
                endLineNumber: position.lineNumber,
                startColumn: word.startColumn,
                endColumn: word.endColumn
            };

            // Reject globals/intrinsics
            const before = word.startColumn > 1 ? line[word.startColumn - 2] : '';
            if (before === '^') return { kind: null, reason: 'Rename for globals/routines is not supported (safe mode).' };
            if (oldName.startsWith('$') || before === '$') return { kind: null, reason: 'Rename for intrinsic names is not supported.' };

            const routineNameUpper = getModelRoutineNameUpper(model);
            const labelDef = getLabelDefinition(line);
            const isLabelDef = labelDef && labelDef.name && range.startColumn === 1 && labelDef.end === word.endColumn - 1 && String(labelDef.name) === String(oldName);

            if (isLabelDef && isValidLabelName(oldName)) {
                return { kind: 'label', oldName, range };
            }

            const masked = maskNonCode(line, segments);
            const commandStartIndex = (() => {
                if (!labelDef) return 0;
                let i = labelDef.end;
                if (i < masked.length && masked[i] === '(') i = skipParen(masked, i);
                return i;
            })();
            const scan = scanLineForEntryRefs(masked, commandStartIndex, routineNameUpper);
            const wordRange0 = { start: word.startColumn - 1, end: word.endColumn - 1 };
            const isEntryLabel = scan.labelRefs.some((r) => rangesOverlap(wordRange0, r.labelRange));

            if (isEntryLabel && isValidLabelName(oldName)) {
                return { kind: 'label', oldName, range };
            }

            if (!isValidLocalName(oldName)) {
                return { kind: null, reason: 'Unsupported symbol for rename (safe mode).' };
            }

            return { kind: 'variable', oldName, range };
        };

        const computeRename = (model, { kind, oldName, newName, includeStringsComments = false } = {}) => {
            const monacoRef = getMonaco();
            if (!monacoRef || !model) return { edits: [], unsafeReasons: [], stringCommentMatches: 0 };

            const routineNameUpper = getModelRoutineNameUpper(model);
            const uri = model.uri;
            const edits = [];
            let stringCommentMatches = 0;

            const unsafeFlagsAgg = { hasXecute: false, hasIndirection: false, hasComputedEntryRef: false };

            for (let lineNumber = 1; lineNumber <= model.getLineCount(); lineNumber++) {
                const line = model.getLineContent(lineNumber);
                const segments = segmentMumpsLine(line);
                const masked = maskNonCode(line, segments);

                const labelDef = getLabelDefinition(line);
                let commandStartIndex = 0;
                if (labelDef) {
                    commandStartIndex = labelDef.end;
                    if (commandStartIndex < masked.length && masked[commandStartIndex] === '(') {
                        commandStartIndex = skipParen(masked, commandStartIndex);
                    }
                }

                const { labelRefs, unsafe } = scanLineForEntryRefs(masked, commandStartIndex, routineNameUpper);
                unsafeFlagsAgg.hasXecute = unsafeFlagsAgg.hasXecute || unsafe.hasXecute;
                unsafeFlagsAgg.hasIndirection = unsafeFlagsAgg.hasIndirection || unsafe.hasIndirection;
                unsafeFlagsAgg.hasComputedEntryRef = unsafeFlagsAgg.hasComputedEntryRef || unsafe.hasComputedEntryRef;

                if (kind === 'label') {
                    const oldUpper = String(oldName).toUpperCase();

                    if (labelDef && labelDef.name && String(labelDef.name).toUpperCase() === oldUpper) {
                        edits.push({
                            resource: uri,
                            textEdit: {
                                range: new monacoRef.Range(lineNumber, 1, lineNumber, 1 + labelDef.end),
                                text: String(newName)
                            },
                            versionId: model.getVersionId()
                        });
                    }

                    for (const ref of labelRefs) {
                        if (!ref?.labelRange) continue;
                        if (!ref.inThisRoutine) continue;
                        if (String(ref.label || '').toUpperCase() !== oldUpper) continue;
                        edits.push({
                            resource: uri,
                            textEdit: {
                                range: new monacoRef.Range(lineNumber, ref.labelRange.start + 1, lineNumber, ref.labelRange.end + 1),
                                text: String(newName)
                            },
                            versionId: model.getVersionId()
                        });
                    }

                    if (includeStringsComments) {
                        const otherSegs = segments.filter((seg) => seg.kind !== 'code');
                        for (const seg of otherSegs) {
                            const matches = findWordMatchesInSegment(line, seg, oldName, { caseInsensitive: true });
                            for (const m of matches) {
                                edits.push({
                                    resource: uri,
                                    textEdit: {
                                        range: new monacoRef.Range(lineNumber, m.start + 1, lineNumber, m.end + 1),
                                        text: String(newName)
                                    },
                                    versionId: model.getVersionId()
                                });
                            }
                        }
                    } else {
                        const otherSegs = segments.filter((seg) => seg.kind !== 'code');
                        for (const seg of otherSegs) {
                            stringCommentMatches += findWordMatchesInSegment(line, seg, oldName, { caseInsensitive: true }).length;
                        }
                    }
                } else if (kind === 'variable') {
                    const excluded = [];
                    if (labelDef?.name) excluded.push({ start: labelDef.start, end: labelDef.end });
                    for (const ref of labelRefs) {
                        if (ref?.labelRange) excluded.push(ref.labelRange);
                    }

                    const targetSegs = includeStringsComments ? segments : segments.filter((seg) => seg.kind === 'code');
                    for (const seg of targetSegs) {
                        const matches = findWordMatchesInSegment(line, seg, oldName, { caseInsensitive: false });
                        for (const m of matches) {
                            const prev = m.start > 0 ? line[m.start - 1] : '';
                            if (prev === '^' || prev === '$') continue;
                            if (seg.kind === 'code' && isInsideAny(m, excluded)) continue;
                            edits.push({
                                resource: uri,
                                textEdit: {
                                    range: new monacoRef.Range(lineNumber, m.start + 1, lineNumber, m.end + 1),
                                    text: String(newName)
                                },
                                versionId: model.getVersionId()
                            });
                        }
                    }

                    if (!includeStringsComments) {
                        const otherSegs = segments.filter((seg) => seg.kind !== 'code');
                        for (const seg of otherSegs) {
                            stringCommentMatches += findWordMatchesInSegment(line, seg, oldName, { caseInsensitive: false }).length;
                        }
                    }
                }
            }

            // De-dup edits (same range can be hit twice via labelDef+scan etc)
            const keyFor = (e) => {
                const r = e?.textEdit?.range;
                return `${e.resource?.toString?.() || ''}:${r?.startLineNumber}:${r?.startColumn}:${r?.endLineNumber}:${r?.endColumn}:${e.textEdit?.text || ''}`;
            };
            const uniq = new Map();
            for (const e of edits) uniq.set(keyFor(e), e);
            const uniqEdits = Array.from(uniq.values()).sort((a, b) => {
                const ra = a.textEdit.range;
                const rb = b.textEdit.range;
                if (ra.startLineNumber !== rb.startLineNumber) return ra.startLineNumber - rb.startLineNumber;
                return ra.startColumn - rb.startColumn;
            });

            const unsafeReasons = [];
            if (unsafeFlagsAgg.hasXecute) unsafeReasons.push('XECUTE detected (dynamic code execution).');
            if (unsafeFlagsAgg.hasComputedEntryRef) unsafeReasons.push('Computed entryrefs detected (e.g., DO @X / $$@X).');
            else if (unsafeFlagsAgg.hasIndirection) unsafeReasons.push('Indirection (@) detected (dynamic symbol access).');

            return { edits: uniqEdits, unsafeReasons, stringCommentMatches };
        };

        let renameRegistered = false;
        function registerMumpsRenameProvider() {
            if (renameRegistered) return;
            renameRegistered = true;

            const monacoRef = getMonaco();
            if (!monacoRef?.languages?.registerRenameProvider) return;

            monacoRef.languages.registerRenameProvider('mumps', {
                resolveRenameLocation: (model, position, token) => {
                    const res = resolveTargetAtPosition(model, position);
                    if (!res.kind) {
                        return {
                            range: new monacoRef.Range(position.lineNumber, position.column, position.lineNumber, position.column),
                            text: '',
                            rejectReason: res.reason || 'Rename is not available here.'
                        };
                    }
                    return {
                        range: res.range,
                        text: res.oldName
                    };
                },

                provideRenameEdits: async (model, position, newNameRaw, token) => {
                    const res = resolveTargetAtPosition(model, position);
                    if (!res.kind) return { edits: [], rejectReason: res.reason || 'Rename is not available here.' };

                    const oldName = res.oldName;
                    const newName = String(newNameRaw || '').trim();

                    if (!newName) return { edits: [], rejectReason: 'New name cannot be empty.' };
                    if (res.kind === 'variable' && !isValidLocalName(newName)) {
                        return { edits: [], rejectReason: 'Invalid local variable name.' };
                    }
                    if (res.kind === 'label' && !isValidLabelName(newName)) {
                        return { edits: [], rejectReason: 'Invalid label name.' };
                    }
                    if (res.kind === 'label' && String(oldName).toUpperCase() === String(newName).toUpperCase()) {
                        return { edits: [], rejectReason: 'New label name is the same.' };
                    }
                    if (res.kind === 'variable' && String(oldName) === String(newName)) {
                        return { edits: [], rejectReason: 'New variable name is the same.' };
                    }

                    const previewFactory = getPreviewDialogFactory();
                    const openPreview = typeof previewFactory === 'function'
                        ? previewFactory({ deps: { getMonaco } })?.open
                        : null;

                    const base = computeRename(model, { kind: res.kind, oldName, newName, includeStringsComments: false });
                    if (!base.edits.length) return { edits: [], rejectReason: 'No references found in this file.' };

                    const wantsUnsafeToggle = Array.isArray(base.unsafeReasons) && base.unsafeReasons.length > 0;
                    const wantsStringsToggle = (base.stringCommentMatches || 0) > 0;

                    if (!openPreview) {
                        // Fallback: apply without preview (should not happen when UI module is loaded).
                        if (wantsUnsafeToggle) {
                            return { edits: [], rejectReason: base.unsafeReasons.join(' ') };
                        }
                        return { edits: base.edits };
                    }

                    const userChoice = await openPreview({
                        title: 'Rename Preview',
                        kind: res.kind,
                        oldName,
                        newName,
                        fileLabel: model?.uri?.path || model?.uri?.toString?.() || 'Current file',
                        model,
                        baseEdits: base.edits,
                        compute: (opts) => computeRename(model, { kind: res.kind, oldName, newName, includeStringsComments: !!opts?.includeStringsComments }),
                        unsafeReasons: base.unsafeReasons,
                        stringCommentMatches: base.stringCommentMatches
                    }, token);

                    if (!userChoice?.apply) return { edits: [] };
                    if (wantsUnsafeToggle && !userChoice.allowUnsafe) return { edits: [] };

                    const finalRes = computeRename(model, {
                        kind: res.kind,
                        oldName,
                        newName,
                        includeStringsComments: !!userChoice.includeStringsComments
                    });

                    return { edits: finalRes.edits };
                }
            });
        }

        return {
            registerMumpsRenameProvider
        };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.mumpsMonaco = window.AhmadIDEModules.mumpsMonaco || {};
        window.AhmadIDEModules.mumpsMonaco.createMumpsRenameProvider = createMumpsRenameProvider;
    }

    if (typeof module !== 'undefined' && module.exports) {
        module.exports = { createMumpsRenameProvider };
    }
})();
