(() => {
    function createGlobalReferenceIndexService({ deps } = {}) {
        const safeRequire = (id) => {
            try {
                const req = deps?.require || (typeof require === 'function' ? require : null);
                if (typeof req !== 'function') return null;
                // In the renderer we often have Monaco/AMD `require` which throws for Node built-ins like `fs`.
                // Only treat `require` as Node-style if it exposes `require.resolve`.
                if (typeof req.resolve !== 'function') return null;
                return req(id);
            } catch (_) {
                return null;
            }
        };

        const fs = deps?.fs || safeRequire('fs');
        const pathMod = deps?.path || safeRequire('path');
        const workspaceApi =
            deps?.workspaceApi || (typeof window !== 'undefined' ? window.ahmadIDE : null);

        const canUseNodeFs =
            !!(fs && pathMod && fs.promises && typeof fs.promises.readFile === 'function');
        const canUseWorkspaceApi =
            !!(workspaceApi && typeof workspaceApi.listFiles === 'function' && typeof workspaceApi.readFile === 'function');

        const ACCESS = Object.freeze({
            READ: 'READ',
            WRITE: 'WRITE',
            KILL: 'KILL',
            MERGE: 'MERGE',
            UNKNOWN: 'UNKNOWN'
        });

        const COMMAND_CANON = Object.freeze({
            S: 'SET',
            SET: 'SET',
            K: 'KILL',
            KILL: 'KILL',
            M: 'MERGE',
            MERGE: 'MERGE',
            W: 'WRITE',
            WRITE: 'WRITE',
            ZW: 'ZWRITE',
            ZWRITE: 'ZWRITE'
        });

        const READ_FUNCS = new Set([
            '$G', '$GET',
            '$D', '$DATA',
            '$O', '$ORDER',
            '$Q', '$QUERY',
            '$NA', '$NAME',
            '$N', '$NEXT'
        ]);

        const WRITE_FUNCS = new Set([
            '$I', '$INCREMENT'
        ]);

        const listeners = new Set();
        const emit = (evt) => {
            listeners.forEach((cb) => {
                try { cb(evt); } catch (_) { }
            });
        };

        const normalizeSlashes = (p) => String(p || '').replace(/\\/g, '/');
        const isMumpsFile = (file) => /\.m$/i.test(String(file || ''));

        const isAlpha = (ch) => (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || ch === '%';
        const isDigit = (ch) => ch >= '0' && ch <= '9';
        const isNameStart = (ch) => isAlpha(ch);
        const isNameChar = (ch) => isAlpha(ch) || isDigit(ch);

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

                // string
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

        const rangesOverlap = (a, b) => !!(a && b && a.start < b.end && b.start < a.end);

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

        const getLabelDefinition = (line) => {
            const s = String(line || '');
            if (!s || !isNameStart(s[0])) return null;
            let i = 0;
            while (i < s.length && isNameChar(s[i])) i++;
            if (i === 0) return null;
            return { name: s.slice(0, i), start: 0, end: i };
        };

        const parseEntryRefAt = (lineMasked, idx) => {
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
                return { nextIndex: i, computed: true, hadIndirection: true, skippedFrom: start };
            }

            if (ch === '^') {
                const caret = i;
                i++;
                i = skipSpaces(s, i);
                const start = i;
                if (i < len && isNameStart(s[i])) {
                    i++;
                    while (i < len && isNameChar(s[i])) i++;
                    return { nextIndex: i, computed: false, hadIndirection: false, routineRange: { start: caret, end: i }, routineNameRange: { start, end: i } };
                }
                return { nextIndex: i, computed: false, hadIndirection: false };
            }

            if (!isNameStart(ch)) return null;

            const labelStart = i;
            i++;
            while (i < len && isNameChar(s[i])) i++;
            const labelEnd = i;

            // Optional offset +expr
            if (i < len && s[i] === '+') {
                i++;
                while (i < len) {
                    const c = s[i];
                    if (c === '^' || c === '(' || c === ',' || c === ' ' || c === '\t') break;
                    i++;
                }
            }

            let routineRange = null;
            let routineNameRange = null;
            if (i < len && s[i] === '^') {
                const caret = i;
                i++;
                i = skipSpaces(s, i);
                const rStart = i;
                if (i < len && isNameStart(s[i])) {
                    i++;
                    while (i < len && isNameChar(s[i])) i++;
                    routineRange = { start: caret, end: i };
                    routineNameRange = { start: rStart, end: i };
                }
            }

            if (i < len && s[i] === '(') {
                i = skipParen(s, i);
            }

            return {
                nextIndex: i,
                computed: false,
                hadIndirection: false,
                labelRange: { start: labelStart, end: labelEnd },
                routineRange,
                routineNameRange
            };
        };

        const parseEntryRefList = (lineMasked, idx) => {
            const s = String(lineMasked || '');
            const len = s.length;
            let i = skipSpaces(s, idx);
            const refs = [];

            while (i < len) {
                const ch = s[i];
                if (!(ch === '@' || ch === '^' || isNameStart(ch))) break;
                const parsed = parseEntryRefAt(s, i);
                if (!parsed) break;
                refs.push(parsed);
                i = skipSpaces(s, parsed.nextIndex);
                if (i < len && s[i] === ',') {
                    i++;
                    i = skipSpaces(s, i);
                    continue;
                }
                break;
            }

            return { refs, nextIndex: i };
        };

        const collectRoutineCaretRanges = (lineMasked) => {
            const s = String(lineMasked || '');
            const len = s.length;
            const ranges = [];

            const add = (r) => {
                if (!r || !Number.isFinite(r.start) || !Number.isFinite(r.end) || r.end <= r.start) return;
                ranges.push({ start: r.start, end: r.end });
            };

            const isBoundaryBefore = (pos) => pos <= 0 || s[pos - 1] === ' ' || s[pos - 1] === '\t' || s[pos - 1] === '.';
            const nextChar = (pos) => (pos < len ? s[pos] : '');
            const isCmdFollowChar = (ch) => ch === '' || ch === ' ' || ch === '\t' || ch === ':' || ch === '^' || ch === '@' || isNameStart(ch);

            let i = 0;
            while (i < len) {
                if (s[i] === '$' && i + 1 < len && s[i + 1] === '$') {
                    let j = i + 2;
                    j = skipSpaces(s, j);
                    const parsed = parseEntryRefAt(s, j);
                    if (parsed?.routineRange) add(parsed.routineRange);
                    i = parsed?.nextIndex ?? (i + 2);
                    continue;
                }

                if (!isBoundaryBefore(i)) {
                    i++;
                    continue;
                }

                let cmd = null;
                let cmdLen = 0;
                const ch = s[i];
                const up2 = s.slice(i, i + 2).toUpperCase();
                const up4 = s.slice(i, i + 4).toUpperCase();

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
                } else if (up4 === 'GOTO') {
                    const after = nextChar(i + 4);
                    if (isCmdFollowChar(after) && after !== '=') {
                        cmd = 'goto';
                        cmdLen = 4;
                    }
                } else if (ch === 'G' || ch === 'g') {
                    const after = nextChar(i + 1);
                    if ((after === '' || after === ' ' || after === '\t' || after === ':' || after === '^' || after === '@' || isNameStart(after)) && after !== '=' && !isDigit(after)) {
                        cmd = 'goto';
                        cmdLen = 1;
                    }
                } else if (up3(s, i) === 'JOB') {
                    const after = nextChar(i + 3);
                    if (isCmdFollowChar(after) && after !== '=') {
                        cmd = 'job';
                        cmdLen = 3;
                    }
                } else if (ch === 'J' || ch === 'j') {
                    const after = nextChar(i + 1);
                    if ((after === '' || after === ' ' || after === '\t' || after === ':' || after === '^' || after === '@' || isNameStart(after)) && after !== '=' && !isDigit(after)) {
                        cmd = 'job';
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

                const parsedList = parseEntryRefList(s, j);
                for (const ref of parsedList.refs) {
                    if (ref?.routineRange) add(ref.routineRange);
                }
                i = parsedList.nextIndex;
            }

            // Exclude $TEXT/$T entryrefs
            const up = s.toUpperCase();
            const textMarkers = ['$TEXT(', '$T('];
            for (const marker of textMarkers) {
                let at = 0;
                while (at < len) {
                    const pos = up.indexOf(marker, at);
                    if (pos === -1) break;
                    const openParen = pos + marker.length - 1;
                    const close = (() => {
                        let depth = 0;
                        for (let k = openParen; k < len; k++) {
                            const c = s[k];
                            if (c === '(') depth++;
                            else if (c === ')') {
                                depth--;
                                if (depth === 0) return k;
                            }
                        }
                        return -1;
                    })();
                    const end = close === -1 ? len : close;
                    const sub = s.slice(openParen, end + 1);
                    const re = /\^([A-Za-z%][A-Za-z0-9]*)/g;
                    let m;
                    while ((m = re.exec(sub))) {
                        add({ start: openParen + m.index, end: openParen + m.index + m[0].length });
                    }
                    at = end + 1;
                }
            }

            // Merge overlapping ranges (small list)
            ranges.sort((a, b) => a.start - b.start);
            const merged = [];
            for (const r of ranges) {
                const last = merged[merged.length - 1];
                if (!last || r.start > last.end) merged.push({ ...r });
                else last.end = Math.max(last.end, r.end);
            }
            return merged;
        };

        function up3(s, i) {
            return String(s || '').slice(i, i + 3).toUpperCase();
        }

        const findEnclosingFunction = (lineMasked, idx) => {
            const s = String(lineMasked || '');
            let depth = 0;
            for (let i = Math.max(0, idx - 1); i >= 0; i--) {
                const ch = s[i];
                if (ch === ')') {
                    depth++;
                    continue;
                }
                if (ch === '(') {
                    if (depth > 0) {
                        depth--;
                        continue;
                    }
                    let j = i - 1;
                    while (j >= 0 && (s[j] === ' ' || s[j] === '\t')) j--;
                    if (j < 0 || s[j] !== '$') return null;
                    let k = j;
                    while (k >= 0 && (s[k] === '$' || isNameChar(s[k]))) k--;
                    const name = s.slice(k + 1, i).toUpperCase();
                    return name || null;
                }
            }
            return null;
        };

        const commandBeforeIndex = (lineMasked, idx) => {
            const s = String(lineMasked || '');
            const len = s.length;
            let i = 0;

            // Skip leading label and optional formal list.
            const labelDef = getLabelDefinition(s);
            if (labelDef) {
                i = labelDef.end;
                if (i < len && s[i] === '(') i = skipParen(s, i);
            }

            // Skip leading indentation dots/spaces.
            i = skipSpaces(s, i);
            while (i < len && s[i] === '.') i++;
            i = skipSpaces(s, i);

            let lastCmd = null;
            while (i < Math.min(len, idx)) {
                const ch = s[i];
                if (isNameStart(ch)) {
                    const start = i;
                    i++;
                    while (i < len && isNameChar(s[i])) i++;
                    const raw = s.slice(start, i);
                    const up = raw.toUpperCase();
                    const canon = COMMAND_CANON[up] || null;
                    if (canon) lastCmd = canon;
                    continue;
                }
                i++;
            }
            return lastCmd;
        };

        const inferAccessType = (lineMasked, matchRange) => {
            const s = String(lineMasked || '');
            const start = matchRange?.start ?? 0;
            const end = matchRange?.end ?? start;

            const func = findEnclosingFunction(s, start);
            if (func) {
                if (READ_FUNCS.has(func)) return ACCESS.READ;
                if (WRITE_FUNCS.has(func)) return ACCESS.WRITE;
            }

            const cmd = commandBeforeIndex(s, start);
            if (cmd === 'KILL') return ACCESS.KILL;
            if (cmd === 'MERGE') return ACCESS.MERGE;
            if (cmd === 'WRITE' || cmd === 'ZWRITE') return ACCESS.READ;

            if (cmd === 'SET') {
                let i = end;
                if (i < s.length && s[i] === '(') i = skipParen(s, i);
                i = skipSpaces(s, i);
                if (i < s.length && s[i] === '=') return ACCESS.WRITE;
                return ACCESS.READ;
            }

            return ACCESS.UNKNOWN;
        };

        const scanText = (text, { fileAbs = '', projectRoot = '' } = {}) => {
            const norm = String(text ?? '').replace(/\r\n/g, '\n').replace(/\r/g, '\n');
            const lines = norm.split('\n');
            const refs = [];
            const globalsUpper = new Set();
            const globalRegex = /\^([A-Za-z%][A-Za-z0-9]*)/g;

            const relPath = (() => {
                const abs = normalizeSlashes(fileAbs);
                const root = normalizeSlashes(projectRoot).replace(/\/+$/, '');
                if (!abs) return '';
                if (!root) return abs;

                try {
                    if (pathMod?.relative) {
                        const rel = pathMod.relative(projectRoot, fileAbs);
                        return normalizeSlashes(rel);
                    }
                } catch (_) { }

                const absCmp = abs.toLowerCase();
                const rootCmp = root.toLowerCase();
                if (absCmp === rootCmp) return '';
                if (absCmp.startsWith(rootCmp + '/')) return abs.slice(root.length + 1);
                return abs;
            })();

            const routineKey = (() => {
                const rel = String(relPath || '').replace(/^\/+/, '');
                return rel.replace(/\.m$/i, '');
            })();

            for (let li = 0; li < lines.length; li++) {
                const lineNumber = li + 1;
                const original = lines[li] ?? '';
                if (!original) continue;

                const segments = segmentMumpsLine(original);
                const masked = maskNonCode(original, segments);
                const routineCaretRanges = collectRoutineCaretRanges(masked);

                globalRegex.lastIndex = 0;
                let m;
                while ((m = globalRegex.exec(masked))) {
                    const name = m[1] || '';
                    const global = '^' + name;
                    const start = m.index;
                    const end = start + m[0].length;

                    if (routineCaretRanges.some((r) => rangesOverlap(r, { start, end }))) continue;

                    const access = inferAccessType(masked, { start, end });
                    const snippet = String(original || '').trimEnd();

                    refs.push({
                        global,
                        globalUpper: global.toUpperCase(),
                        fileAbs,
                        fileRel: relPath,
                        routineKey,
                        line: lineNumber,
                        range: { startColumn: start + 1, endColumn: end + 1 },
                        snippet,
                        access
                    });
                    globalsUpper.add(global.toUpperCase());
                }
            }

            return { refs, globalsUpper, routineKey, fileRel: relPath };
        };

        const state = {
            projectRoot: '',
            indexing: false,
            lastIndexStartedAt: 0,
            byGlobal: new Map(), // globalUpper -> { byFile: Map<fileAbs, refs[]>, total }
            byFile: new Map(), // fileAbs -> { mtimeMs, refs, globalsUpper:Set, fileRel, routineKey }
            watchers: [],
            watchDirs: new Set(),
            watchId: null,
            watchUnsub: null,
            pendingRescan: new Map(), // fileAbs -> timeoutId
            cancelToken: { id: 0, canceled: false }
        };

        const clearWatchers = () => {
            state.watchers.forEach((w) => {
                try { w?.close?.(); } catch (_) { }
            });
            state.watchers = [];
            state.watchDirs.clear();

            if (state.watchUnsub) {
                try { state.watchUnsub(); } catch (_) { }
                state.watchUnsub = null;
            }

            const prevWatchId = state.watchId;
            state.watchId = null;
            if (prevWatchId && typeof workspaceApi?.unwatchFiles === 'function') {
                try { workspaceApi.unwatchFiles(prevWatchId); } catch (_) { }
            }
        };

        const removeFileFromIndex = (fileAbs) => {
            const existing = state.byFile.get(fileAbs);
            if (!existing) return;
            state.byFile.delete(fileAbs);

            (existing.globalsUpper || new Set()).forEach((g) => {
                const entry = state.byGlobal.get(g);
                if (!entry) return;
                const list = entry.byFile.get(fileAbs) || [];
                entry.byFile.delete(fileAbs);
                entry.total = Math.max(0, (entry.total || 0) - list.length);
                if (entry.byFile.size === 0) state.byGlobal.delete(g);
            });
        };

        const upsertFileIndex = (fileAbs, payload) => {
            removeFileFromIndex(fileAbs);
            if (!payload) return;

            state.byFile.set(fileAbs, payload);
            (payload.globalsUpper || new Set()).forEach((g) => {
                const entry = state.byGlobal.get(g) || { byFile: new Map(), total: 0 };
                const refs = payload.refs.filter((r) => r.globalUpper === g);
                entry.byFile.set(fileAbs, refs);
                entry.total += refs.length;
                state.byGlobal.set(g, entry);
            });
        };

        const walkDirsForMumpsFiles = async (roots, token) => {
            const rootList = Array.isArray(roots) ? roots.filter(Boolean) : [];

            if (!canUseNodeFs) {
                if (!canUseWorkspaceApi) return { files: [], dirs: rootList };
                const filesSet = new Set();
                for (const r of rootList) {
                    if (token?.canceled) break;
                    let files = [];
                    try {
                        files = await workspaceApi.listFiles(r, '*.m');
                    } catch (_) {
                        files = [];
                    }
                    (Array.isArray(files) ? files : []).forEach((p) => {
                        const fp = String(p || '').trim();
                        if (fp) filesSet.add(fp);
                    });
                }
                return { files: Array.from(filesSet), dirs: rootList };
            }

            const outFiles = [];
            const outDirs = [];
            const ignoreDirNames = new Set(['.git', 'node_modules', 'dist', 'build', 'out', '.yarn', '.cache']);

            const walk = async (dir) => {
                if (token?.canceled) return;
                let entries = [];
                try {
                    entries = await fs.promises.readdir(dir, { withFileTypes: true });
                } catch (_) {
                    return;
                }
                outDirs.push(dir);
                for (const ent of entries) {
                    if (token?.canceled) return;
                    const name = ent?.name || '';
                    if (!name) continue;
                    const full = pathMod.join(dir, name);
                    if (ent.isDirectory()) {
                        if (ignoreDirNames.has(name)) continue;
                        await walk(full);
                        continue;
                    }
                    if (ent.isFile() && isMumpsFile(name)) {
                        outFiles.push(full);
                    }
                }
            };

            for (const r of rootList) await walk(r);

            return { files: outFiles, dirs: outDirs };
        };

        const scanFileIfNeeded = async (fileAbs, token) => {
            if (!fileAbs || token?.canceled) return null;

            if (!canUseNodeFs) {
                if (!canUseWorkspaceApi) return null;
                let res = null;
                try {
                    res = await workspaceApi.readFile(fileAbs, { maxBytes: 10 * 1024 * 1024 });
                } catch (_) {
                    res = null;
                }

                if (!res?.ok) {
                    removeFileFromIndex(fileAbs);
                    emit({ type: 'file-removed', fileAbs });
                    return null;
                }

                const mtimeMs = Number(res.mtime ?? res.mtimeMs ?? 0) || 0;
                const existing = state.byFile.get(fileAbs);
                if (existing && existing.mtimeMs === mtimeMs) return existing;

                const text = String(res.content ?? '');
                const scanned = scanText(text, { fileAbs, projectRoot: state.projectRoot });
                const payload = {
                    mtimeMs,
                    refs: scanned.refs,
                    globalsUpper: scanned.globalsUpper,
                    fileRel: scanned.fileRel,
                    routineKey: scanned.routineKey
                };
                upsertFileIndex(fileAbs, payload);
                emit({ type: 'file-updated', fileAbs, globals: Array.from(scanned.globalsUpper), refs: scanned.refs.length });
                return payload;
            }

            let stat;
            try {
                stat = await fs.promises.stat(fileAbs);
            } catch (_) {
                removeFileFromIndex(fileAbs);
                emit({ type: 'file-removed', fileAbs });
                return null;
            }

            if (!stat.isFile()) return null;
            const mtimeMs = Number(stat.mtimeMs || 0) || 0;
            const existing = state.byFile.get(fileAbs);
            if (existing && existing.mtimeMs === mtimeMs) return existing;

            let text = '';
            try {
                text = await fs.promises.readFile(fileAbs, 'utf8');
            } catch (_) {
                return null;
            }

            const scanned = scanText(text, { fileAbs, projectRoot: state.projectRoot });
            const payload = {
                mtimeMs,
                refs: scanned.refs,
                globalsUpper: scanned.globalsUpper,
                fileRel: scanned.fileRel,
                routineKey: scanned.routineKey
            };
            upsertFileIndex(fileAbs, payload);
            emit({ type: 'file-updated', fileAbs, globals: Array.from(scanned.globalsUpper), refs: scanned.refs.length });
            return payload;
        };

        const mapConcurrency = async (items, limit, worker, token) => {
            const list = Array.isArray(items) ? items : [];
            const cap = Math.max(1, Math.min(64, Number(limit) || 16));
            let idx = 0;
            let active = 0;
            let done = 0;

            return new Promise((resolve) => {
                const next = () => {
                    if (token?.canceled) return resolve({ done, canceled: true });
                    while (active < cap && idx < list.length) {
                        const item = list[idx++];
                        active++;
                        Promise.resolve()
                            .then(() => worker(item, token))
                            .catch(() => { })
                            .finally(() => {
                                active--;
                                done++;
                                if (done % 50 === 0) {
                                    try { emit({ type: 'progress', done, total: list.length }); } catch (_) { }
                                }
                                if (done >= list.length) {
                                    resolve({ done, canceled: false });
                                } else {
                                    next();
                                }
                            });
                    }
                };
                next();
            });
        };

        const ensureWatching = async (dirs, token) => {
            if (token?.canceled) return;

            if (canUseNodeFs && fs && typeof fs.watch === 'function') {
                const list = Array.isArray(dirs) ? dirs : [];
                list.forEach((d) => state.watchDirs.add(d));

                const watchDir = (dir) => {
                    if (!dir || state.watchDirs.has(`__watched__:${dir}`)) return;
                    state.watchDirs.add(`__watched__:${dir}`);
                    try {
                        const watcher = fs.watch(dir, { persistent: false }, (eventType, filename) => {
                            try {
                                const name = String(filename || '').trim();
                                if (!name) return;
                                if (!isMumpsFile(name)) return;
                                const full = pathMod.join(dir, name);
                                scheduleRescan(full);
                            } catch (_) { }
                        });
                        state.watchers.push(watcher);
                    } catch (_) { }
                };

                list.forEach((d) => watchDir(d));
                return;
            }

            // Renderer-safe watcher via preload IPC (best-effort).
            if (!canUseWorkspaceApi) return;
            if (state.watchUnsub == null && typeof workspaceApi.onWorkspaceFilesChanged === 'function') {
                state.watchUnsub = workspaceApi.onWorkspaceFilesChanged((evt) => {
                    try {
                        const watchId = String(evt?.watchId || '');
                        if (!watchId || watchId !== String(state.watchId || '')) return;
                        const p = String(evt?.path || '').trim();
                        if (p && isMumpsFile(p)) scheduleRescan(p);
                    } catch (_) { }
                });
            }
            if (!state.watchId && typeof workspaceApi.watchFiles === 'function' && state.projectRoot) {
                try {
                    const res = await workspaceApi.watchFiles(state.projectRoot, '*.m', { maxDepth: 60 });
                    if (res?.ok && res.watchId) state.watchId = String(res.watchId);
                } catch (_) { }
            }
        };

        const scheduleRescan = (fileAbs) => {
            if (!fileAbs) return;
            const existing = state.pendingRescan.get(fileAbs);
            if (existing) clearTimeout(existing);
            const t = setTimeout(() => {
                state.pendingRescan.delete(fileAbs);
                scanFileIfNeeded(fileAbs, state.cancelToken).catch(() => { });
            }, 180);
            state.pendingRescan.set(fileAbs, t);
        };

        const getDefaultScanRoots = (projectRoot) => {
            const root = String(projectRoot || '').trim();
            if (!root) return [];
            if (!canUseNodeFs || !pathMod?.join || !fs?.existsSync) return [root];
            const localr = pathMod.join(root, 'localr');
            const routines = pathMod.join(root, 'routines');
            const roots = [];
            try { if (fs.existsSync(localr) && fs.statSync(localr).isDirectory()) roots.push(localr); } catch (_) { }
            try { if (fs.existsSync(routines) && fs.statSync(routines).isDirectory()) roots.push(routines); } catch (_) { }
            return roots.length ? roots : [root];
        };

        const startIndexing = async ({ projectRoot, roots = null, force = false } = {}) => {
            const root = String(projectRoot || '').trim();
            if (!root) return { ok: false, error: 'No project root' };
            if (!canUseNodeFs && !canUseWorkspaceApi) {
                return { ok: false, error: 'Workspace file access is unavailable (no fs / no IPC file API)' };
            }

            if (!force && state.indexing && state.projectRoot === root) {
                return { ok: true, alreadyIndexing: true };
            }

            state.cancelToken.canceled = true;
            state.cancelToken = { id: state.cancelToken.id + 1, canceled: false };
            const token = state.cancelToken;

            state.indexing = true;
            state.projectRoot = root;
            state.lastIndexStartedAt = Date.now();

            clearWatchers();

            emit({ type: 'index-start', projectRoot: root });

            const scanRoots = Array.isArray(roots) && roots.length ? roots : getDefaultScanRoots(root);
            const { files, dirs } = await walkDirsForMumpsFiles(scanRoots, token);
            if (token.canceled) {
                state.indexing = false;
                emit({ type: 'index-canceled', projectRoot: root });
                return { ok: false, canceled: true };
            }

            // Watch directories discovered during walk for incremental updates.
            await ensureWatching(dirs, token);

            emit({ type: 'index-files', count: files.length });

            const started = performance.now ? performance.now() : Date.now();
            await mapConcurrency(files, 24, scanFileIfNeeded, token);
            const ended = performance.now ? performance.now() : Date.now();

            if (token.canceled) {
                state.indexing = false;
                emit({ type: 'index-canceled', projectRoot: root });
                return { ok: false, canceled: true };
            }

            state.indexing = false;
            emit({ type: 'index-done', projectRoot: root, ms: Math.round(ended - started), files: files.length });
            return { ok: true, files: files.length, ms: Math.round(ended - started) };
        };

        const isReady = () => !!state.projectRoot;
        const isIndexing = () => !!state.indexing;

        const normalizeGlobalInput = (q) => {
            const raw = String(q || '').trim();
            if (!raw) return '';
            const withCaret = raw.startsWith('^') ? raw : '^' + raw;
            const m = withCaret.match(/^\^([A-Za-z%][A-Za-z0-9]*)/);
            return m ? ('^' + m[1]).toUpperCase() : '';
        };

        const listGlobals = ({ prefix = '', limit = 200 } = {}) => {
            const pfx = normalizeGlobalInput(prefix);
            const out = [];
            state.byGlobal.forEach((entry, g) => {
                if (pfx && !g.startsWith(pfx)) return;
                out.push({ globalUpper: g, global: g, count: entry.total || 0 });
            });
            out.sort((a, b) => (b.count - a.count) || a.global.localeCompare(b.global));
            return out.slice(0, Math.max(1, Math.min(2000, Number(limit) || 200)));
        };

        const connectedCache = new Map(); // globalUpper -> { byFile: Map<fileKey, refs[]>, cachedAt, hitCount }

        const buildQueryFromByFile = (globalUpper, byFile, access) => {
            const want = Array.isArray(access) && access.length
                ? new Set(access.map((x) => String(x || '').toUpperCase()).filter(Boolean))
                : null;

            const files = [];
            (byFile || new Map()).forEach((refs, fileKey) => {
                const list = Array.isArray(refs) ? refs : [];
                const filtered = want ? list.filter((r) => want.has(String(r.access || '').toUpperCase())) : list;
                if (!filtered.length) return;
                const first = filtered[0] || list[0] || {};
                files.push({
                    fileAbs: String(fileKey || first.fileAbs || first.fileRel || ''),
                    fileRel: String(first.fileRel || fileKey || ''),
                    routineKey: String(first.routineKey || '').trim(),
                    count: filtered.length,
                    refs: filtered
                });
            });

            files.sort((a, b) => (b.count - a.count) || String(a.fileRel || '').localeCompare(String(b.fileRel || '')));
            const total = files.reduce((sum, f) => sum + (f.count || 0), 0);
            return { globalUpper, total, files };
        };

        const queryConnected = async (globalUpper, { access = null, forceRefresh = false } = {}) => {
            const g = normalizeGlobalInput(globalUpper);
            if (!g) return { globalUpper: '', total: 0, files: [] };

            if (!canUseWorkspaceApi || typeof workspaceApi.searchRoutines !== 'function') {
                const err = 'No project open and environment search is unavailable';
                return { globalUpper: g, total: 0, files: [], error: err };
            }

            if (!forceRefresh) {
                const cached = connectedCache.get(g);
                if (cached?.byFile) return buildQueryFromByFile(g, cached.byFile, access);
            }

            let res;
            try {
                res = await workspaceApi.searchRoutines(g, { regex: false, matchCase: false, wholeWords: false });
            } catch (err) {
                const msg = String(err?.message || err || 'Search failed').trim();
                return { globalUpper: g, total: 0, files: [], error: msg };
            }

            if (!res?.ok) {
                const msg = String(res?.error || res?.stderr || 'Search failed').trim();
                return { globalUpper: g, total: 0, files: [], error: msg };
            }

            const hits = Array.isArray(res.hits) ? res.hits : [];
            const byFile = new Map();

            for (const hit of hits) {
                const fileKey = String(hit?.file || '').trim();
                if (!fileKey) continue;
                const snippet = String(hit?.snippet ?? '');
                if (!snippet) continue;

                const lineNumber = Number(hit?.line || 0) || 1;
                const scanned = scanText(snippet, { fileAbs: fileKey, projectRoot: '' });
                const refs = (scanned?.refs || []).filter((r) => r && r.globalUpper === g);
                if (!refs.length) continue;

                const list = byFile.get(fileKey) || [];
                refs.forEach((r) => {
                    r.fileAbs = fileKey;
                    r.fileRel = fileKey;
                    r.line = lineNumber;
                    list.push(r);
                });
                byFile.set(fileKey, list);
            }

            byFile.forEach((list, key) => {
                const arr = Array.isArray(list) ? list : [];
                arr.sort((a, b) => (Number(a.line || 0) - Number(b.line || 0)) || (Number(a.range?.startColumn || 0) - Number(b.range?.startColumn || 0)));
                byFile.set(key, arr);
            });

            connectedCache.set(g, { byFile, cachedAt: Date.now(), hitCount: hits.length });
            return buildQueryFromByFile(g, byFile, access);
        };

        const query = (globalName, { access = null } = {}) => {
            const g = normalizeGlobalInput(globalName);
            if (!g) return { globalUpper: '', total: 0, files: [] };

            const entry = state.byGlobal.get(g);
            if (!entry) return { globalUpper: g, total: 0, files: [] };

            const want = Array.isArray(access) && access.length
                ? new Set(access.map((x) => String(x || '').toUpperCase()).filter(Boolean))
                : null;

            const files = [];
            entry.byFile.forEach((refs, fileAbs) => {
                const fileInfo = state.byFile.get(fileAbs);
                const filtered = want ? refs.filter((r) => want.has(String(r.access || '').toUpperCase())) : refs;
                if (!filtered.length) return;
                files.push({
                    fileAbs,
                    fileRel: fileInfo?.fileRel || normalizeSlashes(fileAbs),
                    routineKey: fileInfo?.routineKey || '',
                    count: filtered.length,
                    refs: filtered
                });
            });

            files.sort((a, b) => (b.count - a.count) || a.fileRel.localeCompare(b.fileRel));
            const total = files.reduce((sum, f) => sum + (f.count || 0), 0);
            return { globalUpper: g, total, files };
        };

        const queryAsync = async (globalName, { access = null, forceRefresh = false } = {}) => {
            const g = normalizeGlobalInput(globalName);
            if (!g) return { globalUpper: '', total: 0, files: [] };

            // Prefer local workspace index if present.
            if (state.projectRoot) return query(g, { access });

            // No local project: run connected environment query (Docker/SSH routines search).
            return queryConnected(g, { access, forceRefresh });
        };

        const subscribe = (cb) => {
            if (typeof cb !== 'function') return () => { };
            listeners.add(cb);
            return () => listeners.delete(cb);
        };

        const dispose = () => {
            state.cancelToken.canceled = true;
            state.pendingRescan.forEach((t) => clearTimeout(t));
            state.pendingRescan.clear();
            clearWatchers();
        };

        return {
            ACCESS,
            startIndexing,
            isReady,
            isIndexing,
            query,
            queryAsync,
            listGlobals,
            subscribe,
            dispose
        };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.services = window.AhmadIDEModules.services || {};
        if (!window.AhmadIDEModules.services.globalReferenceIndexService) {
            window.AhmadIDEModules.services.globalReferenceIndexService = createGlobalReferenceIndexService();
        }
    }

    if (typeof module !== 'undefined' && module.exports) {
        module.exports = { createGlobalReferenceIndexService };
    }
})();
