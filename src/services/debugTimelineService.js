(() => {
    const createNoopLogger = () => ({
        debug: () => { },
        info: () => { },
        warn: () => { },
        error: () => { }
    });

    const clampInt = (n, min, max) => {
        const v = Number.parseInt(String(n), 10);
        if (!Number.isFinite(v)) return min;
        return Math.max(min, Math.min(max, v));
    };

    const DEFAULT_CONFIG = Object.freeze({
        enable: false,
        maxSnapshots: 50,
        captureOnStop: true,
        captureGlobals: false,
        maxValueLength: 200,
        maxLocals: 800,
        maxArrayElements: 250,
        maxStackFrames: 30,
        maxWatchedGlobals: 12
    });

    const scheduleWork = (fn) => {
        if (typeof fn !== 'function') return;
        try {
            if (typeof requestIdleCallback === 'function') {
                requestIdleCallback(() => {
                    try { fn(); } catch (_) { }
                }, { timeout: 150 });
                return;
            }
        } catch (_) { }
        setTimeout(() => {
            try { fn(); } catch (_) { }
        }, 0);
    };

    const truncateText = (value, maxLen) => {
        const text = String(value ?? '');
        const limit = clampInt(maxLen, 20, 100000);
        if (text.length <= limit) return { text, truncated: false, originalLength: text.length };
        return { text: text.slice(0, limit) + '…', truncated: true, originalLength: text.length };
    };

    const sanitizeScalar = (raw, config) => truncateText(raw, config.maxValueLength);

    const sanitizeArray = (raw, config) => {
        const elements = (raw && typeof raw === 'object' && raw._isArray && raw._elements && typeof raw._elements === 'object')
            ? raw._elements
            : null;
        if (!elements) {
            return { kind: 'array', elements: {}, truncated: false, totalElements: 0 };
        }

        const keys = Object.keys(elements);
        keys.sort((a, b) => String(a).localeCompare(String(b)));
        const total = keys.length;
        const limit = clampInt(config.maxArrayElements, 0, 100000);
        const take = limit > 0 ? keys.slice(0, limit) : keys;
        const out = {};
        for (const k of take) out[k] = truncateText(elements[k], config.maxValueLength);
        return {
            kind: 'array',
            elements: out,
            truncated: limit > 0 && total > limit,
            totalElements: total
        };
    };

    const sanitizeLocals = (locals, config) => {
        const src = (locals && typeof locals === 'object') ? locals : {};
        const keys = Object.keys(src);
        keys.sort((a, b) => String(a).localeCompare(String(b)));

        const maxLocals = clampInt(config.maxLocals, 0, 50000);
        const take = maxLocals > 0 ? keys.slice(0, maxLocals) : keys;
        const out = {};

        for (const name of take) {
            const v = src[name];
            if (v && typeof v === 'object' && v._isArray) {
                out[name] = sanitizeArray(v, config);
            } else {
                out[name] = { kind: 'scalar', value: sanitizeScalar(v, config) };
            }
        }

        return {
            locals: out,
            meta: {
                totalLocals: keys.length,
                truncated: maxLocals > 0 && keys.length > maxLocals
            }
        };
    };

    const sanitizeStack = (stack, config) => {
        const src = Array.isArray(stack) ? stack : [];
        const limit = clampInt(config.maxStackFrames, 0, 2000);
        const frames = limit > 0 ? src.slice(0, limit) : src.slice();
        return {
            frames: frames.map((f) => String(f ?? '')),
            meta: { totalFrames: src.length, truncated: limit > 0 && src.length > limit }
        };
    };

    const safeNowMs = () => {
        try { return Date.now(); } catch (_) { return 0; }
    };

    const normalizeGlobalName = (name) => String(name || '').toUpperCase();

    const tokenizeSubscripts = (raw) => {
        const s = String(raw || '');
        const out = [];
        let i = 0;
        let token = '';
        let inString = false;

        while (i < s.length) {
            const ch = s[i];
            if (inString) {
                token += ch;
                if (ch === '"') {
                    if (i + 1 < s.length && s[i + 1] === '"') {
                        token += '"';
                        i += 2;
                        continue;
                    }
                    inString = false;
                }
                i++;
                continue;
            }

            if (ch === '"') {
                inString = true;
                token += ch;
                i++;
                continue;
            }

            if (ch === ',') {
                out.push(token.trim());
                token = '';
                i++;
                continue;
            }

            token += ch;
            i++;
        }
        if (token.trim() || s.endsWith(',')) out.push(token.trim());
        return out;
    };

    const isNumericLiteral = (t) => /^-?\d+(?:\.\d+)?$/.test(String(t || '').trim());
    const isQuotedStringLiteral = (t) => {
        const s = String(t || '').trim();
        if (s.length < 2) return false;
        if (!s.startsWith('"') || !s.endsWith('"')) return false;
        // Best-effort: allow doubled quotes inside.
        const inner = s.slice(1, -1);
        return !/"/.test(inner.replace(/""/g, ''));
    };

    const validateWatchedGlobalExpr = (expr) => {
        const raw = String(expr || '').trim();
        if (!raw) return { ok: false, error: 'Empty global reference' };
        if (raw.length > 256) return { ok: false, error: 'Global reference too long' };
        if (/\s/.test(raw)) return { ok: false, error: 'Whitespace not allowed' };
        if (raw.includes(';')) return { ok: false, error: 'Comments not allowed' };
        if (raw.includes('@')) return { ok: false, error: 'Indirection not allowed' };
        if (raw.includes('$')) return { ok: false, error: 'Functions not allowed' };

        const m = raw.match(/^\^([A-Za-z%][A-Za-z0-9%]*)(?:\((.*)\))?$/);
        if (!m) return { ok: false, error: 'Invalid global syntax' };

        const globalName = normalizeGlobalName(m[1]);
        const subs = m[2];
        if (subs == null) return { ok: true, normalized: `^${globalName}` };

        const parts = tokenizeSubscripts(subs);
        if (!parts.length) return { ok: false, error: 'Empty subscript list' };
        for (const p of parts) {
            if (!p) return { ok: false, error: 'Empty subscript' };
            if (!(isNumericLiteral(p) || isQuotedStringLiteral(p))) {
                return { ok: false, error: 'Only numeric or quoted string subscripts are supported' };
            }
        }
        return { ok: true, normalized: `^${globalName}(${parts.join(',')})` };
    };

    function createDebugTimelineService({ deps } = {}) {
        const logger = deps?.logger || createNoopLogger();
        const settingsService = deps?.settingsService || window.AhmadIDEModules?.services?.settingsService || null;
        const debugEval = deps?.debugEval || ((sessionId, code) => window.ahmadIDE?.debugEval?.(sessionId, code));

        const listeners = new Set();
        const emit = (evt) => {
            for (const cb of listeners) {
                try { cb(evt); } catch (_) { }
            }
        };

        const config = { ...DEFAULT_CONFIG };
        let activeSessionId = null;

        const applySettings = (settings) => {
            const tl = settings?.debugger?.timeline || {};
            config.enable = !!tl.enable;
            config.captureOnStop = tl.captureOnStop !== false;
            config.captureGlobals = !!tl.captureGlobals;
            config.maxSnapshots = clampInt(tl.maxSnapshots ?? DEFAULT_CONFIG.maxSnapshots, 1, 500);
            config.maxValueLength = clampInt(tl.maxValueLength ?? DEFAULT_CONFIG.maxValueLength, 20, 20000);
            emit({ type: 'config', config: { ...config } });
        };

        try {
            if (settingsService?.get) applySettings(settingsService.get());
            settingsService?.subscribe?.((s) => applySettings(s));
        } catch (_) { }

        const sessions = new Map(); // sessionId -> { id, active, seq, snapshots, watchedGlobals }

        const ensureSession = (sessionId) => {
            const id = String(sessionId || '').trim();
            if (!id) return null;
            let s = sessions.get(id);
            if (!s) {
                s = { id, active: true, seq: 0, snapshots: [], watchedGlobals: [] };
                sessions.set(id, s);
            }
            activeSessionId = id;
            return s;
        };

        const enforceLimit = (session) => {
            const max = clampInt(config.maxSnapshots, 1, 500);
            if (session.snapshots.length <= max) return;
            const drop = session.snapshots.length - max;
            session.snapshots.splice(0, drop);
        };

        const readWatchedGlobals = async (sessionId, watched, cfg) => {
            const out = {};
            if (!cfg.captureGlobals) return { values: out, errors: [] };
            if (!debugEval || typeof debugEval !== 'function') return { values: out, errors: ['debugEval unavailable'] };

            const limit = clampInt(cfg.maxWatchedGlobals, 0, 200);
            const take = limit > 0 ? watched.slice(0, limit) : watched.slice();
            const errors = [];

            for (const expr of take) {
                const safe = validateWatchedGlobalExpr(expr);
                const key = safe.ok ? safe.normalized : String(expr || '').trim();
                if (!safe.ok) {
                    out[key] = { ok: false, error: safe.error, value: truncateText('', cfg.maxValueLength) };
                    continue;
                }
                const code = `WRITE $GET(${safe.normalized}),!`;
                try {
                    const res = await debugEval(sessionId, code);
                    if (res?.ok) {
                        const rawOut = String(res?.output ?? '').replace(/\r\n/g, '\n');
                        const firstLine = rawOut.split('\n')[0] ?? '';
                        out[key] = { ok: true, value: truncateText(firstLine, cfg.maxValueLength) };
                    } else {
                        const msg = String(res?.error || 'Eval failed');
                        out[key] = { ok: false, error: msg, value: truncateText('', cfg.maxValueLength) };
                        errors.push(`${key}: ${msg}`);
                    }
                } catch (err) {
                    const msg = String(err?.message || err || 'Eval error');
                    out[key] = { ok: false, error: msg, value: truncateText('', cfg.maxValueLength) };
                    errors.push(`${key}: ${msg}`);
                }
            }

            return { values: out, errors };
        };

        const buildSnapshot = async ({ sessionId, reason, location, stack, locals }) => {
            const ts = safeNowMs();
            const loc = {
                routine: String(location?.routine || ''),
                tag: String(location?.tag || ''),
                line: Number(location?.line || 0) || null
            };

            const localsSan = sanitizeLocals(locals, config);
            const stackSan = sanitizeStack(stack, config);
            const watched = ensureSession(sessionId)?.watchedGlobals || [];
            const globals = await readWatchedGlobals(sessionId, watched, config);

            return {
                id: '',
                seq: 0,
                ts,
                reason: String(reason || 'Stop'),
                location: loc,
                stack: stackSan.frames,
                stackMeta: stackSan.meta,
                locals: localsSan.locals,
                localsMeta: localsSan.meta,
                watchedGlobals: globals.values,
                watchedGlobalsErrors: globals.errors
            };
        };

        const capture = (payload, { source }) => {
            const sessionId = String(payload?.sessionId || '').trim();
            const session = ensureSession(sessionId);
            if (!session) return { ok: false, error: 'No sessionId' };
            if (!config.enable) return { ok: false, skipped: true, reason: 'disabled' };

            const reason = String(payload?.reason || (source === 'manual' ? 'Manual' : 'Stop'));
            const location = payload?.location || {};
            const stack = payload?.stack || [];
            const locals = payload?.locals || {};

            scheduleWork(() => {
                (async () => {
                    const snap = await buildSnapshot({ sessionId, reason, location, stack, locals });
                    session.seq += 1;
                    snap.seq = session.seq;
                    snap.id = `${sessionId}:${snap.seq}`;
                    session.snapshots.push(snap);
                    enforceLimit(session);
                    emit({ type: 'snapshot-added', sessionId, snapshot: snap, source: source || 'auto' });
                })().catch((err) => {
                    const msg = String(err?.message || err || 'Snapshot failed');
                    logger.warn('DEBUG_TIMELINE_SNAPSHOT_FAIL', { sessionId, message: msg });
                    emit({ type: 'snapshot-error', sessionId, error: msg });
                });
            });

            return { ok: true, queued: true };
        };

        const startSession = (sessionId) => {
            const s = ensureSession(sessionId);
            if (!s) return { ok: false, error: 'No sessionId' };
            s.active = true;
            activeSessionId = s.id;
            emit({ type: 'session-start', sessionId: s.id });
            return { ok: true };
        };

        const endSession = (sessionId) => {
            const id = String(sessionId || '').trim();
            const s = sessions.get(id);
            if (!s) return { ok: true, missing: true };
            s.active = false;
            if (activeSessionId === id) activeSessionId = null;
            emit({ type: 'session-end', sessionId: id });
            return { ok: true };
        };

        const clearSession = (sessionId) => {
            const id = String(sessionId || '').trim();
            const s = sessions.get(id);
            if (!s) return { ok: false, error: 'Session not found' };
            s.snapshots = [];
            s.seq = 0;
            emit({ type: 'session-cleared', sessionId: id });
            return { ok: true };
        };

        const setWatchedGlobals = (sessionId, list) => {
            const session = ensureSession(sessionId);
            if (!session) return { ok: false, error: 'No sessionId' };
            const next = Array.isArray(list) ? list : [];
            const cleaned = [];
            const invalid = [];
            for (const item of next) {
                const raw = String(item || '').trim();
                if (!raw) continue;
                const v = validateWatchedGlobalExpr(raw);
                if (v.ok) cleaned.push(v.normalized);
                else invalid.push({ input: raw, error: v.error });
            }
            session.watchedGlobals = cleaned.slice(0, clampInt(config.maxWatchedGlobals, 0, 200));
            emit({ type: 'watched-globals-changed', sessionId: session.id, watched: session.watchedGlobals.slice() });
            return { ok: true, watched: session.watchedGlobals.slice(), invalid };
        };

        const getWatchedGlobals = (sessionId) => {
            const id = String(sessionId || '').trim();
            return sessions.get(id)?.watchedGlobals?.slice() || [];
        };

        const getSnapshots = (sessionId) => {
            const id = String(sessionId || '').trim();
            const s = sessions.get(id);
            return s ? s.snapshots.slice() : [];
        };

        const getConfig = () => ({ ...config });

        const getActiveSessionId = () => activeSessionId;

        const exportSession = (sessionId) => {
            const id = String(sessionId || '').trim();
            const s = sessions.get(id);
            if (!s) return { ok: false, error: 'Session not found' };
            return {
                ok: true,
                payload: {
                    version: 1,
                    sessionId: id,
                    exportedAt: new Date().toISOString(),
                    config: { ...config },
                    watchedGlobals: s.watchedGlobals.slice(),
                    snapshots: s.snapshots.slice()
                }
            };
        };

        const subscribe = (cb) => {
            if (typeof cb !== 'function') return () => { };
            listeners.add(cb);
            return () => listeners.delete(cb);
        };

        return {
            DEFAULT_CONFIG,
            validateWatchedGlobalExpr,
            getConfig,
            getActiveSessionId,
            subscribe,
            startSession,
            endSession,
            clearSession,
            captureOnStop: (payload) => (config.enable && config.captureOnStop) ? capture(payload, { source: 'auto' }) : { ok: false, skipped: true },
            captureManual: (payload) => capture(payload, { source: 'manual' }),
            setWatchedGlobals,
            getWatchedGlobals,
            getSnapshots,
            exportSession
        };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.services = window.AhmadIDEModules.services || {};
        if (!window.AhmadIDEModules.services.debugTimelineService) {
            window.AhmadIDEModules.services.debugTimelineService = createDebugTimelineService();
        }
    }

    if (typeof module !== 'undefined' && module.exports) {
        module.exports = { createDebugTimelineService };
    }
})();
