(() => {
    const STATUS = Object.freeze({
        ADDED: 'added',
        REMOVED: 'removed',
        CHANGED: 'changed',
        UNCHANGED: 'unchanged'
    });

    const safeText = (v) => (v && typeof v === 'object' && typeof v.text === 'string') ? v.text : String(v ?? '');

    const scalarText = (entry) => {
        if (!entry) return '';
        if (entry.kind === 'scalar') return safeText(entry.value);
        return '';
    };

    const arrayElements = (entry) => {
        if (!entry || entry.kind !== 'array') return null;
        const el = entry.elements && typeof entry.elements === 'object' ? entry.elements : {};
        return el;
    };

    const compareScalar = (a, b) => safeText(a) === safeText(b);

    const diffArrayElements = (beforeEntry, afterEntry) => {
        const before = arrayElements(beforeEntry) || {};
        const after = arrayElements(afterEntry) || {};
        const keys = new Set([...Object.keys(before), ...Object.keys(after)]);
        const sorted = Array.from(keys).sort((x, y) => String(x).localeCompare(String(y)));

        const children = [];
        let changedCount = 0;

        for (const k of sorted) {
            const bv = before[k];
            const av = after[k];
            let status = STATUS.UNCHANGED;
            if (bv == null && av != null) status = STATUS.ADDED;
            else if (bv != null && av == null) status = STATUS.REMOVED;
            else if (!compareScalar(bv, av)) status = STATUS.CHANGED;
            if (status !== STATUS.UNCHANGED) changedCount += 1;
            children.push({
                type: 'local-element',
                key: k,
                name: String(k),
                status,
                before: bv || null,
                after: av || null
            });
        }

        return { children, changedCount, total: sorted.length };
    };

    const diffLocals = (beforeLocals, afterLocals) => {
        const b = (beforeLocals && typeof beforeLocals === 'object') ? beforeLocals : {};
        const a = (afterLocals && typeof afterLocals === 'object') ? afterLocals : {};
        const keys = new Set([...Object.keys(b), ...Object.keys(a)]);
        // Filter out globals (starting with ^) - they'll be handled separately
        const localKeys = Array.from(keys).filter(k => !String(k).startsWith('^'));
        const sorted = localKeys.sort((x, y) => String(x).localeCompare(String(y)));

        const rows = [];
        let changed = 0;

        for (const name of sorted) {
            const bv = b[name];
            const av = a[name];
            let status = STATUS.UNCHANGED;

            if (bv == null && av != null) status = STATUS.ADDED;
            else if (bv != null && av == null) status = STATUS.REMOVED;
            else if ((bv?.kind || '') !== (av?.kind || '')) status = STATUS.CHANGED;
            else if (bv?.kind === 'scalar' && av?.kind === 'scalar') {
                status = compareScalar(bv.value, av.value) ? STATUS.UNCHANGED : STATUS.CHANGED;
            } else if (bv?.kind === 'array' && av?.kind === 'array') {
                const res = diffArrayElements(bv, av);
                status = res.changedCount ? STATUS.CHANGED : STATUS.UNCHANGED;
                rows.push({
                    type: 'local',
                    kind: 'array',
                    name,
                    status,
                    before: bv,
                    after: av,
                    children: res.children,
                    meta: {
                        beforeTruncated: !!bv?.truncated,
                        afterTruncated: !!av?.truncated,
                        beforeTotal: bv?.totalElements ?? null,
                        afterTotal: av?.totalElements ?? null
                    }
                });
                if (status !== STATUS.UNCHANGED) changed += 1;
                continue;
            } else {
                status = STATUS.CHANGED;
            }

            rows.push({
                type: 'local',
                kind: bv?.kind || av?.kind || 'scalar',
                name,
                status,
                before: bv || null,
                after: av || null,
                beforeText: scalarText(bv),
                afterText: scalarText(av)
            });
            if (status !== STATUS.UNCHANGED) changed += 1;
        }

        return { rows, changedCount: changed, total: rows.length };
    };

    const diffWatchedGlobals = (before, after) => {
        const b = (before && typeof before === 'object') ? before : {};
        const a = (after && typeof after === 'object') ? after : {};
        const keys = new Set([...Object.keys(b), ...Object.keys(a)]);
        const sorted = Array.from(keys).sort((x, y) => String(x).localeCompare(String(y)));

        const rows = [];
        let changed = 0;

        for (const expr of sorted) {
            const bv = b[expr];
            const av = a[expr];
            let status = STATUS.UNCHANGED;
            if (bv == null && av != null) status = STATUS.ADDED;
            else if (bv != null && av == null) status = STATUS.REMOVED;
            else {
                const bText = safeText(bv?.value);
                const aText = safeText(av?.value);
                const bErr = String(bv?.error || '');
                const aErr = String(av?.error || '');
                status = (bText === aText && bErr === aErr) ? STATUS.UNCHANGED : STATUS.CHANGED;
            }
            if (status !== STATUS.UNCHANGED) changed += 1;
            rows.push({
                type: 'global',
                expr,
                status,
                before: bv || null,
                after: av || null
            });
        }

        return { rows, changedCount: changed, total: rows.length };
    };

    const diffCapturedGlobals = (beforeLocals, afterLocals) => {
        const b = (beforeLocals && typeof beforeLocals === 'object') ? beforeLocals : {};
        const a = (afterLocals && typeof afterLocals === 'object') ? afterLocals : {};
        const keys = new Set([...Object.keys(b), ...Object.keys(a)]);
        // Only process globals (starting with ^)
        const globalKeys = Array.from(keys).filter(k => String(k).startsWith('^'));
        const sorted = globalKeys.sort((x, y) => String(x).localeCompare(String(y)));

        const rows = [];
        let changed = 0;

        for (const expr of sorted) {
            const bv = b[expr];
            const av = a[expr];
            let status = STATUS.UNCHANGED;

            if (bv == null && av != null) status = STATUS.ADDED;
            else if (bv != null && av == null) status = STATUS.REMOVED;
            else if ((bv?.kind || '') !== (av?.kind || '')) status = STATUS.CHANGED;
            else if (bv?.kind === 'scalar' && av?.kind === 'scalar') {
                status = compareScalar(bv.value, av.value) ? STATUS.UNCHANGED : STATUS.CHANGED;
            } else {
                status = STATUS.CHANGED;
            }

            if (status !== STATUS.UNCHANGED) changed += 1;
            rows.push({
                type: 'global',
                expr,
                status,
                before: bv || null,
                after: av || null
            });
        }

        return { rows, changedCount: changed, total: rows.length };
    };

    const diffSnapshots = (beforeSnap, afterSnap) => {
        const before = beforeSnap || null;
        const after = afterSnap || null;
        const locals = diffLocals(before?.locals, after?.locals);
        const capturedGlobals = diffCapturedGlobals(before?.locals, after?.locals);
        const watchedGlobals = diffWatchedGlobals(before?.watchedGlobals, after?.watchedGlobals);

        // Merge captured globals with watched globals
        const allGlobalRows = [...capturedGlobals.rows, ...watchedGlobals.rows];
        const globals = {
            rows: allGlobalRows,
            changedCount: capturedGlobals.changedCount + watchedGlobals.changedCount,
            total: allGlobalRows.length
        };

        return {
            status: STATUS,
            before,
            after,
            locals,
            globals
        };
    };

    function createDebugTimelineDiffService() {
        return { STATUS, diffSnapshots };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.services = window.AhmadIDEModules.services || {};
        if (!window.AhmadIDEModules.services.debugTimelineDiffService) {
            window.AhmadIDEModules.services.debugTimelineDiffService = createDebugTimelineDiffService();
        }
    }

    if (typeof module !== 'undefined' && module.exports) {
        module.exports = { createDebugTimelineDiffService };
    }
})();

