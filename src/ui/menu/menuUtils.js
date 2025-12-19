(() => {
    const isSeparator = (it) => !!it && (it.type === 'separator' || it.separator === true);

    const isDisabled = (it, ctx) => {
        if (!it) return true;
        const d = it.disabled;
        if (typeof d === 'function') return !!d(ctx);
        return !!d;
    };

    const isChecked = (it, ctx) => {
        if (!it) return false;
        const c = it.checked;
        if (typeof c === 'function') return !!c(ctx);
        return !!c;
    };

    const hasSubmenu = (it, ctx) => {
        if (!it) return false;
        const sub = it.submenu;
        const resolved = typeof sub === 'function' ? sub(ctx) : sub;
        return Array.isArray(resolved) && resolved.length > 0;
    };

    const resolveSubmenu = (it, ctx) => {
        const sub = it?.submenu;
        const resolved = typeof sub === 'function' ? sub(ctx) : sub;
        return Array.isArray(resolved) ? resolved : [];
    };

    const resolveLabel = (it) => String(it?.label || '').trim();
    const resolveShortcut = (it) => String(it?.shortcut || '').trim();
    const resolveIcon = (it) => String(it?.icon || '').trim();

    const nextSelectableIndex = (items, start, dir, ctx) => {
        if (!Array.isArray(items) || !items.length) return -1;
        const len = items.length;
        let idx = start;
        for (let i = 0; i < len; i += 1) {
            idx = (idx + dir + len) % len;
            const it = items[idx];
            if (isSeparator(it)) continue;
            if (isDisabled(it, ctx)) continue;
            return idx;
        }
        return -1;
    };

    const firstSelectableIndex = (items, ctx) => nextSelectableIndex(items, -1, 1, ctx);
    const lastSelectableIndex = (items, ctx) => nextSelectableIndex(items, 0, -1, ctx);

    const typeaheadMatchIndex = (items, fromIndex, query, ctx) => {
        const q = String(query || '').trim().toLowerCase();
        if (!q) return -1;
        const len = Array.isArray(items) ? items.length : 0;
        if (!len) return -1;
        for (let i = 0; i < len; i += 1) {
            const idx = (fromIndex + 1 + i) % len;
            const it = items[idx];
            if (!it || isSeparator(it) || isDisabled(it, ctx)) continue;
            const label = resolveLabel(it).toLowerCase();
            if (label.startsWith(q)) return idx;
        }
        return -1;
    };

    const clamp01 = (n) => Math.max(0, Math.min(1, n));

    const pointInPoly = (x, y, poly) => {
        // Ray casting algorithm
        let inside = false;
        for (let i = 0, j = poly.length - 1; i < poly.length; j = i++) {
            const xi = poly[i][0], yi = poly[i][1];
            const xj = poly[j][0], yj = poly[j][1];
            const intersect = ((yi > y) !== (yj > y)) && (x < ((xj - xi) * (y - yi)) / (yj - yi + 1e-9) + xi);
            if (intersect) inside = !inside;
        }
        return inside;
    };

    const makeGracePolygon = (parentItemRect, submenuRect, gracePx) => {
        const g = Math.max(0, Number(gracePx || 0) || 0);
        const pLeft = parentItemRect.left;
        const pRight = parentItemRect.right;
        const pTop = parentItemRect.top;
        const pBottom = parentItemRect.bottom;

        const sLeft = submenuRect.left;
        const sRight = submenuRect.right;
        const sTop = submenuRect.top;
        const sBottom = submenuRect.bottom;

        const opensRight = sLeft >= pRight;
        if (opensRight) {
            const a = [pRight - 1, pTop - g];
            const b = [sLeft + 1, sTop - g];
            const c = [sLeft + 1, sBottom + g];
            const d = [pRight - 1, pBottom + g];
            return [a, b, c, d];
        }

        const opensLeft = sRight <= pLeft;
        if (opensLeft) {
            const a = [pLeft + 1, pTop - g];
            const b = [sRight - 1, sTop - g];
            const c = [sRight - 1, sBottom + g];
            const d = [pLeft + 1, pBottom + g];
            return [a, b, c, d];
        }

        // Fallback: treat as no grace zone if overlapping.
        return [[0, 0]];
    };

    const isPointInGraceZone = (x, y, parentItemRect, submenuRect, gracePx) => {
        if (!parentItemRect || !submenuRect) return false;
        const poly = makeGracePolygon(parentItemRect, submenuRect, gracePx);
        return pointInPoly(x, y, poly);
    };

    const rectContains = (rect, x, y, pad = 0) => {
        const p = Number(pad || 0) || 0;
        return x >= rect.left - p && x <= rect.right + p && y >= rect.top - p && y <= rect.bottom + p;
    };

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.ui = window.AhmadIDEModules.ui || {};
        window.AhmadIDEModules.ui.menu = window.AhmadIDEModules.ui.menu || {};
        window.AhmadIDEModules.ui.menu.utils = {
            isSeparator,
            isDisabled,
            isChecked,
            hasSubmenu,
            resolveSubmenu,
            resolveLabel,
            resolveShortcut,
            resolveIcon,
            nextSelectableIndex,
            firstSelectableIndex,
            lastSelectableIndex,
            typeaheadMatchIndex,
            clamp01,
            rectContains,
            isPointInGraceZone
        };
    }
})();
