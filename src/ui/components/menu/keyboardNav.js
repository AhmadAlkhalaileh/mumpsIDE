(() => {
    const isCharKey = (e) => {
        const key = e?.key || '';
        if (!key || key.length !== 1) return false;
        if (e.ctrlKey || e.metaKey || e.altKey) return false;
        return true;
    };

    const createKeyHandler = (api = {}) => {
        const top = api.top || (() => null);
        const getStack = api.getStack || (() => []);
        const utils = api.utils;
        const setActiveIndex = api.setActiveIndex || (() => { });
        const openSubmenu = api.openSubmenu || (() => { });
        const closeFrom = api.closeFrom || (() => { });
        const closeAll = api.closeAll || (() => { });
        const getActiveItem = api.getActiveItem || (() => null);
        const onItemActivate = api.onItemActivate || (() => { });
        const focusActiveRow = api.focusActiveRow || (() => { });

        if (!utils) throw new Error('keyboardNav requires menu utils');

        return (e) => {
            const m = top();
            if (!m) return;

            const key = e.key;
            const stack = getStack();
            const isChar = isCharKey(e);

            if (key === 'Escape') {
                e.preventDefault();
                closeAll();
                return;
            }
            if (key === 'ArrowDown') {
                e.preventDefault();
                const next = utils.nextSelectableIndex(m.items, m.activeIndex, 1, m.ctx);
                if (next >= 0) setActiveIndex(m, next, { openSubmenu: false, via: 'keyboard' });
                return;
            }
            if (key === 'ArrowUp') {
                e.preventDefault();
                const prev = utils.nextSelectableIndex(m.items, m.activeIndex, -1, m.ctx);
                if (prev >= 0) setActiveIndex(m, prev, { openSubmenu: false, via: 'keyboard' });
                return;
            }
            if (key === 'Home') {
                e.preventDefault();
                const first = utils.firstSelectableIndex(m.items, m.ctx);
                if (first >= 0) setActiveIndex(m, first, { openSubmenu: false, via: 'keyboard' });
                return;
            }
            if (key === 'End') {
                e.preventDefault();
                const last = utils.lastSelectableIndex(m.items, m.ctx);
                if (last >= 0) setActiveIndex(m, last, { openSubmenu: false, via: 'keyboard' });
                return;
            }
            if (key === 'ArrowRight') {
                e.preventDefault();
                const it = getActiveItem(m);
                if (it && utils.hasSubmenu(it, m.ctx)) {
                    openSubmenu(m, m.activeIndex, { via: 'keyboard' });
                    return;
                }
                if (typeof m.rootMeta?.onRequestRootSwitch === 'function' && stack.length === 1) {
                    m.rootMeta.onRequestRootSwitch(1);
                }
                return;
            }
            if (key === 'ArrowLeft') {
                e.preventDefault();
                if (stack.length > 1) {
                    closeFrom(stack.length - 1);
                    focusActiveRow(top());
                    return;
                }
                if (typeof m.rootMeta?.onRequestRootSwitch === 'function') {
                    m.rootMeta.onRequestRootSwitch(-1);
                }
                return;
            }
            if (key === 'Enter' || key === ' ') {
                e.preventDefault();
                if (m.activeIndex >= 0) onItemActivate(m, m.activeIndex, { via: 'keyboard' });
                return;
            }
            if (isChar) {
                const now = Date.now();
                const gap = now - (m.typeahead.ts || 0);
                if (gap > 650) m.typeahead.q = '';
                m.typeahead.ts = now;
                m.typeahead.q += key.toLowerCase();
                const match = utils.typeaheadMatchIndex(m.items, m.activeIndex, m.typeahead.q, m.ctx);
                if (match >= 0) setActiveIndex(m, match, { openSubmenu: false, via: 'keyboard' });
            }
        };
    };

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.ui = window.AhmadIDEModules.ui || {};
        window.AhmadIDEModules.ui.menu = window.AhmadIDEModules.ui.menu || {};
        window.AhmadIDEModules.ui.menu.keyboardNav = { createKeyHandler };
    }
})();
