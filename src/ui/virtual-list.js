(() => {
    function createVirtualList(host, opts = {}) {
        if (!host) throw new Error('createVirtualList requires a host element');

        const rowHeight = Math.max(12, Number(opts.rowHeight || 28));
        const overscan = Math.max(1, Number(opts.overscan || 6));
        const renderRow = opts.renderRow;
        if (typeof renderRow !== 'function') {
            throw new Error('createVirtualList requires opts.renderRow(item, index)');
        }

        const state = {
            items: [],
            destroyed: false,
            lastRangeKey: '',
            raf: 0
        };

        host.classList.add('virtual-list');

        const spacer = document.createElement('div');
        spacer.className = 'virtual-list-spacer';

        const itemsHost = document.createElement('div');
        itemsHost.className = 'virtual-list-items';

        // Reset host content (safe: git panel renders into this container only)
        host.innerHTML = '';
        host.appendChild(spacer);
        host.appendChild(itemsHost);

        const clamp = (n, min, max) => Math.max(min, Math.min(max, n));

        const computeRange = () => {
            const total = state.items.length;
            const viewportHeight = host.clientHeight || 0;
            const scrollTop = host.scrollTop || 0;
            const start = clamp(Math.floor(scrollTop / rowHeight) - overscan, 0, Math.max(0, total - 1));
            const end = clamp(Math.ceil((scrollTop + viewportHeight) / rowHeight) + overscan, 0, total);
            return { start, end, total };
        };

        const render = () => {
            if (state.destroyed) return;
            const { start, end, total } = computeRange();
            const key = `${start}:${end}:${total}`;
            if (key === state.lastRangeKey) return;
            state.lastRangeKey = key;

            spacer.style.height = `${total * rowHeight}px`;
            itemsHost.style.transform = `translateY(${start * rowHeight}px)`;

            itemsHost.innerHTML = '';
            const frag = document.createDocumentFragment();
            for (let i = start; i < end; i += 1) {
                const row = renderRow(state.items[i], i);
                if (!row) continue;
                row.classList.add('virtual-list-row');
                row.style.height = `${rowHeight}px`;
                frag.appendChild(row);
            }
            itemsHost.appendChild(frag);
        };

        const schedule = () => {
            if (state.destroyed) return;
            if (state.raf) return;
            state.raf = (typeof requestAnimationFrame === 'function')
                ? requestAnimationFrame(() => {
                    state.raf = 0;
                    render();
                })
                : setTimeout(() => {
                    state.raf = 0;
                    render();
                }, 16);
        };

        const onScroll = () => schedule();
        host.addEventListener('scroll', onScroll, { passive: true });

        let resizeObs = null;
        if (typeof ResizeObserver !== 'undefined') {
            resizeObs = new ResizeObserver(() => schedule());
            resizeObs.observe(host);
        } else {
            window.addEventListener('resize', onScroll, { passive: true });
        }

        const setItems = (items) => {
            state.items = Array.isArray(items) ? items : [];
            state.lastRangeKey = '';
            schedule();
        };

        const destroy = () => {
            state.destroyed = true;
            try {
                host.removeEventListener('scroll', onScroll);
            } catch (_) { }
            try {
                if (resizeObs) resizeObs.disconnect();
            } catch (_) { }
            try {
                window.removeEventListener('resize', onScroll);
            } catch (_) { }
            try {
                if (state.raf && typeof cancelAnimationFrame === 'function') cancelAnimationFrame(state.raf);
                state.raf = 0;
            } catch (_) { }
        };

        // Initial render
        schedule();

        return {
            setItems,
            refresh: () => { state.lastRangeKey = ''; schedule(); },
            destroy
        };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.ui = window.AhmadIDEModules.ui || {};
        window.AhmadIDEModules.ui.createVirtualList = createVirtualList;
    }
})();

