(() => {
    function createMenuBar({ host, menus, controller, onAction, getContext } = {}) {
        if (!host) throw new Error('MenuBar requires host element');
        if (!controller) throw new Error('MenuBar requires menu controller');
        if (!Array.isArray(menus)) throw new Error('MenuBar requires menus array');

        let focused = false;
        let focusedIndex = 0;
        let openIndex = -1;
        let switchTimer = null;
        let altArmed = false;
        let lastAltTs = 0;

        const items = [];
        const rootMeta = {
            onAction,
            onRequestRootSwitch: (dir) => {
                if (!items.length) return;
                const next = (openIndex + dir + items.length) % items.length;
                openMenu(next, { focusMenu: true });
            }
        };

        const setFocusedVisual = () => {
            host.classList.toggle('ui-menubar--focused', focused);
            items.forEach((btn, idx) => {
                btn.classList.toggle('focused', focused && idx === focusedIndex);
                btn.setAttribute('aria-expanded', idx === openIndex ? 'true' : 'false');
                btn.tabIndex = focused && idx === focusedIndex ? 0 : -1;
            });
        };

        const closeMenus = () => {
            openIndex = -1;
            controller.closeAll();
            setFocusedVisual();
        };

        const openMenu = (idx, { focusMenu = false } = {}) => {
            const def = menus[idx];
            if (!def) return;

            openIndex = idx;
            focusedIndex = idx;
            const ctx = typeof getContext === 'function' ? getContext() : {};
            controller.openAtElement({
                anchorEl: items[idx],
                items: def.items || [],
                ctx,
                onAction,
                rootMeta
            });
            setFocusedVisual();
            if (focusMenu) {
                requestAnimationFrame(() => {
                    const activeRow = document.querySelector('.ui-menu .ui-menu-item.active');
                    activeRow?.focus?.();
                });
            }
        };

        const focusBar = () => {
            focused = true;
            setFocusedVisual();
            items[focusedIndex]?.focus?.();
        };

        const blurBar = () => {
            focused = false;
            closeMenus();
        };

        const onKeyDown = (e) => {
            if (e.key === 'Alt') {
                altArmed = true;
                lastAltTs = Date.now();
                return;
            }
            if (altArmed) altArmed = false;
            if (!focused) return;

            if (e.key === 'Escape') {
                e.preventDefault();
                blurBar();
                return;
            }
            if (e.key === 'ArrowLeft') {
                e.preventDefault();
                focusedIndex = (focusedIndex - 1 + items.length) % items.length;
                if (openIndex >= 0) openMenu(focusedIndex);
                else setFocusedVisual();
                items[focusedIndex]?.focus?.();
                return;
            }
            if (e.key === 'ArrowRight') {
                e.preventDefault();
                focusedIndex = (focusedIndex + 1) % items.length;
                if (openIndex >= 0) openMenu(focusedIndex);
                else setFocusedVisual();
                items[focusedIndex]?.focus?.();
                return;
            }
            if (e.key === 'ArrowDown' || e.key === 'Enter') {
                e.preventDefault();
                openMenu(focusedIndex, { focusMenu: true });
                return;
            }
        };

        const onKeyUp = (e) => {
            if (e.key !== 'Alt') return;
            const now = Date.now();
            const within = now - lastAltTs < 400;
            if (!within || !altArmed) {
                altArmed = false;
                return;
            }
            altArmed = false;
            e.preventDefault();
            if (focused) blurBar();
            else focusBar();
        };

        const onBarPointerDown = (idx, e) => {
            e.preventDefault();
            e.stopPropagation();
            focused = true;
            if (openIndex === idx) {
                closeMenus();
            } else {
                openMenu(idx, { focusMenu: false });
            }
        };

        const onBarPointerEnter = (idx) => {
            if (openIndex < 0) return;
            if (idx === openIndex) return;
            if (switchTimer) clearTimeout(switchTimer);
            switchTimer = setTimeout(() => {
                switchTimer = null;
                openMenu(idx, { focusMenu: false });
            }, 90);
        };

        const mount = () => {
            host.innerHTML = '';
            host.classList.add('ui-menubar');
            host.setAttribute('role', 'menubar');

            menus.forEach((m, idx) => {
                const btn = document.createElement('button');
                btn.type = 'button';
                btn.className = 'ui-menubar-item';
                btn.textContent = m.label || m.id || 'Menu';
                btn.setAttribute('role', 'menuitem');
                btn.setAttribute('aria-haspopup', 'true');
                btn.setAttribute('aria-expanded', 'false');
                btn.tabIndex = idx === 0 ? 0 : -1;
                btn.addEventListener('pointerdown', (e) => onBarPointerDown(idx, e));
                btn.addEventListener('pointerenter', () => onBarPointerEnter(idx));
                items.push(btn);
                host.appendChild(btn);
            });

            document.addEventListener('keydown', onKeyDown, true);
            document.addEventListener('keyup', onKeyUp, true);
            document.addEventListener('pointerdown', (e) => {
                if (!focused) return;
                if (host.contains(e.target)) return;
                if (controller.isOpen() && document.querySelector('.ui-menu')?.contains(e.target)) return;
                blurBar();
            }, true);

            setFocusedVisual();
        };

        return { mount, focusBar, blurBar };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.ui = window.AhmadIDEModules.ui || {};
        window.AhmadIDEModules.ui.menu = window.AhmadIDEModules.ui.menu || {};
        window.AhmadIDEModules.ui.menu.createMenuBar = createMenuBar;
    }
})();
