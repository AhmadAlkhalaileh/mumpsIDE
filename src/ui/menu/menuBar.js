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

            // FIX: Manual injection of Connections menu if missing (registry cache fallback)
            if (def.id === 'tools' && Array.isArray(def.items)) {
                const hasConnections = def.items.some(it => it.id === 'tools.connections' || it.action === 'connections');
                if (!hasConnections) {
                    console.log('[MenuBar] Force-injecting Connections menu item');
                    // Find index of 'tools.terminal' to insert after, or just unshift
                    const termIdx = def.items.findIndex(it => it.id === 'tools.terminal');
                    const newItem = { id: 'tools.connections', label: 'Connections', action: 'connections', icon: 'link' };
                    if (termIdx >= 0) {
                        def.items.splice(termIdx + 1, 0, newItem);
                    } else {
                        def.items.unshift(newItem);
                    }
                }
            }

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
            if (focusMenu) requestAnimationFrame(() => controller.isOpen() && document.activeElement?.classList?.contains('ui-menu') && document.activeElement.focus());
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
            // If Alt was used with another key, do not treat it as a “focus menubar” toggle.
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

            menus.forEach((m, idx) => {
                const btn = document.createElement('button');
                btn.type = 'button';
                btn.className = 'ui-menubar-item';
                btn.textContent = m.label || m.id || 'Menu';
                btn.setAttribute('role', 'menuitem');
                btn.setAttribute('aria-haspopup', 'true');
                btn.setAttribute('aria-expanded', 'false');
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
