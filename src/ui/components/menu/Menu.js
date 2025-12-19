(() => {
    const getCssNumber = (name, fallback) => {
        try {
            const raw = getComputedStyle(document.documentElement).getPropertyValue(name);
            const n = Number(String(raw || '').trim());
            return Number.isFinite(n) ? n : fallback;
        } catch (_) {
            return fallback;
        }
    };

    function createMenuController({ deps } = {}) {
        const globalMenu = typeof window !== 'undefined' ? window.AhmadIDEModules?.ui?.menu : null;
        if (!deps?.forceNew && globalMenu?.controller) return globalMenu.controller;

        const icons = deps?.icons || window.AhmadIDEModules?.ui?.icons;
        const utils = deps?.utils || globalMenu?.utils;
        const createSubmenuIntent = deps?.createSubmenuIntent || globalMenu?.createSubmenuIntent;
        const keyboardNav = deps?.keyboardNav || globalMenu?.keyboardNav;

        if (!icons?.createIcon) throw new Error('MenuController requires ui.icons.createIcon');
        if (!utils) throw new Error('MenuController requires ui.menu.utils');
        if (!createSubmenuIntent) throw new Error('MenuController requires ui.menu.createSubmenuIntent');
        if (!keyboardNav?.createKeyHandler) throw new Error('MenuController requires ui.menu.keyboardNav');

        const intent = createSubmenuIntent({
            openDelayMs: getCssNumber('--ui-submenu-open-delay', 120),
            closeDelayMs: getCssNumber('--ui-submenu-close-delay', 320),
            gracePx: getCssNumber('--ui-submenu-grace-px', 16)
        });

        const stack = [];
        let seq = 0;
        let pointer = { x: 0, y: 0 };
        let globalBound = false;

        const nextMenuId = () => `ui_menu_${Date.now()}_${++seq}`;
        const getMenuEl = (menu) => menu?.el || null;
        const isOpen = () => stack.length > 0;
        const top = () => stack[stack.length - 1] || null;
        const getStack = () => stack;

        const readSubmenuItems = (it, ctx) => utils.resolveSubmenu(it, ctx);

        const closeFrom = (idx) => {
            for (let i = stack.length - 1; i >= idx; i -= 1) {
                const m = stack[i];
                try { m.parentItemEl?.setAttribute('aria-expanded', 'false'); } catch (_) { }
                try { m.el?.remove?.(); } catch (_) { }
            }
            stack.splice(idx);
        };

        const closeAll = () => closeFrom(0);

        const isEventInsideMenus = (evt) => {
            const t = evt?.target;
            if (!t) return false;
            return stack.some((m) => m.el && m.el.contains(t));
        };

        const bindGlobalsOnce = () => {
            if (globalBound) return;
            globalBound = true;
            document.addEventListener('pointerdown', (e) => {
                if (!isOpen()) return;
                if (isEventInsideMenus(e)) return;
                closeAll();
            }, true);
            document.addEventListener('mousemove', (e) => {
                pointer = { x: e.clientX, y: e.clientY };
            }, true);
            window.addEventListener('blur', () => closeAll());
            document.addEventListener('keydown', (e) => {
                if (!isOpen()) return;
                handleKeyDown(e);
            }, true);
        };

        const positionMenu = (menuEl, { x, y }, { preferLeft = false } = {}) => {
            const vw = window.innerWidth;
            const vh = window.innerHeight;
            const rect = menuEl.getBoundingClientRect();

            let left = x;
            let top = y;
            const pad = 8;

            if (!preferLeft && left + rect.width > vw - pad) left = Math.max(pad, vw - rect.width - pad);
            if (preferLeft && left - rect.width < pad) left = Math.max(pad, vw - rect.width - pad);
            if (preferLeft && left - rect.width >= pad) left = left - rect.width;

            if (top + rect.height > vh - pad) top = Math.max(pad, vh - rect.height - pad);
            if (top < pad) top = pad;

            menuEl.style.left = `${Math.round(left)}px`;
            menuEl.style.top = `${Math.round(top)}px`;
        };

        const anchorRect = (anchorEl) => {
            try { return anchorEl?.getBoundingClientRect?.() || null; } catch (_) { return null; }
        };

        const getMenuColumnIcon = (it, ctx) => {
            const type = String(it?.type || '').toLowerCase();
            if (type === 'checkbox') return utils.isChecked(it, ctx) ? 'check' : '';
            if (type === 'radio') return utils.isChecked(it, ctx) ? 'radio' : '';
            return utils.resolveIcon(it);
        };

        const focusActiveRow = (menu) => {
            const row = menu?._activeRow || null;
            if (row && row.focus) {
                try { row.focus({ preventScroll: true }); } catch (_) { row.focus(); }
            }
        };

        const renderMenu = (menu) => {
            const el = document.createElement('div');
            el.className = 'ui-menu';
            el.setAttribute('role', 'menu');
            el.setAttribute('aria-orientation', 'vertical');
            el.tabIndex = -1;
            el.dataset.menuId = menu.id;

            const frag = document.createDocumentFragment();
            menu.items.forEach((it, idx) => {
                if (utils.isSeparator(it)) {
                    const sep = document.createElement('div');
                    sep.className = 'ui-menu-sep';
                    sep.setAttribute('role', 'separator');
                    frag.appendChild(sep);
                    return;
                }

                const type = String(it?.type || '').toLowerCase();
                const role = type === 'checkbox' ? 'menuitemcheckbox'
                    : type === 'radio' ? 'menuitemradio'
                        : 'menuitem';

                const row = document.createElement('div');
                row.className = 'ui-menu-item';
                row.setAttribute('role', role);
                row.dataset.index = String(idx);
                row.tabIndex = -1;
                row.id = `${menu.id}_item_${idx}`;

                const disabled = utils.isDisabled(it, menu.ctx);
                if (disabled) row.setAttribute('aria-disabled', 'true');

                if (type === 'checkbox' || type === 'radio') {
                    row.setAttribute('aria-checked', utils.isChecked(it, menu.ctx) ? 'true' : 'false');
                }

                if (utils.hasSubmenu(it, menu.ctx)) {
                    row.setAttribute('aria-haspopup', 'menu');
                    row.setAttribute('aria-expanded', 'false');
                }

                const iconName = getMenuColumnIcon(it, menu.ctx);
                const iconCell = document.createElement('div');
                iconCell.className = 'ui-menu-col ui-menu-col--icon';
                if (iconName) iconCell.appendChild(icons.createIcon(iconName, { size: 16 }));
                row.appendChild(iconCell);

                const labelCell = document.createElement('div');
                labelCell.className = 'ui-menu-col ui-menu-col--label';
                labelCell.textContent = utils.resolveLabel(it);
                row.appendChild(labelCell);

                const shortcutCell = document.createElement('div');
                shortcutCell.className = 'ui-menu-col ui-menu-col--shortcut';
                shortcutCell.textContent = utils.resolveShortcut(it);
                row.appendChild(shortcutCell);

                const arrowCell = document.createElement('div');
                arrowCell.className = 'ui-menu-col ui-menu-col--arrow';
                if (utils.hasSubmenu(it, menu.ctx)) {
                    arrowCell.appendChild(icons.createIcon('chevron-right', { size: 16 }));
                }
                row.appendChild(arrowCell);

                row.addEventListener('pointerenter', () => onItemHover(menu, idx));
                row.addEventListener('pointerdown', (e) => {
                    if (e.button !== 0) return;
                    row.classList.add('pressed');
                });
                row.addEventListener('pointerup', () => row.classList.remove('pressed'));
                row.addEventListener('click', (e) => {
                    e.stopPropagation();
                    onItemActivate(menu, idx, { via: 'mouse' });
                });

                frag.appendChild(row);
            });
            el.appendChild(frag);

            el.addEventListener('pointerleave', () => scheduleCloseChildMenus(menu));
            el.addEventListener('pointerenter', () => cancelCloseTimers(menu));
            return el;
        };

        const clearActive = (menu) => {
            const el = getMenuEl(menu);
            if (!el) return;
            const prev = menu?._activeRow || null;
            if (prev) {
                prev.classList.remove('active');
                prev.tabIndex = -1;
            } else {
                el.querySelectorAll('.ui-menu-item.active').forEach((n) => {
                    n.classList.remove('active');
                    n.tabIndex = -1;
                });
            }
            menu._activeRow = null;
            el.removeAttribute('aria-activedescendant');
        };

        const setActiveIndex = (menu, idx, { openSubmenu = true, via = 'program' } = {}) => {
            if (!menu || idx == null) return;
            if (menu.activeIndex === idx) return;
            menu.activeIndex = idx;

            const el = getMenuEl(menu);
            if (!el) return;
            clearActive(menu);

            const row = el.querySelector(`.ui-menu-item[data-index="${idx}"]`);
            if (row) {
                row.classList.add('active');
                row.tabIndex = 0;
                el.setAttribute('aria-activedescendant', row.id);
                menu._activeRow = row;
                if (via === 'keyboard' || via === 'program') focusActiveRow(menu);
            }

            if (openSubmenu) scheduleOpenSubmenu(menu, idx, { via });
        };

        const getActiveItem = (menu) => menu?.items?.[menu.activeIndex] || null;

        const scheduleOpenSubmenu = (menu, idx, { via } = {}) => {
            cancelOpenTimers(menu);
            const it = menu.items[idx];
            if (!utils.hasSubmenu(it, menu.ctx)) {
                closeChildMenus(menu);
                return;
            }
            menu._openTimer = setTimeout(() => {
                openSubmenu(menu, idx, { via });
            }, intent.openDelayMs);
        };

        const cancelOpenTimers = (menu) => {
            if (menu?._openTimer) {
                clearTimeout(menu._openTimer);
                menu._openTimer = null;
            }
        };

        const cancelCloseTimers = (menu) => {
            if (menu?._closeTimer) {
                clearTimeout(menu._closeTimer);
                menu._closeTimer = null;
            }
        };

        const closeChildMenus = (menu) => {
            if (!menu) return;
            const idx = stack.indexOf(menu);
            if (idx === -1) return;
            try { menu._activeRow?.setAttribute('aria-expanded', 'false'); } catch (_) { }
            closeFrom(idx + 1);
        };

        const isPointerInGrace = (menu) => {
            const child = stack[stack.indexOf(menu) + 1] || null;
            if (!child) return false;
            const parentEl = getMenuEl(menu);
            const childEl = getMenuEl(child);
            if (!parentEl || !childEl) return false;
            const row = parentEl.querySelector(`.ui-menu-item[data-index="${menu.activeIndex}"]`);
            if (!row) return false;
            const parentRect = row.getBoundingClientRect();
            const childRect = childEl.getBoundingClientRect();
            if (utils.rectContains(childRect, pointer.x, pointer.y, intent.gracePx)) return true;
            return utils.isPointInGraceZone(pointer.x, pointer.y, parentRect, childRect, intent.gracePx);
        };

        const scheduleCloseChildMenus = (menu) => {
            cancelCloseTimers(menu);
            menu._closeTimer = setTimeout(() => {
                if (isPointerInGrace(menu)) {
                    scheduleCloseChildMenus(menu);
                    return;
                }
                closeChildMenus(menu);
            }, intent.closeDelayMs);
        };

        const openSubmenu = (menu, idx, { via } = {}) => {
            const parentEl = getMenuEl(menu);
            if (!parentEl) return;
            const it = menu.items[idx];
            const submenuItems = readSubmenuItems(it, menu.ctx);
            if (!submenuItems.length) return;

            closeChildMenus(menu);

            const parentRow = parentEl.querySelector(`.ui-menu-item[data-index="${idx}"]`);
            if (!parentRow) return;
            const rowRect = parentRow.getBoundingClientRect();
            parentRow.setAttribute('aria-expanded', 'true');

            const subMenu = {
                id: nextMenuId(),
                ctx: menu.ctx,
                items: submenuItems,
                parent: menu,
                parentItemEl: parentRow,
                activeIndex: -1,
                typeahead: { q: '', ts: 0 },
                rootMeta: menu.rootMeta
            };
            subMenu.el = renderMenu(subMenu);
            document.body.appendChild(subMenu.el);
            stack.push(subMenu);

            positionMenu(subMenu.el, { x: rowRect.right + 6, y: rowRect.top - 4 }, { preferLeft: false });
            const rect = subMenu.el.getBoundingClientRect();
            if (rect.right > window.innerWidth - 8) {
                positionMenu(subMenu.el, { x: rowRect.left - 6, y: rowRect.top - 4 }, { preferLeft: true });
            }

            const first = utils.firstSelectableIndex(subMenu.items, subMenu.ctx);
            if (first >= 0) setActiveIndex(subMenu, first, { openSubmenu: false, via });
        };

        const onItemHover = (menu, idx) => {
            if (!menu) return;
            if (menu.activeIndex === idx) return;
            if (isPointerInGrace(menu)) {
                cancelOpenTimers(menu);
                cancelCloseTimers(menu);
                menu._hoverSwitchTimer = menu._hoverSwitchTimer || null;
                if (menu._hoverSwitchTimer) clearTimeout(menu._hoverSwitchTimer);
                menu._hoverSwitchTimer = setTimeout(() => {
                    menu._hoverSwitchTimer = null;
                    setActiveIndex(menu, idx, { openSubmenu: true, via: 'mouse' });
                }, intent.closeDelayMs);
                return;
            }
            setActiveIndex(menu, idx, { openSubmenu: true, via: 'mouse' });
        };

        const activateItem = async (menu, idx) => {
            const it = menu.items[idx];
            if (!it) return;
            if (utils.isDisabled(it, menu.ctx)) return;
            if (utils.hasSubmenu(it, menu.ctx)) {
                openSubmenu(menu, idx, { via: 'keyboard' });
                return;
            }
            const action = String(it.action || '').trim();
            const onSelect = it.onSelect;
            try {
                if (typeof onSelect === 'function') {
                    await onSelect(menu.ctx);
                } else if (action && typeof menu.rootMeta?.onAction === 'function') {
                    await menu.rootMeta.onAction(action, menu.ctx);
                }
            } catch (_) { }
            closeAll();
        };

        const onItemActivate = (menu, idx, { via } = {}) => {
            setActiveIndex(menu, idx, { openSubmenu: false, via });
            activateItem(menu, idx).catch(() => { });
        };

        const handleKeyDown = keyboardNav.createKeyHandler({
            top,
            getStack,
            utils,
            setActiveIndex,
            openSubmenu,
            closeFrom,
            closeAll,
            getActiveItem,
            onItemActivate,
            focusActiveRow
        });

        const openAtPoint = ({ x, y, items, ctx, onAction, rootMeta }) => {
            bindGlobalsOnce();
            closeAll();

            const menu = {
                id: nextMenuId(),
                ctx: ctx || {},
                items: Array.isArray(items) ? items : [],
                parent: null,
                parentItemEl: null,
                activeIndex: -1,
                typeahead: { q: '', ts: 0 },
                rootMeta: rootMeta || { onAction }
            };
            menu.el = renderMenu(menu);
            document.body.appendChild(menu.el);
            stack.push(menu);

            positionMenu(menu.el, { x, y });
            const first = utils.firstSelectableIndex(menu.items, menu.ctx);
            if (first >= 0) setActiveIndex(menu, first, { openSubmenu: false, via: 'program' });
            requestAnimationFrame(() => focusActiveRow(menu));
            return menu;
        };

        const openAtElement = ({ anchorEl, items, ctx, onAction, rootMeta }) => {
            bindGlobalsOnce();
            closeAll();
            const rect = anchorRect(anchorEl);
            if (!rect) return null;

            const menu = {
                id: nextMenuId(),
                ctx: ctx || {},
                items: Array.isArray(items) ? items : [],
                parent: null,
                parentItemEl: null,
                activeIndex: -1,
                typeahead: { q: '', ts: 0 },
                rootMeta: rootMeta || { onAction }
            };
            menu.el = renderMenu(menu);
            document.body.appendChild(menu.el);
            stack.push(menu);

            positionMenu(menu.el, { x: rect.left, y: rect.bottom + 4 });
            const first = utils.firstSelectableIndex(menu.items, menu.ctx);
            if (first >= 0) setActiveIndex(menu, first, { openSubmenu: false, via: 'program' });
            requestAnimationFrame(() => focusActiveRow(menu));
            return menu;
        };

        const controller = {
            isOpen,
            closeAll,
            openAtPoint,
            openAtElement,
            intent
        };

        if (globalMenu && !globalMenu.controller) {
            globalMenu.controller = controller;
        }

        return controller;
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.ui = window.AhmadIDEModules.ui || {};
        window.AhmadIDEModules.ui.menu = window.AhmadIDEModules.ui.menu || {};
        if (!window.AhmadIDEModules.ui.menu.controller) {
            window.AhmadIDEModules.ui.menu.controller = createMenuController({ deps: {} });
        }
        window.AhmadIDEModules.ui.menu.createMenuController = createMenuController;
    }
})();
