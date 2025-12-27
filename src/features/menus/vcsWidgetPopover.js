(() => {
    const safe = (fn, fallback = null) => {
        try { return fn(); } catch (_) { return fallback; }
    };

    const clamp = (n, min, max) => Math.max(min, Math.min(max, n));

    const getMenuController = (controller) => {
        const menu = window.AhmadIDEModules?.ui?.menu;
        return controller || menu?.controller || menu?.createMenuController?.({}) || null;
    };

    const getIcons = () => window.AhmadIDEModules?.ui?.icons || null;

    const getRepoManager = () =>
        window.AhmadIDEModules?.git?.repoManager
        || window.AhmadIDE?.gitRepoManager
        || null;

    const getRepoState = () => safe(() => getRepoManager()?.getState?.() || null, null);

    const normalizeLabel = (label) => {
        const v = String(label || '').trim();
        return v ? v : 'Git';
    };

    const readBranchLabel = (anchorEl) => {
        const fromDataset = safe(() => String(anchorEl?.dataset?.branch || '').trim(), '');
        if (fromDataset) return normalizeLabel(fromDataset);

        const fromLabelEl = safe(() => String(document.getElementById('vcsWidgetLabel')?.textContent || '').trim(), '');
        if (fromLabelEl) return normalizeLabel(fromLabelEl);

        const hint = String(anchorEl?.getAttribute?.('aria-label') || anchorEl?.title || '').trim();
        const m = hint.match(/\bGit\s*[:(]\s*(.+?)\s*\)?$/i);
        if (m) return normalizeLabel(m[1]);

        return 'Git';
    };

    const summarizeRepoState = (repoState) => {
        const s = repoState && typeof repoState === 'object' ? repoState : {};
        return {
            projectRoot: String(s.projectRoot || ''),
            repoRoot: String(s.repoRoot || ''),
            repoDetected: repoState ? !!s.repoDetected : true,
            gitAvailable: repoState ? !!s.gitAvailable : true,
            gitDisabled: repoState ? !!s.gitDisabled : false
        };
    };

    const buildHeader = ({ icons, vcs } = {}) => {
        const header = document.createElement('div');
        header.className = 'vcs-widget-popover-header';

        const titleRow = document.createElement('div');
        titleRow.className = 'vcs-widget-popover-title-row';

        const titleLeft = document.createElement('div');
        titleLeft.className = 'vcs-widget-popover-title-left';

        if (icons?.createIcon) {
            titleLeft.appendChild(icons.createIcon('git', { size: 16 }));
        }

        const titleText = document.createElement('div');
        titleText.className = 'vcs-widget-popover-title-text';

        const title = document.createElement('div');
        title.className = 'vcs-widget-popover-title';
        title.textContent = normalizeLabel(vcs?.branch);

        const meta = document.createElement('div');
        meta.className = 'vcs-widget-popover-meta';
        if (!vcs?.gitAvailable || vcs?.gitDisabled) meta.textContent = 'Git not available';
        else if (!vcs?.repoDetected) meta.textContent = 'No repository detected';
        else meta.textContent = 'Git';

        titleText.appendChild(title);
        titleText.appendChild(meta);

        titleLeft.appendChild(titleText);
        titleRow.appendChild(titleLeft);
        header.appendChild(titleRow);

        const pathRow = document.createElement('div');
        pathRow.className = 'vcs-widget-popover-path';
        pathRow.textContent = vcs?.repoDetected
            ? (vcs.repoRoot || vcs.projectRoot || '')
            : (vcs.projectRoot || '');
        header.appendChild(pathRow);

        return header;
    };

    const decorateVcsMenu = (menuEl, { icons, vcs } = {}) => {
        if (!menuEl) return;
        menuEl.classList.add('vcs-widget-menu');
        menuEl.style.setProperty('--ui-menu-shortcut-col-w', '0px');
        menuEl.style.setProperty('--ui-menu-arrow-col-w', '0px');
        menuEl.style.setProperty('--ui-menu-min-w', '260px');

        safe(() => menuEl.querySelector(':scope > .vcs-widget-popover-header')?.remove());
        safe(() => menuEl.querySelector(':scope > .vcs-widget-popover-divider')?.remove());

        const header = buildHeader({ icons, vcs });
        const divider = document.createElement('div');
        divider.className = 'vcs-widget-popover-divider';

        menuEl.insertBefore(header, menuEl.firstChild);
        menuEl.insertBefore(divider, header.nextSibling);
    };

    const alignMenuToAnchor = (menuEl, anchorEl, { gap = 6 } = {}) => {
        if (!menuEl || !anchorEl) return;
        const rect = safe(() => anchorEl.getBoundingClientRect(), null);
        if (!rect) return;

        const menuRect = safe(() => menuEl.getBoundingClientRect(), null);
        if (!menuRect) return;

        const vw = window.innerWidth;
        const vh = window.innerHeight;
        const pad = 8;

        const preferredLeft = rect.right - menuRect.width;
        const left = clamp(preferredLeft, pad, vw - menuRect.width - pad);
        const top = clamp(rect.bottom + gap, pad, vh - menuRect.height - pad);

        menuEl.style.left = `${Math.round(left)}px`;
        menuEl.style.top = `${Math.round(top)}px`;
    };

    const applyAvailability = (items, vcs) => {
        const shouldDisableOps = !vcs?.gitAvailable || !!vcs?.gitDisabled || !vcs?.repoDetected;
        if (!shouldDisableOps) return items;
        const disableActions = new Set(['vcs:commit', 'vcs:history', 'vcs:push', 'vcs:pull']);
        return (items || []).map((it) => {
            if (!it || it.type === 'separator') return it;
            const action = String(it.action || '').trim();
            if (!disableActions.has(action)) return it;
            return { ...it, disabled: true };
        });
    };

    const createDrawerElements = () => {
        const overlay = document.createElement('div');
        overlay.className = 'vcs-drawer-overlay hidden';

        const drawer = document.createElement('div');
        drawer.className = 'vcs-drawer hidden';
        drawer.setAttribute('role', 'dialog');
        drawer.setAttribute('aria-modal', 'true');
        drawer.setAttribute('aria-label', 'Git');
        drawer.setAttribute('aria-hidden', 'true');
        drawer.tabIndex = -1;
        overlay.appendChild(drawer);

        const header = document.createElement('div');
        header.className = 'vcs-drawer-header';
        drawer.appendChild(header);

        const headerLeft = document.createElement('div');
        headerLeft.className = 'vcs-drawer-header-left';
        header.appendChild(headerLeft);

        const titleText = document.createElement('div');
        titleText.className = 'vcs-drawer-title-text';
        headerLeft.appendChild(titleText);

        const title = document.createElement('div');
        title.className = 'vcs-drawer-title';
        titleText.appendChild(title);

        const meta = document.createElement('div');
        meta.className = 'vcs-drawer-meta';
        titleText.appendChild(meta);

        const headerActions = document.createElement('div');
        headerActions.className = 'vcs-drawer-header-actions';
        header.appendChild(headerActions);

        const closeBtn = document.createElement('button');
        closeBtn.className = 'vcs-drawer-close';
        closeBtn.type = 'button';
        closeBtn.setAttribute('aria-label', 'Close Git drawer');
        headerActions.appendChild(closeBtn);

        const path = document.createElement('div');
        path.className = 'vcs-drawer-path';
        drawer.appendChild(path);

        const list = document.createElement('div');
        list.className = 'vcs-drawer-actions';
        drawer.appendChild(list);

        return { overlay, drawer, headerLeft, title, meta, closeBtn, path, list };
    };

    const formatMeta = (vcs) => {
        if (!vcs?.gitAvailable || vcs?.gitDisabled) return 'Git not available';
        if (!vcs?.repoDetected) return 'No repository detected';
        return 'Git';
    };

    const buildDrawerAction = ({ icons, item, onAction } = {}) => {
        const it = item && typeof item === 'object' ? item : null;
        if (!it) return null;
        if (it.type === 'separator') {
            const sep = document.createElement('div');
            sep.className = 'vcs-drawer-sep';
            return sep;
        }

        const btn = document.createElement('button');
        btn.className = 'vcs-drawer-action';
        btn.type = 'button';
        const disabled = !!it.disabled;
        if (disabled) {
            btn.disabled = true;
            btn.setAttribute('aria-disabled', 'true');
        }

        const iconWrap = document.createElement('span');
        iconWrap.className = 'vcs-drawer-action-icon';
        if (icons?.createIcon && it.icon) {
            iconWrap.appendChild(icons.createIcon(it.icon, { size: 16 }));
        } else if (it.icon) {
            iconWrap.textContent = String(it.icon).slice(0, 2);
        }

        const label = document.createElement('span');
        label.className = 'vcs-drawer-action-label';
        label.textContent = String(it.label || it.id || '').trim() || 'Action';

        btn.appendChild(iconWrap);
        btn.appendChild(label);

        const action = String(it.action || '').trim();
        if (action && !disabled) {
            btn.addEventListener('click', (e) => {
                e.preventDefault();
                e.stopPropagation();
                try { onAction?.(action); } catch (_) { }
            });
        }

        return btn;
    };

    const openVcsWidgetDrawer = (() => {
        let els = null;
        let isOpen = false;
        let closeTimer = null;
        let escListener = null;
        let activeButtonEl = null;

        const ensure = () => {
            if (els) return els;
            els = createDrawerElements();
            document.body.appendChild(els.overlay);
            return els;
        };

        const readMotionMs = () => {
            try {
                const raw = String(getComputedStyle(document.documentElement).getPropertyValue('--ui-motion-normal') || '').trim();
                if (!raw) return 180;
                if (raw.endsWith('ms')) return Math.max(0, parseFloat(raw));
                if (raw.endsWith('s')) return Math.max(0, parseFloat(raw) * 1000);
                const n = parseFloat(raw);
                return Number.isFinite(n) ? n : 180;
            } catch (_) {
                return 180;
            }
        };

        const setButtonActive = (open, { buttonEl } = {}) => {
            const btn = buttonEl || activeButtonEl;
            if (!btn) return;
            btn.classList.toggle('active', !!open);
        };

        const hardHide = () => {
            if (!els) return;
            els.overlay.classList.add('hidden');
            els.drawer.classList.add('hidden');
            els.overlay.classList.remove('open');
            els.drawer.classList.remove('open');
            els.drawer.setAttribute('aria-hidden', 'true');
        };

        const open = ({ buttonEl } = {}) => {
            if (!els) return;

            try {
                if (closeTimer) clearTimeout(closeTimer);
            } catch (_) { }
            closeTimer = null;

            els.overlay.classList.remove('hidden');
            els.drawer.classList.remove('hidden');
            els.drawer.setAttribute('aria-hidden', 'false');
            setButtonActive(true, { buttonEl });

            try {
                requestAnimationFrame(() => {
                    if (!els || !isOpen) return;
                    els.overlay.classList.add('open');
                    els.drawer.classList.add('open');
                    try { els.closeBtn?.focus?.({ preventScroll: true }); } catch (_) { }
                });
            } catch (_) {
                els.overlay.classList.add('open');
                els.drawer.classList.add('open');
                try { els.closeBtn?.focus?.({ preventScroll: true }); } catch (_) { }
            }

            isOpen = true;
        };

        const close = ({ buttonEl } = {}) => {
            if (!els) return;
            const btn = buttonEl || activeButtonEl;
            setButtonActive(false, { buttonEl: btn });
            els.overlay.classList.remove('open');
            els.drawer.classList.remove('open');
            els.drawer.setAttribute('aria-hidden', 'true');
            try {
                if (escListener) window.removeEventListener('keydown', escListener, true);
            } catch (_) { }
            escListener = null;
            isOpen = false;

            try {
                if (closeTimer) clearTimeout(closeTimer);
            } catch (_) { }

            const ms = Math.max(0, readMotionMs());
            closeTimer = setTimeout(() => {
                closeTimer = null;
                hardHide();
                try { btn?.focus?.({ preventScroll: true }); } catch (_) { }
            }, ms + 40);
        };

        return ({ anchorEl, ctxExtra } = {}) => {
            if (!anchorEl) return null;
            const drawerEls = ensure();
            const icons = getIcons();
            activeButtonEl = anchorEl;

            if (isOpen) {
                close({ buttonEl: anchorEl });
                return null;
            }

            const repoState = summarizeRepoState(getRepoState());
            const branch = readBranchLabel(anchorEl);
            const vcs = { ...repoState, branch };
            const ctx = { ...(ctxExtra || {}), vcs };

            const registry = window.AhmadIDEModules?.app?.menuRegistry;
            const baseItems = registry?.get ? registry.get('toolbar.vcs', ctx) : [];
            const items = applyAvailability(baseItems, vcs);

            drawerEls.title.textContent = normalizeLabel(vcs.branch);
            drawerEls.meta.textContent = formatMeta(vcs);
            drawerEls.path.textContent = vcs.repoDetected
                ? (vcs.repoRoot || vcs.projectRoot || '')
                : (vcs.projectRoot || '');

            drawerEls.headerLeft.innerHTML = '';
            if (icons?.createIcon) {
                drawerEls.headerLeft.appendChild(icons.createIcon('git', { size: 16 }));
            }
            const titleText = document.createElement('div');
            titleText.className = 'vcs-drawer-title-text';
            titleText.appendChild(drawerEls.title);
            titleText.appendChild(drawerEls.meta);
            drawerEls.headerLeft.appendChild(titleText);

            drawerEls.closeBtn.innerHTML = '';
            if (icons?.createIcon) drawerEls.closeBtn.appendChild(icons.createIcon('close', { size: 14 }));
            else drawerEls.closeBtn.textContent = '×';

            drawerEls.list.innerHTML = '';

            const dispatch = window.AhmadIDEModules?.app?.runMenuAction || window.runMenuAction || null;
            const onAction = (action) => {
                close({ buttonEl: anchorEl });
                if (typeof dispatch === 'function') {
                    try { dispatch(action, ctx); } catch (_) { }
                }
            };

            (items || []).forEach((it) => {
                const node = buildDrawerAction({ icons, item: it, onAction });
                if (node) drawerEls.list.appendChild(node);
            });

            drawerEls.overlay.onclick = (e) => {
                if (e.target === drawerEls.overlay) close({ buttonEl: anchorEl });
            };
            drawerEls.closeBtn.onclick = (e) => {
                e.preventDefault();
                e.stopPropagation();
                close({ buttonEl: anchorEl });
            };

            escListener = (e) => {
                if (e.key === 'Escape') {
                    e.preventDefault();
                    close({ buttonEl: anchorEl });
                }
            };
            try { window.addEventListener('keydown', escListener, true); } catch (_) { }

            open({ buttonEl: anchorEl });
            return { close: () => close({ buttonEl: anchorEl }) };
        };
    })();

    const openVcsWidgetPopover = ({ controller, anchorEl, ctxExtra } = {}) => {
        const ctrl = getMenuController(controller);
        if (!ctrl || typeof ctrl.openAtElement !== 'function') return null;
        if (!anchorEl) return null;

        const registry = window.AhmadIDEModules?.app?.menuRegistry;
        if (!registry?.get) return null;

        const icons = getIcons();
        const repoState = summarizeRepoState(getRepoState());
        const branch = readBranchLabel(anchorEl);

        const vcs = { ...repoState, branch };
        const ctx = { ...(ctxExtra || {}), vcs };

        const baseItems = registry.get('toolbar.vcs', ctx);
        const items = applyAvailability(baseItems, vcs);

        const onAction = (action, menuCtx) => {
            if (typeof runMenuAction === 'function') runMenuAction(action, menuCtx || ctx || {});
        };

        const openPopover = window.AhmadIDEModules?.ui?.menu?.openPopover;
        const opened = typeof openPopover === 'function'
            ? openPopover({ controller: ctrl, anchorEl, items, ctx, onAction, rootMeta: { onAction } })
            : ctrl.openAtElement({ anchorEl, items, ctx, onAction, rootMeta: { onAction } });

        if (opened?.el) {
            decorateVcsMenu(opened.el, { icons, vcs });
            alignMenuToAnchor(opened.el, anchorEl);
        }

        return opened;
    };

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.features = window.AhmadIDEModules.features || {};
        window.AhmadIDEModules.features.menus = window.AhmadIDEModules.features.menus || {};
        window.AhmadIDEModules.features.menus.openVcsWidgetDrawer = openVcsWidgetDrawer;
        window.AhmadIDEModules.features.menus.openVcsWidgetPopover = openVcsWidgetPopover;
    }
})();
