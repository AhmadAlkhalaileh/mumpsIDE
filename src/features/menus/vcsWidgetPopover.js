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
        window.AhmadIDEModules.features.menus.openVcsWidgetPopover = openVcsWidgetPopover;
    }
})();
