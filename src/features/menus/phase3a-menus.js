(() => {
    const loadScript = (src) => {
        const existing = document.querySelector(`script[data-src="${src}"]`) || document.querySelector(`script[src="${src}"]`);
        if (existing) return;
        const s = document.createElement('script');
        s.src = src;
        s.async = false;
        s.dataset.src = src;
        document.head.appendChild(s);
    };

    const ensureConnectionsMenu = () => {
        if (window.AhmadIDEModules?.features?.menus?.createConnectionsMenu) return;
        loadScript('./src/features/menus/connectionsMenu.js');
    };

    const safe = (fn, fallback = null) => {
        try { return fn(); } catch (_) { return fallback; }
    };

    const getToolWindowCtx = () => {
        const leftVisible = safe(() => !!toolWindowState?.left?.visible, false);
        const bottomVisible = safe(() => !!toolWindowState?.bottom?.visible, false);
        return { leftVisible, bottomVisible };
    };

    const getRunConfigActive = () => {
        const apiActive = window.AhmadIDEModules?.app?.runConfig?.getActive?.();
        if (apiActive) return String(apiActive).trim();
        const menuActive = document.querySelector('#runConfigMenu .run-config-item.active')?.getAttribute('data-config');
        if (menuActive) return String(menuActive).trim();
        const btn = document.getElementById('runConfigBtn');
        const label = String(btn?.getAttribute('aria-label') || btn?.title || '').toLowerCase();
        if (label.includes('debug')) return 'debug-current';
        if (label.includes('run')) return 'run-current';
        return '';
    };

    const openMenuAt = (controller, anchorEl, menuId, ctxExtra = {}) => {
        const registry = window.AhmadIDEModules?.app?.menuRegistry;
        if (!registry) return;
        const items = registry.get(menuId, ctxExtra);
        const onAction = (action, ctx) => {
            if (typeof runMenuAction === 'function') runMenuAction(action, ctxExtra || ctx || {});
        };
        const openPopover = window.AhmadIDEModules?.ui?.menu?.openPopover;
        if (typeof openPopover === 'function') {
            openPopover({ controller, anchorEl, items, ctx: ctxExtra, onAction });
            return;
        }
        controller.openAtElement({ anchorEl, items, ctx: ctxExtra, onAction });
    };

    const bindToolbarMenus = (controller) => {
        const runCfgBtn = document.getElementById('runConfigBtn');
        if (runCfgBtn) {
            runCfgBtn.addEventListener('click', (e) => {
                e.preventDefault();
                e.stopImmediatePropagation();
                openMenuAt(controller, runCfgBtn, 'toolbar.runConfig', {
                    toolWindows: getToolWindowCtx(),
                    runConfig: { active: getRunConfigActive() }
                });
            }, true);
        }

        const vcsBtn = document.getElementById('vcsWidgetBtn');
        if (vcsBtn) {
            vcsBtn.addEventListener('click', (e) => {
                e.preventDefault();
                e.stopImmediatePropagation();
                openMenuAt(controller, vcsBtn, 'toolbar.vcs', {
                    toolWindows: getToolWindowCtx()
                });
            }, true);
        }
    };

    function bootstrap() {
        ensureConnectionsMenu();
        const controller = window.AhmadIDEModules?.ui?.menu?.controller;
        if (!controller) return;
        bindToolbarMenus(controller);
    }

    if (document.readyState === 'loading') {
        document.addEventListener('DOMContentLoaded', bootstrap, { once: true });
    } else {
        bootstrap();
    }
})();
