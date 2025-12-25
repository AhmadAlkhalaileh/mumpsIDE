(() => {
    const TOKENS = {
        space: {
            0: '0px',
            1: '4px',
            2: '8px',
            3: '12px',
            4: '16px',
            5: '20px',
            6: '24px',
            7: '32px'
        },
        radius: {
            1: '6px',
            2: '8px',
            3: '10px'
        },
        border: {
            width: '1px',
            subtle: 'rgba(68, 71, 90, 0.5)',
            strong: 'rgba(68, 71, 90, 0.8)'
        },
        surface: {
            bg: 'var(--bg)',
            panel: 'var(--panel)',
            panel2: 'var(--panel-2)',
            overlay: 'rgba(0,0,0,0.55)',
            elevated: 'var(--glass)'
        },
        menu: {
            bg: 'var(--panel)',
            border: 'var(--border)',
            hover: 'var(--hover-bg)',
            active: 'var(--selection-bg)',
            activeBorder: 'var(--accent)'
        },
        text: {
            primary: 'var(--text)',
            bright: 'var(--text-bright)',
            muted: 'var(--muted)'
        },
        focus: {
            ring: '0 0 0 2px var(--accent-soft-border)',
            outline: 'var(--accent)'
        },
        shadow: {
            xs: '0 1px 3px rgba(0, 0, 0, 0.18)',
            sm: '0 4px 12px rgba(0, 0, 0, 0.25)',
            md: '0 10px 28px rgba(0, 0, 0, 0.45)',
            lg: '0 18px 40px rgba(0, 0, 0, 0.5)'
        },
        z: {
            base: 1,
            rail: 10,
            toolbar: 90,
            menubar: 100,
            popover: 200,
            overlay: 6000,
            menu: 7000,
            dialog: 10000,
            toast: 10010,
            splash: 99999
        },
        icon: {
            12: '12px',
            16: '16px',
            20: '20px'
        },
        metrics: {
            menubarRowH: '40px',
            toolbarRowH: '48px',
            statusbarH: '24px',
            menubarItemH: '32px',
            menuMinW: '220px',
            menuRowH: '28px',
            menuPadX: '10px',
            menuIconColW: '24px',
            menuShortcutColW: '112px',
            menuArrowColW: '16px',
            submenuOpenDelay: '120',
            submenuCloseDelay: '320',
            submenuGracePx: '16',
            stripeW: '36px',
            stripeBottomH: '28px',
            sidebarLeftW: '280px',
            sidebarRightW: '300px',
            toolwindowHeaderH: '32px',
            dialogMaxW: '980px',
            dialogMaxH: '720px',
            dialogInset: '56px',
            dialogHeaderH: '44px',
            dialogSidebarW: '260px'
        },
        typography: {
            uiFamily: 'var(--font-ui)',
            uiSize: 'var(--font-size-ui)',
            codeFamily: 'var(--font-code)',
            codeSize: 'var(--font-size-code)'
        },
        motion: {
            fast: '120ms',
            normal: '180ms',
            slow: '220ms',
            ease: 'cubic-bezier(0.2, 0.0, 0.0, 1.0)'
        }
    };

    const apply = (root = document.documentElement) => {
        if (!root) return;
        const style = root.style;
        const set = (k, v) => style.setProperty(k, v);

        Object.entries(TOKENS.space).forEach(([k, v]) => set(`--ui-space-${k}`, v));
        Object.entries(TOKENS.radius).forEach(([k, v]) => set(`--ui-radius-${k}`, v));

        set('--ui-border-width', TOKENS.border.width);
        set('--ui-border-subtle', TOKENS.border.subtle);
        set('--ui-border-strong', TOKENS.border.strong);

        set('--ui-surface-bg', TOKENS.surface.bg);
        set('--ui-surface-panel', TOKENS.surface.panel);
        set('--ui-surface-panel2', TOKENS.surface.panel2);
        set('--ui-surface-overlay', TOKENS.surface.overlay);
        set('--ui-surface-elevated', TOKENS.surface.elevated);

        set('--ui-menu-bg', TOKENS.menu.bg);
        set('--ui-menu-border', TOKENS.menu.border);
        set('--ui-menu-hover', TOKENS.menu.hover);
        set('--ui-menu-active', TOKENS.menu.active);
        set('--ui-menu-active-border', TOKENS.menu.activeBorder);

        set('--ui-text-primary', TOKENS.text.primary);
        set('--ui-text-bright', TOKENS.text.bright);
        set('--ui-text-muted', TOKENS.text.muted);

        set('--ui-focus-ring', TOKENS.focus.ring);
        set('--ui-focus-outline', TOKENS.focus.outline);

        Object.entries(TOKENS.icon).forEach(([k, v]) => set(`--ui-icon-${k}`, v));

        set('--ui-shadow-xs', TOKENS.shadow.xs);
        set('--ui-shadow-sm', TOKENS.shadow.sm);
        set('--ui-shadow-md', TOKENS.shadow.md);
        set('--ui-shadow-lg', TOKENS.shadow.lg);

        Object.entries(TOKENS.z).forEach(([k, v]) => set(`--ui-z-${k}`, String(v)));

        set('--ui-menubar-row-h', TOKENS.metrics.menubarRowH);
        set('--ui-toolbar-row-h', TOKENS.metrics.toolbarRowH);
        set('--ui-statusbar-h', TOKENS.metrics.statusbarH);
        set('--ui-menubar-h', TOKENS.metrics.menubarItemH);
        set('--ui-menu-min-w', TOKENS.metrics.menuMinW);
        set('--ui-menu-row-h', TOKENS.metrics.menuRowH);
        set('--ui-menu-pad-x', TOKENS.metrics.menuPadX);
        set('--ui-menu-icon-col-w', TOKENS.metrics.menuIconColW);
        set('--ui-menu-shortcut-col-w', TOKENS.metrics.menuShortcutColW);
        set('--ui-menu-arrow-col-w', TOKENS.metrics.menuArrowColW);
        set('--ui-submenu-open-delay', TOKENS.metrics.submenuOpenDelay);
        set('--ui-submenu-close-delay', TOKENS.metrics.submenuCloseDelay);
        set('--ui-submenu-grace-px', TOKENS.metrics.submenuGracePx);
        set('--ui-stripe-w', TOKENS.metrics.stripeW);
        set('--ui-stripe-bottom-h', TOKENS.metrics.stripeBottomH);
        set('--ui-sidebar-left-w', TOKENS.metrics.sidebarLeftW);
        set('--ui-sidebar-right-w', TOKENS.metrics.sidebarRightW);
        set('--ui-toolwindow-header-h', TOKENS.metrics.toolwindowHeaderH);
        set('--ui-dialog-max-w', TOKENS.metrics.dialogMaxW);
        set('--ui-dialog-max-h', TOKENS.metrics.dialogMaxH);
        set('--ui-dialog-inset', TOKENS.metrics.dialogInset);
        set('--ui-dialog-header-h', TOKENS.metrics.dialogHeaderH);
        set('--ui-dialog-sidebar-w', TOKENS.metrics.dialogSidebarW);

        set('--ui-font-ui', TOKENS.typography.uiFamily);
        set('--ui-font-ui-size', TOKENS.typography.uiSize);
        set('--ui-font-code', TOKENS.typography.codeFamily);
        set('--ui-font-code-size', TOKENS.typography.codeSize);

        set('--ui-motion-fast', TOKENS.motion.fast);
        set('--ui-motion-normal', TOKENS.motion.normal);
        set('--ui-motion-slow', TOKENS.motion.slow);
        set('--ui-ease', TOKENS.motion.ease);
    };

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.ui = window.AhmadIDEModules.ui || {};
        window.AhmadIDEModules.ui.theme = window.AhmadIDEModules.ui.theme || {};
        window.AhmadIDEModules.ui.theme.tokens = TOKENS;
        window.AhmadIDEModules.ui.theme.applyTokens = apply;
        // Apply once on load (inline vars override stylesheet defaults).
        try { apply(); } catch (_) { }
    }
})();
