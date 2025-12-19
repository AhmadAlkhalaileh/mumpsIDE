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
            subtle: 'rgba(255,255,255,0.08)',
            strong: 'rgba(255,255,255,0.14)'
        },
        surface: {
            bg: 'var(--bg)',
            panel: 'var(--panel)',
            panel2: 'var(--panel-2)',
            overlay: 'rgba(0,0,0,0.55)',
            elevated: 'rgba(60, 63, 65, 0.96)'
        },
        text: {
            primary: 'var(--text)',
            bright: 'var(--text-bright)',
            muted: 'var(--muted)'
        },
        focus: {
            ring: '0 0 0 2px rgba(74, 158, 255, 0.35)',
            outline: 'rgba(74, 158, 255, 0.55)'
        },
        icon: {
            1: '12px',
            2: '16px',
            3: '20px'
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

        set('--ui-text-primary', TOKENS.text.primary);
        set('--ui-text-bright', TOKENS.text.bright);
        set('--ui-text-muted', TOKENS.text.muted);

        set('--ui-focus-ring', TOKENS.focus.ring);
        set('--ui-focus-outline', TOKENS.focus.outline);

        Object.entries(TOKENS.icon).forEach(([k, v]) => set(`--ui-icon-${k}`, v));

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

