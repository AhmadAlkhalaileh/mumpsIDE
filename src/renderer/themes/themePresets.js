(() => {
    function createThemePresets() {
        // Theme presets (IDE shell + Monaco code themes)
        const ideThemes = {
            'jb-light': {
                name: ' Light',
                vars: {
                    '--bg': '#f5f7fb',
                    '--glass': 'rgba(255, 255, 255, 0.7)',
                    '--panel': '#ffffff',
                    '--panel-2': '#f4f6fb',
                    '--panel-strong': '#ffffff',
                    '--panel-soft': '#f5f7fb',
                    '--sidebar': '#f5f7fb',
                    '--rail-bg': '#eef1f7',
                    '--editor-bg': '#ffffff',
                    '--gutter-bg': '#f3f5f9',
                    '--terminal-bg': '#f8f9fb',
                    '--terminal-input-bg': '#edf0f7',
                    '--accent': '#3574f0',
                    '--accent-2': '#5b8def',
                    '--accent-blue': '#3574f0',
                    '--accent-green': '#3fb87f',
                    '--accent-orange': '#ed8b00',
                    '--accent-soft': 'rgba(53, 116, 240, 0.12)',
                    '--accent-soft-border': 'rgba(53, 116, 240, 0.24)',
                    '--text': '#1f232a',
                    '--text-bright': '#0f4c81',
                    '--muted': '#6a7280',
                    '--border': '#dfe3eb',
                    '--tree-selected': '#e6f0ff',
                    '--tree-selected-text': '#0f1b2d',
                    '--selection-bg': '#d9e4ff',
                    '--hover-bg': '#ecf0f7',
                    '--glow-1': 'rgba(53, 116, 240, 0.08)',
                    '--glow-2': 'rgba(91, 141, 239, 0.06)',
                    '--font-ui': 'Inter, \"Segoe UI\", \"SF Pro Display\", system-ui, sans-serif',
                    '--font-code': 'ui-monospace, \"SFMono-Regular\", \"Menlo\", \"Monaco\", \"Consolas\", \"Liberation Mono\", \"Courier New\", monospace',
                    '--font-size-ui': '13px',
                    '--font-size-code': '13px'
                }
            },
            earth: {
                name: 'Storm Dark',
                vars: {
                    '--bg': '#282a36',
                    '--glass': 'rgba(40, 42, 54, 0.92)',
                    '--panel': '#2f3141',
                    '--panel-2': '#2b2d3a',
                    '--panel-strong': '#343746',
                    '--panel-soft': '#242631',
                    '--sidebar': '#2b2d3a',
                    '--rail-bg': '#2b2d3a',
                    '--editor-bg': '#282a36',
                    '--gutter-bg': '#282a36',
                    '--terminal-bg': '#282a36',
                    '--terminal-input-bg': '#2f3141',
                    '--accent': '#bd93f9',
                    '--accent-2': '#ff79c6',
                    '--accent-blue': '#8be9fd',
                    '--accent-green': '#50fa7b',
                    '--accent-orange': '#ffb86c',
                    '--accent-soft': 'rgba(189, 147, 249, 0.15)',
                    '--accent-soft-border': 'rgba(189, 147, 249, 0.35)',
                    '--text': '#f8f8f2',
                    '--text-bright': '#ffffff',
                    '--muted': '#6272a4',
                    '--border': '#44475a',
                    '--tree-selected': '#44475a',
                    '--tree-selected-text': '#f8f8f2',
                    '--selection-bg': '#44475a',
                    '--hover-bg': '#343746',
                    '--glow-1': 'rgba(189, 147, 249, 0.05)',
                    '--glow-2': 'rgba(139, 233, 253, 0.03)',
                    '--font-ui': 'Inter, \"Segoe UI\", \"SF Pro Display\", system-ui, sans-serif',
                    '--font-code': '\"ains Mono\", \"Fira Code\", ui-monospace, \"SFMono-Regular\", monospace',
                    '--font-size-ui': '13px',
                    '--font-size-code': '13px'
                }
            },
            desert: {
                name: 'Desert Contrast',
                vars: {
                    '--bg': '#21160f',
                    '--glass': 'rgba(255,255,255,0.05)',
                    '--panel': 'rgba(42, 28, 20, 0.9)',
                    '--panel-2': 'rgba(52, 35, 24, 0.94)',
                    '--panel-strong': '#21160f',
                    '--panel-soft': '#2b1c14',
                    '--sidebar': '#2b1c14',
                    '--rail-bg': '#2f2017',
                    '--editor-bg': '#1f140e',
                    '--gutter-bg': '#1a100b',
                    '--terminal-bg': '#23160f',
                    '--terminal-input-bg': '#2e1e15',
                    '--accent': '#f0a35c',
                    '--accent-2': '#ffd59a',
                    '--accent-blue': '#4a9fe8',
                    '--accent-green': '#5fb865',
                    '--accent-orange': '#f0a35c',
                    '--accent-soft': 'rgba(240,163,92,0.20)',
                    '--accent-soft-border': 'rgba(240,163,92,0.32)',
                    '--text': '#f7ecde',
                    '--text-bright': '#ffd59a',
                    '--muted': '#c7b8a4',
                    '--border': 'rgba(255,255,255,0.1)',
                    '--tree-selected': '#2f4554',
                    '--tree-selected-text': '#ffffff',
                    '--selection-bg': '#214283',
                    '--hover-bg': '#3a2b22',
                    '--glow-1': 'rgba(240,163,92,0.10)',
                    '--glow-2': 'rgba(255,213,154,0.06)',
                    '--font-ui': 'Inter, \"Segoe UI\", \"SF Pro Display\", system-ui, sans-serif',
                    '--font-code': 'ui-monospace, \"SFMono-Regular\", \"Menlo\", \"Monaco\", \"Consolas\", \"Liberation Mono\", \"Courier New\", monospace',
                    '--font-size-ui': '13px',
                    '--font-size-code': '13px'
                }
            }
        };
        const defaultIdeTheme = 'earth';
        const defaultCodeTheme = 'mumps-earth';
        const codeThemes = ['mumps-light', 'mumps-earth', 'mumps-dark'];
        let currentCodeTheme = (() => {
            try {
                const stored = localStorage.getItem('ahmadIDE:theme:code');
                if (stored && codeThemes.includes(stored)) return stored;
                return defaultCodeTheme;
            } catch (e) {
                return defaultCodeTheme;
            }
        })();

        return { ideThemes, defaultIdeTheme, defaultCodeTheme, codeThemes, currentCodeTheme };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.renderer = window.AhmadIDEModules.renderer || {};
        window.AhmadIDEModules.renderer.themes = window.AhmadIDEModules.renderer.themes || {};
        window.AhmadIDEModules.renderer.themes.createThemePresets = createThemePresets;
    }
})();
