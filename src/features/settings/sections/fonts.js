(() => {
    function renderFontsSection(ctx) {
        const renderUIFontsGroup = window.AhmadIDEModules?.features?.settings?.sections?.renderUIFontsGroup;
        const renderEditorFontsGroup = window.AhmadIDEModules?.features?.settings?.sections?.renderEditorFontsGroup;
        const renderTerminalFontsGroup = window.AhmadIDEModules?.features?.settings?.sections?.renderTerminalFontsGroup;
        if (!renderUIFontsGroup || !renderEditorFontsGroup || !renderTerminalFontsGroup) {
            throw new Error('Font sections missing (ui/editor/terminal)');
        }

        const root = document.createElement('div');
        const ui = renderUIFontsGroup(ctx);
        const editor = renderEditorFontsGroup(ctx);
        const terminal = renderTerminalFontsGroup(ctx);
        root.appendChild(ui.group);
        root.appendChild(editor.group);
        root.appendChild(terminal.group);

        const applyPreviewStyles = () => {
            ui.applyPreviewStyles?.();
            editor.applyPreviewStyles?.();
            terminal.applyPreviewStyles?.();
        };
        applyPreviewStyles();

        return { root, applyPreviewStyles };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.features = window.AhmadIDEModules.features || {};
        window.AhmadIDEModules.features.settings = window.AhmadIDEModules.features.settings || {};
        window.AhmadIDEModules.features.settings.sections = window.AhmadIDEModules.features.settings.sections || {};
        window.AhmadIDEModules.features.settings.sections.renderFontsSection = renderFontsSection;
    }
})();
