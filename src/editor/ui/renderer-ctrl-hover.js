(() => {
    function createCtrlHoverManager({ deps } = {}) {
        const getMonaco = deps?.getMonaco || (() => (typeof monaco !== 'undefined' ? monaco : null));
        const parseRoutineReferenceAtPosition = deps?.parseRoutineReferenceAtPosition;
        const goToDeclaration = deps?.goToDeclaration || (async () => false);
        const toggleBreakpoint = deps?.toggleBreakpoint || (() => { });

        if (!parseRoutineReferenceAtPosition || typeof parseRoutineReferenceAtPosition !== 'function') {
            throw new Error('createCtrlHoverManager requires deps.parseRoutineReferenceAtPosition');
        }

        function bindCtrlHoverAndGutter(editor, dbgState) {
            if (!editor) return;
            const monaco = getMonaco();
            if (!monaco) {
                throw new Error('bindCtrlHoverAndGutter requires Monaco');
            }

            // Ctrl+Hover: Change cursor to pointer only when hovering over valid tag/routine
            let isCtrlPressed = false;
            let currentHoverDecoration = [];

            document.addEventListener('keydown', (e) => {
                if (e.ctrlKey || e.metaKey) {
                    isCtrlPressed = true;
                }
            });

            document.addEventListener('keyup', (e) => {
                if (!e.ctrlKey && !e.metaKey) {
                    isCtrlPressed = false;
                    // Clear cursor override
                    const editorDom = editor.getDomNode();
                    if (editorDom) {
                        editorDom.style.cursor = '';
                    }
                    // Clear hover decoration
                    if (currentHoverDecoration.length > 0) {
                        editor.deltaDecorations(currentHoverDecoration, []);
                        currentHoverDecoration = [];
                    }
                }
            });

            // Track mouse movement to detect hovering over clickable targets
            editor.onMouseMove((e) => {
                if (!isCtrlPressed || !e.target.position) {
                    // Clear cursor and decoration if not Ctrl+hovering
                    const editorDom = editor.getDomNode();
                    if (editorDom && !isCtrlPressed) {
                        editorDom.style.cursor = '';
                    }
                    if (currentHoverDecoration.length > 0 && !isCtrlPressed) {
                        editor.deltaDecorations(currentHoverDecoration, []);
                        currentHoverDecoration = [];
                    }
                    return;
                }

                const model = editor.getModel();
                if (!model) return;

                const ref = parseRoutineReferenceAtPosition(model, e.target.position);

                if (ref) {
                    // Valid target detected - change cursor to pointer
                    const editorDom = editor.getDomNode();
                    if (editorDom) {
                        editorDom.style.cursor = 'pointer';
                    }

                    // Add underline decoration to show it's clickable
                    const lineContent = model.getLineContent(e.target.position.lineNumber);
                    const column = e.target.position.column;

                    // Find the exact range of the tag/routine text
                    let startCol = column;
                    let endCol = column;

                    // Expand left to find start of word
                    while (startCol > 1 && /[A-Z0-9%^]/.test(lineContent[startCol - 2])) {
                        startCol--;
                    }
                    // Expand right to find end of word
                    while (endCol <= lineContent.length && /[A-Z0-9%^]/.test(lineContent[endCol - 1])) {
                        endCol++;
                    }

                    currentHoverDecoration = editor.deltaDecorations(currentHoverDecoration, [{
                        range: new monaco.Range(
                            e.target.position.lineNumber,
                            startCol,
                            e.target.position.lineNumber,
                            endCol
                        ),
                        options: {
                            inlineClassName: 'ctrl-hover-underline'
                        }
                    }]);
                } else {
                    // No valid target - reset cursor
                    const editorDom = editor.getDomNode();
                    if (editorDom) {
                        editorDom.style.cursor = '';
                    }
                    if (currentHoverDecoration.length > 0) {
                        editor.deltaDecorations(currentHoverDecoration, []);
                        currentHoverDecoration = [];
                    }
                }
            });

            // --- Breakpoint gutter toggle + Ctrl+Click navigation ---
            editor.onMouseDown(async (e) => {
                const t = e.target.type;

                // Handle breakpoint toggle in gutter
                if (
                    t === monaco.editor.MouseTargetType.GUTTER_GLYPH_MARGIN ||
                    t === monaco.editor.MouseTargetType.GUTTER_LINE_NUMBERS
                ) {
                    const line = e.target.position && e.target.position.lineNumber;
                    if (!line) return;
                    toggleBreakpoint(line, dbgState, editor);
                    e.event.preventDefault();
                    return;
                }

                // Handle Ctrl+Click navigation
                const isCtrlPressed = e.event.ctrlKey || e.event.metaKey;
                if (isCtrlPressed && e.target.position) {
                    const handled = await goToDeclaration(editor, e.target.position, { silentIfMissing: true });
                    if (handled) {
                        e.event.preventDefault();
                        e.event.stopPropagation();
                    }
                }
            });
        }

        return {
            bindCtrlHoverAndGutter
        };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.ui = window.AhmadIDEModules.ui || {};
        window.AhmadIDEModules.ui.createCtrlHoverManager = createCtrlHoverManager;
    }
})();
