(() => {
    function createEditorMonacoActions() {
        function registerEditorActions(editor) {
            // Add -style context menu actions
            editor.addAction({
                id: 'cut',
                label: 'Cut',
                keybindings: [monaco.KeyMod.CtrlCmd | monaco.KeyCode.KeyX],
                run: (ed) => {
                    document.execCommand('cut');
                }
            });

            editor.addAction({
                id: 'copy',
                label: 'Copy',
                keybindings: [monaco.KeyMod.CtrlCmd | monaco.KeyCode.KeyC],
                run: (ed) => {
                    document.execCommand('copy');
                }
            });

            editor.addAction({
                id: 'paste',
                label: 'Paste',
                keybindings: [monaco.KeyMod.CtrlCmd | monaco.KeyCode.KeyV],
                run: (ed) => {
                    document.execCommand('paste');
                }
            });

            editor.addAction({
                id: 'selectAll',
                label: 'Select All',
                keybindings: [monaco.KeyMod.CtrlCmd | monaco.KeyCode.KeyA],
                run: (ed) => {
                    ed.setSelection(ed.getModel().getFullModelRange());
                }
            });

            // Note: Ctrl+S is handled by global keydown handler to avoid conflicts

            editor.addAction({
                id: 'commentLine',
                label: 'Comment Line',
                keybindings: [monaco.KeyMod.CtrlCmd | monaco.KeyCode.Slash],
                run: (ed) => {
                    ed.trigger('keyboard', 'editor.action.commentLine', {});
                }
            });

            editor.addAction({
                id: 'formatDocument',
                label: 'Reformat Code',
                keybindings: [monaco.KeyMod.CtrlCmd | monaco.KeyMod.Alt | monaco.KeyCode.KeyL],
                run: (ed) => {
                    ed.trigger('keyboard', 'editor.action.formatDocument', {});
                }
            });

            editor.addAction({
                id: 'gotoLine',
                label: 'Go to Line...',
                keybindings: [monaco.KeyMod.CtrlCmd | monaco.KeyCode.KeyG],
                run: (ed) => {
                    ed.trigger('keyboard', 'editor.action.gotoLine', {});
                }
            });

            editor.addAction({
                id: 'findReplace',
                label: 'Find and Replace...',
                keybindings: [monaco.KeyMod.CtrlCmd | monaco.KeyCode.KeyR],
                run: (ed) => {
                    ed.trigger('keyboard', 'editor.action.startFindReplaceAction', {});
                }
            });

            editor.addAction({
                id: 'deleteLineAction',
                label: 'Delete Line',
                keybindings: [monaco.KeyMod.CtrlCmd | monaco.KeyCode.KeyY],
                run: (ed) => {
                    ed.trigger('keyboard', 'editor.action.deleteLines', {});
                }
            });

            editor.addAction({
                id: 'duplicateLine',
                label: 'Duplicate Line',
                keybindings: [monaco.KeyMod.CtrlCmd | monaco.KeyCode.KeyD],
                run: (ed) => {
                    const selection = ed.getSelection();
                    const lineNumber = selection.startLineNumber;
                    const lineContent = ed.getModel().getLineContent(lineNumber);
                    const position = { lineNumber: lineNumber, column: 1 };
                    ed.executeEdits('duplicate-line', [{
                        range: new monaco.Range(lineNumber, 1, lineNumber, 1),
                        text: lineContent + '\n'
                    }]);
                }
            });

            editor.addAction({
                id: 'moveLineUp',
                label: 'Move Line Up',
                keybindings: [monaco.KeyMod.Alt | monaco.KeyMod.Shift | monaco.KeyCode.UpArrow],
                run: (ed) => {
                    ed.trigger('keyboard', 'editor.action.moveLinesUpAction', {});
                }
            });

            editor.addAction({
                id: 'moveLineDown',
                label: 'Move Line Down',
                keybindings: [monaco.KeyMod.Alt | monaco.KeyMod.Shift | monaco.KeyCode.DownArrow],
                run: (ed) => {
                    ed.trigger('keyboard', 'editor.action.moveLinesDownAction', {});
                }
            });
        }

        return { registerEditorActions };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.renderer = window.AhmadIDEModules.renderer || {};
        window.AhmadIDEModules.renderer.editor = window.AhmadIDEModules.renderer.editor || {};
        window.AhmadIDEModules.renderer.editor.createEditorMonacoActions = createEditorMonacoActions;
    }
})();
