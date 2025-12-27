(() => {
    function createEditorToolbarWiring({ deps } = {}) {
        const canSaveActiveTab = deps?.canSaveActiveTab;
        const showToast = deps?.showToast;
        const saveRoutineFlow = deps?.saveRoutineFlow;
        const newRoutineFlow = deps?.newRoutineFlow;
        const appendOutput = deps?.appendOutput;
        const mumpsLinter = deps?.mumpsLinter;
        const hasLintRules = deps?.hasLintRules;
        const applyLintMarkers = deps?.applyLintMarkers;
        const renderProblems = deps?.renderProblems;

        function wireEditorToolbar($, editor, routineState, terminalState) {
            $('#saveRoutineBtn').on('click', async () => {
                if (!canSaveActiveTab()) {
                    showToast('info', 'Diff', 'Diff tabs are read-only');
                    return;
                }
                await saveRoutineFlow(editor, routineState, terminalState);
            });
            $('#undoBtn').on('click', () => {
                editor.trigger('keyboard', 'undo', {});
            });
            $('#redoBtn').on('click', () => {
                editor.trigger('keyboard', 'redo', {});
            });
            $('#newRoutineBtn').on('click', async () => {
                await newRoutineFlow(editor, routineState, terminalState);
            });

            $('#lintBtn').on('click', async () => {
                const code = editor.getValue();
                appendOutput('🧹 Linting...', terminalState);
                const linter = window._mumpsLinter || mumpsLinter;
                if (hasLintRules(linter)) {
                    const res = linter.lint(code || '', { mode: 'edit' });
                    applyLintMarkers(editor.getModel(), res.issues || []);
                    renderProblems((res.issues || []).map(i => ({
                        message: i.message || i.description || '',
                        severity: i.severity || 'info',
                        line: i.line || null,
                        code: i.ruleId || i.code || null
                    })));
                    const summary = res.summary || { errors: 0, warnings: 0, info: 0 };
                    appendOutput(`✓ Lint: ${summary.errors} errors, ${summary.warnings} warnings, ${summary.info} info`, terminalState);
                } else {
                    const res = await window.ahmadIDE.lint(code);
                    if (res.ok) {
                        appendOutput(`✓ ${res.summary}`, terminalState);
                        renderProblems([{ message: res.summary, severity: 'info' }]);
                    } else {
                        appendOutput(`✗ Lint error: ${res.error || res.stderr}`, terminalState);
                        renderProblems([{ message: res.error || 'Lint failed', severity: 'error' }]);
                    }
                }
            });
        }

        return { wireEditorToolbar };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.renderer = window.AhmadIDEModules.renderer || {};
        window.AhmadIDEModules.renderer.editor = window.AhmadIDEModules.renderer.editor || {};
        window.AhmadIDEModules.renderer.editor.createEditorToolbarWiring = createEditorToolbarWiring;
    }
})();
