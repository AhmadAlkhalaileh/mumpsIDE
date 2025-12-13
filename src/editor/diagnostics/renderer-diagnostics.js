(() => {
    function createDiagnosticsManager({ state, deps } = {}) {
        if (!state?.maxLintTextLength || typeof state.maxProblemItems !== 'number') {
            throw new Error('createDiagnosticsManager requires { state.maxLintTextLength, state.maxProblemItems }');
        }
        if (!state?.lastValidatedVersionIdRef || !state?.lintSkipNotifiedRef) {
            throw new Error('createDiagnosticsManager requires { state.lastValidatedVersionIdRef, state.lintSkipNotifiedRef }');
        }
        if (!state?.regex) {
            throw new Error('createDiagnosticsManager requires { state.regex }');
        }

        const showToast = deps?.showToast || (() => { });
        const renderProblems = deps?.renderProblems || (() => { });
        const getMonaco = deps?.getMonaco || (() => (typeof monaco !== 'undefined' ? monaco : null));
        const mumpsLinter = deps?.mumpsLinter || null;
        const MUMPSLexerClass = deps?.MUMPSLexerClass || null;
        const MUMPSParserClass = deps?.MUMPSParserClass || null;

        const maxLintTextLength = state.maxLintTextLength;
        const maxProblemItems = state.maxProblemItems;
        const lastValidatedVersionIdRef = state.lastValidatedVersionIdRef;
        const lintSkipNotifiedRef = state.lintSkipNotifiedRef;

        const RE_DQUOTE = state.regex.RE_DQUOTE;
        const RE_PAREN_OPEN = state.regex.RE_PAREN_OPEN;
        const RE_PAREN_CLOSE = state.regex.RE_PAREN_CLOSE;
        const RE_LINE_START = state.regex.RE_LINE_START;
        const RE_SUSPICIOUS = state.regex.RE_SUSPICIOUS;

        function markerSeverity(sev) {
            const monacoRef = getMonaco();
            if (sev === 'error') return monacoRef.MarkerSeverity.Error;
            if (sev === 'warning') return monacoRef.MarkerSeverity.Warning;
            return monacoRef.MarkerSeverity.Info;
        }

        function normalizeSeverity(sev) {
            if (!sev) return 'info';
            const lower = (sev + '').toLowerCase();
            if (lower.startsWith('err')) return 'error';
            if (lower.startsWith('warn')) return 'warning';
            return 'info';
        }

        function hasLintRules(linter) {
            return !!(linter && linter.rules && Object.keys(linter.rules || {}).length);
        }

        function applyLintMarkers(model, issues) {
            const monacoRef = getMonaco();
            const markers = (issues || []).map(issue => ({
                severity: markerSeverity(issue.severity),
                message: issue.message || issue.description || 'Issue',
                startLineNumber: issue.line || 1,
                startColumn: issue.column || 1,
                endLineNumber: issue.line || 1,
                endColumn: (issue.column || 1) + 1
            }));
            monacoRef.editor.setModelMarkers(model, 'mumps-check', markers);
        }

        function validateMumps(model) {
            if (!model) return;
            const monacoRef = getMonaco();
            const versionId = (typeof model.getVersionId === 'function') ? model.getVersionId() : null;
            if (versionId !== null && versionId === lastValidatedVersionIdRef.value) {
                return;
            }
            const text = model.getValue();
            const isHuge = text.length > maxLintTextLength;
            if (isHuge) {
                monacoRef.editor.setModelMarkers(model, 'mumps-check', []);
                renderProblems([{
                    severity: 'info',
                    message: `Lint disabled for large file (${Math.round(text.length / 1000)} KB)`,
                    line: 1,
                    code: 'LINT_SKIPPED'
                }]);
                if (!lintSkipNotifiedRef.value) {
                    showToast('info', 'Linting paused', 'Large file detected; skipping lint to keep typing responsive.');
                    lintSkipNotifiedRef.value = true;
                }
                lastValidatedVersionIdRef.value = versionId;
                return [];
            }
            // Reset the skip notification only after we are back under the threshold
            if (lintSkipNotifiedRef.value && !isHuge) {
                lintSkipNotifiedRef.value = false;
            }
            lastValidatedVersionIdRef.value = versionId;
            const linter = window._mumpsLinter || mumpsLinter;
            const combinedMarkers = [];
            const problems = [];
            const Parser = window._mumpsParserClass || MUMPSParserClass;

            if (hasLintRules(linter)) {
                const res = linter.lint(text || '', { mode: 'edit' });
                (res.issues || []).forEach(issue => {
                    const sev = normalizeSeverity(issue.severity);
                    problems.push({
                        severity: sev,
                        message: issue.message || issue.description || 'Issue',
                        line: issue.line || null,
                        code: issue.ruleId || issue.code || null
                    });
                    combinedMarkers.push({
                        severity: markerSeverity(sev),
                        message: issue.ruleId ? `[${issue.ruleId}] ${issue.message || issue.description || 'Issue'}` : (issue.message || issue.description || 'Issue'),
                        startLineNumber: issue.line || 1,
                        startColumn: issue.column || 1,
                        endLineNumber: issue.line || 1,
                        endColumn: (issue.column || 1) + 1
                    });
                });
            } else {
                const lines = text.split('\n');
                let openQuotes = 0;
                let parenBalance = 0;
                lines.forEach((line, idx) => {
                    const lineNo = idx + 1;
                    RE_DQUOTE.lastIndex = 0;
                    const quoteCount = (line.match(RE_DQUOTE) || []).length;
                    openQuotes = (openQuotes + quoteCount) % 2;
                    if (openQuotes === 1) {
                        problems.push({
                            severity: 'warning',
                            message: 'Unclosed string literal',
                            line: lineNo,
                            code: 'LEX_UNCLOSED_STRING'
                        });
                        combinedMarkers.push({
                            severity: monacoRef.MarkerSeverity.Warning,
                            message: '[LEX_UNCLOSED_STRING] Unclosed string literal',
                            startLineNumber: lineNo,
                            startColumn: 1,
                            endLineNumber: lineNo,
                            endColumn: line.length + 1
                        });
                    }
                    RE_PAREN_OPEN.lastIndex = 0;
                    RE_PAREN_CLOSE.lastIndex = 0;
                    const opens = (line.match(RE_PAREN_OPEN) || []).length;
                    const closes = (line.match(RE_PAREN_CLOSE) || []).length;
                    parenBalance += opens - closes;
                    if (RE_LINE_START.test(line)) {
                        problems.push({
                            severity: 'warning',
                            message: 'Line should start with a label, command, or comment',
                            line: lineNo,
                            code: 'LEX_LINE_START'
                        });
                        combinedMarkers.push({
                            severity: monacoRef.MarkerSeverity.Warning,
                            message: '[LEX_LINE_START] Line should start with a label, command, or comment',
                            startLineNumber: lineNo,
                            startColumn: 1,
                            endLineNumber: lineNo,
                            endColumn: line.length + 1
                        });
                    }
                    if (RE_SUSPICIOUS.test(line)) {
                        problems.push({
                            severity: 'info',
                            message: 'Suspicious character for MUMPS',
                            line: lineNo,
                            code: 'LEX_SUSPICIOUS_CHAR'
                        });
                        combinedMarkers.push({
                            severity: monacoRef.MarkerSeverity.Info,
                            message: '[LEX_SUSPICIOUS_CHAR] Suspicious character for MUMPS',
                            startLineNumber: lineNo,
                            startColumn: 1,
                            endLineNumber: lineNo,
                            endColumn: line.length + 1
                        });
                    }
                });
                if (parenBalance !== 0) {
                    problems.push({
                        severity: 'warning',
                        message: 'Unbalanced parentheses detected',
                        line: 1,
                        code: 'LEX_PAREN_BALANCE'
                    });
                    combinedMarkers.push({
                        severity: monacoRef.MarkerSeverity.Warning,
                        message: '[LEX_PAREN_BALANCE] Unbalanced parentheses detected',
                        startLineNumber: 1,
                        startColumn: 1,
                        endLineNumber: lines.length,
                        endColumn: 1
                    });
                }
            }

            const Lexer = window._mumpsLexerClass || MUMPSLexerClass;
            if (Lexer) {
                try {
                    const lexer = new Lexer(text || '');
                    lexer.tokenize();
                    (lexer.errors || []).forEach(err => {
                        problems.push({
                            severity: 'error',
                            message: err.message || 'Syntax error',
                            line: err.line || null,
                            code: err.code || 'LEX_ERROR'
                        });
                        combinedMarkers.push({
                            severity: monacoRef.MarkerSeverity.Error,
                            message: `[${err.code || 'LEX_ERROR'}] ${err.message || 'Syntax error'}`,
                            startLineNumber: err.line || 1,
                            startColumn: err.column || 1,
                            endLineNumber: err.line || 1,
                            endColumn: (err.column || 1) + 1
                        });
                    });
                } catch (e) {
                    // ignore lexer failures
                }
            }

            // TEMPORARILY DISABLED: Parser is causing IDE to freeze
            // TODO: Fix MUMPSParser infinite loop issue
            if (false && Parser) {
                try {
                    const parser = new Parser();
                    parser.parse(text || '');
                    (parser.getErrors ? parser.getErrors() : parser.errors || []).forEach(err => {
                        const sev = normalizeSeverity(err.severity || 'error');
                        problems.push({
                            severity: sev,
                            message: err.message || 'Parse error',
                            line: err.line || null,
                            code: err.code || 'PARSE_ERROR'
                        });
                        combinedMarkers.push({
                            severity: markerSeverity(sev),
                            message: `[${err.code || 'PARSE_ERROR'}] ${err.message || 'Parse error'}`,
                            startLineNumber: err.line || 1,
                            startColumn: err.column || 1,
                            endLineNumber: err.line || 1,
                            endColumn: (err.column || 1) + 1
                        });
                    });
                } catch (e) {
                    // ignore parser failures to avoid blocking editing
                }
            }

            const limitedMarkers = combinedMarkers.slice(0, maxProblemItems);
            monacoRef.editor.setModelMarkers(model, 'mumps-check', limitedMarkers);
            renderProblems(problems);
            return limitedMarkers;
        }

        return {
            markerSeverity,
            normalizeSeverity,
            hasLintRules,
            applyLintMarkers,
            validateMumps
        };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.diagnostics = window.AhmadIDEModules.diagnostics || {};
        window.AhmadIDEModules.diagnostics.createDiagnosticsManager = createDiagnosticsManager;
    }
})();

