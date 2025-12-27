(() => {
    function createMumpsMonacoManager({ deps } = {}) {
        const $ = deps?.$ || (typeof window !== 'undefined' ? (window.$ || window.jQuery || null) : null);
        const getMonaco = deps?.getMonaco || (() => (typeof monaco !== 'undefined' ? monaco : null));
        const getMumpsCommands = (() => {
            let cached = null;
            return () => {
                if (cached) return cached;
                try {
                    const Lexer = (typeof MUMPSLexer !== 'undefined')
                        ? MUMPSLexer
                        : (typeof window !== 'undefined' ? window.MUMPSLexer : null);
                    if (typeof Lexer === 'function') {
                        const lx = new Lexer('');
                        if (lx?.commands && typeof lx.commands.has === 'function') {
                            cached = lx.commands;
                            return cached;
                        }
                    }
                } catch (_) { }
                cached = new Set([
                    'B', 'BREAK',
                    'C', 'CLOSE',
                    'D', 'DO',
                    'E', 'ELSE',
                    'F', 'FOR',
                    'G', 'GOTO',
                    'H', 'HALT', 'HANG',
                    'I', 'IF',
                    'J', 'JOB',
                    'K', 'KILL',
                    'L', 'LOCK',
                    'M', 'MERGE',
                    'N', 'NEW',
                    'O', 'OPEN',
                    'Q', 'QUIT',
                    'R', 'READ',
                    'S', 'SET',
                    'TC', 'TCOMMIT',
                    'TRE', 'TRESTART',
                    'TRO', 'TROLLBACK',
                    'TS', 'TSTART',
                    'U', 'USE',
                    'V', 'VIEW',
                    'W', 'WRITE',
                    'X', 'XECUTE',
                    // Common Z-commands
                    'ZB', 'ZBREAK',
                    'ZCO', 'ZCOMPILE',
                    'ZCON', 'ZCONTINUE',
                    'ZD', 'ZDEALLOCATE',
                    'ZE', 'ZEDIT',
                    'ZG', 'ZGOTO',
                    'ZH', 'ZHALT',
                    'ZHE', 'ZHELP',
                    'ZK', 'ZKILL',
                    'ZL', 'ZLINK',
                    'ZM', 'ZMESSAGE',
                    'ZP', 'ZPRINT',
                    'ZRU', 'ZRUPDATE',
                    'ZSH', 'ZSHOW',
                    'ZST', 'ZSTEP',
                    'ZSY', 'ZSYSTEM',
                    'ZTCO', 'ZTCOMMIT',
                    'ZTS', 'ZTSTART',
                    'ZWI', 'ZWITHDRAW',
                    'ZW', 'ZWRITE'
                ]);
                return cached;
            };
        })();

        let mumpsAutocompleteCache = null;
        async function loadAutocompleteData() {
            if (mumpsAutocompleteCache) return mumpsAutocompleteCache;
            const url = './assets/mumps/autocomplete-data.json';
            try {
                if ($ && $.getJSON) {
                    mumpsAutocompleteCache = await new Promise((resolve, reject) => {
                        $.getJSON(url, resolve).fail((_, textStatus, err) => reject(err || textStatus));
                    });
                } else {
                    const res = await fetch(url);
                    if (!res.ok) throw new Error(res.statusText);
                    mumpsAutocompleteCache = await res.json();
                }
            } catch (e) {
                mumpsAutocompleteCache = null;
            }
            return mumpsAutocompleteCache;
        }

        function sampleMumps() {
            return [
                'HELLO ; sample routine',
                '    WRITE "Hello, Ahmad IDE!",!',
                '    SET X=1',
                '    IF X=1 WRITE "X is one",!',
                '    QUIT'
            ].join('\n');
        }

        let formattingRegistered = false;
        let foldingRegistered = false;

        const stripComment = (line) => {
            const s = String(line || '');
            let inString = false;
            for (let i = 0; i < s.length; i++) {
                const ch = s[i];
                if (ch === '"') inString = !inString;
                if (ch === ';' && !inString) return s.slice(0, i);
            }
            return s;
        };

        const isTagLine = (line) => {
            const s = String(line || '');
            if (!s) return false;
            if (s[0] === ';') return false;
            if (/^\s/.test(s)) return false;
            return /^[A-Za-z%][A-Za-z0-9]*/.test(s);
        };

        const isTopLevelQuitLine = (line) => {
            const code = stripComment(line);
            const trimmedLeft = String(code || '').trimStart();
            if (!trimmedLeft) return false;
            if (trimmedLeft.startsWith('.')) return false;
            return /^(QUIT|Q)\b/i.test(trimmedLeft);
        };

        const hasQuitCommandInTagLine = (line) => {
            const s = String(line || '');
            if (!isTagLine(s)) return false;
            const code = stripComment(s);
            const m = code.match(/^([A-Za-z%][A-Za-z0-9]*)(\([^)]*\))?(.*)$/);
            if (!m) return false;
            const commandField = String(m[3] || '').trimStart();
            if (!commandField) return false;
            if (commandField.startsWith(';')) return false;
            return /(?:^|\s)(QUIT|Q)\b/i.test(commandField);
        };

        const computeTagFoldingRanges = (lines) => {
            const srcLines = Array.isArray(lines) ? lines : [];
            const tagLines = [];
            for (let i = 0; i < srcLines.length; i++) {
                if (isTagLine(srcLines[i])) tagLines.push(i + 1); // 1-based line numbers
            }

            const out = [];
            for (let i = 0; i < tagLines.length; i++) {
                const startLine = tagLines[i];
                const nextTagLine = tagLines[i + 1] || (srcLines.length + 1);
                const endLine = Math.max(startLine, nextTagLine - 1);

                let hasQuit = hasQuitCommandInTagLine(srcLines[startLine - 1] || '');
                for (let ln = startLine + 1; ln <= endLine; ln++) {
                    if (hasQuit) break;
                    const lineText = srcLines[ln - 1] || '';
                    if (isTopLevelQuitLine(lineText)) {
                        hasQuit = true;
                    }
                }

                if (hasQuit && endLine > startLine) {
                    out.push({
                        start: startLine,
                        end: endLine
                    });
                }
            }

            return out;
        };

        const formatMumpsText = (text, { tabSize = 4 } = {}) => {
            const norm = String(text ?? '').replace(/\r\n/g, '\n').replace(/\r/g, '\n');
            const lines = norm.split('\n');

            const trimRight = (s) => String(s || '').replace(/[ \t]+$/g, '');
            const COMMANDS = new Set([
                // Full commands
                'BREAK', 'B',
                'CLOSE', 'C',
                'DO', 'D',
                'ELSE', 'E',
                'FOR', 'F',
                'GOTO', 'G',
                'HALT',
                'HANG', 'H',
                'IF', 'I',
                'JOB', 'J',
                'KILL', 'K',
                'LOCK', 'L',
                'MERGE', 'M',
                'NEW', 'N',
                'OPEN', 'O',
                'QUIT', 'Q',
                'READ', 'R',
                'SET', 'S',
                'USE', 'U',
                'VIEW', 'V',
                'WRITE', 'W',
                'XECUTE', 'X',
                // Common Z-commands (don’t try to be exhaustive)
                'ZGOTO', 'ZG',
                'ZWRITE', 'ZW',
                'ZPRINT', 'ZP',
                'ZBREAK', 'ZB',
                'ZHALT', 'ZH',
                'ZLINK', 'ZL'
            ]);
            const isCommandToken = (tok) => COMMANDS.has(String(tok || '').toUpperCase());

            const formatCommandField = (field) => {
                const f = String(field || '').trimStart();
                if (!f) return '';
                const m = f.match(/^(\.+)(.*)$/);
                if (!m) return f;
                const dots = m[1] || '';
                const rest = String(m[2] || '').trimStart();
                return rest ? `${dots} ${rest}` : dots;
            };

            const clampInt = (n, min, max) => Math.max(min, Math.min(max, n));

            const detectIndentSize = () => {
                const freq = new Map();
                for (const line of lines) {
                    const s = String(line || '');
                    const m = s.match(/^(\s+)[.;A-Za-z%$]/);
                    if (!m) continue;
                    const len = m[1].length;
                    if (!len) continue;
                    if (len > 24) continue;
                    freq.set(len, (freq.get(len) || 0) + 1);
                }
                let bestLen = 0;
                let bestCount = -1;
                for (const [len, count] of freq.entries()) {
                    if (count > bestCount || (count === bestCount && len < bestLen)) {
                        bestLen = len;
                        bestCount = count;
                    }
                }
                const fallback = clampInt(Number(tabSize) || 4, 1, 24);
                // MUMPS typically uses 1 leading space for the command field.
                // If we cannot infer indentation from the text, default to 1.
                return bestLen > 0 ? bestLen : 1;
            };

            const INDENT = ' '.repeat(detectIndentSize());

            const formatIndentedLine = (line) => {
                const s = String(line || '');
                const trimmed = s.trimStart();
                if (!trimmed) return '';
                if (trimmed.startsWith(';')) return `${INDENT}${trimmed}`;
                if (trimmed.startsWith('.')) return `${INDENT}${formatCommandField(trimmed)}`;
                return `${INDENT}${formatCommandField(trimmed)}`;
            };

            const isLabelLikeToken = (raw) => {
                const m = String(raw || '').match(/^([A-Za-z%][A-Za-z0-9]*)/);
                if (!m) return { ok: false };
                const name = m[1];
                const after = String(raw).slice(name.length);
                // Labels at column 1 must be followed by whitespace, params "(...)", comment ";", or EOL.
                if (!after) return { ok: true, name, after: '' };
                if (after.startsWith('(')) return { ok: true, name, after };
                if (/^\s/.test(after)) return { ok: true, name, after };
                if (after.startsWith(';')) return { ok: true, name, after };
                return { ok: false };
            };

            const formatLine = (line) => {
                const raw = trimRight(line);
                if (!raw.trim()) return '';

                const startsWithWs = /^\s/.test(raw);
                if (startsWithWs) return formatIndentedLine(raw);

                // Keep top-level comment lines at column 1.
                if (raw.startsWith(';')) return raw;

                // Lines that start with dot at column 1 are still command-field lines.
                if (raw.startsWith('.')) return formatIndentedLine(raw);

                const info = isLabelLikeToken(raw);
                if (info.ok) {
                    // If the label name is a command token, it's often a missing-indent command line.
                    // Heuristic: treat it as a command line unless it clearly looks like a label-with-params.
                    if (isCommandToken(info.name) && !String(info.after || '').startsWith('(')) {
                        return formatIndentedLine(raw);
                    }
                    // Label line: preserve spacing (only trim right).
                    return raw;
                }

                // Unknown top-level content: preserve.
                return raw;
            };

            const out = lines.map(formatLine).join('\n');
            return out;
        };

        function registerMumpsFormatting() {
            if (formattingRegistered) return;
            formattingRegistered = true;

            const monacoRef = getMonaco();
            if (!monacoRef?.languages?.registerDocumentFormattingEditProvider) return;

            monacoRef.languages.registerDocumentFormattingEditProvider('mumps', {
                provideDocumentFormattingEdits: (model, options, token) => {
                    if (!model) return [];
                    const original = model.getValue();
                    const formatted = formatMumpsText(original, options || {});
                    if (formatted === original) return [];
                    return [{
                        range: model.getFullModelRange(),
                        text: formatted
                    }];
                }
            });

            if (monacoRef.languages.registerDocumentRangeFormattingEditProvider) {
                monacoRef.languages.registerDocumentRangeFormattingEditProvider('mumps', {
                    provideDocumentRangeFormattingEdits: (model, range, options, token) => {
                        if (!model || !range) return [];
                        const startLine = Math.max(1, Math.min(model.getLineCount(), range.startLineNumber));
                        const endLine = Math.max(startLine, Math.min(model.getLineCount(), range.endLineNumber));
                        const fullLineRange = new monacoRef.Range(startLine, 1, endLine, model.getLineMaxColumn(endLine));
                        const original = model.getValueInRange(fullLineRange);
                        const formatted = formatMumpsText(original, options || {});
                        if (formatted === original) return [];
                        return [{ range: fullLineRange, text: formatted }];
                    }
                });
            }
        }

        function registerMumpsLanguage() {
            const monacoRef = getMonaco();
            monacoRef.languages.register({
                id: 'mumps',
                extensions: ['.m', '.mps', '.mumps'],
                aliases: ['MUMPS', 'M'],
            });

            monacoRef.languages.setLanguageConfiguration('mumps', {
                comments: { lineComment: ';' },
                brackets: [['(', ')']],
                autoClosingPairs: [
                    { open: '(', close: ')' },
                    { open: '"', close: '"', notIn: ['string'] }
                ],
                surroundingPairs: [
                    { open: '(', close: ')' },
                    { open: '"', close: '"' }
                ],
                wordPattern: /\$?[A-Za-z%][\w.%]*/,
                indentationRules: {
                    increaseIndentPattern: /^\s*\b(IF|ELSE|FOR|DO)\b.*$/i,
                    decreaseIndentPattern: /^\s*\b(QUIT|Q)\b/i
                }
            });

            monacoRef.languages.setMonarchTokensProvider('mumps', {
                defaultToken: '',
                ignoreCase: true,
                tokenizer: {
                    root: [
                        [/^[A-Za-z%][A-Za-z0-9]*/, 'label'],
                        [/;.*/, 'comment'],
                        [/"([^"]|"")*"/, 'string'],
                        [/\$[A-Z][A-Z0-9]*/, 'predefined'],
                        [/\b(SET|S|NEW|N|KILL|K|DO|D|IF|ELSE|FOR|F|QUIT|Q|WRITE|W|READ|R|GOTO|G|HANG|H|OPEN|O|CLOSE|C|MERGE|M|VIEW|USE|LOCK|L|XECUTE|X)\b/, 'keyword'],
                        [/[0-9]+(\.[0-9]+)?/, 'number'],
                        [/\^[A-Za-z][\w]*/, 'type.identifier'],
                        [/[$A-Za-z%][\w.]*/, 'identifier'],
                    ]
                }
            });

            // Formatting (Format Code / Reformat)
            registerMumpsFormatting();

            // Folding (Tags): fold tag blocks that have a top-level QUIT/Q.
            if (!foldingRegistered && monacoRef?.languages?.registerFoldingRangeProvider) {
                foldingRegistered = true;
                monacoRef.languages.registerFoldingRangeProvider('mumps', {
                    provideFoldingRanges: (model, context, token) => {
                        try {
                            const lineCount = model?.getLineCount?.() || 0;
                            if (!lineCount) return [];
                            const lines = [];
                            for (let i = 1; i <= lineCount; i++) {
                                lines.push(model.getLineContent(i));
                            }
                            const ranges = computeTagFoldingRanges(lines);
                            const kind = monacoRef.languages.FoldingRangeKind?.Region;
                            return ranges.map(r => (kind ? { ...r, kind } : r));
                        } catch (_) {
                            return [];
                        }
                    }
                });
            }

            // Refactor: Rename Symbol (F2)
            try {
                const renameFactory = (typeof window !== 'undefined')
                    ? window.AhmadIDEModules?.mumpsMonaco?.createMumpsRenameProvider
                    : null;
                const renameProvider = (typeof renameFactory === 'function')
                    ? renameFactory({ deps: { getMonaco } })
                    : null;
                renameProvider?.registerMumpsRenameProvider?.();
            } catch (_) { }
        }

        function registerMumpsThemes() {
            const monacoRef = getMonaco();
            monacoRef.editor.defineTheme('mumps-light', {
                base: 'vs',
                inherit: true,
                rules: [
                    { token: 'comment', foreground: '8c8c8c', fontStyle: 'italic' },
                    { token: 'string', foreground: '6a8759' },
                    { token: 'number', foreground: '1750eb' },
                    { token: 'keyword', foreground: '0033b3', fontStyle: 'bold' },
                    { token: 'predefined', foreground: '871094', fontStyle: 'bold' },
                    { token: 'label', foreground: '0033b3', fontStyle: 'bold' },
                    { token: 'type.identifier', foreground: '000000' },
                ],
                colors: {
                    'editor.background': '#ffffff',
                    'editorGutter.background': '#f0f0f0',
                    'editorLineNumber.foreground': '#999999',
                    'editorLineNumber.activeForeground': '#4d4d4d',
                    'editor.selectionBackground': '#a6d2ff',
                    'editor.selectionHighlightBackground': '#e8f2ff',
                    'editor.inactiveSelectionBackground': '#d4d4d4',
                    'editor.lineHighlightBackground': '#fcfcfc',
                    'editor.lineHighlightBorder': '#efefef',
                    'editorCursor.foreground': '#000000',
                    'editorBracketMatch.border': '#4a9eff',
                    'editorIndentGuide.background': '#d3d3d3',
                    'editorIndentGuide.activeBackground': '#939393',
                    'editorWhitespace.foreground': '#d3d3d3'
                }
            });

            monacoRef.editor.defineTheme('mumps-dark', {
                base: 'vs-dark',
                inherit: true,
                rules: [
                    { token: 'comment', foreground: '808080', fontStyle: 'italic' },
                    { token: 'string', foreground: '6a8759' },
                    { token: 'number', foreground: '6897bb' },
                    { token: 'keyword', foreground: 'cc7832', fontStyle: 'bold' },
                    { token: 'predefined', foreground: 'ffc66d', fontStyle: 'bold' },
                    { token: 'label', foreground: 'ffc66d', fontStyle: 'bold' },
                    { token: 'type.identifier', foreground: 'a9b7c6' },
                ],
                colors: {
                    'editor.background': '#2b2b2b',
                    'editorGutter.background': '#313335',
                    'editorLineNumber.foreground': '#606366',
                    'editorLineNumber.activeForeground': '#a4a3a3',
                    'editor.selectionBackground': '#214283',
                    'editor.selectionHighlightBackground': '#3a3d41',
                    'editor.inactiveSelectionBackground': '#3a3d41',
                    'editor.lineHighlightBackground': '#323232',
                    'editor.lineHighlightBorder': '#3a3d41',
                    'editorCursor.foreground': '#bbbbbb',
                    'editorBracketMatch.border': '#4a9eff',
                    'editorIndentGuide.background': '#3c3f41',
                    'editorIndentGuide.activeBackground': '#515658',
                    'editorWhitespace.foreground': '#3c3f41'
                }
            });

            monacoRef.editor.defineTheme('mumps-earth', {
                base: 'vs-dark',
                inherit: true,
                rules: [
                    { token: 'comment', foreground: '6272a4', fontStyle: 'italic' },
                    { token: 'string', foreground: 'f1fa8c' },
                    { token: 'number', foreground: 'bd93f9' },
                    { token: 'keyword', foreground: 'ff79c6', fontStyle: 'bold' },
                    { token: 'predefined', foreground: '8be9fd', fontStyle: 'bold' },
                    { token: 'label', foreground: '50fa7b', fontStyle: 'bold' },
                    { token: 'type.identifier', foreground: 'f8f8f2' },
                ],
                colors: {
                    'editor.background': '#282a36',
                    'editorGutter.background': '#282a36',
                    'editorLineNumber.foreground': '#6272a4',
                    'editorLineNumber.activeForeground': '#f8f8f2',
                    'editor.selectionBackground': '#44475a',
                    'editor.selectionHighlightBackground': '#44475a',
                    'editor.inactiveSelectionBackground': '#44475a',
                    'editor.lineHighlightBackground': '#343746',
                    'editor.lineHighlightBorder': '#44475a',
                    'editorCursor.foreground': '#f8f8f2',
                    'editorBracketMatch.border': '#bd93f9',
                    'editorIndentGuide.background': '#44475a',
                    'editorIndentGuide.activeBackground': '#6272a4',
                    'editorWhitespace.foreground': '#44475a'
                }
            });
        }

        function registerMumpsCompletion(data) {
            const monacoRef = getMonaco();
            const fallbackKeywords = [
                'SET', 'NEW', 'KILL', 'DO', 'IF', 'ELSE', 'FOR', 'QUIT',
                'WRITE', 'READ', 'GOTO', 'HANG', 'OPEN', 'CLOSE', 'MERGE',
                'VIEW', 'USE', 'LOCK', 'XECUTE', 'BREAK', 'HALT', 'JOB'
            ];
            const fallbackSysvars = [
                '$T', '$D', '$O', '$P', '$L', '$E', '$JOB', '$IO', '$ZT', '$ZB', '$ZEOF', '$ZTRAP', '$ZERROR',
                '$PIECE', '$EXTRACT', '$FIND', '$QLENGTH', '$QSUBSCRIPT', '$QUERY', '$ZDATE', '$ZTIME', '$HOROLOG'
            ];
            const fallbackSnippets = [
                {
                    label: 'IF/ELSE',
                    kind: monacoRef.languages.CompletionItemKind.Snippet,
                    insertText: 'IF ${1:condition} {\n  ${2:; code}\n} ELSE  {\n  ${3:; code}\n}\n',
                    documentation: 'IF/ELSE structure',
                    insertTextRules: monacoRef.languages.CompletionItemInsertTextRule.InsertAsSnippet
                },
                {
                    label: 'FOR loop',
                    kind: monacoRef.languages.CompletionItemKind.Snippet,
                    insertText: 'FOR ${1:i}=1:1:${2:n} {\n  ${3:; code}\n}\n',
                    documentation: 'FOR loop snippet',
                    insertTextRules: monacoRef.languages.CompletionItemInsertTextRule.InsertAsSnippet
                },
                {
                    label: 'Label template',
                    kind: monacoRef.languages.CompletionItemKind.Snippet,
                    insertText: 'MAIN ; Routine\n    SET ${1:var}=0\n    QUIT\n',
                    documentation: 'Simple routine template',
                    insertTextRules: monacoRef.languages.CompletionItemInsertTextRule.InsertAsSnippet
                }
            ];

            const kindMap = {
                Command: monacoRef.languages.CompletionItemKind.Keyword,
                Function: monacoRef.languages.CompletionItemKind.Function,
                Variable: monacoRef.languages.CompletionItemKind.Variable,
                Snippet: monacoRef.languages.CompletionItemKind.Snippet
            };

            const buildSuggestion = (entry) => {
                if (!entry || !entry.label) return null;
                const insertText = entry.insertText || entry.label;
                const isSnippet = entry.kind === 'Snippet' || /\$\{\d+:?/.test(insertText);
                return {
                    label: entry.label,
                    kind: kindMap[entry.kind] || monacoRef.languages.CompletionItemKind.Text,
                    insertText,
                    detail: entry.detail || entry.abbr || '',
                    documentation: entry.documentation || '',
                    filterText: entry.abbr || entry.label,
                    sortText: entry.abbr ? `0_${entry.abbr}` : undefined,
                    insertTextRules: isSnippet ? monacoRef.languages.CompletionItemInsertTextRule.InsertAsSnippet : undefined
                };
            };

            const buildDataset = () => {
                const suggestions = [];
                if (data && typeof data === 'object') {
                    ['commands', 'intrinsicFunctions', 'intrinsicVariables', 'snippets'].forEach((key) => {
                        (data[key] || []).forEach((entry) => {
                            const sug = buildSuggestion(entry);
                            if (sug) suggestions.push(sug);
                        });
                    });
                }

                if (!suggestions.length) {
                    fallbackKeywords.forEach(k => {
                        suggestions.push({
                            label: k,
                            kind: monacoRef.languages.CompletionItemKind.Keyword,
                            insertText: k + ' ',
                            documentation: `${k} command`
                        });
                    });
                    fallbackSysvars.forEach(v => {
                        suggestions.push({
                            label: v,
                            kind: monacoRef.languages.CompletionItemKind.Variable,
                            insertText: v,
                            documentation: 'System variable'
                        });
                    });
                    suggestions.push(...fallbackSnippets);
                }
                return suggestions;
            };

            monacoRef.languages.registerCompletionItemProvider('mumps', {
                triggerCharacters: [' ', '$', '^', '.'],
                provideCompletionItems: (model, position, context, token) => {
                    // Get the word range to ensure proper replacement
                    const word = model.getWordUntilPosition(position);
                    const range = {
                        startLineNumber: position.lineNumber,
                        endLineNumber: position.lineNumber,
                        startColumn: word.startColumn,
                        endColumn: word.endColumn
                    };

                    // Add range to each suggestion to replace the typed word
                    const suggestions = buildDataset().map(sug => ({
                        ...sug,
                        range: range
                    }));

                    return { suggestions };
                }
            });
        }

        function attachDotIndentGuides(editor, opts = {}) {
            const monacoRef = getMonaco();
            if (!monacoRef || !editor) return null;

            if (editor.__mumpsDotIndentGuides) {
                return editor.__mumpsDotIndentGuides;
            }

            const MAX_LEVEL = Math.max(1, Math.min(4, Number(opts.maxLevel ?? 4) || 4));
            let disposed = false;
            let timer = null;
            let scheduled = false;

            const collection = (typeof editor.createDecorationsCollection === 'function')
                ? editor.createDecorationsCollection([])
                : null;
            let legacyDecorations = [];

            const dotLevelForLine = (line) => {
                const s = String(line || '');
                const m = s.match(/^\s*(\.+)(?=\s*[A-Za-z%$;])/);
                if (!m) return 0;
                return m[1].length || 0;
            };

            const classForLevel = (level) => {
                const lvl = Math.max(0, Math.min(MAX_LEVEL, level));
                return lvl > 0 ? `mumps-dot-indent mumps-dot-indent-${lvl}` : '';
            };

            const update = () => {
                scheduled = false;
                if (disposed) return;

                const model = editor.getModel?.();
                if (!model) {
                    try { collection?.clear?.(); } catch (_) { }
                    if (!collection && legacyDecorations.length) {
                        try { legacyDecorations = editor.deltaDecorations(legacyDecorations, []); } catch (_) { }
                    }
                    return;
                }

                const lineCount = model.getLineCount?.() || 0;
                const visibleRanges = editor.getVisibleRanges?.() || [];
                const decs = [];

                for (const vr of visibleRanges) {
                    const start = Math.max(1, Math.min(lineCount, vr.startLineNumber));
                    const end = Math.max(1, Math.min(lineCount, vr.endLineNumber));
                    for (let ln = start; ln <= end; ln++) {
                        const text = model.getLineContent(ln);
                        const level = dotLevelForLine(text);
                        if (!level) continue;
                        const cls = classForLevel(level);
                        if (!cls) continue;
                        decs.push({
                            range: new monacoRef.Range(ln, 1, ln, 1),
                            options: {
                                isWholeLine: true,
                                linesDecorationsClassName: cls
                            }
                        });
                    }
                }

                try {
                    if (collection) {
                        collection.set(decs);
                    } else {
                        legacyDecorations = editor.deltaDecorations(legacyDecorations, decs);
                    }
                } catch (_) { }
            };

            const schedule = () => {
                if (disposed) return;
                if (scheduled) return;
                scheduled = true;
                if (timer) clearTimeout(timer);
                timer = setTimeout(update, 60);
            };

            const disposables = [];
            try {
                disposables.push(editor.onDidScrollChange?.(schedule));
                disposables.push(editor.onDidChangeModelContent?.(schedule));
                disposables.push(editor.onDidChangeModel?.(() => {
                    try { collection?.clear?.(); } catch (_) { }
                    schedule();
                }));
                disposables.push(editor.onDidDispose?.(() => {
                    api.dispose();
                }));
            } catch (_) { }

            const api = {
                dispose: () => {
                    if (disposed) return;
                    disposed = true;
                    if (timer) clearTimeout(timer);
                    timer = null;
                    try { collection?.clear?.(); } catch (_) { }
                    if (!collection && legacyDecorations.length) {
                        try { editor.deltaDecorations(legacyDecorations, []); } catch (_) { }
                    }
                    legacyDecorations = [];
                    disposables.forEach((d) => {
                        try { d?.dispose?.(); } catch (_) { }
                    });
                }
            };

            editor.__mumpsDotIndentGuides = api;
            schedule();
            return api;
        }

        return {
            loadAutocompleteData,
            sampleMumps,
            registerMumpsLanguage,
            registerMumpsThemes,
            registerMumpsCompletion,
            attachDotIndentGuides,
            registerMumpsFormatting
        };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.mumpsMonaco = window.AhmadIDEModules.mumpsMonaco || {};
        window.AhmadIDEModules.mumpsMonaco.createMumpsMonacoManager = createMumpsMonacoManager;
    }
})();
