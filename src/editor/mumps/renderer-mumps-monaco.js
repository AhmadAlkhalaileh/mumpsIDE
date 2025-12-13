(() => {
    function createMumpsMonacoManager({ deps } = {}) {
        const $ = deps?.$ || (typeof window !== 'undefined' ? (window.$ || window.jQuery || null) : null);
        const getMonaco = deps?.getMonaco || (() => (typeof monaco !== 'undefined' ? monaco : null));

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
                '    WRITE "Hello, Ahmad IDE!", !',
                '    SET X=1',
                '    IF X=1 WRITE "X is one", !',
                '    QUIT'
            ].join('\n');
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
                provideCompletionItems: () => ({ suggestions: buildDataset() })
            });
        }

        return {
            loadAutocompleteData,
            sampleMumps,
            registerMumpsLanguage,
            registerMumpsThemes,
            registerMumpsCompletion
        };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.mumpsMonaco = window.AhmadIDEModules.mumpsMonaco || {};
        window.AhmadIDEModules.mumpsMonaco.createMumpsMonacoManager = createMumpsMonacoManager;
    }
})();
