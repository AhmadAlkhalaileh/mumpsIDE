/**
 * MUMPS Snippets Service
 * Manages user-configurable code templates with variable substitution
 */

(() => {
    'use strict';

    class MumpsSnippetsService {
        constructor({ settingsService, showToast } = {}) {
            this.settingsService = settingsService;
            this.showToast = showToast || (() => { });
            this.snippets = new Map();
            this.loadDefaultSnippets();
            this.loadUserSnippets();

            if (this.settingsService?.subscribe) {
                this.settingsService.subscribe(() => {
                    this.loadUserSnippets();
                });
            }
        }

        /**
         * Load built-in default snippets
         */
        loadDefaultSnippets() {
            const defaults = {
                'entry-point': {
                    name: 'Entry Point Header',
                    prefix: 'mentry',
                    description: 'Standard entry point with header comment',
                    body: [
                        '${TAG}(${PARAMS}) ; ${DESCRIPTION}',
                        '    ; Created: ${DATE}',
                        '    ; Author: ${USER}',
                        '    ; Routine: ${ROUTINE}',
                        '    NEW ${4:VAR}',
                        '    $0',
                        '    QUIT'
                    ]
                },
                'routine-tag': {
                    name: 'Routine Tag',
                    prefix: 'mtag',
                    description: 'Standard tag with comment',
                    body: [
                        '${TAG} ; ${DESCRIPTION}',
                        '    $0',
                        '    QUIT'
                    ]
                },
                'error-trap': {
                    name: 'Error Trap Scaffold',
                    prefix: 'merror',
                    description: 'Standard error handler',
                    body: [
                        'ERROR   ; Error trap',
                        '    NEW ERR,ERRCODE,ERRMSG',
                        '    SET ERR=$GET($ECODE)',
                        '    IF ERR\'="" DO',
                        '    . SET ERRCODE=$PIECE(ERR,",",2)',
                        '    . SET ERRMSG=$TEXT(@ERRCODE)',
                        '    . WRITE "Error: ",ERRMSG,!',
                        '    . DO LOG^ERRORLOG(ERR)',
                        '    QUIT'
                    ]
                },
                'piece-helper': {
                    name: '$PIECE Helper',
                    prefix: 'mp',
                    description: '$PIECE pattern',
                    body: [
                        'SET ${1:VAR}=$PIECE(${2:STRING},"${3:^}",${4:1})'
                    ]
                },
                'translate-helper': {
                    name: '$TRANSLATE Helper',
                    prefix: 'mtr',
                    description: '$TRANSLATE pattern',
                    body: [
                        'SET ${1:VAR}=$TRANSLATE(${2:STRING},"${3:FROM}","${4:TO}")'
                    ]
                },
                'extract-helper': {
                    name: '$EXTRACT Helper',
                    prefix: 'me',
                    description: '$EXTRACT pattern',
                    body: [
                        'SET ${1:VAR}=$EXTRACT(${2:STRING},${3:START},${4:END})'
                    ]
                },
                'for-loop': {
                    name: 'FOR Loop',
                    prefix: 'mfor',
                    description: 'Standard FOR loop',
                    body: [
                        'FOR ${1:I}=${2:1}:1:${3:N} DO',
                        '. $0',
                        'QUIT'
                    ]
                },
                'if-block': {
                    name: 'IF Block',
                    prefix: 'mif',
                    description: 'IF with DO block',
                    body: [
                        'IF ${1:CONDITION} DO',
                        '. $0',
                        'QUIT'
                    ]
                },
                'new-label': {
                    name: 'New Label',
                    prefix: 'new-label',
                    description: 'Create a new tag/label (Alias for mtag)',
                    body: [
                        '${TAG} ; ${DESCRIPTION}',
                        '    $0',
                        '    QUIT'
                    ]
                },
                'newlabel': {
                    name: 'New Label',
                    prefix: 'newlabel',
                    description: 'Create a new tag/label (Alias for mtag)',
                    body: [
                        '${TAG} ; ${DESCRIPTION}',
                        '    $0',
                        '    QUIT'
                    ]
                },
                'tag': {
                    name: 'New Tag',
                    prefix: 'tag',
                    description: 'Create a new tag/label (Alias for mtag)',
                    body: [
                        '${TAG} ; ${DESCRIPTION}',
                        '    $0',
                        '    QUIT'
                    ]
                }
            };

            Object.entries(defaults).forEach(([id, snippet]) => {
                this.snippets.set(id, snippet);
            });

            // Optional: VA FileMan API snippet pack (built-in)
            try {
                const settings = this.settingsService?.get?.() || {};
                const enabled = settings?.mumps?.fileman?.snippetsEnabled !== false;
                const filemanDefaults = enabled
                    ? window.AhmadIDEModules?.mumps?.fileman?.getDefaultSnippets?.()
                    : null;
                if (filemanDefaults && typeof filemanDefaults === 'object') {
                    Object.entries(filemanDefaults).forEach(([id, snippet]) => {
                        if (snippet && (snippet.prefix || snippet.name) && snippet.body) {
                            this.snippets.set(id, snippet);
                        }
                    });
                }
            } catch (_) { }
        }

        /**
         * Load user-defined snippets from settings
         */
        loadUserSnippets() {
            if (!this.settingsService) {
                console.log('[Snippets] No settings service, skipping user snippets');
                return;
            }

            try {
                this.snippets.clear();
                this.loadDefaultSnippets();

                const allSettings = this.settingsService.get() || {};
                const userSnippets = allSettings.mumps?.snippets || {};
                Object.entries(userSnippets).forEach(([id, snippet]) => {
                    // Allow overriding built-ins if ID matches
                    if (this.snippets.has(id)) {
                        this.snippets.set(id, { ...this.snippets.get(id), ...snippet });
                    } else if (snippet.body && (snippet.prefix || snippet.name)) {
                        // New user snippets need prefix or name
                        this.snippets.set(`user:${id}`, snippet);
                    }
                });

                // Explicitly check for routine-tag override
                const directTag = allSettings.mumps?.snippets?.['routine-tag'];
                if (directTag && directTag.body) {
                    const current = this.snippets.get('routine-tag');
                    if (current) {
                        this.snippets.set('routine-tag', { ...current, ...directTag });
                    }
                }

                // EMERGENCY FALLBACK: Check 'mytag' key if routine-tag fails
                const fallbackTag = allSettings.mumps?.snippets?.['mytag'];
                if (fallbackTag && fallbackTag.body) {
                    const current = this.snippets.get('routine-tag');
                    if (current) {
                        this.snippets.set('routine-tag', { ...current, ...fallbackTag });
                    }
                }

                // Sync aliases with routine-tag if customized
                const routineTag = this.snippets.get('routine-tag');
                if (routineTag) {
                    ['new-label', 'newlabel', 'tag'].forEach(alias => {
                        if (this.snippets.has(alias)) {
                            this.snippets.set(alias, {
                                ...routineTag,
                                prefix: alias,
                                name: this.snippets.get(alias).name
                            });
                        }
                    });
                }
            } catch (err) {
                console.warn('[Snippets] Failed to load user snippets:', err);
            }
        }

        /**
         * Get all snippets
         */
        getAll() {
            return Array.from(this.snippets.entries()).map(([id, snippet]) => ({
                id,
                ...snippet
            }));
        }

        /**
         * Get snippet by ID
         */
        get(id) {
            return this.snippets.get(id);
        }

        /**
         * Get routine template with variables substituted
         * @param {string} routineName - The name of the routine
         * @returns {string} The expanded routine template
         */
        getRoutineTemplate(routineName) {
            const snippet = this.get('entry-point');
            if (snippet && snippet.body) {
                const body = Array.isArray(snippet.body) ? snippet.body.join('\n') : snippet.body;

                // Get user name from settings
                let userName = 'Developer';
                try {
                    if (this.settingsService && typeof this.settingsService.get === 'function') {
                        const allSettings = this.settingsService.get() || {};
                        userName = allSettings.mumps?.userName || 'Developer';
                    }
                } catch (e) { /* ignore */ }

                // Get current date/time
                const now = new Date();
                const date = now.toISOString().split('T')[0];

                // Replace our custom variables first
                let result = body
                    .replace(/\$\{TAG\}/g, routineName)
                    .replace(/\$\{ROUTINE\}/g, routineName)
                    .replace(/\$\{DATE\}/g, date)
                    .replace(/\$\{USER\}/g, userName)
                    .replace(/\$\{DESCRIPTION\}/g, `${routineName} routine`)
                    .replace(/\$\{PARAMS\}/g, '');

                // Remove Monaco placeholders like ${1:TAG}, ${2:VAR}, $0, $1 for plain text use
                result = result
                    .replace(/\$\{\d+:[^}]*\}/g, '')  // ${1:TAG} -> empty
                    .replace(/\$\d+/g, '')             // $0, $1 -> empty
                    .replace(/\(\s*\)/g, '')           // Remove empty parentheses ()
                    .replace(/[ \t]+;/g, ' ;');        // Clean up multiple spaces/tabs before semicolons (preserve newlines)

                // Ensure routine name is at the start
                let finalResult = result.trim();
                if (!finalResult.startsWith(routineName)) {
                    finalResult = `${routineName} ${finalResult}`;
                }
                return finalResult + '\n';
            }
            // Fallback default template
            return `${routineName} ; ${routineName} routine\n    QUIT\n`;
        }

        /**
         * Get snippets by prefix
         */
        getByPrefix(prefix) {
            return this.getAll().filter(s => s.prefix === prefix);
        }

        /**
         * Expand snippet body with variable substitution
         */
        expandSnippet(snippet, context = {}) {
            let body = Array.isArray(snippet.body) ? snippet.body.join('\n') : snippet.body;

            // Auto-fix: If this is a tag snippet and user forgot the ${TAG} variable, append default body
            if (snippet.prefix && ['mtag', 'newlabel', 'tag', 'new-label'].includes(snippet.prefix)) {
                if (!body.includes('${TAG}')) {
                    body = body ? (body.trim() + '\n${TAG} ; ${DESCRIPTION}\n    $0\n    QUIT') : '${TAG} ; ${DESCRIPTION}\n    $0\n    QUIT';
                }
            }

            // Get current date/time
            const now = new Date();
            const date = now.toISOString().split('T')[0]; // YYYY-MM-DD
            const time = now.toTimeString().split(' ')[0]; // HH:MM:SS

            // Default context
            let userName = 'Developer';
            try {
                if (this.settingsService && typeof this.settingsService.get === 'function') {
                    const allSettings = this.settingsService.get() || {};
                    userName = allSettings.user?.name || allSettings.mumps?.userName || 'Developer';
                }
            } catch (e) {
                // Ignore
            }

            const defaultContext = {
                DATE: date,
                TIME: time,
                USER: userName,
                ROUTINE: context.routineName || '',
                TAG: '${1:TAG}',
                DESCRIPTION: '${3:DESCRIPTION}',
                PARAMS: '${2:PARAMS}'
            };

            const finalContext = { ...defaultContext, ...context };

            // Replace template variables
            let expanded = body;
            Object.entries(finalContext).forEach(([key, value]) => {
                const regex = new RegExp(`\\$\\{${key}\\}`, 'g');
                expanded = expanded.replace(regex, value);
            });

            return expanded;
        }

        /**
         * Escape snippet text so Monaco inserts literal MUMPS `$` tokens.
         * Monaco snippet syntax treats `$FOO` as variables, so we must escape `$`
         * except for placeholder tokens like `$1`/`$0` and `${...}`.
         */
        escapeForMonacoSnippet(text) {
            const src = String(text ?? '');
            return src.replace(/\$(?!\d|\{)/g, '\\$');
        }

        /**
         * Try to derive routine name from a Monaco model URI
         */
        getRoutineNameFromModel(model) {
            try {
                const uriStr = model?.uri?.path || model?.uri?.toString?.() || '';
                const base = String(uriStr).split('/').pop() || '';
                return base.replace(/\.(m|mumps)$/i, '');
            } catch (_) {
                return '';
            }
        }

        /**
         * Convert snippet to Monaco snippet format
         */
        toMonacoSnippet(snippet) {
            const body = Array.isArray(snippet.body) ? snippet.body : [snippet.body];
            const prefixes = Array.isArray(snippet.prefix) ? snippet.prefix : [snippet.prefix];
            const label = (prefixes.find((p) => typeof p === 'string' && p.trim().length) || snippet.name || '').trim();

            return {
                label,
                kind: window.monaco?.languages.CompletionItemKind.Snippet,
                insertText: body.join('\n'),
                insertTextRules: window.monaco?.languages.CompletionItemInsertTextRule.InsertAsSnippet,
                documentation: snippet.documentation || snippet.description || snippet.name,
                detail: snippet.name
            };
        }

        /**
         * Insert snippet at cursor
         */
        insertSnippet(editor, snippetId, context = {}) {
            const snippet = this.get(snippetId);
            if (!snippet) {
                this.showToast('error', 'Snippet', `Snippet '${snippetId}' not found`);
                return false;
            }

            const model = editor.getModel();
            const position = editor.getPosition();

            if (!model || !position) {
                return false;
            }

            // Get routine name from model URI
            const routineName = this.getRoutineNameFromModel(model);

            // Expand snippet
            const expanded = this.escapeForMonacoSnippet(this.expandSnippet(snippet, {
                routineName,
                ...context
            }));

            // Insert as snippet (supports $0, $1, etc. placeholders)
            const snippetController = editor.getContribution('snippetController2');
            if (snippetController) {
                snippetController.insert(expanded, { overwriteBefore: 0, overwriteAfter: 0 });
            } else {
                // Fallback: insert as plain text
                editor.executeEdits('snippet', [{
                    range: {
                        startLineNumber: position.lineNumber,
                        startColumn: position.column,
                        endLineNumber: position.lineNumber,
                        endColumn: position.column
                    },
                    text: expanded
                }]);
            }

            this.showToast('success', 'Snippet', `Inserted: ${snippet.name}`);
            return true;
        }

        /**
         * Register completion provider with Monaco
         */
        registerCompletionProvider(monaco) {
            const normalizeTriggerText = (s) => {
                const fn = window.AhmadIDEModules?.mumps?.fileman?.normalizeTriggerText;
                if (typeof fn === 'function') return fn(s);
                return String(s || '').toLowerCase().replace(/\s+/g, ' ').trimEnd();
            };

            const provider = monaco.languages.registerCompletionItemProvider('mumps', {
                triggerCharacters: [' ', '$', '^'],
                provideCompletionItems: (model, position) => {
                    const textUntilPosition = model.getValueInRange({
                        startLineNumber: position.lineNumber,
                        startColumn: 1,
                        endLineNumber: position.lineNumber,
                        endColumn: position.column
                    });

                    // Get word before cursor
                    const word = model.getWordUntilPosition(position);
                    const wordTextRaw = String(word.word || '');

                    const buildTypedSpan = () => {
                        const linePrefix = String(textUntilPosition || '');
                        const tagRoutineMatch = linePrefix.match(/(?:\$\$)?[A-Za-z%][A-Za-z0-9]*\^[A-Za-z%][A-Za-z0-9]*$/i);
                        if (tagRoutineMatch && typeof tagRoutineMatch.index === 'number') {
                            return {
                                typed: tagRoutineMatch[0] || '',
                                startColumn: tagRoutineMatch.index + 1,
                                endColumn: position.column
                            };
                        }

                        const filemanAliasMatch = linePrefix.match(/fileman[ \t]+[A-Za-z0-9_-]*$/i);
                        if (filemanAliasMatch && typeof filemanAliasMatch.index === 'number') {
                            return {
                                typed: filemanAliasMatch[0] || '',
                                startColumn: filemanAliasMatch.index + 1,
                                endColumn: position.column
                            };
                        }

                        return {
                            typed: wordTextRaw,
                            startColumn: word.startColumn,
                            endColumn: word.endColumn
                        };
                    };

                    const span = buildTypedSpan();
                    const typedNorm = normalizeTriggerText(span.typed);
                    const typedNormNoDollar = typedNorm.replace(/^\$\$/, '');

                    // Find matching snippets
                    const suggestions = [];
                    const routineName = this.getRoutineNameFromModel(model);

                    this.getAll().forEach(snippet => {
                        const prefixes = Array.isArray(snippet.prefix) ? snippet.prefix : [snippet.prefix];
                        const prefixList = prefixes
                            .filter((p) => typeof p === 'string')
                            .map((p) => String(p || ''))
                            .filter((p) => p.trim().length);
                        if (!prefixList.length) return;

                        const matches = prefixList.some((p) => {
                            const pNorm = normalizeTriggerText(p);
                            const pNormNoDollar = pNorm.replace(/^\$\$/, '');
                            if (!typedNorm) return true;
                            return pNorm.startsWith(typedNorm) || pNormNoDollar.startsWith(typedNormNoDollar);
                        });
                        if (!matches) return;

                        const expanded = this.escapeForMonacoSnippet(this.expandSnippet(snippet, { routineName }));
                        const label = (prefixList[0] || snippet.name || '').trim();

                        suggestions.push({
                            label,
                            kind: monaco.languages.CompletionItemKind.Snippet,
                            insertText: expanded,
                            insertTextRules: monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet,
                            documentation: snippet.documentation || snippet.description || snippet.name,
                            detail: snippet.name,
                            range: {
                                startLineNumber: position.lineNumber,
                                startColumn: span.startColumn,
                                endLineNumber: position.lineNumber,
                                endColumn: span.endColumn
                            }
                        });
                    });

                    return { suggestions };
                }
            });

            console.log('[Snippets] Completion provider registered');
            return provider;
        }

        /**
         * Save user snippet
         */
        saveUserSnippet(id, snippet) {
            if (!this.settingsService) {
                this.showToast('error', 'Snippets', 'Settings service not available');
                return false;
            }

            try {
                const userSnippets = this.settingsService.get('mumps.snippets', {});
                userSnippets[id] = snippet;
                this.settingsService.set('mumps.snippets', userSnippets);

                this.snippets.set(`user:${id}`, snippet);
                this.showToast('success', 'Snippets', `Saved snippet: ${snippet.name}`);
                return true;
            } catch (err) {
                this.showToast('error', 'Snippets', `Failed to save: ${err.message}`);
                return false;
            }
        }
    }

    // Export
    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.mumps = window.AhmadIDEModules.mumps || {};
        window.AhmadIDEModules.mumps.MumpsSnippetsService = MumpsSnippetsService;
    }
})();
