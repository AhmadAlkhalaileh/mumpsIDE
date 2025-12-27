/**
 * MUMPS NEW Variable Autocomplete Provider
 * Suggests variables that are used in scope but not NEW'd
 */

(() => {
    'use strict';

    class MumpsNewAutocompleteProvider {
        constructor({ monaco, settingsService, showToast } = {}) {
            this.monaco = monaco;
            this.settingsService = settingsService;
            this.showToast = showToast || (() => {});
            this.disposables = [];
        }

        /**
         * Register completion provider
         */
        register() {
            if (!this.monaco) {
                console.error('[NEW Autocomplete] Monaco not available');
                return;
            }

            const provider = this.monaco.languages.registerCompletionItemProvider('mumps', {
                triggerCharacters: [' ', ','],
                provideCompletionItems: (model, position, context) => {
                    return this.provideCompletions(model, position, context);
                }
            });

            this.disposables.push(provider);
            console.log('[NEW Autocomplete] Provider registered');
        }

        /**
         * Provide completion suggestions
         */
        provideCompletions(model, position, context) {
            const line = model.getLineContent(position.lineNumber);
            const textBefore = line.substring(0, position.column - 1);

            // Check if we're in a NEW context
            if (!this.isInNewContext(textBefore)) {
                return { suggestions: [] };
            }

            // Analyze scope for unnewed variables
            const analysis = this.analyzeScopeVariables(model, position);

            const suggestions = [];

            // Suggest unnewed variables
            analysis.unnewed.forEach(varName => {
                suggestions.push({
                    label: varName,
                    kind: this.monaco.languages.CompletionItemKind.Variable,
                    insertText: varName,
                    documentation: `Variable used but not NEW'd in this tag`,
                    detail: '(unnewed variable)',
                    sortText: `0_${varName}` // Prioritize unnewed vars
                });
            });

            // Suggest favorites from settings
            const favorites = this.getFavoriteVariables();
            favorites.forEach(varName => {
                if (!analysis.unnewed.has(varName) && !analysis.newed.has(varName)) {
                    suggestions.push({
                        label: varName,
                        kind: this.monaco.languages.CompletionItemKind.Variable,
                        insertText: varName,
                        documentation: `Favorite variable from settings`,
                        detail: '(favorite)',
                        sortText: `1_${varName}`
                    });
                }
            });

            // Add "NEW all missing" option
            if (analysis.unnewed.size > 0) {
                const allVars = Array.from(analysis.unnewed).join(',');
                suggestions.push({
                    label: `✨ NEW all missing (${analysis.unnewed.size})`,
                    kind: this.monaco.languages.CompletionItemKind.Snippet,
                    insertText: allVars,
                    documentation: `Add all unnewed variables: ${allVars}`,
                    detail: '(add all)',
                    sortText: '2_all'
                });
            }

            return { suggestions };
        }

        /**
         * Check if cursor is in NEW/N command context
         */
        isInNewContext(textBefore) {
            const src = String(textBefore ?? '');
            const trimmedLeft = src.trimStart();

            // Allow:
            // - `NEW ` / `N `
            // - `NEW X,` / `NEW X,Y`
            // - Command lines with leading dots: `. NEW X`
            // - NEW appearing later in the line: `IF X=1 NEW `
            const newPattern = /(?:^|[\s.])(?:NEW|N)\s+$/i;
            const afterComma = /(?:^|[\s.])(?:NEW|N)\s+[A-Z%][A-Z0-9]*(?:\s*,\s*[A-Z%][A-Z0-9]*)*\s*,\s*$/i;
            return newPattern.test(src) || afterComma.test(src);
        }

        /**
         * Analyze scope to find NEW'd and used variables
         */
        analyzeScopeVariables(model, position) {
            const newed = new Set();
            const used = new Set();

            // Find current tag
            const tagStart = this.findTagStart(model, position.lineNumber);
            const tagEnd = this.findTagEnd(model, tagStart);

            // Scan tag lines
            for (let lineNum = tagStart; lineNum <= tagEnd; lineNum++) {
                const line = model.getLineContent(lineNum);
                const trimmed = line.trim();

                // Skip empty lines and full-line comments
                if (!trimmed || trimmed.startsWith(';')) continue;

                // Extract code part (before comment)
                const codePart = this.extractCode(trimmed);

                // Find NEW'd variables
                const newMatch = /\b(?:NEW|N)\s+([^;]+)/gi;
                let match;
                while ((match = newMatch.exec(codePart)) !== null) {
                    const varList = match[1].split(',').map(v => v.trim());
                    varList.forEach(v => {
                        const varName = this.extractVarName(v);
                        if (varName) newed.add(varName);
                    });
                }

                // Find used variables
                const varPattern = /\b([A-Z%][A-Z0-9]*)\s*(?:\(|=)/gi;
                while ((match = varPattern.exec(codePart)) !== null) {
                    const varName = match[1];
                    // Skip MUMPS commands
                    if (!this.isCommand(varName)) {
                        used.add(varName);
                    }
                }
            }

            // Find unnewed variables
            const unnewed = new Set();
            used.forEach(varName => {
                if (!newed.has(varName)) {
                    unnewed.add(varName);
                }
            });

            return { newed, used, unnewed };
        }

        /**
         * Find the start line of current tag
         */
        findTagStart(model, lineNumber) {
            for (let i = lineNumber; i >= 1; i--) {
                const line = model.getLineContent(i);
                // Tag starts at column 1 (no leading whitespace)
                if (line.match(/^[A-Z%][A-Z0-9]*\s*(?:\(|;|$)/i)) {
                    return i;
                }
            }
            return 1;
        }

        /**
         * Find the end line of current tag
         */
        findTagEnd(model, tagStart) {
            const lineCount = model.getLineCount();
            for (let i = tagStart + 1; i <= lineCount; i++) {
                const line = model.getLineContent(i);
                // Next tag starts
                if (line.match(/^[A-Z%][A-Z0-9]*\s*(?:\(|;|$)/i)) {
                    return i - 1;
                }
            }
            return lineCount;
        }

        /**
         * Extract code before comment
         */
        extractCode(line) {
            const commentIndex = line.indexOf(';');
            return commentIndex >= 0 ? line.substring(0, commentIndex) : line;
        }

        /**
         * Extract variable name from NEW statement token
         */
        extractVarName(token) {
            // Remove parentheses and leading symbols
            const cleaned = token.replace(/^[@*]+/, '').split('(')[0].trim();
            if (/^[A-Z%][A-Z0-9]*$/i.test(cleaned)) {
                return cleaned.toUpperCase();
            }
            return null;
        }

        /**
         * Check if token is a MUMPS command
         */
        isCommand(token) {
            const commands = new Set([
                'SET', 'WRITE', 'READ', 'DO', 'GOTO', 'IF', 'ELSE', 'FOR', 'QUIT',
                'HALT', 'HANG', 'JOB', 'KILL', 'LOCK', 'MERGE', 'NEW', 'OPEN',
                'CLOSE', 'USE', 'VIEW', 'XECUTE', 'BREAK', 'TSTART', 'TCOMMIT',
                'TROLLBACK', 'TRESTART', 'READ'
            ]);
            return commands.has(token.toUpperCase());
        }

        /**
         * Get favorite variables from settings
         */
        getFavoriteVariables() {
            const defaults = ['ERR', 'I', 'J', 'K', 'X', 'Y', 'CNT', 'IDX', 'TMP'];

            if (!this.settingsService) return defaults;

            try {
                if (typeof this.settingsService.get === 'function') {
                    return this.settingsService.get('mumps.favoriteVariables', defaults);
                }
            } catch {
                // Ignore
            }

            return defaults;
        }

        /**
         * Cleanup
         */
        dispose() {
            this.disposables.forEach(d => d.dispose());
            this.disposables = [];
        }
    }

    // Export
    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.mumps = window.AhmadIDEModules.mumps || {};
        window.AhmadIDEModules.mumps.MumpsNewAutocompleteProvider = MumpsNewAutocompleteProvider;
    }
})();
