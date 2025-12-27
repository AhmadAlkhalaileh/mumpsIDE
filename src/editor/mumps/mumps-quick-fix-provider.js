/**
 * MUMPS Quick Fix Provider - Monaco Code Actions
 * Provides automatic fixes for linter issues
 *
 * Supports fixes for:
 * - M030: Remove trailing whitespace
 * - M031: Normalize mixed indentation in line
 * - M032: Normalize file indentation (convert all)
 * - M033: Re-indent line to proper alignment
 * - M037: Add variable to nearest NEW (optional)
 */

(() => {
    'use strict';

    class MumpsQuickFixProvider {
        constructor(monacoRef) {
            this.monaco = monacoRef;
            this.disposables = [];
        }

        /**
         * Register this provider with Monaco
         */
        register() {
            if (!this.monaco || !this.monaco.languages) {
                console.error('[Quick Fix] Monaco not available');
                return;
            }

            const provider = this.monaco.languages.registerCodeActionProvider('mumps', {
                provideCodeActions: (model, range, context, token) => {
                    return this.provideCodeActions(model, range, context, token);
                }
            });

            this.disposables.push(provider);
            console.log('[Quick Fix] Provider registered for MUMPS');
        }

        /**
         * Provide quick fix actions for diagnostics in range
         */
        provideCodeActions(model, range, context, token) {
            const actions = [];

            // Get markers (diagnostics) in the current range
            const markers = context.markers || [];

            for (const marker of markers) {
                // Check if this is a MUMPS linter diagnostic with autofix
                if (!marker.code || !String(marker.code).startsWith('M0')) continue;

                const ruleId = String(marker.code);
                const fixActions = this.createFixActions(model, marker, ruleId);
                actions.push(...fixActions);
            }

            return {
                actions,
                dispose: () => {}
            };
        }

        /**
         * Create fix actions for a specific diagnostic
         */
        createFixActions(model, marker, ruleId) {
            const actions = [];

            switch (ruleId) {
                case 'M030': // Trailing whitespace
                    actions.push(this.createRemoveTrailingWhitespaceAction(model, marker));
                    break;

                case 'M031': // Mixed tabs/spaces in line
                    actions.push(this.createNormalizeLineIndentAction(model, marker));
                    break;

                case 'M032': // Inconsistent file indentation
                    actions.push(this.createNormalizeFileIndentAction(model, marker, true));  // prefer spaces
                    actions.push(this.createNormalizeFileIndentAction(model, marker, false)); // prefer tabs
                    break;

                case 'M033': // Wrong indentation alignment
                    actions.push(this.createReindentLineAction(model, marker));
                    break;

                case 'M037': // Unused NEW variable
                    // Optional: could add "Remove variable from NEW" action
                    // Skipping for now as it's more complex
                    break;

                default:
                    break;
            }

            return actions.filter(Boolean);
        }

        /**
         * M030: Remove trailing whitespace from line
         */
        createRemoveTrailingWhitespaceAction(model, marker) {
            const lineNum = marker.startLineNumber;
            const line = model.getLineContent(lineNum);
            const trimmed = line.replace(/[ \t]+$/, '');

            return {
                title: '💡 Remove trailing whitespace',
                kind: 'quickfix',
                diagnostics: [marker],
                edit: {
                    edits: [{
                        resource: model.uri,
                        textEdit: {
                            range: {
                                startLineNumber: lineNum,
                                startColumn: 1,
                                endLineNumber: lineNum,
                                endColumn: line.length + 1
                            },
                            text: trimmed
                        }
                    }]
                },
                isPreferred: true
            };
        }

        /**
         * M031: Normalize mixed tabs/spaces in single line
         */
        createNormalizeLineIndentAction(model, marker) {
            const lineNum = marker.startLineNumber;
            const line = model.getLineContent(lineNum);
            const leadingMatch = line.match(/^(\s+)/);

            if (!leadingMatch) return null;

            const leading = leadingMatch[1];
            const rest = line.slice(leading.length);

            // Convert to spaces (4 spaces per tab)
            const normalized = leading.replace(/\t/g, '    ') + rest;

            return {
                title: '💡 Normalize indentation to spaces',
                kind: 'quickfix',
                diagnostics: [marker],
                edit: {
                    edits: [{
                        resource: model.uri,
                        textEdit: {
                            range: {
                                startLineNumber: lineNum,
                                startColumn: 1,
                                endLineNumber: lineNum,
                                endColumn: line.length + 1
                            },
                            text: normalized
                        }
                    }]
                },
                isPreferred: true
            };
        }

        /**
         * M032: Normalize entire file indentation
         */
        createNormalizeFileIndentAction(model, marker, preferSpaces) {
            const lineCount = model.getLineCount();
            const edits = [];

            for (let i = 1; i <= lineCount; i++) {
                const line = model.getLineContent(i);
                const leadingMatch = line.match(/^(\s+)/);

                if (!leadingMatch) continue;

                const leading = leadingMatch[1];
                const rest = line.slice(leading.length);
                let normalized;

                if (preferSpaces) {
                    // Convert tabs to 4 spaces
                    normalized = leading.replace(/\t/g, '    ') + rest;
                } else {
                    // Convert 4 spaces to tabs
                    normalized = leading.replace(/ {4}/g, '\t') + rest;
                }

                if (normalized !== line) {
                    edits.push({
                        resource: model.uri,
                        textEdit: {
                            range: {
                                startLineNumber: i,
                                startColumn: 1,
                                endLineNumber: i,
                                endColumn: line.length + 1
                            },
                            text: normalized
                        }
                    });
                }
            }

            const title = preferSpaces
                ? '💡 Convert entire file to spaces'
                : '💡 Convert entire file to tabs';

            return {
                title,
                kind: 'quickfix',
                diagnostics: [marker],
                edit: { edits },
                isPreferred: preferSpaces // Prefer spaces by default
            };
        }

        /**
         * M033: Re-indent line to proper alignment
         */
        createReindentLineAction(model, marker) {
            const lineNum = marker.startLineNumber;
            const line = model.getLineContent(lineNum);
            const leadingMatch = line.match(/^(\s*)/);

            if (!leadingMatch) return null;

            const currentIndent = leadingMatch[1].length;
            const rest = line.slice(currentIndent);

            // Get expected indent from marker message or default to 4
            const expectedIndent = 4;

            // Calculate nearest valid indent level
            const nearestLevel = Math.round(currentIndent / expectedIndent);
            const newIndent = nearestLevel * expectedIndent;
            const newLeading = ' '.repeat(newIndent);
            const newLine = newLeading + rest;

            return {
                title: `💡 Re-indent to ${newIndent} spaces`,
                kind: 'quickfix',
                diagnostics: [marker],
                edit: {
                    edits: [{
                        resource: model.uri,
                        textEdit: {
                            range: {
                                startLineNumber: lineNum,
                                startColumn: 1,
                                endLineNumber: lineNum,
                                endColumn: line.length + 1
                            },
                            text: newLine
                        }
                    }]
                },
                isPreferred: true
            };
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
        window.AhmadIDEModules.mumps.MumpsQuickFixProvider = MumpsQuickFixProvider;

        // Auto-register if Monaco is available
        if (window.monaco) {
            const provider = new MumpsQuickFixProvider(window.monaco);
            provider.register();
            window.AhmadIDEModules.mumps.quickFixProvider = provider;
        }
    }
})();
