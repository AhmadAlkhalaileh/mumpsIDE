/**
 * MUMPS Tag Header Generator
 * Generates documentation headers for individual MUMPS tags when user right-clicks on a tag name
 */

(() => {
    'use strict';

    class MumpsTagHeaderGenerator {
        constructor({ showToast, settingsService } = {}) {
            this.showToast = showToast || (() => { });
            this.settingsService = settingsService;
            this.HEADER_LINE = ';;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;';
        }

        /**
         * Get author name from settings
         */
        getAuthorName() {
            if (this.settingsService) {
                const settings = this.settingsService.get() || {};
                return settings.mumps?.tagHeader?.author || settings.mumps?.userName || 'Developer';
            }
            return 'Developer';
        }

        /**
         * Check if the current cursor/selection is on a tag definition line
         * Returns tag info if valid, null otherwise
         */
        getTagAtCursor(editor) {
            if (!editor) return null;

            const model = editor.getModel();
            const position = editor.getPosition();
            if (!model || !position) return null;

            const lineNumber = position.lineNumber;
            const line = model.getLineContent(lineNumber);

            // Tag must start at column 1 (not indented)
            if (!line || line[0] === ' ' || line[0] === '\t') return null;

            // Skip comment-only lines
            if (line.trim().startsWith(';')) return null;

            // Parse tag line
            const tagInfo = this.parseTagLine(line);
            if (!tagInfo) return null;

            const tagNameStartCol = 1;
            const tagNameEndCol = tagInfo.name.length + 1; // Monaco columns are 1-based; end is exclusive-ish.

            // Prefer selection-based detection (user highlighted the tag name).
            const selection = editor.getSelection?.();
            if (selection) {
                const isSingleLine = selection.startLineNumber === lineNumber && selection.endLineNumber === lineNumber;
                const hasRange = selection.startColumn !== selection.endColumn;
                if (isSingleLine && hasRange) {
                    const selStart = Math.min(selection.startColumn, selection.endColumn);
                    const selEnd = Math.max(selection.startColumn, selection.endColumn);
                    // Only show when selection is inside the tag name (not the code body).
                    if (selStart >= tagNameStartCol && selEnd <= tagNameEndCol) {
                        return { ...tagInfo, lineNumber };
                    }
                    return null;
                }
            }

            // Cursor-based detection (user clicked on tag name).
            if (position.column < tagNameStartCol || position.column > tagNameEndCol) return null;

            return {
                ...tagInfo,
                lineNumber
            };
        }

        /**
         * Parse a tag line to extract name and parameters
         * Returns { name, params: string[], signature } or null
         */
        parseTagLine(line) {
            if (!line || typeof line !== 'string') return null;

            // Tag pattern: starts at column 1, alphanumeric + % allowed
            const match = line.match(/^([A-Z%][A-Z0-9]*)/i);
            if (!match) return null;

            const name = match[1];
            const afterName = line.slice(name.length);

            // Extract parameters if present
            let params = [];
            const trimmedAfter = afterName.replace(/^\s*/, '');

            if (trimmedAfter.startsWith('(')) {
                const endParen = trimmedAfter.indexOf(')');
                if (endParen !== -1) {
                    const paramsStr = trimmedAfter.slice(1, endParen);
                    // Split by comma, handle spaces
                    params = paramsStr.split(',')
                        .map(p => p.trim())
                        .filter(p => p.length > 0);
                }
            }

            return {
                name,
                params,
                signature: params.length > 0 ? `${name}(${params.join(',')})` : name
            };
        }

        /**
         * Get custom template from settings or use default
         */
        getTemplate() {
            if (this.settingsService) {
                const settings = this.settingsService.get() || {};
                const template = settings.mumps?.tagHeader?.template;
                if (template && typeof template === 'string' && template.trim()) {
                    return template;
                }
            }
            // Default template
            return this.getDefaultTemplate();
        }

        /**
         * Default template
         */
        getDefaultTemplate() {
            return `${this.HEADER_LINE}
; Name: \${TAG}
;
; Description: \${DESCRIPTION}
;
\${PARAMS_IN}
;
; Out: \${OUT}
;
; Return: \${RETURN}
;
; Notes: \${NOTES}
;
; Author: \${AUTHOR}
${this.HEADER_LINE}`;
        }

        /**
         * Generate the documentation header for a tag
         */
        generateHeader(tagName, params, author) {
            const template = this.getTemplate();

            // Build params section
            let paramsIn = '';
            if (params.length > 0) {
                paramsIn = params.map((param, idx) => {
                    const isByRef = /^\./.test(param);
                    const cleanParam = param.replace(/^\./, '');
                    const prefix = idx === 0 ? '; In: ' : ';     ';
                    const byRefText = isByRef ? ' (By Reference)' : '';
                    return `${prefix}${cleanParam.padEnd(8)}${byRefText} : `;
                }).join('\n');
            } else {
                paramsIn = '; In: N/A';
            }

            // Replace placeholders
            let header = template
                .replace(/\$\{TAG\}/g, tagName)
                .replace(/\$\{DESCRIPTION\}/g, '')
                .replace(/\$\{PARAMS_IN\}/g, paramsIn)
                .replace(/\$\{OUT\}/g, '')
                .replace(/\$\{RETURN\}/g, 'N/A')
                .replace(/\$\{NOTES\}/g, 'N/A')
                .replace(/\$\{AUTHOR\}/g, author);

            header = this.normalizeHeaderWhitespace(header, { tagName });
            return header;
        }

        normalizeHeaderWhitespace(header, { tagName } = {}) {
            const indent = Math.max(0, String(tagName || '').length - 1);
            const indentStr = indent ? ' '.repeat(indent) : '';
            const lines = String(header || '').split(/\r?\n/);
            const out = lines.map((line) => {
                let s = String(line || '');
                // Remove trailing whitespace on every line
                s = s.replace(/[ \t]+$/g, '');

                // Common template mistake: prefixing the ${PARAMS_IN} line with ';'
                // which results in ';; In:' for the first param line.
                s = s.replace(/^;;\s*In:/i, '; In:');

                // Align all comment lines to the last char of the tag name (tag starts at col 1)
                // Example: MAKE1 => indent 4 spaces, so ';' starts under '1'.
                if (/^\s*;/.test(s)) {
                    s = s.replace(/^\s+(?=;)/g, '');
                    s = indentStr + s;
                }
                return s;
            });

            // Drop leading/trailing empty lines
            while (out.length && !out[0]) out.shift();
            while (out.length && !out[out.length - 1]) out.pop();

            return out.join('\n');
        }

        /**
         * Insert the header above the tag line
         */
        insertHeader(editor, tagInfo, header) {
            if (!editor || !tagInfo) return false;

            const model = editor.getModel();
            if (!model) return false;

            const headerText = String(header || '').replace(/\r?\n+$/g, '');

            // Insert at the beginning of the tag line
            editor.executeEdits('tag-header-generator', [{
                range: {
                    startLineNumber: tagInfo.lineNumber,
                    startColumn: 1,
                    endLineNumber: tagInfo.lineNumber,
                    endColumn: 1
                },
                text: headerText + '\n'
            }]);

            // Position cursor on Description line for easy editing
            const headerLines = headerText.split('\n');
            const descLineIndex = headerLines.findIndex(l => l.includes('Description:'));
            if (descLineIndex !== -1) {
                const descLine = tagInfo.lineNumber + descLineIndex;
                const col = headerLines[descLineIndex].length + 1;
                editor.setPosition({ lineNumber: descLine, column: col });
                editor.focus();
            }

            return true;
        }

        /**
         * Main action: Generate tag header for the tag at cursor
         */
        generateTagHeader(editor) {
            if (!editor) {
                this.showToast('error', 'Tag Header', 'No active editor');
                return false;
            }

            const tagInfo = this.getTagAtCursor(editor);
            if (!tagInfo) {
                this.showToast('warning', 'Tag Header', 'Place cursor on a tag name at the start of a line');
                return false;
            }

            try {
                const author = this.getAuthorName();
                const header = this.generateHeader(tagInfo.name, tagInfo.params, author);

                const success = this.insertHeader(editor, tagInfo, header);
                if (success) {
                    this.showToast('success', 'Tag Header', `Generated header for ${tagInfo.signature}`);
                    return true;
                } else {
                    this.showToast('error', 'Tag Header', 'Failed to insert header');
                    return false;
                }
            } catch (err) {
                console.error('[Tag Header Generator] Error:', err);
                this.showToast('error', 'Tag Header', `Failed: ${err.message}`);
                return false;
            }
        }

        /**
         * Register Monaco action with context menu
         */
        registerCommand(editor, monaco) {
            if (!editor || !monaco) return;

            try {
                // Create a precondition context key for when on a tag
                const contextKey = editor.createContextKey('mumpsOnTagName', false);

                // Update context key on cursor position change
                editor.onDidChangeCursorPosition(() => {
                    const tagInfo = this.getTagAtCursor(editor);
                    contextKey.set(!!tagInfo);
                });

                // Register the action
                editor.addAction({
                    id: 'mumps.generateTagHeader',
                    label: 'Generate Tag Header',
                    keybindings: [],
                    precondition: 'mumpsOnTagName',
                    contextMenuGroupId: 'navigation',
                    contextMenuOrder: 1.4,
                    run: (ed) => {
                        this.generateTagHeader(ed);
                    }
                });

                console.log('[Tag Header Generator] Command registered');
            } catch (err) {
                console.warn('[Tag Header Generator] Failed to register command:', err);
            }
        }
    }

    // Export
    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.mumps = window.AhmadIDEModules.mumps || {};
        window.AhmadIDEModules.mumps.MumpsTagHeaderGenerator = MumpsTagHeaderGenerator;
        // Global fallback
        window.MumpsTagHeaderGenerator = MumpsTagHeaderGenerator;
    }
})();
