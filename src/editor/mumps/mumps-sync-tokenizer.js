/**
 * Synchronous MUMPS Tokenizer for Monaco
 * Bypasses Monaco's lazy tokenization to provide immediate syntax highlighting
 */

(() => {
    function createSyncMumpsTokenizer() {
        // Token types
        const TOKEN_TYPES = {
            LABEL: 'label',
            COMMENT: 'comment',
            STRING: 'string',
            KEYWORD: 'keyword',
            PREDEFINED: 'predefined',
            NUMBER: 'number',
            GLOBAL: 'type.identifier',
            IDENTIFIER: 'identifier'
        };

        // MUMPS keywords
        const KEYWORDS = new Set([
            'SET', 'S', 'NEW', 'N', 'KILL', 'K', 'DO', 'D',
            'IF', 'ELSE', 'FOR', 'F', 'QUIT', 'Q',
            'WRITE', 'W', 'READ', 'R', 'GOTO', 'G',
            'HANG', 'H', 'OPEN', 'O', 'CLOSE', 'C',
            'MERGE', 'M', 'VIEW', 'USE', 'LOCK', 'L',
            'XECUTE', 'X', 'TCOMMIT', 'TC', 'TSTART', 'TS'
        ]);

        /**
         * Tokenize a single line synchronously
         */
        function tokenizeLine(line) {
            const tokens = [];
            let i = 0;
            const len = line.length;

            // Check if it's a label (starts with letter/%)
            if (len > 0 && /^[A-Za-z%]/.test(line[0])) {
                let j = 0;
                while (j < len && /[A-Za-z0-9]/.test(line[j])) j++;
                if (j > 0) {
                    tokens.push({ startIndex: 0, type: TOKEN_TYPES.LABEL });
                    i = j;
                }
            }

            // Tokenize rest of line
            while (i < len) {
                const ch = line[i];

                // Comment
                if (ch === ';') {
                    tokens.push({ startIndex: i, type: TOKEN_TYPES.COMMENT });
                    break; // Rest of line is comment
                }

                // String
                else if (ch === '"') {
                    const start = i;
                    i++;
                    while (i < len) {
                        if (line[i] === '"') {
                            if (i + 1 < len && line[i + 1] === '"') {
                                i += 2; // Escaped quote
                            } else {
                                i++;
                                break;
                            }
                        } else {
                            i++;
                        }
                    }
                    tokens.push({ startIndex: start, type: TOKEN_TYPES.STRING });
                    continue;
                }

                // System variable/function (starts with $)
                else if (ch === '$') {
                    const start = i;
                    i++;
                    while (i < len && /[A-Za-z0-9]/.test(line[i])) i++;
                    tokens.push({ startIndex: start, type: TOKEN_TYPES.PREDEFINED });
                    continue;
                }

                // Global (starts with ^)
                else if (ch === '^') {
                    const start = i;
                    i++;
                    while (i < len && /[A-Za-z0-9]/.test(line[i])) i++;
                    tokens.push({ startIndex: start, type: TOKEN_TYPES.GLOBAL });
                    continue;
                }

                // Number
                else if (/[0-9]/.test(ch)) {
                    const start = i;
                    while (i < len && /[0-9.]/.test(line[i])) i++;
                    tokens.push({ startIndex: start, type: TOKEN_TYPES.NUMBER });
                    continue;
                }

                // Keyword or identifier
                else if (/[A-Za-z]/.test(ch)) {
                    const start = i;
                    while (i < len && /[A-Za-z0-9]/.test(line[i])) i++;
                    const word = line.substring(start, i).toUpperCase();
                    const type = KEYWORDS.has(word) ? TOKEN_TYPES.KEYWORD : TOKEN_TYPES.IDENTIFIER;
                    tokens.push({ startIndex: start, type });
                    continue;
                }

                // Skip other characters
                else {
                    i++;
                }
            }

            return tokens;
        }

        return { tokenizeLine };
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.mumpsSyncTokenizer = createSyncMumpsTokenizer();
    }

    if (typeof module !== 'undefined' && module.exports) {
        module.exports = { createSyncMumpsTokenizer };
    }
})();
