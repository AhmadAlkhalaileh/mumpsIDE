/**
 * MUMPS Routine Name Validator
 * Strict validation rules for MUMPS routine names
 */

class MUMPSValidator {
    constructor() {
        // Load token definitions
        this.tokens = null;
        this.loadTokens();
    }

    async loadTokens() {
        try {
            const response = await fetch('./assets/mumps/mumps-tokens.json');
            this.tokens = await response.json();
        } catch (error) {
            this.tokens = null;
        }
    }

    /**
     * Validate routine name according to MUMPS standards
     * Rules:
     * - Must be 1-16 characters
     * - Must start with uppercase letter A-Z
     * - Can only contain A-Z and 0-9
     * - No lowercase, spaces, or special characters
     */
    validateRoutineName(name) {
        const errors = [];
        // Check if empty
        if (!name || name.trim() === '') {
            return {
                valid: false,
                errors: ['Routine name cannot be empty']
            };
        }

        // Check length (1-16 characters)
        if (name.length < 1 || name.length > 16) {
            errors.push('Routine name must be 1-16 characters long');
        }

        // Check first character (must be uppercase letter)
        const firstChar = name.charAt(0);
        if (!/^[A-Z]$/.test(firstChar)) {
            if (/^[a-z]$/.test(firstChar)) {
                errors.push('Routine name must start with an UPPERCASE letter (A-Z)');
            } else if (/^[0-9]$/.test(firstChar)) {
                errors.push('Routine name cannot start with a digit');
            } else {
                errors.push('Routine name must start with an uppercase letter (A-Z)');
            }
        }

        // Check all characters (must be A-Z or 0-9)
        for (let i = 0; i < name.length; i++) {
            const char = name.charAt(i);
            if (!/^[A-Z0-9]$/.test(char)) {
                if (/^[a-z]$/.test(char)) {
                    errors.push(`Character '${char}' at position ${i + 1} must be UPPERCASE`);
                } else if (char === ' ') {
                    errors.push(`Spaces are not allowed in routine names (position ${i + 1})`);
                } else {
                    errors.push(`Invalid character '${char}' at position ${i + 1}. Only A-Z and 0-9 allowed`);
                }
                break; // Report first invalid character only
            }
        }

        return {
            valid: errors.length === 0,
            errors: errors
        };
    }

    /**
     * Validate routine label matches routine name
     */
    validateRoutineLabel(routineName, code) {
        const errors = [];

        // Extract first label from code
        const lines = code.split('\n');
        let firstLabel = null;

        for (const line of lines) {
            const trimmed = line.trim();
            if (trimmed === '' || trimmed.startsWith(';')) continue;

            // Check if line starts with a label (format: LABEL ; or LABEL(args) ;)
            const labelMatch = trimmed.match(/^([A-Z][A-Z0-9]*)\s*(;|\()/i);
            if (labelMatch) {
                firstLabel = labelMatch[1];
                break;
            }
        }

        if (firstLabel) {
            if (firstLabel !== routineName) {
                errors.push(`Routine label '${firstLabel}' must match routine name '${routineName}'`);
            }
            // Check if label is uppercase
            if (!/^[A-Z][A-Z0-9]*$/.test(firstLabel)) {
                errors.push(`Routine label must be uppercase (found: '${firstLabel}')`);
            }
        } else {
            errors.push(`Routine must start with a label matching the routine name '${routineName}'`);
        }

        return {
            valid: errors.length === 0,
            errors: errors
        };
    }

    /**
     * Validate routine code syntax
     */
    validateRoutineSyntax(code) {
        const errors = [];
        const warnings = [];
        const lines = code.split('\n');

        for (let i = 0; i < lines.length; i++) {
            const line = lines[i];
            const lineNum = i + 1;

            // Skip empty lines and comments
            if (line.trim() === '' || line.trim().startsWith(';') || line.trim().startsWith(' ')) continue;

            // Check for unclosed quotes
            const quotes = (line.match(/"/g) || []).length;
            if (quotes % 2 !== 0) {
                errors.push({
                    line: lineNum,
                    column: line.lastIndexOf('"') + 1,
                    severity: 'error',
                    code: 'M002',
                    message: 'Unclosed string literal'
                });
            }

            // Check for mismatched parentheses
            const openParen = (line.match(/\(/g) || []).length;
            const closeParen = (line.match(/\)/g) || []).length;
            if (openParen !== closeParen) {
                errors.push({
                    line: lineNum,
                    column: 1,
                    severity: 'error',
                    code: 'M007',
                    message: 'Mismatched parentheses'
                });
            }

            // Check for lowercase commands (if not in string)
            const commandMatch = line.trim().match(/^\.?\s*([a-z]+)/);
            if (commandMatch && !this.isInString(line, commandMatch.index)) {
                warnings.push({
                    line: lineNum,
                    column: line.indexOf(commandMatch[1]) + 1,
                    severity: 'warning',
                    code: 'M003',
                    message: `Command '${commandMatch[1]}' should be uppercase`
                });
            }
        }

        return {
            valid: errors.length === 0,
            errors: errors,
            warnings: warnings
        };
    }

    /**
     * Check if position is inside a string
     */
    isInString(line, pos) {
        let inString = false;
        for (let i = 0; i < pos && i < line.length; i++) {
            if (line[i] === '"') inString = !inString;
        }
        return inString;
    }

    /**
     * Generate routine template
     */
    generateRoutineTemplate(routineName) {
        const today = new Date();
        const dateStr = today.toLocaleDateString('en-US', {
            year: 'numeric',
            month: 'short',
            day: 'numeric'
        }).replace(',', '').toUpperCase();

        return `${routineName} ;EHS/AKH - routine description ; ${dateStr}
;;version number;global;**patch number**;date
;Description :
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Entry point
;
 QUIT
`;
    }

    /**
     * Format routine with proper label
     */
    formatRoutine(routineName, code) {
        const lines = code.split('\n');
        const trimmedLines = [];
        let hasLabel = false;

        for (const line of lines) {
            const trimmed = line.trim();
            if (trimmed === '' || trimmed.startsWith(';')) {
                trimmedLines.push(line);
                continue;
            }

            // Check if first non-empty line is a label
            if (!hasLabel) {
                const labelMatch = trimmed.match(/^([A-Z][A-Z0-9]*)\s*(;|\()/i);
                if (labelMatch) {
                    hasLabel = true;
                    // Replace label with routine name if different
                    trimmedLines.push(line.replace(labelMatch[1], routineName));
                } else {
                    // Insert routine label
                    trimmedLines.unshift(`${routineName} ;`);
                    trimmedLines.push(line);
                    hasLabel = true;
                }
            } else {
                trimmedLines.push(line);
            }
        }

        // If no label found, prepend one
        if (!hasLabel) {
            trimmedLines.unshift(`${routineName} ;`);
        }

        return trimmedLines.join('\n');
    }
}

// Export for use in browser
if (typeof window !== 'undefined') {
    window.MUMPSValidator = MUMPSValidator;
}
