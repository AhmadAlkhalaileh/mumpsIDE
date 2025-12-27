/**
 * MUMPS Linter - XINDEX-Style Code Quality Checker
 * Analyzes MUMPS code for common issues and best practices
 *
 * Style enforced:
 * - Every label starts at column 1 (no leading spaces).
 * - All executable lines under a label must be indented (at least one leading space).
 * - Label names: 1–16 chars, uppercase, [A-Z%][A-Z0-9]*.
 * - Commands may be abbreviated (Q, I, F, W, etc.), validated as real commands.
 * - Comma rules:
 *     - Trailing comma (end-of-line) is error.
 *     - Empty argument between commas is error.
 */
// Match label at column 1: label can be followed by space+command, params, comment, or EOL
const RE_LABEL_AT_COL1 = /^([A-Z%][A-Z0-9]*)(\s+[A-Z]|\s*\(|\s*;|\s*$)/i;
const RE_LABEL_TRIM = /^([A-Z%][A-Z0-9]*)\s*(\(|;|$)/i;
const RE_DO_GOTO = /\b(?:DO|D|GOTO|G)\s+([A-Z%][A-Z0-9]*)/gi;

class MUMPSLinter {
    constructor() {
        this.rules = {};
        this.config = {
            enforceUppercase: true,
            checkUnusedLabels: true,
            checkUnusedVariables: false,  // M001 is only informational
            maxLineLength: 255,
            maxNestingDepth: 3,
            checkMagicNumbers: false,
            checkUnreachableAfterQuit: true,
            disallowSpaceAfterComma: true, // No spaces allowed after commas in MUMPS syntax
            enforceLabelIndentation: true,
            enforceCommandIndentation: true
        };

        this.loadRules();
    }

    async loadRules() {
        const url = './assets/mumps/linter-rules.json';

        try {
            if (typeof window !== 'undefined' && typeof $ !== 'undefined' && $.getJSON) {
                const data = await new Promise((resolve, reject) => {
                    $.getJSON(url).done(resolve).fail(reject);
                });
                this.rules = data.rules || {};
                this.config = Object.assign({}, this.config, data.config || {});
                return;
            }

            if (typeof fetch !== 'undefined') {
                const response = await fetch(url);
                if (!response.ok) throw new Error(`HTTP ${response.status}`);
                const data = await response.json();
                this.rules = data.rules || {};
                this.config = Object.assign({}, this.config, data.config || {});
                return;
            }
        } catch (error) {
            // Keep defaults
        }
    }

    getRuleMeta(code, defaultMessage, defaultDescription) {
        const rule = this.rules && this.rules[code];
        return {
            message: (rule && rule.message) || defaultMessage,
            description: (rule && rule.description) || defaultDescription
        };
    }

    getValidCommands() {
        return [
            // Standard MUMPS commands (ANSI X11.1)
            'BREAK', 'CLOSE', 'DO', 'ELSE', 'FOR', 'GOTO', 'HALT',
            'HANG', 'IF', 'JOB', 'KILL', 'LOCK', 'MERGE', 'NEW',
            'OPEN', 'QUIT', 'READ', 'SET', 'USE', 'VIEW', 'WRITE',
            'XECUTE',
            // Transaction processing commands
            'TCOMMIT', 'TRESTART', 'TROLLBACK', 'TSTART',
            // YottaDB/GT.M Z-commands (extensions)
            'ZALLOCATE', 'ZBREAK', 'ZCOMPILE', 'ZCONTINUE', 'ZDEALLOCATE',
            'ZEDIT', 'ZGOTO', 'ZHALT', 'ZHELP', 'ZKILL', 'ZLINK',
            'ZMESSAGE', 'ZPRINT', 'ZRUPDATE', 'ZSHOW', 'ZSTEP',
            'ZSYSTEM', 'ZTCOMMIT', 'ZTRIGGER', 'ZTSTART', 'ZWITHDRAW', 'ZWRITE'
        ];
    }

    getCommandMatches(token) {
        if (!token) return [];
        const t = token.toUpperCase();
        const cmds = this.getValidCommands();
        return cmds.filter(c => c.startsWith(t));
    }

    isCommandToken(token) {
        return this.getCommandMatches(token).length > 0;
    }

    /**
     * Lint MUMPS source code
     * @param {string} code
     * @returns {{issues: Array, summary: {errors: number, warnings: number, info: number}}}
     */
    lint(code) {
        const cfg = Object.assign({}, this.config);
        const issues = [];
        const lines = code.split('\n');

        const newedVariables = new Set();
        const definedLabels = new Set();
        const calledLabels = new Set();
        const variables = new Set();
        let firstLabelName = null;

        // First pass: labels & calls
        for (let i = 0; i < lines.length; i++) {
            const line = lines[i];
            const trimmed = line.trim();

            if (!trimmed || trimmed.startsWith(';')) continue;

            const labelMatchLine = line.match(RE_LABEL_AT_COL1);
            if (labelMatchLine) {
                const name = labelMatchLine[1].toUpperCase();
                definedLabels.add(name);
                if (!firstLabelName) {
                    firstLabelName = name;
                }
            }

            RE_DO_GOTO.lastIndex = 0;
            let refMatch;
            while ((refMatch = RE_DO_GOTO.exec(trimmed)) !== null) {
                const label = refMatch[1];
                if (label) calledLabels.add(label.toUpperCase());
            }
        }

        // Second pass: analyze lines
        for (let i = 0; i < lines.length; i++) {
            const line = lines[i];
            const lineNum = i + 1;

            const leadingSpacesMatch = line.match(/^\s*/);
            const leadingSpaces = leadingSpacesMatch ? leadingSpacesMatch[0].length : 0;
            const trimmed = line.trim();

            if (!trimmed || trimmed.startsWith(';')) continue;

            // Check if line is a label at column 1 first
            const isLabelAtColumn1 = RE_LABEL_AT_COL1.test(line);

            // Extract first token for command checking (but only if not a label)
            const firstTokenMatch = trimmed.match(/^([A-Z%][A-Z0-9]*)/i);
            const firstToken = firstTokenMatch ? firstTokenMatch[1] : null;
            const isCommandKeyword = (!isLabelAtColumn1 && firstToken) ? this.isCommandToken(firstToken) : false;

            if (isLabelAtColumn1) {
                // Proper label line
                if (cfg.enforceLabelIndentation) {
                    // OK by definition
                }

                // Validate label name: 1-16 chars, [A-Z%][A-Z0-9]*
                const match = line.match(/^([A-Z%][A-Z0-9]*)/i);
                if (match) {
                    const rawLabel = match[1];
                    const upperLabel = rawLabel.toUpperCase();
                    const validPattern = /^[A-Z%][A-Z0-9]*$/;
                    const tooLong = upperLabel.length > 16; // MUMPS standard: 16 char max

                    if (!validPattern.test(upperLabel) || tooLong) {
                        const meta = this.getRuleMeta(
                            'M028-INVALID-LABEL',
                            "Invalid label name '{label}'",
                            'Labels must be 1–16 uppercase alphanumeric characters starting with a letter or %'
                        );
                        issues.push({
                            ruleId: 'M028',
                            severity: (this.rules['M028-INVALID-LABEL'] && this.rules['M028-INVALID-LABEL'].severity) || 'error',
                            line: lineNum,
                            column: 1,
                            message: meta.message.replace('{label}', rawLabel),
                            description: meta.description
                        });
                    }
                }

                newedVariables.clear();
            } else {
                // Mis-indented label (not at col1, not a command)
                // Don't flag QUIT commands (Q/QUIT) as mis-indented labels
                const isQuitCommand = /^\s*Q(?:UIT)?(\s|$|;)/i.test(line);

                if (
                    cfg.enforceLabelIndentation &&
                    leadingSpaces > 0 &&
                    !isCommandKeyword &&
                    !isQuitCommand &&
                    RE_LABEL_TRIM.test(trimmed)
                ) {
                    const meta = this.getRuleMeta(
                        'M021-LABEL-AT-COLUMN1',
                        'Label must start at column 1 (no leading spaces)',
                        'Routine labels must be placed in the first column without indentation'
                    );
                    issues.push({
                        ruleId: 'M021',
                        severity: (this.rules['M021-LABEL-AT-COLUMN1'] && this.rules['M021-LABEL-AT-COLUMN1'].severity) || 'error',
                        line: lineNum,
                        column: 1,
                        message: meta.message,
                        description: meta.description
                    });
                }

                // Command indentation (style preference, not an error)
                if (cfg.enforceCommandIndentation) {
                    if (leadingSpaces === 0) {
                        const meta = this.getRuleMeta(
                            'M022-COMMAND-INDENTATION',
                            'Executable lines should be indented under their label',
                            'Commands associated with a label should be indented at least one character to the right for better readability'
                        );
                        issues.push({
                            ruleId: 'M022',
                            severity: (this.rules['M022-COMMAND-INDENTATION'] && this.rules['M022-COMMAND-INDENTATION'].severity) || 'info',
                            line: lineNum,
                            column: 1,
                            message: meta.message,
                            description: meta.description
                        });
                    }
                }

                this.analyzeLine(
                    line,
                    lineNum,
                    lines,
                    newedVariables,
                    variables,
                    issues,
                    cfg
                );
            }
        }

        // M004 Unused labels check - DISABLED
        // Labels may be called externally or used as entry points, so this check is not reliable

        const summary = {
            errors: issues.filter(i => i.severity === 'error').length,
            warnings: issues.filter(i => i.severity === 'warning').length,
            info: issues.filter(i => i.severity === 'info').length
        };

        return { issues, summary };
    }

    analyzeLine(line, lineNum, allLines, newedVariables, variables, issues, cfg) {
        const trimmed = line.trim();
        const codePart = this.extractCodeBeforeComment(trimmed);

        // Unknown / ambiguous command
        {
            const cmdMatch = codePart.match(/^\s*\.?\s*([A-Za-z%]+)/);
            if (cmdMatch) {
                const rawCmd = cmdMatch[1];
                const matches = this.getCommandMatches(rawCmd);
                const upperCmd = rawCmd.toUpperCase();

                if (matches.length === 0) {
                    const meta = this.getRuleMeta(
                        'M026-UNKNOWN-COMMAND',
                        "Unknown or invalid command '{cmd}'",
                        'Command is neither a full keyword nor a valid abbreviation'
                    );
                    issues.push({
                        ruleId: 'M026',
                        severity: (this.rules['M026-UNKNOWN-COMMAND'] && this.rules['M026-UNKNOWN-COMMAND'].severity) || 'error',
                        line: lineNum,
                        column: line.indexOf(rawCmd) + 1,
                        message: meta.message.replace('{cmd}', rawCmd),
                        description: meta.description
                    });
                } else if (matches.length > 1 && !matches.includes(upperCmd)) {
                    // Special case: 'H' followed by a number is clearly HANG, not ambiguous
                    let isAmbiguous = true;
                    if (upperCmd === 'H') {
                        // Check if followed by whitespace and a number (HANG timeout)
                        const afterCmd = codePart.slice(cmdMatch.index + rawCmd.length);
                        if (/^\s+\d/.test(afterCmd)) {
                            isAmbiguous = false; // It's HANG with timeout
                        }
                    }

                    if (isAmbiguous) {
                        const meta = this.getRuleMeta(
                            'M027-AMBIGUOUS-COMMAND',
                            "Ambiguous command abbreviation '{cmd}'",
                            'Use a longer command name or the full keyword to avoid ambiguity'
                        );
                        issues.push({
                            ruleId: 'M027',
                            severity: (this.rules['M027-AMBIGUOUS-COMMAND'] && this.rules['M027-AMBIGUOUS-COMMAND'].severity) || 'warning',
                            line: lineNum,
                            column: line.indexOf(rawCmd) + 1,
                            message: meta.message.replace('{cmd}', rawCmd),
                            description: meta.description
                        });
                    }
                }
            }
        }

        // Unclosed string
        {
            const quoteCount = (codePart.match(/"/g) || []).length;
            if (quoteCount % 2 !== 0) {
                const meta = this.getRuleMeta(
                    'M002-UNCLOSED-STRING',
                    'Unclosed string literal',
                    'String literals must contain an even number of double quotes'
                );
                issues.push({
                    ruleId: 'M002',
                    severity: (this.rules['M002-UNCLOSED-STRING'] && this.rules['M002-UNCLOSED-STRING'].severity) || 'error',
                    line: lineNum,
                    column: line.lastIndexOf('"') + 1 || 1,
                    message: meta.message,
                    description: meta.description
                });
            }
        }

        // Lowercase command at start
        if (cfg.enforceUppercase) {
            const cmdMatchLc = codePart.match(/^\s*\.?\s*([a-z]+)/);
            if (cmdMatchLc) {
                const rawCmd = cmdMatchLc[1];
                const matches = this.getCommandMatches(rawCmd);
                if (matches.length > 0) {
                    const upperCmd = rawCmd.toUpperCase();
                    const meta = this.getRuleMeta(
                        'M003-LOWERCASE-COMMAND',
                        "Command '{cmd}' should be uppercase",
                        'MUMPS commands should be uppercase for consistency'
                    );
                    issues.push({
                        ruleId: 'M003',
                        severity: (this.rules['M003-LOWERCASE-COMMAND'] && this.rules['M003-LOWERCASE-COMMAND'].severity) || 'warning',
                        line: lineNum,
                        column: line.indexOf(rawCmd) + 1,
                        message: meta.message.replace('{cmd}', rawCmd),
                        description: meta.description,
                        autofix: true,
                        fix: { text: upperCmd }
                    });
                }
            }
        }

        // Mismatched parens (skip strings)
        {
            let openParen = 0;
            let closeParen = 0;
            let inString = false;

            for (let i = 0; i < codePart.length; i++) {
                const ch = codePart[i];
                if (ch === '"') {
                    inString = !inString;
                } else if (!inString) {
                    if (ch === '(') openParen++;
                    else if (ch === ')') closeParen++;
                }
            }

            if (openParen !== closeParen) {
                const meta = this.getRuleMeta(
                    'M007-MISMATCHED-PARENS',
                    'Mismatched parentheses',
                    'Opening and closing parentheses must match'
                );
                issues.push({
                    ruleId: 'M007',
                    severity: (this.rules['M007-MISMATCHED-PARENS'] && this.rules['M007-MISMATCHED-PARENS'].severity) || 'error',
                    line: lineNum,
                    column:
                        openParen > closeParen
                            ? line.lastIndexOf('(') + 1
                            : line.lastIndexOf(')') + 1,
                    message: meta.message,
                    description: meta.description
                });
            }
        }

        // M020: space after comma (MUMPS syntax rule)
        if (this.config.disallowSpaceAfterComma) {
            const meta = this.getRuleMeta(
                'M020-SPACE-AFTER-COMMA',
                'Space after comma not allowed',
                'MUMPS syntax does not allow spaces after commas in argument lists'
            );

            let inString = false;
            const raw = line;
            for (let i = 0; i < raw.length; i++) {
                const ch = raw[i];

                if (ch === '"') {
                    inString = !inString;
                    continue;
                }

                if (!inString && ch === ',') {
                    let j = i + 1;
                    let spaceCount = 0;
                    while (j < raw.length && (raw[j] === ' ' || raw[j] === '\t')) {
                        spaceCount++;
                        j++;
                    }

                    if (
                        spaceCount > 0 &&
                        j < raw.length &&
                        raw[j] !== ';' &&
                        raw[j] !== '\r' &&
                        raw[j] !== '\n'
                    ) {
                        issues.push({
                            ruleId: 'M020',
                            severity: (this.rules['M020-SPACE-AFTER-COMMA'] && this.rules['M020-SPACE-AFTER-COMMA'].severity) || 'error',
                            line: lineNum,
                            column: i + 2,
                            message: meta.message,
                            description: meta.description
                        });
                    }
                }
            }
        }

        // Trailing comma (M023)
        {
            const meta = this.getRuleMeta(
                'M023-TRAILING-COMMA',
                'Trailing comma at end of command',
                'Commands cannot end with a comma; remove the comma or add the missing argument'
            );

            let inString = false;
            let lastNonWs = null;
            const raw = codePart;

            for (let i = 0; i < raw.length; i++) {
                const ch = raw[i];

                if (ch === '"') {
                    inString = !inString;
                    // Include quotes as non-whitespace so commas before strings aren't flagged
                    lastNonWs = { ch, index: i };
                    continue;
                }

                if (!inString && ch !== ' ' && ch !== '\t') {
                    lastNonWs = { ch, index: i };
                }
            }

            // Only flag if last non-whitespace char is a comma (not a quote or other char)
            if (lastNonWs && lastNonWs.ch === ',') {
                const trimmedLine = line.trim();
                const startOfCodeInLine = line.indexOf(trimmedLine);
                const column = startOfCodeInLine + lastNonWs.index + 1;

                issues.push({
                    ruleId: 'M023',
                    severity: (this.rules['M023-TRAILING-COMMA'] && this.rules['M023-TRAILING-COMMA'].severity) || 'error',
                    line: lineNum,
                    column,
                    message: meta.message,
                    description: meta.description
                });
            }
        }

        // Empty argument between commas (M024)
        // NOTE: Empty arguments are VALID in function parameters (e.g., FUNC(1,,3))
        //       but INVALID in command arguments (e.g., WRITE "a",,!)
        {
            const meta = this.getRuleMeta(
                'M024-EMPTY-ARGUMENT',
                'Empty argument between commas in command',
                'Command arguments cannot be empty; function parameters may have empty arguments'
            );

            let inString = false;
            let parenDepth = 0;
            const raw = codePart;

            for (let i = 0; i < raw.length; i++) {
                const ch = raw[i];

                if (ch === '"') {
                    inString = !inString;
                    continue;
                }

                if (!inString) {
                    if (ch === '(') parenDepth++;
                    else if (ch === ')') parenDepth--;

                    // Only flag empty arguments OUTSIDE parentheses (command args)
                    if (ch === ',' && parenDepth === 0) {
                        let j = i + 1;
                        while (j < raw.length && (raw[j] === ' ' || raw[j] === '\t')) {
                            j++;
                        }
                        if (j >= raw.length || raw[j] === ',' || raw[j] === ';') {
                            const trimmedLine = line.trim();
                            const startOfCodeInLine = line.indexOf(trimmedLine);
                            const column = startOfCodeInLine + i + 1;
                            issues.push({
                                ruleId: 'M024',
                                severity: (this.rules['M024-EMPTY-ARGUMENT'] && this.rules['M024-EMPTY-ARGUMENT'].severity) || 'error',
                                line: lineNum,
                                column,
                                message: meta.message,
                                description: meta.description
                            });
                        }
                    }
                }
            }
        }

        // NEW / SET (M001)
        {
            const newRegex = /\b(?:NEW|N)\s+([^;]+)/gi;
            let newMatch;
            while ((newMatch = newRegex.exec(codePart)) !== null) {
                const list = newMatch[1]
                    .split(',')
                    .map(t => t.trim())
                    .filter(Boolean);

                list.forEach(token => {
                    const name = token.replace(/^[@*]*/, '').split('(')[0];
                    if (name && /^[A-Z%][A-Z0-9]*$/i.test(name)) {
                        newedVariables.add(name.toUpperCase());
                    }
                });
            }

            const setRegex = /\b(?:SET|S)\s+([^;]+)/gi;
            let setMatch;
            while ((setMatch = setRegex.exec(codePart)) !== null) {
                const rhs = setMatch[1];

                const assignments = rhs.split(',');
                assignments.forEach(assign => {
                    const part = assign.trim();
                    const lhs = part.split('=')[0].trim();
                    if (!lhs) return;

                    if (lhs[0] === '^' || lhs[0] === '$') return;

                    const varName = lhs.split('(')[0];

                    if (varName && /^[A-Z%][A-Z0-9]*$/i.test(varName)) {
                        const upper = varName.toUpperCase();
                        variables.add(upper);

                        if (!newedVariables.has(upper)) {
                            const meta = this.getRuleMeta(
                                'M001-UNNEWED-VARIABLE',
                                "Variable '{var}' used without NEW",
                                'Local variables should be NEWed before use to avoid unintended side effects'
                            );
                            const severity =
                                (this.rules['M001-UNNEWED-VARIABLE'] && this.rules['M001-UNNEWED-VARIABLE'].severity) || 'info';

                            issues.push({
                                ruleId: 'M001',
                                severity,
                                line: lineNum,
                                column: line.indexOf(varName) + 1,
                                message: meta.message.replace('{var}', varName),
                                description: meta.description
                            });
                        }
                    }
                });
            }
        }

        // GOTO considered harmful
        if (/\b(?:GOTO|G)\s+/i.test(codePart)) {
            const meta = this.getRuleMeta(
                'M009-GOTO-CONSIDERED-HARMFUL',
                'Consider using DO instead of GOTO',
                'GOTO can make code harder to follow; DO is preferred'
            );
            const upper = line.toUpperCase();
            const gIndex = upper.indexOf('GOTO') !== -1
                ? upper.indexOf('GOTO')
                : upper.indexOf(' G ');
            issues.push({
                ruleId: 'M009',
                severity: (this.rules['M009-GOTO-CONSIDERED-HARMFUL'] && this.rules['M009-GOTO-CONSIDERED-HARMFUL'].severity) || 'info',
                line: lineNum,
                column: (gIndex !== -1 ? gIndex : 0) + 1,
                message: meta.message,
                description: meta.description
            });
        }

        // Line length
        if (cfg.maxLineLength && line.length > cfg.maxLineLength) {
            const meta = this.getRuleMeta(
                'M013-LONG-LINE',
                `Line exceeds ${cfg.maxLineLength} characters ({length} chars)`,
                'Long lines can be hard to read'
            );

            issues.push({
                ruleId: 'M013',
                severity: (this.rules['M013-LONG-LINE'] && this.rules['M013-LONG-LINE'].severity) || 'info',
                line: lineNum,
                column: cfg.maxLineLength + 1,
                message: meta.message.replace('{length}', line.length),
                description: meta.description
            });
        }

        // LOCK without timeout
        {
            const lockRegex = /\b(?:LOCK|L)\s+\+?([^\s:]+)(?::(\d+))?/i;
            const lockMatch = lockRegex.exec(codePart);
            if (lockMatch && !lockMatch[2]) {
                const meta = this.getRuleMeta(
                    'M016-LOCK-WITHOUT-TIMEOUT',
                    'LOCK without timeout may cause deadlock',
                    'Always specify timeout for LOCK commands'
                );

                const upper = line.toUpperCase();
                const lockIndex =
                    upper.indexOf('LOCK') !== -1
                        ? upper.indexOf('LOCK')
                        : upper.indexOf(' L ');

                issues.push({
                    ruleId: 'M016',
                    severity: (this.rules['M016-LOCK-WITHOUT-TIMEOUT'] && this.rules['M016-LOCK-WITHOUT-TIMEOUT'].severity) || 'warning',
                    line: lineNum,
                    column: (lockIndex !== -1 ? lockIndex : 0) + 1,
                    message: meta.message,
                    description: meta.description
                });
            }
        }

        // Unreachable after QUIT/HALT on same line
        if (this.config.checkUnreachableAfterQuit) {
            const upperCode = codePart.toUpperCase();
            const quitRegex = /\b(QUIT|Q|HALT|H)\b/;
            const qm = quitRegex.exec(upperCode);

            if (qm) {
                const cmdText = qm[1];
                const cmdPos = qm.index;

                const afterCmd = codePart.slice(cmdPos + cmdText.length);
                let rest = afterCmd;

                const spacesMatch = rest.match(/^(\s+)/);
                if (spacesMatch && spacesMatch[1].length >= 2) {
                    const afterSpaces = rest.slice(spacesMatch[1].length);

                    if (afterSpaces && /^[A-Z%]/i.test(afterSpaces[0])) {
                        const meta = this.getRuleMeta(
                            'M005-UNREACHABLE-CODE',
                            'Code after QUIT/HALT is unreachable',
                            'Commands after QUIT or HALT on the same line will never execute'
                        );

                        const full = trimmed;
                        const cmdChar = afterSpaces[0];
                        const column =
                            full.indexOf(cmdChar, cmdPos + cmdText.length) + 1 || 1;

                        issues.push({
                            ruleId: 'M005',
                            severity: (this.rules['M005-UNREACHABLE-CODE'] && this.rules['M005-UNREACHABLE-CODE'].severity) || 'warning',
                            line: lineNum,
                            column,
                            message: meta.message,
                            description: meta.description
                        });
                    }
                }
            }
        }
    }

    extractCodeBeforeComment(line) {
        // Extract code before comment, respecting strings
        let inString = false;
        for (let i = 0; i < line.length; i++) {
            if (line[i] === '"') {
                inString = !inString;
            } else if (line[i] === ';' && !inString) {
                return line.substring(0, i);
            }
        }
        return line;
    }

    findLabelLine(lines, label) {
        const upper = label.toUpperCase();
        for (let i = 0; i < lines.length; i++) {
            const match = lines[i].match(/^([A-Z%][A-Z0-9]*)/i);
            if (match && match[1].toUpperCase() === upper) {
                return i + 1;
            }
        }
        return 1;
    }

    formatResults(results) {
        if (!results.issues.length) {
            return '✓ No issues found';
        }

        let output = `Found ${results.issues.length} issue(s):\n\n`;

        results.issues.forEach(issue => {
            const icon =
                issue.severity === 'error'
                    ? '✗'
                    : issue.severity === 'warning'
                        ? '⚠'
                        : 'ℹ';
            output += `${icon} Line ${issue.line}, Col ${issue.column}: [${issue.ruleId}] (${issue.severity.toUpperCase()}) ${issue.message}\n`;
        });

        output += `\nSummary: ${results.summary.errors} errors, ${results.summary.warnings} warnings, ${results.summary.info} info\n`;

        return output;
    }

    getLineIssues(results, lineNumber) {
        return results.issues.filter(issue => issue.line === lineNumber);
    }
}

if (typeof module !== 'undefined' && module.exports) {
    module.exports = { MUMPSLinter };
}

if (typeof window !== 'undefined') {
    window.MUMPSLinter = MUMPSLinter;
}
