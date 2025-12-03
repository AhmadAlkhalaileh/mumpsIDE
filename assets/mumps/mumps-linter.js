/**
 * MUMPS Linter - XINDEX-Style Code Quality Checker
 * Analyzes MUMPS code for common issues and best practices
 */

class MUMPSLinter {
    constructor() {
        this.rules = null;
        this.config = null;
        this.loadRules();
    }

    async loadRules() {
        try {
            const response = await fetch('./assets/mumps/linter-rules.json');
            const data = await response.json();
            this.rules = data.rules;
            this.config = data.config;
        } catch (error) {
            this.rules = {};
            this.config = {};
        }
    }

    /**
     * Lint MUMPS source code
     * @param {string} code - MUMPS source code
     * @returns {Object} - Lint results with issues array
     */
    lint(code) {
        if (!this.rules) {
            return { issues: [], summary: { errors: 0, warnings: 0, info: 0 } };
        }

        const issues = [];
        const lines = code.split('\n');

        // Track state
        const newedVariables = new Set();
        const definedLabels = new Set();
        const calledLabels = new Set();
        const variables = new Set();

        // First pass: collect labels and calls
        for (let i = 0; i < lines.length; i++) {
            const line = lines[i];
            const trimmed = line.trim();

            // Skip empty lines and comments
            if (!trimmed || trimmed.startsWith(';')) continue;

            // Collect label definitions
            const labelMatch = trimmed.match(/^([A-Z%][A-Z0-9]*)\s*(\(|;|$)/i);
            if (labelMatch) {
                definedLabels.add(labelMatch[1].toUpperCase());
            }

            // Collect label calls (DO, GOTO)
            const doMatch = trimmed.match(/\b(?:DO|D|GOTO|G)\s+([A-Z%][A-Z0-9]*)/gi);
            if (doMatch) {
                doMatch.forEach(match => {
                    const label = match.split(/\s+/)[1];
                    if (label) calledLabels.add(label.toUpperCase());
                });
            }
        }

        // Second pass: analyze each line
        for (let i = 0; i < lines.length; i++) {
            const line = lines[i];
            const lineNum = i + 1;
            const trimmed = line.trim();

            // Skip empty lines and comments
            if (!trimmed || trimmed.startsWith(';')) continue;

            // Check if this is a label line
            const isLabel = /^[A-Z%][A-Z0-9]*\s*(\(|;|$)/i.test(trimmed);

            if (isLabel) {
                // Reset NEW tracking for new label
                newedVariables.clear();
            } else {
                // Analyze command line
                this.analyzeLine(line, lineNum, lines, newedVariables, variables, issues);
            }
        }

        // Check for unused labels (M004)
        if (this.config.checkUnusedLabels) {
            definedLabels.forEach(label => {
                if (!calledLabels.has(label) && label !== 'MAIN' && label !== Array.from(definedLabels)[0]) {
                    issues.push({
                        ruleId: 'M004',
                        severity: 'info',
                        line: this.findLabelLine(lines, label),
                        column: 1,
                        message: this.rules['M004-UNUSED-LABEL'].message.replace('{label}', label),
                        description: this.rules['M004-UNUSED-LABEL'].description
                    });
                }
            });
        }

        // Summarize issues
        const summary = {
            errors: issues.filter(i => i.severity === 'error').length,
            warnings: issues.filter(i => i.severity === 'warning').length,
            info: issues.filter(i => i.severity === 'info').length
        };

        return { issues, summary };
    }

    analyzeLine(line, lineNum, allLines, newedVariables, variables, issues) {
        const trimmed = line.trim();

        // M002: Check for unclosed strings
        const quotes = (trimmed.match(/"/g) || []).length;
        if (quotes % 2 !== 0) {
            issues.push({
                ruleId: 'M002',
                severity: 'error',
                line: lineNum,
                column: line.lastIndexOf('"') + 1,
                message: this.rules['M002-UNCLOSED-STRING'].message,
                description: this.rules['M002-UNCLOSED-STRING'].description
            });
        }

        // M003: Check for lowercase commands
        if (this.config.enforceUppercase) {
            const cmdMatch = trimmed.match(/^\s*\.?\s*([a-z]+)/);
            if (cmdMatch) {
                const cmd = cmdMatch[1].toUpperCase();
                const validCmds = ['SET', 'WRITE', 'DO', 'QUIT', 'FOR', 'IF', 'KILL', 'NEW', 'GOTO', 'HALT', 'HANG', 'READ'];
                if (validCmds.includes(cmd)) {
                    issues.push({
                        ruleId: 'M003',
                        severity: 'warning',
                        line: lineNum,
                        column: line.indexOf(cmdMatch[1]) + 1,
                        message: this.rules['M003-LOWERCASE-COMMAND'].message.replace('{cmd}', cmdMatch[1]),
                        description: this.rules['M003-LOWERCASE-COMMAND'].description,
                        autofix: true,
                        fix: { text: cmd }
                    });
                }
            }
        }

        // M007: Check for mismatched parentheses
        const openParen = (trimmed.match(/\(/g) || []).length;
        const closeParen = (trimmed.match(/\)/g) || []).length;
        if (openParen !== closeParen) {
            issues.push({
                ruleId: 'M007',
                severity: 'error',
                line: lineNum,
                column: openParen > closeParen ? line.lastIndexOf('(') + 1 : line.lastIndexOf(')') + 1,
                message: this.rules['M007-MISMATCHED-PARENS'].message,
                description: this.rules['M007-MISMATCHED-PARENS'].description
            });
        }

        // M001: Check for unnewed variables (simplified)
        if (this.config.checkUnusedVariables) {
            debugger;
            const newMatch = trimmed.match(/\b(?:NEW|N)\s+([A-Z%][A-Z0-9]*)/gi);
            if (newMatch) {
                newMatch.forEach(match => {
                    const varName = match.split(/\s+/)[1];
                    if (varName) newedVariables.add(varName.toUpperCase());
                });
            }

            const setMatch = trimmed.match(/\b(?:SET|S)\s+([A-Z%][A-Z0-9]*)/gi);
            if (setMatch) {
                setMatch.forEach(match => {
                    const varName = match.split(/\s+/)[1].split('=')[0];
                    if (varName && !varName.startsWith('^') && !varName.startsWith('$')) {
                        variables.add(varName.toUpperCase());
                        if (!newedVariables.has(varName.toUpperCase())) {
                            issues.push({
                                ruleId: 'M001',
                                severity: 'info',
                                line: lineNum,
                                column: line.indexOf(varName) + 1,
                                message: this.rules['M001-UNNEWED-VARIABLE'].message.replace('{var}', varName),
                                description: this.rules['M001-UNNEWED-VARIABLE'].description
                            });
                        }
                    }
                });
            }
        }


        // M009: Check for GOTO usage
        if (/\b(?:GOTO|G)\s+/i.test(trimmed)) {
            issues.push({
                ruleId: 'M009',
                severity: 'info',
                line: lineNum,
                column: 1,
                message: this.rules['M009-GOTO-CONSIDERED-HARMFUL'].message,
                description: this.rules['M009-GOTO-CONSIDERED-HARMFUL'].description
            });
        }

        // M013: Check line length
        if (this.config.maxLineLength && line.length > this.config.maxLineLength) {
            issues.push({
                ruleId: 'M013',
                severity: 'info',
                line: lineNum,
                column: this.config.maxLineLength + 1,
                message: this.rules['M013-LONG-LINE'].message.replace('{length}', line.length),
                description: this.rules['M013-LONG-LINE'].description
            });
        }

        // M016: Check for LOCK without timeout
        const lockMatch = trimmed.match(/\b(?:LOCK|L)\s+\+?([^\s:]+)(?::(\d+))?/i);
        if (lockMatch && !lockMatch[2]) {
            issues.push({
                ruleId: 'M016',
                severity: 'warning',
                line: lineNum,
                column: line.indexOf('LOCK') + 1 || line.indexOf('L') + 1,
                message: this.rules['M016-LOCK-WITHOUT-TIMEOUT'].message,
                description: this.rules['M016-LOCK-WITHOUT-TIMEOUT'].description
            });
        }

        // M005: Check for unreachable code after QUIT/HALT
        if (lineNum < allLines.length) {
            if (/\b(?:QUIT|Q|HALT|H)\s*$/i.test(trimmed)) {
                const nextLine = allLines[lineNum];
                if (nextLine && nextLine.trim() && !nextLine.trim().startsWith(';')) {
                    const nextIsLabel = /^[A-Z%][A-Z0-9]*\s*(\(|;|$)/i.test(nextLine.trim());
                    if (!nextIsLabel) {
                        issues.push({
                            ruleId: 'M005',
                            severity: 'warning',
                            line: lineNum + 1,
                            column: 1,
                            message: this.rules['M005-UNREACHABLE-CODE'].message,
                            description: this.rules['M005-UNREACHABLE-CODE'].description
                        });
                    }
                }
            }
        }
    }

    findLabelLine(lines, label) {
        for (let i = 0; i < lines.length; i++) {
            const match = lines[i].match(/^([A-Z%][A-Z0-9]*)/i);
            if (match && match[1].toUpperCase() === label) {
                return i + 1;
            }
        }
        return 1;
    }

    /**
     * Format lint results for display
     */
    formatResults(results) {
        if (results.issues.length === 0) {
            return '✓ No issues found';
        }

        let output = `Found ${results.issues.length} issue(s):\n\n`;

        results.issues.forEach(issue => {
            const icon = issue.severity === 'error' ? '✗' : issue.severity === 'warning' ? '⚠' : 'ℹ';
            output += `${icon} Line ${issue.line}, Col ${issue.column}: [${issue.ruleId}] ${issue.message}\n`;
        });

        output += `\nSummary: ${results.summary.errors} errors, ${results.summary.warnings} warnings, ${results.summary.info} info\n`;

        return output;
    }

    /**
     * Get issues for a specific line
     */
    getLineIssues(results, lineNumber) {
        return results.issues.filter(issue => issue.line === lineNumber);
    }
}

// Export
if (typeof module !== 'undefined' && module.exports) {
    module.exports = { MUMPSLinter };
}

if (typeof window !== 'undefined') {
    window.MUMPSLinter = MUMPSLinter;
}
