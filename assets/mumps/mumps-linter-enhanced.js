/**
 * MUMPS Linter Enhanced - Extension Module
 * Adds advanced linting rules to existing MUMPSLinter
 *
 * NEW RULES:
 * - M030: Trailing whitespace
 * - M031: Mixed tabs and spaces
 * - M032: Inconsistent indentation
 * - M033: Wrong indentation alignment
 * - M034: Excessive GOTO usage
 * - M035: Backward GOTO jump
 * - M036: GOTO spaghetti (complex jump patterns)
 */

(function() {
    'use strict';

    // Extend the existing MUMPSLinter class
    if (typeof window === 'undefined' || !window.MUMPSLinter) {
        console.error('[Linter Enhanced] MUMPSLinter base class not found');
        return;
    }

    const OriginalLinter = window.MUMPSLinter;

    class MUMPSLinterEnhanced extends OriginalLinter {
        constructor() {
            super();

            // Add new config options
            this.config = Object.assign(this.config || {}, {
                checkTrailingWhitespace: true,
                checkMixedIndentation: true,
                checkIndentationAlignment: true,
                indentSize: 4,  // Configurable indent size
                maxGotoPerTag: 3,  // Warn if > 3 GOTO statements per tag
                warnBackwardGoto: true,
                detectGotoSpaghetti: true
            });
        }

        /**
         * Helper: Get rule metadata
         */
        getRuleMeta(id, message, description) {
            return { id, message, description };
        }

        /**
         * Helper: Extract code before comment
         */
        extractCodeBeforeComment(line) {
            const commentIndex = line.indexOf(';');
            return commentIndex >= 0 ? line.substring(0, commentIndex) : line;
        }

        /**
         * Helper: Check if token is a MUMPS command
         */
        isCommandToken(token) {
            const commands = [
                'SET', 'WRITE', 'READ', 'DO', 'GOTO', 'IF', 'ELSE', 'FOR', 'QUIT',
                'HALT', 'HANG', 'JOB', 'KILL', 'LOCK', 'MERGE', 'NEW', 'OPEN',
                'CLOSE', 'USE', 'VIEW', 'XECUTE', 'BREAK', 'TSTART', 'TCOMMIT',
                'TROLLBACK', 'TRESTART'
            ];
            return commands.includes(token.toUpperCase());
        }

        /**
         * Enhanced lint - calls parent then adds new rules
         */
        lint(code, options) {
            // Call parent linter first
            const result = super.lint(code, options);
            const issues = result.issues || [];
            const lines = code.split('\n');

            // Add enhanced rules
            this.checkWhitespaceIssues(lines, issues);
            this.checkVariableHygiene(lines, issues);
            this.checkIndentationAlignment(lines, issues);
            const gotoData = this.analyzeGotoPatterns(lines, issues);

            // Recalculate summary
            result.summary = {
                errors: issues.filter(i => i.severity === 'error').length,
                warnings: issues.filter(i => i.severity === 'warning').length,
                info: issues.filter(i => i.severity === 'info').length
            };

            // Store GOTO analysis for visualization
            result.gotoAnalysis = gotoData;

            return result;
        }

        /**
         * M030, M031, M032: Whitespace issues
         */
        checkWhitespaceIssues(lines, issues) {
            if (!this.config.checkTrailingWhitespace && !this.config.checkMixedIndentation) {
                return;
            }

            let hasSpaces = false;
            let hasTabs = false;

            for (let i = 0; i < lines.length; i++) {
                const line = lines[i];
                const lineNum = i + 1;

                // Skip empty lines
                if (line.trim() === '') continue;

                // M030: Trailing whitespace
                if (this.config.checkTrailingWhitespace) {
                    const trailingMatch = line.match(/[ \t]+$/);
                    if (trailingMatch) {
                        const meta = this.getRuleMeta(
                            'M030-TRAILING-WHITESPACE',
                            'Trailing whitespace at end of line',
                            'Remove trailing spaces and tabs for cleaner code'
                        );

                        issues.push({
                            ruleId: 'M030',
                            severity: 'info',
                            line: lineNum,
                            column: line.length - trailingMatch[0].length + 1,
                            message: meta.message,
                            description: meta.description,
                            autofix: true,
                            fix: { removeTrailingWhitespace: true }
                        });
                    }
                }

                // M031: Mixed tabs and spaces (file-level check)
                if (this.config.checkMixedIndentation) {
                    const leadingMatch = line.match(/^(\s+)/);
                    if (leadingMatch) {
                        const leading = leadingMatch[1];
                        if (/\t/.test(leading)) hasTabs = true;
                        if (/ /.test(leading)) hasSpaces = true;

                        // If line has both tabs and spaces in leading whitespace
                        if (/\t/.test(leading) && / /.test(leading)) {
                            const meta = this.getRuleMeta(
                                'M031-MIXED-INDENTATION',
                                'Mixed tabs and spaces in indentation',
                                'Use either tabs or spaces consistently, not both'
                            );

                            issues.push({
                                ruleId: 'M031',
                                severity: 'warning',
                                line: lineNum,
                                column: 1,
                                message: meta.message,
                                description: meta.description,
                                autofix: true,
                                fix: { normalizeIndentation: true, indentSize: this.config.indentSize }
                            });
                        }
                    }
                }
            }

            // M032: Mixed tabs/spaces across file (file-level warning)
            if (this.config.checkMixedIndentation && hasSpaces && hasTabs) {
                const meta = this.getRuleMeta(
                    'M032-INCONSISTENT-INDENTATION',
                    'File uses both tabs and spaces for indentation',
                    'Choose either tabs or spaces and use consistently throughout the file'
                );

                issues.push({
                    ruleId: 'M032',
                    severity: 'info',
                    line: 1,
                    column: 1,
                    message: meta.message,
                    description: meta.description,
                    autofix: true,
                    fix: { normalizeFileIndentation: true, preferSpaces: hasSpaces, indentSize: this.config.indentSize }
                });
            }
        }

        /**
         * M033: Indentation alignment checker
         * Fixed to detect file's indent style and only warn about inconsistencies
         */
        checkIndentationAlignment(lines, issues) {
            if (!this.config.checkIndentationAlignment) return;

            const RE_LABEL = /^([A-Z%][A-Z0-9]*)\s*(\(|;|$)/i;

            // Detect the file's actual indent style (1, 2, 4, etc.)
            const indentSizes = new Map(); // size -> count
            for (let i = 0; i < lines.length; i++) {
                const line = lines[i];
                const trimmed = line.trim();

                // Skip empty lines, comments, and labels
                if (!trimmed || trimmed.startsWith(';') || RE_LABEL.test(line)) continue;

                const leadingMatch = line.match(/^(\s*)/);
                const indentLevel = leadingMatch ? leadingMatch[1].length : 0;

                if (indentLevel > 0) {
                    indentSizes.set(indentLevel, (indentSizes.get(indentLevel) || 0) + 1);
                }
            }

            // Find the most common indent size (1, 2, 4, etc.)
            let detectedIndent = this.config.indentSize || 4;
            if (indentSizes.size > 0) {
                const sortedIndents = Array.from(indentSizes.entries())
                    .sort((a, b) => b[1] - a[1]);
                detectedIndent = sortedIndents[0][0];

                // If the most common is a multiple, use the GCD of all indents
                const allIndents = Array.from(indentSizes.keys()).sort((a, b) => a - b);
                if (allIndents.length > 1) {
                    const gcd = (a, b) => b === 0 ? a : gcd(b, a % b);
                    detectedIndent = allIndents.reduce(gcd);
                }
            }

            // Now check for lines that don't follow the detected pattern
            for (let i = 0; i < lines.length; i++) {
                const line = lines[i];
                const lineNum = i + 1;
                const trimmed = line.trim();

                // Skip empty lines and comments
                if (!trimmed || trimmed.startsWith(';')) continue;

                // Labels should be at column 1
                const isLabel = RE_LABEL.test(line);
                if (isLabel) {
                    // Check if label is NOT at column 1
                    if (line[0] === ' ' || line[0] === '\t') {
                        const meta = this.getRuleMeta(
                            'M033-LABEL-INDENTED',
                            'Label should be at column 1 (not indented)',
                            'Labels must start at the beginning of the line'
                        );

                        issues.push({
                            ruleId: 'M033',
                            severity: 'warning',
                            line: lineNum,
                            column: 1,
                            message: meta.message,
                            description: meta.description,
                            autofix: true,
                            fix: { removeIndentation: true }
                        });
                    }
                    continue;
                }

                // Executable lines should be indented
                const leadingMatch = line.match(/^(\s*)/);
                const leading = leadingMatch ? leadingMatch[1] : '';
                const indentLevel = leading.length;

                // Commands at column 1 (not indented) - this is wrong
                if (indentLevel === 0) {
                    const meta = this.getRuleMeta(
                        'M033-NO-INDENT',
                        'Command should be indented (not at column 1 like a label)',
                        'Indent commands to distinguish them from labels'
                    );

                    issues.push({
                        ruleId: 'M033',
                        severity: 'warning',
                        line: lineNum,
                        column: 1,
                        message: meta.message,
                        description: meta.description,
                        autofix: true,
                        fix: { reindentLine: true, expectedIndent: detectedIndent, currentIndent: 0 }
                    });
                }

                // Only warn if indent is inconsistent AND not a simple indent
                // (allow 1, 2, 3, 4, etc. as long as it's consistent in context)
                // Skip this check - too strict and annoying
            }
        }

        /**
         * Enhanced variable hygiene - detect unused NEWs
         */
        checkVariableHygiene(lines, issues) {
            const RE_LABEL = /^([A-Z%][A-Z0-9]*)/i;
            const newedVars = new Map(); // varName -> line number where NEW'd
            const usedVars = new Set();
            let currentTag = null;

            for (let i = 0; i < lines.length; i++) {
                const line = lines[i];
                const lineNum = i + 1;
                const trimmed = line.trim();

                if (!trimmed || trimmed.startsWith(';')) continue;

                // New tag - reset scope
                const labelMatch = line.match(RE_LABEL);
                if (labelMatch && line[0] !== ' ' && line[0] !== '\t') {
                    currentTag = labelMatch[1];
                    newedVars.clear();
                    usedVars.clear();
                    continue;
                }

                const codePart = this.extractCodeBeforeComment(trimmed);

                // Track NEW declarations
                const newRegex = /\b(?:NEW|N)\s+([^;]+)/gi;
                let newMatch;
                while ((newMatch = newRegex.exec(codePart)) !== null) {
                    const list = newMatch[1].split(',').map(t => t.trim()).filter(Boolean);
                    list.forEach(token => {
                        const varName = token.replace(/^[@*]*/, '').split('(')[0];
                        if (varName && /^[A-Z%][A-Z0-9]*$/i.test(varName)) {
                            newedVars.set(varName.toUpperCase(), lineNum);
                        }
                    });
                }

                // Track variable usage (SET, IF, WRITE, etc.)
                const varRx = /\b([A-Z%][A-Z0-9]*)\s*(?:\(|=|>|<|\+|\-|\*|\/)/gi;
                let varMatch;
                while ((varMatch = varRx.exec(codePart)) !== null) {
                    const varName = varMatch[1].toUpperCase();
                    // Skip MUMPS commands
                    if (!this.isCommandToken(varName)) {
                        usedVars.add(varName);
                    }
                }
            }

            // Check for NEW'd but never used (optional - info level)
            for (const [varName, newLine] of newedVars.entries()) {
                if (!usedVars.has(varName)) {
                    const meta = this.getRuleMeta(
                        'M037-UNUSED-NEW',
                        `Variable '${varName}' is NEW'd but never used`,
                        'Remove unused variable declarations'
                    );

                    issues.push({
                        ruleId: 'M037',
                        severity: 'info',
                        line: newLine,
                        column: 1,
                        message: meta.message,
                        description: meta.description
                    });
                }
            }
        }

        /**
         * GOTO Spaghetti Detector - M034, M035, M036
         */
        analyzeGotoPatterns(lines, issues) {
            const RE_LABEL = /^([A-Z%][A-Z0-9]*)/i;
            const RE_GOTO = /\b(?:GOTO|G)\s+([A-Z%][A-Z0-9]*)/gi;

            const labelLines = new Map(); // labelName -> lineNumber
            const gotoStatements = []; // { from: lineNum, to: labelName, line }
            let currentTag = null;
            let currentTagLine = 0;
            let gotoCountPerTag = 0;

            // First pass: find all labels
            for (let i = 0; i < lines.length; i++) {
                const line = lines[i];
                const labelMatch = line.match(RE_LABEL);
                if (labelMatch && line[0] !== ' ' && line[0] !== '\t') {
                    labelLines.set(labelMatch[1].toUpperCase(), i + 1);
                }
            }

            // Second pass: analyze GOTO statements
            for (let i = 0; i < lines.length; i++) {
                const line = lines[i];
                const lineNum = i + 1;
                const trimmed = line.trim();

                if (!trimmed || trimmed.startsWith(';')) continue;

                // Track current tag
                const labelMatch = line.match(RE_LABEL);
                if (labelMatch && line[0] !== ' ' && line[0] !== '\t') {
                    // Check previous tag for excessive GOTOs
                    if (currentTag && gotoCountPerTag > this.config.maxGotoPerTag) {
                        const meta = this.getRuleMeta(
                            'M034-EXCESSIVE-GOTO',
                            `Tag '${currentTag}' has ${gotoCountPerTag} GOTO statements (max ${this.config.maxGotoPerTag} recommended)`,
                            'Excessive GOTO usage makes code hard to follow; consider refactoring'
                        );

                        issues.push({
                            ruleId: 'M034',
                            severity: 'warning',
                            line: currentTagLine,
                            column: 1,
                            message: meta.message,
                            description: meta.description
                        });
                    }

                    currentTag = labelMatch[1].toUpperCase();
                    currentTagLine = lineNum;
                    gotoCountPerTag = 0;
                    continue;
                }

                // Find GOTO statements
                const codePart = this.extractCodeBeforeComment(trimmed);
                RE_GOTO.lastIndex = 0;
                let gotoMatch;
                while ((gotoMatch = RE_GOTO.exec(codePart)) !== null) {
                    const targetLabel = gotoMatch[1].toUpperCase();
                    const targetLine = labelLines.get(targetLabel);

                    gotoCountPerTag++;

                    gotoStatements.push({
                        from: lineNum,
                        to: targetLabel,
                        toLine: targetLine || null,
                        fromTag: currentTag
                    });

                    // M035: Backward GOTO (jump up)
                    if (this.config.warnBackwardGoto && targetLine && targetLine < lineNum) {
                        const meta = this.getRuleMeta(
                            'M035-BACKWARD-GOTO',
                            `Backward GOTO to '${targetLabel}' (line ${targetLine})`,
                            'Backward jumps can create loops and make code harder to understand'
                        );

                        issues.push({
                            ruleId: 'M035',
                            severity: 'info',
                            line: lineNum,
                            column: line.toUpperCase().indexOf('GOTO') !== -1
                                ? line.toUpperCase().indexOf('GOTO') + 1
                                : line.toUpperCase().indexOf(' G ') + 1,
                            message: meta.message,
                            description: meta.description
                        });
                    }
                }
            }

            // M036: Detect spaghetti patterns (complex jump networks)
            if (this.config.detectGotoSpaghetti) {
                const spaghettiScore = this.calculateSpaghettiScore(gotoStatements, labelLines);
                if (spaghettiScore > 10) {
                    const meta = this.getRuleMeta(
                        'M036-GOTO-SPAGHETTI',
                        `Complex GOTO pattern detected (spaghetti score: ${spaghettiScore})`,
                        'Multiple interconnected GOTOs create hard-to-maintain code; consider refactoring'
                    );

                    issues.push({
                        ruleId: 'M036',
                        severity: 'warning',
                        line: 1,
                        column: 1,
                        message: meta.message,
                        description: meta.description
                    });
                }
            }

            // Return GOTO data for visualization
            return {
                labels: Object.fromEntries(labelLines),
                gotos: gotoStatements,
                spaghettiScore: this.calculateSpaghettiScore(gotoStatements, labelLines),
                totalGotos: gotoStatements.length,
                backwardGotos: gotoStatements.filter(g => g.toLine && g.toLine < g.from).length,
                forwardGotos: gotoStatements.filter(g => g.toLine && g.toLine > g.from).length
            };
        }

        /**
         * Calculate "spaghetti score" - higher = more complex
         */
        calculateSpaghettiScore(gotos, labels) {
            if (gotos.length === 0) return 0;

            let score = 0;

            // Base score: number of GOTOs
            score += gotos.length * 2;

            // Backward jumps increase score more
            const backward = gotos.filter(g => g.toLine && g.toLine < g.from).length;
            score += backward * 3;

            // Crossing jumps (spaghetti pattern)
            for (let i = 0; i < gotos.length; i++) {
                for (let j = i + 1; j < gotos.length; j++) {
                    const g1 = gotos[i];
                    const g2 = gotos[j];
                    if (!g1.toLine || !g2.toLine) continue;

                    // Check if jumps cross each other
                    const cross = (
                        (g1.from < g2.from && g1.toLine > g2.toLine) ||
                        (g1.from > g2.from && g1.toLine < g2.toLine)
                    );

                    if (cross) score += 4;
                }
            }

            return score;
        }
    }

    // Replace global linter
    window.MUMPSLinter = MUMPSLinterEnhanced;
    console.log('[Linter Enhanced] Extended linter loaded with advanced rules');

})();
