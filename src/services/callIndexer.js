/**
 * MUMPS Call Indexer Service (Browser-compatible version)
 * Static analysis of routine calls for Call Hierarchy feature
 *
 * Detects:
 * - DO ^ROUTINE
 * - DO LABEL^ROUTINE
 * - GOTO LABEL^ROUTINE
 * - $$FUNCTION^ROUTINE
 * - GOTO LABEL (internal)
 * - DO LABEL (internal)
 */

(() => {
    class CallIndexer {
        constructor() {
            // Map: routineName -> { calls: [{caller, callee, type, range, filePath}], labels: [string] }
            this.index = new Map();

            // Map: filePath -> routineName (for quick lookup)
            this.fileToRoutine = new Map();

            // Cache of file modification times for invalidation
            this.fileTimes = new Map();

            // Indexing in progress flag
            this.indexing = false;

            // Statistics
            this.stats = {
                totalRoutines: 0,
                totalCalls: 0,
                lastIndexTime: null,
                indexDuration: 0
            };
        }

        /**
         * Index all .m files in a directory
         * @param {string} directoryPath - Path to routines directory
         * @param {Function} progressCallback - Optional callback(current, total)
         * @returns {Promise<Object>} Indexing statistics
         */
        async indexDirectory(directoryPath, progressCallback = null) {
            if (this.indexing) {
                console.warn('[Call Indexer] Indexing already in progress');
                return null;
            }

            this.indexing = true;
            const startTime = Date.now();

            try {
                console.log('[Call Indexer] Starting index of:', directoryPath);

                // Find all .m files using IPC
                const files = await this.findMumpsFiles(directoryPath);
                console.log(`[Call Indexer] Found ${files.length} MUMPS files`);

                // Index each file
                let processed = 0;
                for (const filePath of files) {
                    await this.indexFile(filePath);
                    processed++;

                    if (progressCallback && processed % 10 === 0) {
                        progressCallback(processed, files.length);
                    }
                }

                this.stats.lastIndexTime = new Date();
                this.stats.indexDuration = Date.now() - startTime;
                this.stats.totalRoutines = this.index.size;
                this.stats.totalCalls = Array.from(this.index.values())
                    .reduce((sum, entry) => sum + entry.calls.length, 0);

                console.log('[Call Indexer] Indexing complete:', this.stats);

                return this.stats;

            } catch (error) {
                console.error('[Call Indexer] Indexing error:', error);
                throw error;
            } finally {
                this.indexing = false;
            }
        }

        /**
         * Find all .m files in directory using IPC
         * @param {string} dir - Directory path
         * @returns {Promise<string[]>} Array of file paths
         */
        async findMumpsFiles(dir) {
            // Use IPC to get file list from main process
            if (window.ahmadIDE?.listFiles) {
                try {
                    const result = await window.ahmadIDE.listFiles(dir, '*.m');
                    return result || [];
                } catch (error) {
                    console.error('[Call Indexer] Error listing files:', error);
                    return [];
                }
            }

            // Fallback: empty array
            console.warn('[Call Indexer] No file listing API available');
            return [];
        }

        /**
         * Index a single file
         * @param {string} filePath - Path to .m file
         * @returns {Promise<void>}
         */
        async indexFile(filePath) {
            try {
                // Read file using IPC
                let content = '';
                let lastModified = Date.now();

                if (window.ahmadIDE?.readFile) {
                    const result = await window.ahmadIDE.readFile(filePath);
                    if (result && result.content) {
                        content = result.content;
                        lastModified = result.mtime || Date.now();
                    }
                } else {
                    console.warn('[Call Indexer] No file reading API available');
                    return;
                }

                // Check if file needs reindexing
                const cachedTime = this.fileTimes.get(filePath);
                if (cachedTime && cachedTime >= lastModified) {
                    // File hasn't changed, skip
                    return;
                }

                // Extract routine name from file path
                const routineName = filePath.split('/').pop().split('\\').pop().replace(/\.m$/i, '');

                // Parse file for calls and labels
                const { calls, labels } = this.parseRoutine(content, routineName, filePath);

                // Update index
                this.index.set(routineName, { calls, labels, filePath });
                this.fileToRoutine.set(filePath, routineName);
                this.fileTimes.set(filePath, lastModified);

            } catch (error) {
                console.error('[Call Indexer] Error indexing file:', filePath, error);
            }
        }

        /**
         * Parse routine content for calls and labels
         * @param {string} content - File content
         * @param {string} routineName - Routine name
         * @param {string} filePath - File path
         * @returns {Object} { calls: [], labels: [] }
         */
        parseRoutine(content, routineName, filePath) {
            const calls = [];
            const labels = new Set();
            const lines = content.split('\n');

            let currentLabel = null;

            for (let i = 0; i < lines.length; i++) {
                const line = lines[i];
                const lineNumber = i + 1;

                // Skip empty lines
                if (!line.trim()) continue;

                // Detect labels (starts at column 1, not indented)
                if (line.length > 0 && line[0] !== ' ' && line[0] !== '\t' && line[0] !== ';') {
                    const labelMatch = line.match(/^([A-Z%][A-Z0-9]*)/i);
                    if (labelMatch) {
                        const labelName = labelMatch[1];
                        labels.add(labelName);
                        currentLabel = labelName;
                    }
                }

                // Skip comment-only lines
                if (line.trim().startsWith(';')) continue;

                // Remove inline comments (conservative - only if preceded by space)
                let codePart = line;
                const commentIndex = line.indexOf(' ;');
                if (commentIndex !== -1) {
                    codePart = line.substring(0, commentIndex);
                }

                // Detect calls in code part (pass routineName for internal transfer detection)
                const detectedCalls = this.detectCalls(codePart, lineNumber, currentLabel || routineName, routineName);
                calls.push(...detectedCalls.map(call => ({
                    ...call,
                    filePath,
                    callerRoutine: routineName
                })));
            }

            return {
                calls,
                labels: Array.from(labels)
            };
        }

        /**
         * Detect calls in a line of code
         * @param {string} code - Code line (comments removed)
         * @param {number} lineNumber - Line number
         * @param {string} callerLabel - Current label context
         * @param {string} routineName - Current routine name (for internal transfers)
         * @returns {Array} Array of call objects
         */
        detectCalls(code, lineNumber, callerLabel, routineName) {
            const calls = [];

            // Pattern 1: DO/GOTO with caret (DO ^ROUTINE or GOTO ^ROUTINE)
            const doGotoCaretRegex = /\b(DO|GOTO|G)\s+\^([A-Z%][A-Z0-9]*)/gi;
            let match;

            while ((match = doGotoCaretRegex.exec(code)) !== null) {
                const callType = match[1].toUpperCase() === 'G' ? 'GOTO' : match[1].toUpperCase();
                const calleeRoutine = match[2];
                const startCol = match.index;

                calls.push({
                    caller: callerLabel,
                    callee: calleeRoutine,
                    calleeLabel: null,
                    callType: callType,
                    range: {
                        startLine: lineNumber,
                        startCol: startCol,
                        endLine: lineNumber,
                        endCol: startCol + match[0].length
                    }
                });
            }

            // Pattern 2: LABEL^ROUTINE
            const labelRoutineRegex = /\b(DO|GOTO|G|\$\$)\s*([A-Z%][A-Z0-9]*)\^([A-Z%][A-Z0-9]*)/gi;

            while ((match = labelRoutineRegex.exec(code)) !== null) {
                const prefix = match[1].toUpperCase();
                let callType;

                if (prefix === '$$') {
                    callType = 'EXTRINSIC';
                } else if (prefix === 'G' || prefix === 'GOTO') {
                    callType = 'GOTO';
                } else {
                    callType = 'DO';
                }

                const calleeLabel = match[2];
                const calleeRoutine = match[3];
                const startCol = match.index;

                calls.push({
                    caller: callerLabel,
                    callee: calleeRoutine,
                    calleeLabel: calleeLabel,
                    callType: callType,
                    range: {
                        startLine: lineNumber,
                        startCol: startCol,
                        endLine: lineNumber,
                        endCol: startCol + match[0].length
                    }
                });
            }

            // Pattern 3: Internal GOTO/DO LABEL (no caret - same routine)
            const internalTransferRegex = /\b(DO|GOTO|G)\s+([A-Z%][A-Z0-9]*)(?!\^)/gi;

            while ((match = internalTransferRegex.exec(code)) !== null) {
                const prefix = match[1].toUpperCase();
                const targetLabel = match[2];
                const startCol = match.index;

                // Skip if this looks like a variable (lowercase after first char) or keyword
                if (/[a-z]/.test(targetLabel.substring(1))) continue;

                const callType = (prefix === 'G' || prefix === 'GOTO') ? 'GOTO_INTERNAL' : 'DO_INTERNAL';

                calls.push({
                    caller: callerLabel,
                    callee: routineName,
                    calleeLabel: targetLabel,
                    callType: callType,
                    isInternal: true,
                    range: {
                        startLine: lineNumber,
                        startCol: startCol,
                        endLine: lineNumber,
                        endCol: startCol + match[0].length
                    }
                });
            }

            return calls;
        }

        /**
         * Get outgoing calls from a routine or label
         * @param {string} routineName - Routine name
         * @param {string} labelName - Optional label name
         * @returns {Array} Array of calls
         */
        getOutgoingCalls(routineName, labelName = null) {
            const entry = this.index.get(routineName);
            if (!entry) return [];

            if (labelName) {
                // Filter calls from specific label
                return entry.calls.filter(call => call.caller === labelName);
            }

            // Return all calls from routine
            return entry.calls;
        }

        /**
         * Get incoming calls to a routine or label
         * @param {string} routineName - Routine name
         * @param {string} labelName - Optional label name
         * @returns {Array} Array of calls
         */
        getIncomingCalls(routineName, labelName = null) {
            const incomingCalls = [];

            // Search all indexed routines for calls to this routine
            for (const [callerRoutine, entry] of this.index.entries()) {
                for (const call of entry.calls) {
                    if (call.callee === routineName) {
                        if (labelName === null || call.calleeLabel === labelName) {
                            incomingCalls.push({
                                ...call,
                                callerRoutine: callerRoutine
                            });
                        }
                    }
                }
            }

            return incomingCalls;
        }

        /**
         * Get all labels in a routine
         * @param {string} routineName - Routine name
         * @returns {Array} Array of label names
         */
        getLabels(routineName) {
            const entry = this.index.get(routineName);
            return entry ? entry.labels : [];
        }

        /**
         * Get routine info
         * @param {string} routineName - Routine name
         * @returns {Object|null} Routine info
         */
        getRoutineInfo(routineName) {
            const entry = this.index.get(routineName);
            if (!entry) return null;

            return {
                name: routineName,
                filePath: entry.filePath,
                labels: entry.labels,
                outgoingCallsCount: entry.calls.length,
                incomingCallsCount: this.getIncomingCalls(routineName).length
            };
        }

        /**
         * Invalidate file cache (call when file changes)
         * @param {string} filePath - Path to changed file
         */
        invalidateFile(filePath) {
            const routineName = this.fileToRoutine.get(filePath);
            if (routineName) {
                this.index.delete(routineName);
                this.fileToRoutine.delete(filePath);
                this.fileTimes.delete(filePath);
                console.log('[Call Indexer] Invalidated:', routineName);
            }
        }

        /**
         * Clear entire index
         */
        clearIndex() {
            this.index.clear();
            this.fileToRoutine.clear();
            this.fileTimes.clear();
            this.stats = {
                totalRoutines: 0,
                totalCalls: 0,
                lastIndexTime: null,
                indexDuration: 0
            };
            console.log('[Call Indexer] Index cleared');
        }

        /**
         * Get indexing statistics
         * @returns {Object} Stats object
         */
        getStatistics() {
            return { ...this.stats };
        }

        /**
         * Get all routine names
         * @returns {Array} Array of routine names
         */
        getAllRoutines() {
            return Array.from(this.index.keys()).sort();
        }

        /**
         * Search routines by name pattern
         * @param {string} pattern - Search pattern (case-insensitive)
         * @returns {Array} Array of matching routine names
         */
        searchRoutines(pattern) {
            const regex = new RegExp(pattern, 'i');
            return this.getAllRoutines().filter(name => regex.test(name));
        }
    }

    // Export to global scope
    window.AhmadIDEModules = window.AhmadIDEModules || {};
    window.AhmadIDEModules.callIndexer = new CallIndexer();
    console.log('[Call Indexer] Initialized and attached to window.AhmadIDEModules.callIndexer');
})();
