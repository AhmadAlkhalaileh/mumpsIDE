/**
 * MUMPS Symbol Indexer for Smart Rename
 * Indexes tag definitions and references across workspace for safe cross-routine rename
 */

(() => {
    class MumpsSymbolIndexer {
        constructor() {
            // Map: tagName (uppercase) -> [{ routine, line, filePath }]
            this.tagDefinitions = new Map();

            // Map: tagName (uppercase) -> [{ routine, line, column, pattern, kind, confidence, filePath }]
            this.tagReferences = new Map();

            // Map: routineName (uppercase) -> { tags: Set, filePath }
            this.routineIndex = new Map();

            // Indexing state
            this.isIndexing = false;
            this.lastIndexTime = null;
        }

        /**
         * Index workspace using existing CallIndexer
         * @param {CallIndexer} callIndexer - Existing call indexer instance
         */
        buildFromCallIndexer(callIndexer) {
            if (!callIndexer?.index) return;

            this.clear();

            // Process each routine in the call index
            for (const [routineName, routineData] of callIndexer.index.entries()) {
                const routineUpper = String(routineName).toUpperCase();
                const filePath = routineData.filePath || '';

                // Index tag definitions
                if (Array.isArray(routineData.labels)) {
                    const tagSet = new Set();

                    routineData.labels.forEach(labelName => {
                        const tagUpper = String(labelName).toUpperCase();
                        tagSet.add(tagUpper);

                        if (!this.tagDefinitions.has(tagUpper)) {
                            this.tagDefinitions.set(tagUpper, []);
                        }

                        this.tagDefinitions.get(tagUpper).push({
                            routine: routineName,
                            tag: labelName,
                            filePath,
                            // Line number not available in CallIndexer - will need file read
                            line: null
                        });
                    });

                    this.routineIndex.set(routineUpper, {
                        tags: tagSet,
                        filePath,
                        name: routineName
                    });
                }

                // Index tag references from calls
                if (Array.isArray(routineData.calls)) {
                    routineData.calls.forEach(call => {
                        const labelName = call.calleeLabel;
                        if (!labelName) return;

                        const tagUpper = String(labelName).toUpperCase();
                        const targetRoutine = call.callee;
                        const targetRoutineUpper = String(targetRoutine).toUpperCase();

                        // Determine confidence based on call type
                        let confidence = 'low';
                        let kind = 'unknown';

                        if (call.callType === 'DO' || call.callType === 'DO_INTERNAL') {
                            kind = 'do';
                            confidence = call.isInternal ? 'high' : 'high'; // DO TAG^ROU is safe
                        } else if (call.callType === 'GOTO' || call.callType === 'GOTO_INTERNAL') {
                            kind = 'goto';
                            confidence = call.isInternal ? 'high' : 'high'; // GOTO TAG^ROU is safe
                        } else if (call.callType === 'EXTRINSIC') {
                            kind = 'extrinsic';
                            confidence = 'high'; // $$TAG^ROU is safe
                        }

                        // Only cross-routine or internal references
                        const isCrossRoutine = !call.isInternal && targetRoutineUpper !== routineUpper;
                        const isInternal = call.isInternal || targetRoutineUpper === routineUpper;

                        if (!this.tagReferences.has(tagUpper)) {
                            this.tagReferences.set(tagUpper, []);
                        }

                        this.tagReferences.get(tagUpper).push({
                            routine: routineName,
                            targetRoutine,
                            line: call.range?.startLine || null,
                            column: call.range?.startCol || null,
                            kind,
                            confidence,
                            isInternal,
                            isCrossRoutine,
                            filePath: routineData.filePath,
                            pattern: this._buildPattern(call)
                        });
                    });
                }
            }

            this.lastIndexTime = new Date();
        }

        /**
         * Build pattern string from call data
         */
        _buildPattern(call) {
            const cmd = call.callType === 'EXTRINSIC' ? '$$' :
                       call.callType === 'GOTO' || call.callType === 'GOTO_INTERNAL' ? 'G' : 'D';
            const label = call.calleeLabel || '';
            const routine = call.isInternal ? '' : `^${call.callee || ''}`;
            return `${cmd} ${label}${routine}`;
        }

        /**
         * Get all definitions of a tag
         * @param {string} tagName - Tag name (case-insensitive)
         * @returns {Array} Array of tag definition locations
         */
        getTagDefinitions(tagName) {
            const tagUpper = String(tagName || '').toUpperCase();
            return this.tagDefinitions.get(tagUpper) || [];
        }

        /**
         * Get all references to a tag
         * @param {string} tagName - Tag name (case-insensitive)
         * @param {Object} options - Filter options
         * @returns {Array} Array of tag references
         */
        getTagReferences(tagName, options = {}) {
            const tagUpper = String(tagName || '').toUpperCase();
            let refs = this.tagReferences.get(tagUpper) || [];

            if (options.routine) {
                const routineUpper = String(options.routine).toUpperCase();
                refs = refs.filter(r => String(r.routine).toUpperCase() === routineUpper);
            }

            if (options.targetRoutine) {
                const targetUpper = String(options.targetRoutine).toUpperCase();
                refs = refs.filter(r => String(r.targetRoutine).toUpperCase() === targetUpper);
            }

            if (options.minConfidence) {
                const levels = { low: 0, medium: 1, high: 2 };
                const minLevel = levels[options.minConfidence] || 0;
                refs = refs.filter(r => (levels[r.confidence] || 0) >= minLevel);
            }

            if (options.crossRoutineOnly) {
                refs = refs.filter(r => r.isCrossRoutine);
            }

            return refs;
        }

        /**
         * Check if a tag name already exists in a routine (collision detection)
         * @param {string} tagName - Tag name to check
         * @param {string} routineName - Routine name
         * @returns {boolean} True if tag exists in routine
         */
        hasTagInRoutine(tagName, routineName) {
            const tagUpper = String(tagName || '').toUpperCase();
            const routineUpper = String(routineName || '').toUpperCase();
            const routineData = this.routineIndex.get(routineUpper);
            return routineData?.tags?.has(tagUpper) || false;
        }

        /**
         * Get all routines that define a specific tag
         * @param {string} tagName - Tag name
         * @returns {Array} Array of routine names
         */
        getRoutinesWithTag(tagName) {
            const defs = this.getTagDefinitions(tagName);
            return [...new Set(defs.map(d => d.routine))];
        }

        /**
         * Clear all indexes
         */
        clear() {
            this.tagDefinitions.clear();
            this.tagReferences.clear();
            this.routineIndex.clear();
            this.lastIndexTime = null;
        }

        /**
         * Get statistics
         */
        getStats() {
            return {
                totalTags: this.tagDefinitions.size,
                totalReferences: Array.from(this.tagReferences.values())
                    .reduce((sum, refs) => sum + refs.length, 0),
                totalRoutines: this.routineIndex.size,
                lastIndexTime: this.lastIndexTime
            };
        }
    }

    if (typeof window !== 'undefined') {
        window.AhmadIDEModules = window.AhmadIDEModules || {};
        window.AhmadIDEModules.mumps = window.AhmadIDEModules.mumps || {};
        window.AhmadIDEModules.mumps.MumpsSymbolIndexer = MumpsSymbolIndexer;
    }

    if (typeof module !== 'undefined' && module.exports) {
        module.exports = { MumpsSymbolIndexer };
    }
})();
