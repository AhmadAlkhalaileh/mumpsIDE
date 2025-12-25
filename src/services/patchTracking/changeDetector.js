/**
 * Change Detector
 * Compares localr vs routines folders to detect changes
 */

class ChangeDetector {
    constructor() {
        this.dockerScanner = null;
    }

    /**
     * Initialize detector with Docker scanner
     * @param {Object} dockerScanner - Docker scanner instance
     */
    init(dockerScanner) {
        this.dockerScanner = dockerScanner;
    }

    /**
     * Detect changes between localr and routines
     * @param {string} connectionId - SSH connection ID
     * @param {Object} scanResult - Scan result from Docker scanner
     * @returns {Promise<Object>} Change detection result
     */
    async detectChanges(connectionId, scanResult) {
        if (!this.dockerScanner) {
            throw new Error('Change detector not initialized. Call init() first.');
        }

        console.log('[Change Detector] Analyzing changes...');

        const { localr, routines, paths, environment } = scanResult;

        console.log(`[Change Detector] Found ${localr.length} files in localr, ${routines.length} in routines`);

        // Build maps for faster lookup
        const localrMap = new Map();
        localr.forEach(file => localrMap.set(file.name, file));

        const routinesMap = new Map();
        routines.forEach(file => routinesMap.set(file.name, file));

        const changes = {
            modified: [],
            addedToLocalr: [],
            removedFromLocalr: [],
            unchanged: []
        };

        // Check for modifications and additions
        let processed = 0;
        const total = localrMap.size;
        for (const [routineName, localrFile] of localrMap) {
            processed++;
            if (processed % 100 === 0) {
                console.log(`[Change Detector] Progress: ${processed}/${total} files analyzed...`);
            }

            const routinesFile = routinesMap.get(routineName);

            if (!routinesFile) {
                // File exists in localr but not in routines (added)
                changes.addedToLocalr.push({
                    routine: routineName,
                    localrPath: localrFile.fullPath,
                    status: 'added'
                });
            } else {
                // File exists in both - compare using pre-computed checksums (FAST!)
                const comparison = await this.dockerScanner.compareRoutines(
                    connectionId,
                    localrFile.fullPath,
                    routinesFile.fullPath,
                    localrFile.checksum,      // Pass pre-computed checksum
                    routinesFile.checksum     // Pass pre-computed checksum
                );

                if (comparison.isDifferent) {
                    changes.modified.push({
                        routine: routineName,
                        localrPath: localrFile.fullPath,
                        routinesPath: routinesFile.fullPath,
                        status: 'modified',
                        localr: comparison.localr,
                        routines: comparison.routines,
                        diff: {
                            linesDiff: comparison.localr.lines - comparison.routines.lines,
                            sizeDiff: comparison.localr.size - comparison.routines.size
                        }
                    });
                } else {
                    changes.unchanged.push({
                        routine: routineName,
                        status: 'unchanged'
                    });
                }
            }
        }

        // Check for removals (exists in routines but not in localr)
        for (const [routineName, routinesFile] of routinesMap) {
            if (!localrMap.has(routineName)) {
                changes.removedFromLocalr.push({
                    routine: routineName,
                    routinesPath: routinesFile.fullPath,
                    status: 'removed'
                });
            }
        }

        const result = {
            environment,
            detectedAt: new Date().toISOString(),
            summary: {
                total: localr.length + routines.length - changes.unchanged.length,
                modified: changes.modified.length,
                added: changes.addedToLocalr.length,
                removed: changes.removedFromLocalr.length,
                unchanged: changes.unchanged.length
            },
            changes
        };

        console.log('[Change Detector] Detection complete:', result.summary);

        return result;
    }

    /**
     * Correlate changes with patch metadata
     * @param {Object} changeResult - Change detection result
     * @param {Object} patchMetadata - Patch metadata from KIDS file
     * @returns {Object} Correlation result
     */
    correlateWithPatch(changeResult, patchMetadata) {
        console.log('[Change Detector] Correlating changes with patch:', patchMetadata.patchId);

        const patchRoutines = new Set(patchMetadata.routines);
        const matched = [];
        const unmatched = [];

        // Check which modified routines match the patch
        changeResult.changes.modified.forEach(change => {
            if (patchRoutines.has(change.routine)) {
                matched.push({
                    ...change,
                    matchedToPatch: patchMetadata.patchId
                });
            } else {
                unmatched.push(change);
            }
        });

        // Check which added routines match the patch (new files in localr)
        changeResult.changes.addedToLocalr.forEach(change => {
            if (patchRoutines.has(change.routine)) {
                matched.push({
                    ...change,
                    matchedToPatch: patchMetadata.patchId
                });
            } else {
                unmatched.push(change);
            }
        });

        // Check for patch routines that weren't found in changes
        const missing = [];
        patchRoutines.forEach(routineName => {
            const foundInModified = changeResult.changes.modified.some(c => c.routine === routineName);
            const foundInAdded = changeResult.changes.addedToLocalr.some(c => c.routine === routineName);
            if (!foundInModified && !foundInAdded) {
                missing.push(routineName);
            }
        });

        const correlation = {
            patchId: patchMetadata.patchId,
            patchRoutines: patchMetadata.routines,
            matched,
            unmatched,
            missing,
            matchRate: (matched.length / patchRoutines.size) * 100,
            summary: {
                totalInPatch: patchRoutines.size,
                matchedChanges: matched.length,
                unmatchedChanges: unmatched.length,
                missingRoutines: missing.length
            }
        };

        console.log('[Change Detector] Correlation complete:', {
            matchRate: `${correlation.matchRate.toFixed(1)}%`,
            matched: matched.length,
            missing: missing.length
        });

        return correlation;
    }

    /**
     * Get detailed diff for a routine
     * @param {string} localrContent - Content from localr
     * @param {string} routinesContent - Content from routines
     * @returns {Object} Detailed diff information
     */
    getDetailedDiff(localrContent, routinesContent) {
        const localrLines = localrContent.split('\n');
        const routinesLines = routinesContent.split('\n');

        const diff = {
            added: [],
            removed: [],
            modified: []
        };

        const maxLines = Math.max(localrLines.length, routinesLines.length);

        for (let i = 0; i < maxLines; i++) {
            const localrLine = localrLines[i] || '';
            const routinesLine = routinesLines[i] || '';

            if (localrLine !== routinesLine) {
                if (!routinesLine) {
                    diff.added.push({ line: i + 1, content: localrLine });
                } else if (!localrLine) {
                    diff.removed.push({ line: i + 1, content: routinesLine });
                } else {
                    diff.modified.push({
                        line: i + 1,
                        before: routinesLine,
                        after: localrLine
                    });
                }
            }
        }

        return diff;
    }

    /**
     * Validate changes against patch metadata
     * @param {Object} correlation - Correlation result
     * @returns {Object} Validation result
     */
    validateChanges(correlation) {
        const warnings = [];
        const errors = [];

        // Check if match rate is acceptable
        if (correlation.matchRate < 100) {
            warnings.push(`Match rate is ${correlation.matchRate.toFixed(1)}% - some routines don't match`);
        }

        // Check for missing routines
        if (correlation.missing.length > 0) {
            errors.push(`Missing routines from patch: ${correlation.missing.join(', ')}`);
        }

        // Check for unmatched changes
        if (correlation.unmatched.length > 0) {
            warnings.push(`Found ${correlation.unmatched.length} changes not in patch`);
        }

        const isValid = errors.length === 0;

        return {
            isValid,
            warnings,
            errors,
            canCommit: isValid
        };
    }
}

// Export singleton instance
module.exports = new ChangeDetector();
